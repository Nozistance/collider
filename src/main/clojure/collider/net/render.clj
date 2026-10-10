(ns collider.net.render
  "Packets for each player from the tick."
  (:require [collider.game.deltas :as deltas]
            [collider.log :as log]
            [collider.net.render.audience :as audience]
            [collider.net.render.fx :as fx]
            [collider.net.render.join :as join]
            [collider.net.render.tracked :as tracked]
            [collider.net.render.view :as view])
  (:import (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

(defn- forget-packets [deltas]
  (for [[eid ds] deltas
        [tag _ _ gone] ds
        :when (and (= :tracking tag) (seq gone))]
    [eid {:packet :remove-entities :eids gone}]))

(defn- player-of [sight eid]
  (get-in (audience/own-level sight eid) [:entities eid]))

(defn- join-bursts [world sight ^Deltas deltas]
  (for [m (deltas/out-of deltas)
        :when (= :joined (:msg m))
        :let [eid (:to m)
              lv (audience/own-level sight eid)]
        p (join/join-packets world lv eid)]
    [eid p]))

(defn- entity-delta-packets [sight deltas pick]
  (for [[eid ds] deltas
        :when (pick eid)
        :let [lv (audience/own-level sight eid)]
        d ds
        p (case (first d)
            :chunks-sent (view/chunk-packets lv d)
            :tracking (tracked/tracking-packets lv d)
            nil)]
    [eid p]))

(defn- explosion-packets [sight m]
  (for [eid (audience/audience sight m)]
    [eid (fx/explode-packet m eid)]))

(defn- msg-packets [sight viewers m]
  (if (= :explosion (:msg m))
    (explosion-packets sight m)
    (let [pkts (fx/fx-packets (audience/level-of sight (:dim m)) m)]
      (when (seq pkts)
        (for [eid (audience/recipients sight viewers m)
              p pkts]
          [eid p])))))

(defn- level-entry-packets [sight ^Deltas deltas]
  (for [m (deltas/out-of deltas)
        :when (identical? :change-dimension (:msg m))
        :let [eid (:to m)]
        p (join/resent-packets (player-of sight eid))]
    [eid p]))

(defn- arrivals [^Deltas deltas]
  (into #{} (keep #(when (identical? :change-dimension (:msg %))
                     (:to %)))
        (deltas/out-of deltas)))

(defn- position? [p]
  (and (map? p) (identical? :player-position (:packet p))))

(defn- teleport-id ^long [sight eid ^long k]
  (let [e (player-of sight eid)]
    (mod (- (long (:tp-id e 1)) k) (long Integer/MAX_VALUE))))

(defn- teleported [[eid p]] (when (position? p) eid))

(defn- number-next [sight [acc remaining] [eid p :as pair]]
  (if (position? p)
    (let [k (dec (long (remaining eid)))
          q (assoc p :teleport-id (teleport-id sight eid k))]
      [(conj acc [eid q]) (assoc remaining eid k)])
    [(conj acc pair) remaining]))

(defn- numbered [sight pairs]
  (let [remaining (frequencies (keep teleported pairs))
        step #(number-next sight %1 %2)]
    (if (empty? remaining)
      pairs
      (first (reduce step [[] remaining] pairs)))))

(def ^:private moves-player #{:teleport :change-dimension})

(defn- teleports? [^Deltas deltas]
  (some #(moves-player (:msg %)) (deltas/out-of deltas)))

(defn- block-event-key [m]
  (when (= :block-event (:msg m))
    [(:dim m) (:pos m) (:action m) (:param m)]))

(defn- once-each [msgs]
  (let [seen (volatile! #{})]
    (filter (fn [m]
              (let [k (block-event-key m)]
                (or (nil? k)
                    (when-not (contains? @seen k)
                      (vswap! seen conj k)
                      true))))
            msgs)))

(defn- before-bodies?
  "Returns true when effect m goes out before the bodies that come and
  go this tick. A player is listed before others see it, and an entity
  puffs before it is gone."
  [m]
  (or (identical? :tab-add (:msg m))
      (and (identical? :status (:msg m)) (identical? :poof (:kind m)))))

(defn- ordered [world sight ^Deltas deltas]
  (let [es (deltas/entities-of deltas)
        viewers (delay (audience/viewer-index sight es))
        new (arrivals deltas)
        msgs (once-each (deltas/out-of deltas))
        msg-out (fn [ms] (mapcat #(msg-packets sight viewers %) ms))]
    (vec (concat
           (join-bursts world sight deltas)
           (msg-out (filter before-bodies? msgs))
           (entity-delta-packets sight es (complement new))
           (msg-out (remove before-bodies? msgs))
           (entity-delta-packets sight es new)
           (forget-packets es)
           (level-entry-packets sight deltas)))))

(defn render
  "Returns [eid packet] for every player after a tick of deltas.
  A player entering a level gets its chunks after all else."
  [world ^Deltas deltas]
  (let [sight (audience/sight-of world)
        pairs (ordered world sight deltas)
        pairs (if (teleports? deltas) (numbered sight pairs) pairs)]
    (doseq [f (get-in world [:hooks :packets-out])]
      (try (f world pairs)
           (catch Throwable t
             (log/once! :packets-out log/warn "packet watcher failed:"
                        (str t)))))
    pairs))
