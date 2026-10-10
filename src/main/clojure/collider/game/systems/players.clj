(ns collider.game.systems.players
  "The player list, logins and the tab header."
  (:require [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.systems.sleep :as sleep]
            [collider.log :as log])
  (:import (java.util Locale)))

(set! *warn-on-reflection* true)

(def latency-interval 600)

(def ^:private duplicate-login-reason
  "You logged in from another location")

(defn disconnect-deltas
  "Returns the deltas that drop player eid, entity e, with text. It
  leaves its bed, hears why, is cut off and leaves the level."
  [world eid e text]
  (concat
    (sleep/vacated-deltas world eid e)
    [(out/to eid (out/disconnect text))
     (out/to eid (out/close))
     [:remove-entity eid]]))

(defn- kicked-deltas [world eid]
  (disconnect-deltas world eid (get-in world [:entities eid])
                     duplicate-login-reason))

(defn- other-logins [world pname]
  (let [owner (get-in world [:players pname])]
    (for [[eid e] (:entities world)
          :when (and (= :player (:type e))
                     (= pname (:name e))
                     (not= eid owner))]
      eid)))

(defn- duplicate-login-deltas [world events]
  (mapcat (fn [[tag _ pname]]
            (when (= :player-join tag)
              (mapcat #(kicked-deltas world %)
                      (other-logins world pname))))
          events))

(defn- joined-deltas [world events]
  (for [[tag eid] events :when (= :player-join tag)
        d [(out/to eid (out/joined))
           (out/except eid (out/system-chat
                             (entity/joined-text
                               (get-in world [:entities eid]))))]]
    d))

(defn- left-deltas [world]
  (for [e (:quits world)]
    (out/all (out/system-chat (entity/left-text e)))))

(defn- add-entry [e]
  (cond-> {:uuid (:uuid e) :name (:name e) :ping (or (:ping e) 0)
           :game-mode (:game-mode e)}
    (:properties e) (assoc :properties (:properties e))))

(defn- join-list-deltas [joined all]
  (mapcat (fn [[eid e]]
            [(out/to eid (out/tab-add all))
             (out/except eid (out/tab-add [(add-entry e)]))])
          joined))

(defn- leave-list-deltas [world live left]
  (let [listed (:listed world)]
    (mapcat (fn [eid]
              (when-not (contains? live (get listed eid))
                [(out/all (out/tab-remove [(get listed eid)]))]))
            left)))

(defn- uuids-of [ps]
  (into {} (map (fn [[eid e]] [eid (:uuid e)])) ps))

(defn- latency-deltas [world all]
  (when (zero? (rem (long (:tick world)) latency-interval))
    [(out/all (out/tab-latency (all)))]))

(defn- listed-all? [listed ps]
  (and (= (count listed) (count ps))
       (every? (fn [[eid _]] (contains? listed eid)) ps)))

(defn- list-changes [world ps]
  (let [listed (:listed world)
        cur (uuids-of ps)
        joined (remove (fn [[eid _]] (contains? listed eid)) ps)
        left (sort (remove cur (keys listed)))
        all (mapv (comp add-entry val) ps)]
    (concat
      (join-list-deltas joined all)
      (leave-list-deltas world (into #{} (map val) cur) left)
      (latency-deltas world (constantly all))
      (when (or (seq joined) (seq left))
        [[:listed (uuids-of joined) left]]))))

(defn- list-deltas [world ps]
  (let [listed (:listed world)]
    (if (listed-all? listed ps)
      (latency-deltas world #(mapv (comp add-entry val) ps))
      (list-changes world ps))))

(def ^:private tab-header-interval 20)

(defn- fmt ^String [^String pattern v]
  (String/format Locale/ROOT pattern
                 (to-array [(double (or v 0.0))])))

(defn- server-title [commit]
  (if commit
    (str "Collider Server (" commit ")")
    "Collider Server"))

(defn- per-tick [x]
  (let [x (double (or x 0.0))]
    (if (< x 1000.0) (fmt "%.1f" x) (log/human-count x))))

(defn- tab-header-msg
  [commit {:keys [tps mspt p50-ms p99-ms deltas-per-tick]}]
  (out/tab-header (server-title commit)
                  (str "TPS " (fmt "%.1f" (or tps 20.0))
                       "\nMSPT " (fmt "%.2f" mspt) " ms"
                       "\np50 " (fmt "%.2f" p50-ms) " ms"
                       "\np99 " (fmt "%.2f" p99-ms) " ms"
                       "\n\u0394/t " (per-tick deltas-per-tick))))

(defn- tab-header-deltas [world events]
  (when-let [perf (:perf world)]
    (let [commit (get-in world [:config :commit])
          msg (tab-header-msg commit perf)]
      (concat
        (when (zero? (rem (long (:tick world)) tab-header-interval))
          [(out/all msg)])
        (for [[tag eid] events :when (= :player-join tag)]
          (out/to eid msg))))))

(defn swing-deltas
  "Returns the deltas of one swing a client plays itself.
  The server only passes it on, and only as often as an arm
  can swing."
  [world [tag eid hand]]
  (when (= :swing tag)
    (when-let [p (get-in world [:entities eid])]
      (let [t (:tick world)]
        (vec (player/swing-deltas eid p (or hand :main) t false))))))

(defn player-list
  {:wake :always :once true}
  [world d]
  (let [ps (level/player-entries world)
        joins (player/joins d)]
    (deltas/of-vec
      (into [] cat [(joined-deltas world joins)
                    (left-deltas world)
                    (duplicate-login-deltas world joins)
                    (list-deltas world ps)
                    (tab-header-deltas world joins)]))))
