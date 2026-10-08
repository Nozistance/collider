(ns collider.devtools.trace
  "What one player does, followed from packet to packet. The hooks
  hold vars, so a reloaded trace takes effect on the next tick."
  (:require [clojure.string :as str]
            [collider.cli :as cli]
            [collider.log :as log])
  (:import (java.util.concurrent ConcurrentLinkedQueue)))

(set! *warn-on-reflection* true)

(defonce ^:private state (atom nil))

(def ^:private quiet-packets
  #{:move-player-pos :move-player-pos-rot :move-player-rot
    :move-player-status-only :keep-alive :client-tick-end
    :chunk-batch-received})

(def ^:private quiet-events
  #{:move :keepalive-echo :client-tick-end :chunk-batch-ack})

(def ^:private quiet-deltas #{:track})

(defn- shown [x]
  (binding [*print-length* 8 *print-level* 4] (pr-str x)))

(defn- say! [kind & xs]
  (when-let [sink (:sink @state)]
    (sink (str (format "%-4s" kind) (str/join " " xs)))))

(defn- entity? [world id]
  (and (integer? id)
       (some #(contains? (:entities %) id) (vals (:levels world)))))

(defn- touched [world evs]
  (into #{} (comp cat (filter #(entity? world %))) evs))

(defn packet-in
  "Shows a packet the player sent and the event it became."
  [eid m ev]
  (when (and (= eid (:eid @state)) (not (quiet-packets (:packet m))))
    (say! "in" (:packet m) (shown (dissoc m :packet)) "->" (shown ev))))

(defn events
  "Shows the events of the player this tick, keeping them as they are."
  [world evs]
  (let [eid (:eid @state)
        mine (filterv #(and (= eid (nth % 1 nil))
                            (not (quiet-events (nth % 0))))
                      evs)]
    (swap! state assoc :ids (conj (touched world mine) eid)
           :active? (boolean (seq mine)))
    (doseq [ev mine] (say! "ev" (shown ev)))
    evs))

(defn- about [ids d]
  (let [x (nth d 1 nil)]
    (and (not (quiet-deltas (nth d 0)))
         (or (contains? ids x) (contains? ids (:to x))))))

(defn deltas
  "Shows the deltas of a phase about what the player touched."
  [_ ds]
  (let [{:keys [active? ids]} @state]
    (when active?
      (doseq [d ds :when (about ids d)] (say! "d" (shown d))))
    ds))

(defn packets-out
  "Shows the names of the packets the player gets on an active tick."
  [_ pairs]
  (let [{:keys [eid active?]} @state]
    (when active?
      (when-let [ps (seq (keep (fn [[e p]] (when (= e eid) (:packet p)))
                               pairs))]
        (say! "out" (str/join ", " (map name ps)))))))

(def ^:private own
  {:event-filters #'events :delta-filters #'deltas
   :packets-in #'packet-in :packets-out #'packets-out})

(defn hooks-with
  "Returns hooks with the trace hooks added once."
  [hooks]
  (reduce-kv (fn [h k v]
               (update h k #(if (some #{v} %) (vec %) (conj (vec %) v))))
             hooks own))

(defn hooks-without
  "Returns hooks without the trace hooks."
  [hooks]
  (reduce-kv (fn [h k v] (update h k #(vec (remove #{v} %)))) hooks own))

(defn follow!
  "Follows player eid, writing each line to the sink of opts, the log
  when there is none."
  [eid opts]
  (reset! state {:eid eid :sink (or (:sink opts) #(log/info %))}))

(defn stop! [] (reset! state nil))

(defn- player-eid [world nm]
  (some (fn [lv]
          (some (fn [[id e]] (when (= nm (:name e)) id)) (:entities lv)))
        (vals (:levels world))))

(defn- set-hooks! [{:keys [world ^ConcurrentLinkedQueue queue]} f]
  (doseq [[k fs] (f (:hooks @world))]
    (.offer queue [:hooks-set k fs])))

(defn on!
  "Traces the player named nm on the running server, or on server."
  ([nm] (on! @cli/running nm))
  ([server nm]
   (if-let [eid (player-eid @(:world server) nm)]
     (do (follow! eid {}) (set-hooks! server hooks-with) eid)
     (throw (ex-info (str "no player " nm) {})))))

(defn off!
  "Stops tracing on the running server, or on server."
  ([] (off! @cli/running))
  ([server] (set-hooks! server hooks-without) (stop!)))
