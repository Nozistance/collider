(ns collider.devtools.trace
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

(def ^:private colours
  {"tick" "1" "in" "36" "e" "33" "d" "32" "out" "35"})

(defn- painted [colour? kind s]
  (if colour? (str "\u001b[" (colours kind) "m" s "\u001b[0m") s))

(defn- shown [x]
  (binding [*print-length* 8 *print-level* 4] (pr-str x)))

(defn- spoken [v]
  (if (and (vector? v) (keyword? (first v)))
    (str/join " " (cons (name (first v)) (map shown (rest v))))
    (shown v)))

(defn- say! [kind text]
  (swap! state update :lines (fnil conj []) [kind text]))

(defn- entity? [world id]
  (and (integer? id)
       (some #(contains? (:entities %) id) (vals (:levels world)))))

(defn- touched [world evs]
  (into #{} (comp cat (filter #(entity? world %))) evs))

(defn packet-in
  [eid m ev]
  (when (and (= eid (:eid @state)) (not (quiet-packets (:packet m))))
    (say! "in" (str (name (:packet m)) " " (shown (dissoc m :packet))
                    " -> " (if ev (spoken ev) "-")))))

(defn events
  [world evs]
  (let [eid (:eid @state)
        mine (filterv #(and (= eid (nth % 1 nil))
                            (not (quiet-events (nth % 0))))
                      evs)]
    (swap! state assoc :ids (conj (touched world mine) eid)
           :active? (boolean (seq mine)))
    (doseq [ev mine] (say! "e" (spoken ev)))
    evs))

(defn- stats-only? [d]
  (and (= :merge-entity (nth d 0)) (= [:stats] (keys (nth d 2 nil)))))

(defn- about [ids d]
  (let [x (nth d 1 nil)]
    (and (not (quiet-deltas (nth d 0))) (not (stats-only? d))
         (or (contains? ids x) (contains? ids (:to x))))))

(defn deltas
  [_ ds]
  (let [{:keys [active? ids]} @state]
    (when active?
      (doseq [d ds :when (about ids d)] (say! "d" (spoken d))))
    ds))

(defn packet-names
  [ps]
  (->> (partition-by identity (map (comp name :packet) ps))
       (map #(if (next %) (str (first %) " x" (count %)) (first %)))
       (str/join ", ")))

(defn- player-name [world eid]
  (some #(get-in % [:entities eid :name]) (vals (:levels world))))

(defn- block [world {:keys [eid colour?]} lines]
  (let [line (fn [[kind text]]
               (str "\n  " (painted colour? kind (format "%-4s" kind)) text))]
    (apply str (painted colour? "tick"
                        (str "tick " (:tick world) " "
                             (player-name world eid)))
           (map line lines))))

(defn packets-out
  [world pairs]
  (let [{:keys [eid active? sink]} @state]
    (when active?
      (when-let [ps (seq (keep (fn [[e p]] (when (= e eid) p)) pairs))]
        (say! "out" (packet-names ps)))
      (let [[old] (swap-vals! state assoc :lines [] :active? false)]
        (sink (block world old (:lines old)))))))

(def ^:private own
  {:event-filters #'events :delta-filters #'deltas
   :packets-in #'packet-in :packets-out #'packets-out})

(defn hooks-with
  [hooks]
  (reduce-kv (fn [h k v]
               (update h k #(if (some #{v} %) (vec %) (conj (vec %) v))))
             hooks own))

(defn hooks-without
  [hooks]
  (reduce-kv (fn [h k v] (update h k #(vec (remove #{v} %)))) hooks own))

(defn follow!
  [eid opts]
  (reset! state {:eid eid :sink (or (:sink opts) #(log/info %))
                 :colour? (:color? opts true) :lines []}))

(defn stop! [] (reset! state nil))

(defn- player-eid [world nm]
  (some (fn [lv]
          (some (fn [[id e]] (when (= nm (:name e)) id)) (:entities lv)))
        (vals (:levels world))))

(defn- set-hooks! [{:keys [world ^ConcurrentLinkedQueue queue]} f]
  (doseq [[k fs] (f (:hooks @world))]
    (.offer queue [:hooks-set k fs])))

(defn on!
  ([nm] (on! @cli/running nm))
  ([server nm]
   (if-let [eid (player-eid @(:world server) nm)]
     (do (follow! eid {}) (set-hooks! server hooks-with) eid)
     (throw (ex-info (str "no player " nm) {})))))

(defn off!
  ([] (off! @cli/running))
  ([server] (set-hooks! server hooks-without) (stop!)))

