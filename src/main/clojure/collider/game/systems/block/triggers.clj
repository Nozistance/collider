(ns collider.game.systems.block.triggers
  "The block events of a level, run after its block ticks."
  (:require [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.block.instrument :as instrument]
            [collider.game.block.piston :as piston]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- sent [f]
  (fn [_ pos st x y]
    (when-let [ds (f pos st)]
      (conj ds (out/all (out/block-event pos x y))))))

(def ^:private taken
  "What a block does on its event, by type. It gives the deltas, the
  packet of the event among them when the block takes it, or nil."
  {:note (sent instrument/sounded)
   :bell (sent (fn [_ _] []))
   :piston-base piston/triggered})

(defn- ran [w taken [pos b x y]]
  (let [st (chunk/at (:chunks w) pos)
        f (taken (block/type-of st))]
    (when (and f (= b (block/block-of st)))
      (f w pos st x y))))

(defn- popped [acc]
  (when-let [[e & es] (:block-events (first acc))]
    [e (apply/then acc [[:block-events-ran (vec es)]])]))

(defn drained
  "Returns the deltas of the block events queued in w, run one at a
  time as taken gives them, each on the world the ones before left.
  An event queued meanwhile runs in the same pass. Those whose chunk
  does not tick stay queued."
  [w taken]
  (let [active (areas/ticking-chunks w)]
    (loop [acc [w deltas/empty-deltas] later []]
      (if-let [[e acc] (popped acc)]
        (if (areas/active-at? active (first e))
          (recur (apply/then acc (ran (first acc) taken e)) later)
          (recur acc (conj later e)))
        (let [kept [:block-events-ran (vec (distinct later))]]
          (second (apply/then acc [kept])))))))

(defn block-events
  "Returns the deltas of the block events queued in w, in order, and
  keeps those whose chunk does not tick."
  {:wake {:keys [:block-events]}}
  [w _d]
  (if (:block-events w) (drained w taken) deltas/empty-deltas))
