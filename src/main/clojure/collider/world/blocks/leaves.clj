(ns collider.world.blocks.leaves
  "Leaves and their distance from a log."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- distance-at ^long [^long st]
  (cond
    (block/tagged? st "prevents_nearby_leaf_decay") 0
    (block/leaves? st) (block/prop-long st :distance)
    :else 7))

(defn distance-state
  "Returns the leaves st at p with the distance that its
  neighbours give."
  ^long [chunks p ^long st]
  (let [at (fn [off] (chunk/at-void chunks (mapv + p off)))
        step (fn [d off] (min (long d) (inc (distance-at (at off)))))
        d (reduce step 7 (vals dir/offset))]
    (block/state (block/block-of st)
                 (assoc (block/props-of st)
                        :distance (keyword (str d))))))

(defn- wake [chunks _dim tick p _old side]
  (when side
    (let [st (chunk/at-void chunks p)
          n (chunk/at-void chunks (mapv + p (dir/offset side)))
          d (inc (distance-at (max 0 n)))]
      (when (or (not= 1 d) (not= d (block/prop-long st :distance)))
        (inc (long tick))))))

(defn- due [chunks p _ctx]
  (let [st (chunk/at-void chunks p)
        st' (distance-state chunks p st)]
    (when (not= st st') [[p st']])))

(def rule
  {:name   :leaves
   :match? (fn [_chunks st _p] (block/leaves? st))
   :wake   wake
   :due    due})
