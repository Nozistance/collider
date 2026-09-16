(ns collider.world.blocks.climb
  "Blocks a living entity climbs."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(defn- st-at ^long [chunks x y z]
  (if (chunk/in-range? (long y))
    (chunk/chunks-get-block chunks (gen/flat-chunk) x y z)
    block/air))

(defn- ladder-trapdoor? [chunks st [x y z]]
  (let [props (block/props-of st)
        below (st-at chunks x (dec (long y)) z)]
    (and (= :true (:open props))
         (= :ladder (block/block-of below))
         (= (:facing props) (:facing (block/props-of below))))))

(defn on-climbable?
  "Returns true when an entity at pos stands on something it climbs."
  [chunks pos]
  (let [[x y z] (mapv #(long (Math/floor (double %))) pos)
        st (st-at chunks x y z)]
    (or (block/tagged? st "climbable")
        (and (contains? block/trapdoor-types (block/type-of st))
             (ladder-trapdoor? chunks st [x y z])))))
