(ns collider.world.blocks.climb
  "Blocks a living entity climbs."
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- st-at ^long [chunks x y z]
  (if (chunk/in-range? (long y))
    (chunk/block-state chunks x y z)
    block/air))

(defn- ladder-trapdoor? [chunks st [x y z]]
  (let [props (block/props-of st)
        below (st-at chunks x (dec (long y)) z)]
    (and (= :true (:open props))
         (= :ladder (block/block-of below))
         (= (:facing props) (:facing (block/props-of below))))))

(defn on-climbable?
  "Returns true when the block at pos lets a living entity climb."
  [chunks pos]
  (let [[x y z :as cell] (v/cell pos)
        st (st-at chunks x y z)]
    (or (block/tagged? st "climbable")
        (and (contains? block/trapdoor-types (block/type-of st))
             (ladder-trapdoor? chunks st cell)))))

(defn- glides-through? [chunks pos]
  (let [[x y z] (v/cell pos)]
    (block/tagged? (st-at chunks x y z) "can_glide_through")))

(defn climbing?
  "Returns true when living body e holds on to a climbable block at
  pos, its own place unless given. A glider passes through the vines
  it can glide through, and a flying or spectating
  player climbs nothing."
  ([chunks e] (climbing? chunks e (:pos e)))
  ([chunks e pos]
   (and (not (:flying e)) (not= :spectator (:game-mode e))
        (not (and (:fall-flying e) (glides-through? chunks pos)))
        (on-climbable? chunks pos))))
