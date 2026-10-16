(ns collider.world.blocks.cauldron
  "What cauldrons hold and how a body gets into it."
  (:require [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(def precipitation
  {:water-cauldron :rain :powder-snow-cauldron :snow})

(def ^:private ^:const floor-pixels 4)

(def ^:private ^:const base-pixels 6)

(def ^:private ^:const level-pixels 3)

(def ^:private ^:const lava-pixels 15)

(defn lava? [^long st] (= :lava-cauldron (block/type-of st)))

(defn layered? [^long st] (= :layered-cauldron (block/type-of st)))

(defn lowered
  "Returns layered cauldron st one level lower, empty below the first."
  ^long [^long st]
  (let [lvl (block/prop-long st :level)]
    (if (= 1 lvl)
      (block/state :cauldron)
      (block/with-long st :level (dec lvl)))))

(defn doused
  "Returns layered cauldron st after a burning body in it goes out.
  Snow melts into water a level lower."
  ^long [^long st]
  (lowered (if (= :snow (precipitation (block/block-of st)))
             (block/with-long (block/state :water-cauldron) :level
                              (block/prop-long st :level))
             st)))

(defn- content-pixels [^long st]
  (cond (layered? st) (+ base-pixels
                         (* level-pixels (block/prop-long st :level)))
        (lava? st) lava-pixels))

(defn inside-boxes
  "Returns the boxes in sixteenths a body touches to be inside st, nil
  when that is the whole block."
  [^long st]
  (when-let [top (content-pixels st)]
    (conj (vec (block/collision-boxes st))
          [2 floor-pixels 2 14 top 14])))
