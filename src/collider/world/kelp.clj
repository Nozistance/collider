(ns collider.world.kelp
  "Kelp: the top block is the head (:kelp, with :age), the rest is :kelp-plant.
   A head under another kelp becomes a stem; a stem with nothing above becomes
   a head. Kelp needs support and holds a water source, so its rule also runs
   the support and liquid rules for the cell."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def ^:private kelp-types #{:kelp :kelp-plant})

(defn kelp? [st] (contains? kelp-types (block/type-of (long st))))

(defn head-state
  "Head with an age from the position, as vanilla picks a random one."
  ^long [pos]
  (block/state :kelp {:age (keyword (str (mod (hash pos) 25)))}))

(defn- above [chunks [x y z]]
  (let [y (inc (long y))]
    (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk [x y z]) 0)))

(def rule
  {:name   :kelp
   :match? (fn [_chunks st _p] (kelp? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)
                   up? (kelp? (above chunks p))]
               (or (seq ((:due support/rule) chunks p))
                   (concat
                    (case (block/type-of st)
                      :kelp (when up? [[p (block/state :kelp-plant)]])
                      :kelp-plant (when-not up? [[p (head-state p)]])
                      nil)
                    (liquid/update-cell chunks gen/flat-chunk p)))))})
