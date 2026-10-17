(ns collider.world.blocks.bell
  "Bells that a neighbour's signal rings."
  (:require [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- powered? [^long st] (= :true (:powered (block/props-of st))))

(defn- rung [chunks p]
  (let [st (chunk/at chunks p)
        on (signal/has-neighbor-signal? chunks p)]
    (when (not= on (powered? st))
      [[p (block/with st :powered (block/flag on)) (when on [[:ring st]])]])))

(defn- wake [chunks dim tick p old side]
  (or ((:wake support/rule) chunks dim tick p old side)
      (when (not= (signal/has-neighbor-signal? chunks p)
                  (powered? (chunk/at chunks p)))
        :neighbor)))

(def rule
  "The block rule of bells. They are held as attached blocks are and
  rung by a signal."
  (assoc support/rule
         :name :bell
         :match? (fn [_chunks st _p] (= :bell (block/type-of st)))
         :pass :both
         :wake wake
         :reshape (fn [chunks p ctx]
                    (if (= :neighbor (:pass ctx))
                      (rung chunks p)
                      ((:reshape support/rule) chunks p ctx)))))
