(ns collider.world.blocks.lamp
  "Redstone lamp, lit while a neighbour powers it."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:const off-delay
  "The ticks a lamp stays lit after its power goes."
  4)

(defn- lamp? [^long st] (= :redstone-lamp (block/type-of st)))

(defn- lit? [^long st] (= :true (:lit (block/props-of st))))

(defn lit
  "Returns the lamp st lit when on? is true, else dark."
  ^long [^long st on?]
  (block/with st :lit (block/flag on?)))

(defn- wake [chunks _dim tick p _old _side]
  (let [st (chunk/at chunks p)
        on? (signal/has-neighbor-signal? chunks p)]
    (cond
      (= on? (lit? st)) nil
      on? :neighbor
      :else (+ (long tick) off-delay))))

(defn- turned [chunks p on?]
  (let [st (chunk/at chunks p)]
    (when (and (lamp? st) (not= on? (lit? st))
               (= on? (signal/has-neighbor-signal? chunks p)))
      [[p (lit st on?) nil update/clients]])))

(def rule
  "The block rule of the redstone lamp."
  {:name    :lamp
   :match?  (fn [_chunks st _p] (lamp? st))
   :pass    :neighbor
   :reach   2
   :place?  false
   :wake    wake
   :reshape (fn [chunks p _ctx] (turned chunks p true))
   :due     (fn [chunks p _ctx] (turned chunks p false))})
