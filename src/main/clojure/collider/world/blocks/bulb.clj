(ns collider.world.blocks.bulb
  "Copper bulbs that a rising signal turns on or off."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn bulb?
  "Returns true when st is a copper bulb of any stage."
  [^long st]
  (contains? #{:copper-bulb-block :weathering-copper-bulb}
             (block/type-of st)))

(defn lit?
  "Returns true when the bulb st is lit."
  [^long st]
  (= :true (:lit (block/props-of st))))

(defn- powered? [^long st] (= :true (:powered (block/props-of st))))

(defn- flipped [chunks p _ctx]
  (let [st (chunk/at chunks p)
        on (signal/has-neighbor-signal? chunks p)]
    (when (and (bulb? st) (not= on (powered? st)))
      (let [rise? (not (powered? st))
            lit (if rise? (not (lit? st)) (lit? st))
            sound (if lit :block.copper-bulb.turn-on
                      :block.copper-bulb.turn-off)]
        [[p (-> st (block/with :lit (block/flag lit))
                (block/with :powered (block/flag on)))
          (when rise? [[:sound sound 1.0 1.0]])]]))))

(defn- wake [chunks _dim _tick p old side]
  (let [st (chunk/at chunks p)]
    (when (and (or (some? side)
                   (not= (block/block-of st) (block/block-of old)))
               (not= (signal/has-neighbor-signal? chunks p)
                     (powered? st)))
      :neighbor)))

(def rule
  "The block rule of copper bulbs."
  {:name    :bulb
   :match?  (fn [_chunks st _p] (bulb? st))
   :pass    :neighbor
   :reach   2
   :wake    wake
   :reshape flipped})
