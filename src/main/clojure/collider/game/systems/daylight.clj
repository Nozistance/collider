(ns collider.game.systems.daylight
  "Daylight detectors and the signal they read off the sky."
  (:require [collider.game.changes :as changes]
            [collider.game.clock :as clock]
            [collider.num :as num]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.dimension :as dimension]
            [collider.world.env.weather :as weather]
            [collider.world.light :as light]))

(set! *warn-on-reflection* true)

(def ^:private sun "minecraft:visual/sun_angle")

(def ^:private sky-level "minecraft:gameplay/sky_light_level")

(defn- base
  "Returns attribute k of the dimension of level lv, else default."
  [lv k default]
  (get (:attributes (dimension/type-of (:dim lv))) k default))

(defn- darken
  "Returns how much time and weather dim the sky of level lv. Without
  a day track the sky light level of the dimension holds."
  ^long [lv]
  (if (clock/tracked? (:dim lv) sky-level)
    (light/sky-darken (clock/day-ticks lv) (weather/rain-level lv)
                      (weather/thunder-level lv))
    (long (- 15.0 (double (base lv :gameplay/sky-light-level 15.0))))))

(defn- by-sun
  "Returns light lit scaled by the height of the sun in level lv."
  ^long [lv ^long lit]
  (let [deg (double (or (clock/sampled lv (:dim lv) sun)
                        (base lv :visual/sun-angle 0.0)))
        a (num/f32 (* deg (num/f32 (/ Math/PI 180.0))))
        to (if (< a (num/f32 Math/PI)) 0.0 (num/f32 (* Math/PI 2.0)))
        a (num/f32 (+ a (num/f32 (* (num/f32 (- to a))
                                    (num/f32 0.2)))))]
    (Math/round (unchecked-float (* (double lit) (num/cos a))))))

(defn target
  "Returns the power of a detector in level lv under sky light sky."
  ^long [lv ^long sky inverted?]
  (let [lit (- sky (darken lv))
        p (cond inverted? (- 15 lit)
                (pos? lit) (by-sun lv lit)
                :else lit)]
    (max 0 (min 15 p))))

(defn- updated
  "Returns the step that sets the power of the detector at pos to
  what the sky gives it in chunks, with time and weather of lv."
  [lv pos]
  (fn [chunks]
    (let [st (chunk/at chunks pos)
          [x y z] pos
          p (target lv (light/sky-light-at chunks x y z)
                    (= :true (:inverted (block/props-of st))))]
      (when (not= p (block/prop-long st :power))
        [[pos (block/with-long st :power p) nil 3]]))))

(defn tick-deltas
  "Returns the deltas of one tick of the daylight detector at pos. It
  reads the sky every twenty ticks, in a level with sky light only."
  [world [pos _]]
  (when (and (zero? (rem (long (:tick world)) 20))
             (:has-skylight (dimension/type-of (:dim world))))
    (changes/set-deltas world [(updated world pos)])))

(defn inverted-deltas
  "Returns the deltas of player eid inverting the detector at pos."
  [world eid pos st]
  (let [on? (= :true (:inverted (block/props-of st)))
        inv (block/flag (not on?))]
    (changes/change-deltas
      world [[pos (block/with st :inverted inv)
              [[:game-event :block-change eid]] 2]
             (updated world pos)])))
