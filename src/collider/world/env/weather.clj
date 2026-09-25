(ns collider.world.env.weather
  "The rain and thunder cycle, and what falls at a position."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.env.biome :as biome]
            [collider.world.light :as light]
            [collider.world.space.spawn :as spawn]))

(set! *warn-on-reflection* true)

(def rain-delay [12000 180000])

(def rain-duration [12000 24000])

(def thunder-delay [12000 180000])

(def thunder-duration [3600 15600])

(def fields
  [:clear-weather-time :rain-time :thunder-time :raining? :thundering?
   :rain-level :o-rain-level :thunder-level :o-thunder-level])

(defn can-have-weather?
  "Whether the level of dimension dim has rain and thunder."
  [dim]
  (let [t (data/dimension-type dim)]
    (boolean (and (:has-skylight t) (not (:has-ceiling t))
                  (not= :the-end dim)))))

(defn sample
  "Returns a whole number of ticks within bounds, inclusive.
  The roll, in [0, 1), picks it."
  ^long [^double roll bounds]
  (let [lo (long (nth bounds 0))
        span (- (long (nth bounds 1)) lo)
        k (long (Math/floor (* roll (inc span))))]
    (+ lo (min span k))))

(defn rain-level
  "Returns the rain level, 0.0 to 1.0."
  ^double [ctx]
  (double (:rain-level ctx 0.0)))

(defn thunder-level
  "Returns the thunder level as a player sees it: scaled by rain."
  ^double [ctx]
  (let [th (float (:thunder-level ctx 0.0))]
    (double (float (* th (float (rain-level ctx)))))))

(defn raw-thunder-level
  "Returns the thunder level, 0.0 to 1.0, not scaled by rain."
  ^double [ctx]
  (double (:thunder-level ctx 0.0)))

(defn raining?
  "Returns true when the rain level makes it rain."
  [ctx]
  (> (rain-level ctx) 0.2))

(defn thundering?
  "Returns true when the thunder level makes it thunder."
  [ctx]
  (> (thunder-level ctx) 0.9))

(defn sky-darken
  "Returns how much the weather and the time dim the sky light.
  The value is 0 to 15."
  ^long [ctx ^long time]
  (light/sky-darken time (rain-level ctx) (thunder-level ctx)))

(defn brightness
  "Returns the light level at x y z under the weather.
  The time of day dims the sky part too."
  [ctx chunks x y z time]
  (let [rain (rain-level ctx) thunder (thunder-level ctx)]
    (if (:sky? ctx true)
      (long (light/brightness chunks x y z time rain thunder))
      (long (light/block-light-at chunks x y z)))))

(defn- can-see-sky? [chunks p]
  (let [[x y z] p]
    (>= (long (light/sky-light-at chunks x y z)) 15)))

(defn- under-cover? [chunks p]
  (let [[x y z] p]
    (> (spawn/motion-blocking-height chunks x z) (long y))))

(defn precipitation-at
  "Returns what falls at block p now: :none, :rain or :snow."
  [ctx chunks p]
  (cond
    (not (raining? ctx)) :none
    (not (can-see-sky? chunks p)) :none
    (under-cover? chunks p) :none
    :else (biome/precipitation-at (biome/at (:dim ctx) p) p)))

(defn raining-at?
  "Returns true when rain falls at block p now."
  [ctx chunks p]
  (= :rain (precipitation-at ctx chunks p)))

(defn- step-level ^double [^double level raising?]
  (let [l (float level)
        step (float 0.01)
        v (float (if raising? (+ l step) (- l step)))]
    (double (float (min (float 1.0) (max (float 0.0) v))))))

(defn- toggled [^long timer flag]
  (let [n (dec timer)] [n (if (zero? n) (not flag) flag)]))

(defn- cleared-timers [^long clear raining thundering]
  {:clear-weather-time (dec clear)
   :thunder-time       (if thundering 0 1)
   :rain-time          (if raining 0 1)
   :thundering?        false
   :raining?           false})

(defn- next-phase [left on? t salt duration delay]
  (if (pos? (long left))
    (toggled left on?)
    [(sample (random/of-key t salt) (if on? duration delay)) on?]))

(defn- thunder-phase [w t on?]
  (next-phase (long (:thunder-time w 0)) on? t :thunder-time
              thunder-duration thunder-delay))

(defn- rain-phase [w t on?]
  (next-phase (long (:rain-time w 0)) on? t :rain-time
              rain-duration rain-delay))

(defn- running-timers [w t clear raining thundering]
  (let [[tt th] (thunder-phase w t thundering)
        [rt rn] (rain-phase w t raining)]
    {:clear-weather-time clear
     :thunder-time       tt
     :rain-time          rt
     :thundering?        th
     :raining?           rn}))

(defn- timers [w]
  (let [t (long (:tick w 0))
        clear (long (:clear-weather-time w 0))
        raining (boolean (:raining? w))
        thundering (boolean (:thundering? w))]
    (if (pos? clear)
      (cleared-timers clear raining thundering)
      (running-timers w t clear raining thundering))))

(defn- cycled [w]
  (if (get-in w [:rules :advance-weather] true)
    (timers w)
    {:clear-weather-time (long (:clear-weather-time w 0))
     :thunder-time       (long (:thunder-time w 0))
     :rain-time          (long (:rain-time w 0))
     :thundering?        (boolean (:thundering? w))
     :raining?           (boolean (:raining? w))}))

(defn advance
  "Returns the weather fields one tick later."
  [w]
  (let [m (cycled w)
        thunder (double (:thunder-level w 0.0))
        rain (double (:rain-level w 0.0))]
    (assoc m
      :o-thunder-level thunder
      :thunder-level (step-level thunder (:thundering? m))
      :o-rain-level rain
      :rain-level (step-level rain (:raining? m)))))

(defn parameters
  "Returns weather fields as /weather sets them."
  [^long clear-time ^long weather-time raining? thundering?]
  {:clear-weather-time clear-time
   :rain-time          weather-time
   :thunder-time       weather-time
   :raining?           (boolean raining?)
   :thundering?        (boolean thundering?)})

(def reset-cycle
  {:rain-time 0 :raining? false :thunder-time 0 :thundering? false})
