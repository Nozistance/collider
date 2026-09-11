(ns collider.world.weather
  (:require [collider.random :as random]
            [collider.world.biome :as biome]
            [collider.world.gen :as gen]
            [collider.world.light :as light]
            [collider.world.spawn :as spawn]))

(set! *warn-on-reflection* true)

(def rain-delay [12000 180000])
(def rain-duration [12000 24000])
(def thunder-delay [12000 180000])
(def thunder-duration [3600 15600])

(def fields
  [:clear-weather-time :rain-time :thunder-time :raining? :thundering?
   :rain-level :o-rain-level :thunder-level :o-thunder-level])

(defn sample ^long [^double roll bounds]
  (let [lo (long (nth bounds 0))
        hi (long (nth bounds 1))]
    (+ lo (min (- hi lo) (long (Math/floor (* roll (inc (- hi lo)))))))))

(defn rain-level ^double [ctx]
  (double (:rain-level ctx 0.0)))

(defn thunder-level ^double [ctx]
  (double (float (* (float (:thunder-level ctx 0.0)) (float (rain-level ctx))))))

(defn raw-thunder-level ^double [ctx]
  (double (:thunder-level ctx 0.0)))

(defn raining? [ctx]
  (> (rain-level ctx) 0.2))

(defn thundering? [ctx]
  (> (thunder-level ctx) 0.9))

(defn sky-darken ^long [ctx ^long time]
  (light/sky-darken time (rain-level ctx) (thunder-level ctx)))

(defn brightness [ctx chunks template x y z time]
  (long (light/brightness chunks template x y z time (rain-level ctx) (thunder-level ctx))))

(defn- can-see-sky? [chunks p]
  (>= (long (light/sky-light-at chunks gen/flat-chunk (nth p 0) (nth p 1) (nth p 2))) 15))

(defn precipitation-at [ctx chunks p]
  (cond
    (not (raining? ctx)) :none
    (not (can-see-sky? chunks p)) :none
    (> (spawn/motion-blocking-height chunks gen/flat-chunk (nth p 0) (nth p 2)) (long (nth p 1))) :none
    :else (biome/precipitation-at (biome/at chunks p) p)))

(defn raining-at? [ctx chunks p]
  (= :rain (precipitation-at ctx chunks p)))

(defn- step-level ^double [^double level raising?]
  (let [v (float (if raising? (+ (float level) (float 0.01)) (- (float level) (float 0.01))))]
    (double (float (min (float 1.0) (max (float 0.0) v))))))

(defn- toggled [^long timer flag]
  (let [n (dec timer)] [n (if (zero? n) (not flag) flag)]))

(defn- timers [w]
  (let [t          (long (:tick w 0))
        clear      (long (:clear-weather-time w 0))
        rain-t     (long (:rain-time w 0))
        thunder-t  (long (:thunder-time w 0))
        raining    (boolean (:raining? w))
        thundering (boolean (:thundering? w))]
    (if (pos? clear)
      {:clear-weather-time (dec clear)
       :thunder-time       (if thundering 0 1)
       :rain-time          (if raining 0 1)
       :thundering?        false
       :raining?           false}
      (let [[tt th] (if (pos? thunder-t)
                      (toggled thunder-t thundering)
                      [(sample (random/of-key [t :thunder-time])
                               (if thundering thunder-duration thunder-delay))
                       thundering])
            [rt rn] (if (pos? rain-t)
                      (toggled rain-t raining)
                      [(sample (random/of-key [t :rain-time])
                               (if raining rain-duration rain-delay))
                       raining])]
        {:clear-weather-time clear
         :thunder-time       tt
         :rain-time          rt
         :thundering?        th
         :raining?           rn}))))

(defn- cycled [w]
  (if (get-in w [:rules :advance-weather] true)
    (timers w)
    {:clear-weather-time (long (:clear-weather-time w 0))
     :thunder-time       (long (:thunder-time w 0))
     :rain-time          (long (:rain-time w 0))
     :thundering?        (boolean (:thundering? w))
     :raining?           (boolean (:raining? w))}))

(defn advance [w]
  (let [m       (cycled w)
        thunder (double (:thunder-level w 0.0))
        rain    (double (:rain-level w 0.0))]
    (assoc m
           :o-thunder-level thunder
           :thunder-level   (step-level thunder (:thundering? m))
           :o-rain-level    rain
           :rain-level      (step-level rain (:raining? m)))))

(defn parameters [^long clear-time ^long rain-time raining? thundering?]
  {:clear-weather-time clear-time
   :rain-time          rain-time
   :thunder-time       rain-time
   :raining?           (boolean raining?)
   :thundering?        (boolean thundering?)})

(def reset-cycle
  {:rain-time 0 :raining? false :thunder-time 0 :thundering? false})
