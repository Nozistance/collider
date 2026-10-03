(ns collider.world.light
  "Block light, sky light, and the sky brightness of the day cycle."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world Light)))

(set! *warn-on-reflection* true)

(defn light-at
  "Returns the brighter of the sky and block light at x y z."
  {:inline (fn [c x y z]
             `(Light/at ~c (long ~x) (long ~y) (long ~z)))}
  ^long [chunks x y z]
  (Light/at chunks (long x) (long y) (long z)))

(defn block-light-at
  {:inline (fn [c x y z]
             `(Light/blockAt ~c (long ~x) (long ~y) (long ~z)))}
  ^long [chunks x y z]
  (Light/blockAt chunks (long x) (long y) (long z)))

(defn sky-light-at
  "Returns the sky light at x y z, full above the world."
  {:inline (fn [c x y z]
             `(Light/skyAt ~c (long ~x) (long ~y) (long ~z)))}
  ^long [chunks x y z]
  (Light/skyAt chunks (long x) (long y) (long z)))

(defn sky-light-level
  "Returns the sky brightness from 0.0 to 15.0 at a time of day.
  The rain and thunder levels from 0.0 to 1.0 dim it."
  {:inline (fn
             ([t] `(Light/skyLevel (long ~t) 0.0 0.0))
             ([t r h]
              `(Light/skyLevel (long ~t) (double ~r) (double ~h))))
   :inline-arities #{1 3}}
  (^double [^long time] (Light/skyLevel time 0.0 0.0))
  (^double [^long time ^double rain-level ^double thunder-level]
   (Light/skyLevel time rain-level thunder-level)))

(defn sky-darken
  "Returns how much time and weather dim the sky light, 0 to 15."
  {:inline (fn
             ([t] `(Light/darken (long ~t) 0.0 0.0))
             ([t r h]
              `(Light/darken (long ~t) (double ~r) (double ~h))))
   :inline-arities #{1 3}}
  (^long [^long time] (Light/darken time 0.0 0.0))
  (^long [^long time ^double rain-level ^double thunder-level]
   (Light/darken time rain-level thunder-level)))

(defn- brightness-form [chunks x y z time rain thunder]
  `(Light/brightness ~chunks (long ~x) (long ~y) (long ~z)
                     (long ~time) (double ~rain) (double ~thunder)))

(defn brightness
  "Returns the light level at x y z.
  The time of day and the weather dim the sky part."
  {:inline (fn [c x y z t & [r h]]
             (brightness-form c x y z t (or r 0.0) (or h 0.0)))
   :inline-arities #{5 7}}
  ([chunks x y z time] (brightness chunks x y z time 0.0 0.0))
  ([chunks x y z time rain-level thunder-level]
   (Light/brightness chunks (long x) (long y) (long z) (long time)
                     (double rain-level) (double thunder-level))))

(defn relight-batch
  "Returns chunks relit after changes [pos old new].
  Sky light moves only when sky? is true."
  ([chunks changes] (relight-batch chunks changes true))
  ([chunks changes sky?]
   (Light/relit chunks changes (boolean sky?) (block/tables))))

(defn relight
  [chunks pos old-state new-state]
  (relight-batch chunks [[pos old-state new-state]]))
