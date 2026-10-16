(ns collider.world.light
  "Block light, sky light, and the sky brightness of the day cycle."
  (:require [collider.world.block :as block])
  (:import (collider.world Light)))

(set! *warn-on-reflection* true)

(def ^:const block-channel Light/BLOCK)

(def ^:const sky-channel Light/SKY)

(defn light-at
  "Returns the brighter of the sky and block light at x y z."
  ^long [chunks ^long x ^long y ^long z]
  (Light/at chunks x y z))

(defn block-light-at
  ^long [chunks ^long x ^long y ^long z]
  (Light/blockAt chunks x y z))

(defn sky-light-at
  "Returns the sky light at x y z, full above the world."
  ^long [chunks ^long x ^long y ^long z]
  (Light/skyAt chunks x y z))

(defn sky-darken
  "Returns how much time and weather dim the sky light, 0 to 15."
  (^long [^long time] (Light/darken time 0.0 0.0))
  (^long [^long time ^double rain-level ^double thunder-level]
   (Light/darken time rain-level thunder-level)))

(defn brightness
  "Returns the light level at x y z.
  The time of day and the weather dim the sky part."
  {:inline (fn [chunks x y z time rain thunder]
             `(Light/brightness ~chunks (long ~x) (long ~y) (long ~z)
                                (long ~time) (double ~rain)
                                (double ~thunder)))
   :inline-arities #{7}}
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
