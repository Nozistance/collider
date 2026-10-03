(ns collider.world.noise
  "Perlin noise fields of the feature placers."
  (:import (collider.world Noise)))

(set! *warn-on-reflection* true)

(defn noise
  "Returns the noise field of seed with the octave amplitudes amps."
  ^Noise [^long seed ^long octave ^doubles amps]
  (Noise/of seed (int octave) amps))

(defn at
  [^Noise n x y z scale]
  (.at n (double x) (double y) (double z) (float scale)))

(defn at-float
  "Returns the noise at block x y z scaled by scale, as a float."
  [^Noise n x y z scale]
  (.atFloat n (int x) (int y) (int z) (float scale)))
