(ns collider.world.update
  "Flags that tell a block change whom to tell and what to skip.")

(def ^:const neighbors 1)

(def ^:const clients 2)

(def ^:const all 3)

(def ^:const invisible 4)

(def ^:const immediate 8)

(def ^:const known-shape 16)

(def ^:const suppress-drops 32)

(def ^:const moved-by-piston 64)

(def ^:const skip-wire-shape 128)

(def ^:const skip-entity-side-effects 256)

(def ^:const skip-on-place 512)

(def ^:const quiet (bit-or skip-entity-side-effects invisible))

(def ^:const silent (bit-or skip-entity-side-effects clients))

(def ^:const strict
  (bit-or skip-on-place skip-entity-side-effects suppress-drops
          known-shape clients))
