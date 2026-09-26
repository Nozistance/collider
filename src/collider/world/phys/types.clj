(ns collider.world.phys.types
  "The boxes one sweep gathers and the outcome of a move.")

(deftype Sweep [^doubles a ^long n])

(deftype Move [pos vel ^boolean on-ground])
