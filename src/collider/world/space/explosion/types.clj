(ns collider.world.space.explosion.types
  "The blocks an explosion may reach, read once.")

(deftype Region [^objects grid ^objects cols ^long cx0 ^long cz0
                 ^long sy0 ^long ncx ^long ncz ^long nsy
                 read-absent ^clojure.lang.Atom loaded])
