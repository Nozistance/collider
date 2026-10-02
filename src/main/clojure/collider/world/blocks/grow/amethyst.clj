(ns collider.world.blocks.grow.amethyst
  "Amethyst buds growing on budding amethyst."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.grow.common :refer [chance?]]))

(set! *warn-on-reflection* true)

(def ^:private next-stage
  {:small-amethyst-bud :medium-amethyst-bud
   :medium-amethyst-bud :large-amethyst-bud
   :large-amethyst-bud :amethyst-cluster})

(defn- amethyst-next [^long target dir]
  (cond
    (or (zero? target) (block/water-source? target))
    :small-amethyst-bud
    (not= dir (block/facing-of target)) nil
    :else (next-stage (block/block-of target))))

(defn budding-tick
  "Returns the bud a random tick of budding amethyst at p grows."
  [chunks p _st roll _time _world]
  (when (chance? roll :gate 5)
    (let [dir (dir/six (random/below (roll :dir) 6))
          q (dir/toward p dir)
          target (chunk/at chunks q)
          wet (block/flag (block/water? target))]
      (when-let [b (amethyst-next target dir)]
        [[q (block/state b {:facing dir :waterlogged wet})]]))))
