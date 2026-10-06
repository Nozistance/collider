(ns collider.world.blocks.grow
  "Random growth and bone meal of every block class."
  (:require [collider.world.block :as block]
            [collider.world.block.oxidation :as oxidation]
            [collider.world.update :as update]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.grow.amethyst :as amethyst]
            [collider.world.blocks.grow.bamboo :as bamboo]
            [collider.world.blocks.grow.common :refer [flagged]]
            [collider.world.blocks.grow.copper :as copper]
            [collider.world.blocks.grow.crop :as crop]
            [collider.world.blocks.grow.flower :as flower]
            [collider.world.blocks.grow.melt :as melt]
            [collider.world.blocks.grow.mushroom :as mushroom]
            [collider.world.blocks.grow.pickles :as pickles]
            [collider.world.blocks.grow.sapling :as sapling]
            [collider.world.blocks.grow.sprout :as sprout]
            [collider.world.blocks.grow.turf :as turf]
            [collider.world.blocks.grow.underwater :as underwater]
            [collider.world.blocks.grow.vine :as vine]
            [collider.world.blocks.leaves :as leaves]
            [collider.world.env.biome :as biome]
            [collider.world.env.dimension :as dimension]))

(set! *warn-on-reflection* true)

(defn- table [pairs]
  (into {} (for [[classes f] pairs k classes] [k f])))

(def ^:private crops
  [:crop :carrot :potato :beetroot :torchflower-crop])

(def ^:private ticks
  (table
   [[crops crop/tick]
    [[:stem] crop/stem-tick]
    [[:pitcher-crop] crop/pitcher-tick]
    [[:sugar-cane] crop/cane-tick]
    [[:cactus] crop/cactus-tick]
    [[:bamboo-stalk] bamboo/tick]
    [[:bamboo-sapling] bamboo/sapling-tick]
    [[:sweet-berry-bush] crop/berry-tick]
    [[:kelp] crop/kelp-tick]
    [[:mushroom] mushroom/tick]
    [[:grass :mycelium] turf/spread-tick]
    [[:farmland] turf/farmland-tick]
    [[:cocoa] crop/cocoa-tick]
    [[:ice] melt/ice-tick]
    [[:snow-layer] melt/snow-tick]
    [[:vine] vine/tick]
    [[:budding-amethyst] amethyst/budding-tick]
    [[:eyeblossom] flower/eyeblossom-tick]
    [[:flower-pot] flower/potted-tick]
    [[:nether-wart] crop/nether-wart-tick]
    [[:sapling] sapling/tick]
    [[:mangrove-propagule] sapling/propagule-tick]
    [[:chorus-flower] flower/chorus-tick]
    [[:mangrove-leaves :tinted-particle-leaves
      :untinted-particle-leaves] leaves/decay-tick]
    [[:weeping-vines :twisting-vines :cave-vines] vine/plant-tick]]))

(def ^:private client-only
  #{:vine :mushroom :chorus-flower})

(defn- ticked [chunks p st roll time world]
  (if-let [f (ticks (block/type-of st))]
    (f chunks p st roll time world)
    (when (oxidation/ages? st)
      (copper/tick chunks p st roll))))

(defn random-tick
  [chunks p st roll time world]
  (let [st (long st)
        cs (ticked chunks p st roll time world)]
    (if (and (seq cs) (client-only (block/type-of st)))
      (flagged update/clients cs)
      cs)))

(defn random-drops
  "Returns the drops of leaves too far from their log.
  Returns nil for any other block."
  [^long st roll]
  (when (and (block/leaves? st) (leaves/decaying? st))
    (block/drops st roll)))

(defn- berries-meal [chunks p st roll]
  (some-> (vine/berries-meal chunks p st roll)
          (update :changes #(flagged update/clients %))))

(def ^:private meals
  (table
   [[(conj crops :stem) crop/meal]
    [[:pitcher-crop] crop/pitcher-meal]
    [[:tall-grass] crop/tall-grass-meal]
    [[:tall-flower] crop/tall-flower-meal]
    [[:flower-bed] crop/petals-meal]
    [[:sweet-berry-bush] crop/berry-meal]
    [[:rooted-dirt] sprout/roots-meal]
    [[:bonemealable-feature-placer] turf/placer-meal]
    [[:cocoa] crop/cocoa-meal]
    [[:bamboo-sapling] bamboo/sapling-meal]
    [[:kelp] crop/kelp-meal]
    [[:weeping-vines :twisting-vines] vine/plant-meal]
    [[:weeping-vines-plant :twisting-vines-plant] vine/body-meal]
    [[:cave-vines :cave-vines-plant] berries-meal]
    [[:glow-lichen] sprout/lichen-meal]
    [[:hanging-moss] sprout/hanging-moss-meal]
    [[:mossy-carpet] sprout/carpet-meal]
    [[:big-dripleaf :big-dripleaf-stem :small-dripleaf]
     dripleaf/meal]
    [[:bush :firefly-bush] sprout/bush-meal]
    [[:short-dry-grass] sprout/short-dry-grass-meal]
    [[:tall-dry-grass] sprout/tall-dry-grass-meal]
    [[:bamboo-stalk] bamboo/meal]
    [[:sea-pickle] pickles/pickle-meal]
    [[:sapling] sapling/meal]
    [[:azalea] sapling/azalea-meal]
    [[:mangrove-propagule] sapling/propagule-meal]
    [[:seagrass] underwater/seagrass-meal]]))

(defn bonemeal
  "Returns the block changes and drops of bone meal on st at p in
  level dim, or nil when it does nothing."
  [chunks p st roll dim]
  (let [st (long st) t (block/type-of st)]
    (case t
      :grass (turf/turf-meal chunks p st roll (biome/at dim p))
      :mushroom
      (mushroom/meal chunks (dimension/bounds dim) p st roll)
      (when-let [f (meals t)]
        (f chunks p st roll)))))

(def tilled
  "The blocks a hoe turns each tillable block into."
  {:grass-block [:farmland] :dirt-path [:farmland] :dirt [:farmland]
   :coarse-dirt [:dirt] :rooted-dirt [:dirt :hanging-roots]})

(def flattened
  "The blocks a shovel flattens into a dirt path."
  #{:grass-block :dirt :podzol :coarse-dirt :mycelium :rooted-dirt})
