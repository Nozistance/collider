(ns collider.world.blocks.grow
  (:require [collider.world.block :as block]
            [collider.world.blocks.grow.bamboo :as bamboo]
            [collider.world.blocks.dripleaf :as dripleaf]
            [collider.world.blocks.grow.crop :as crop]
            [collider.world.blocks.grow.ground :as ground]
            [collider.world.blocks.grow.mushroom :as mushroom]
            [collider.world.blocks.grow.vine :as vine]
            [collider.world.blocks.grow.weather :as weather]))

(set! *warn-on-reflection* true)

(defn- table [pairs]
  (into {} (for [[classes f] pairs k classes] [k f])))

(def ^:private ticks
  (table [[[:crop :carrot :potato :beetroot :torchflower-crop] crop/tick]
          [[:stem] crop/stem-tick]
          [[:pitcher-crop] crop/pitcher-tick]
          [[:sugar-cane] crop/cane-tick]
          [[:cactus] crop/cactus-tick]
          [[:bamboo-stalk] bamboo/tick]
          [[:bamboo-sapling] bamboo/sapling-tick]
          [[:sweet-berry-bush] crop/berry-tick]
          [[:kelp] crop/kelp-tick]
          [[:mushroom] mushroom/tick]
          [[:grass :mycelium] ground/spread-tick]
          [[:farmland] ground/farmland-tick]
          [[:cocoa] crop/cocoa-tick]
          [[:ice] ground/ice-tick]
          [[:snow-layer] ground/snow-tick]
          [[:vine] vine/tick]
          [[:budding-amethyst] ground/budding-tick]
          [[:eyeblossom] ground/eyeblossom-tick]
          [[:flower-pot] ground/potted-tick]
          [[:nether-wart] crop/nether-wart-tick]
          [[:mangrove-propagule] crop/propagule-tick]
          [[:chorus-flower] ground/chorus-tick]
          [[:mangrove-leaves :tinted-particle-leaves :untinted-particle-leaves] ground/leaves-tick]
          [[:weeping-vines :twisting-vines :cave-vines] vine/plant-tick]]))

(defn random-tick
  ([chunks p st roll time] (random-tick chunks p st roll time nil))
  ([chunks p st roll time ctx]
   (let [st (long st)]
     (if-let [f (ticks (block/type-of st))]
       (f chunks p st roll time ctx)
       (when (block/weathering? st) (weather/tick chunks p st roll))))))

(defn random-drops [^long st roll]
  (when (and (block/leaves? st)
             (= :false (:persistent (block/props-of st)))
             (= 7 (block/prop-long st :distance)))
    (block/drops st roll)))

(def ^:private meals
  (table [[[:crop :carrot :potato :beetroot :torchflower-crop :stem] crop/meal]
          [[:pitcher-crop] crop/pitcher-meal]
          [[:tall-grass] crop/doubled]
          [[:tall-flower] crop/tall-flower-meal]
          [[:flower-bed] crop/petals-meal]
          [[:sweet-berry-bush] crop/berry-meal]
          [[:mushroom] mushroom/meal]
          [[:rooted-dirt] ground/roots-meal]
          [[:cocoa] crop/cocoa-meal]
          [[:bamboo-sapling] bamboo/sapling-meal]
          [[:kelp] crop/kelp-meal]
          [[:weeping-vines :twisting-vines] vine/plant-meal]
          [[:weeping-vines-plant :twisting-vines-plant] vine/body-meal]
          [[:cave-vines :cave-vines-plant] vine/berries-meal]
          [[:glow-lichen] ground/lichen-meal]
          [[:hanging-moss] ground/hanging-moss-meal]
          [[:mossy-carpet] ground/carpet-meal]
          [[:big-dripleaf :big-dripleaf-stem :small-dripleaf] dripleaf/meal]
          [[:bush :firefly-bush] ground/bush-meal]
          [[:short-dry-grass] ground/short-dry-grass-meal]
          [[:tall-dry-grass] ground/tall-dry-grass-meal]
          [[:bamboo-stalk] bamboo/meal]
          [[:sea-pickle] ground/pickle-meal]
          [[:mangrove-propagule] crop/propagule-meal]
          [[:seagrass] crop/seagrass-meal]]))

(defn bonemeal [chunks p st roll]
  (let [st (long st)]
    (when-let [f (meals (block/type-of st))]
      (f chunks p st roll))))

(def tilled
  {:grass-block [:farmland] :dirt-path [:farmland] :dirt [:farmland]
   :coarse-dirt [:dirt] :rooted-dirt [:dirt :hanging-roots]})

(def flattened
  #{:grass-block :dirt :podzol :coarse-dirt :mycelium :rooted-dirt})
