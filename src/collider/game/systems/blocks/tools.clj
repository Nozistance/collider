(ns collider.game.systems.blocks.tools
  "Using tools and other items on blocks."
  (:require [collider.data :as data]
            [collider.game.block.tnt :as tnt]
            [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.game.state :as state]
            [collider.game.systems.blocks.cauldron :as cauldron]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.reach :as reach]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.grow :as grow]
            [collider.world.blocks.grow.underwater :as underwater]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.biome :as biome]))

(set! *warn-on-reflection* true)

(defn- tool-set [tag] (delay (set (data/tag-values "item" tag))))

(def ^:private axe-items (tool-set "axes"))

(def ^:private hoe-items (tool-set "hoes"))

(def ^:private shovel-items (tool-set "shovels"))

(defn axes
  "Returns the items tagged as axes."
  []
  @axe-items)

(defn hoes
  "Returns the items tagged as hoes."
  []
  @hoe-items)

(defn shovels
  "Returns the items tagged as shovels."
  []
  @shovel-items)

(def ^:private lightable-types #{:candle :candle-cake :campfire})

(defn- lightable [world pos]
  (let [cur (edit/block-at world pos) props (block/props-of cur)]
    (when (and (contains? lightable-types (block/type-of cur))
               (= :false (:lit props))
               (not= :true (:waterlogged props)))
      (block/state (block/block-of cur) (assoc props :lit :true)))))

(defn- fire-deltas [world pos off snd]
  (let [[_ y' _ :as pos'] (mapv + pos off)
        st (fire/state-for (:chunks world) pos')]
    (when (and (chunk/in-level? world y')
               (zero? (edit/block-at world pos'))
               (support/supported? (:chunks world) pos' st))
      (conj (edit/change-deltas world [[pos' st]]) (snd pos')))))

(defn- flint-sound [world eid pos]
  (let [pitch (random/pitch (:tick world) pos :flint)]
    (out/except eid (out/block-sound :fire/ignite pos 1.0 pitch))))

(defn- charge-sound [world pos]
  (let [t (:tick world)
        p (- (random/of-key t pos :charge-a)
             (random/of-key t pos :charge-b))
        pitch (+ 1.0 (* 0.2 (double p)))]
    (out/all (out/block-sound :firecharge/use pos 1.0 pitch))))

(defn- sounded [world changes fx]
  (concat (edit/change-deltas world changes) fx))

(defn firecharge-deltas
  "Returns the deltas of a fire charge used on the block at pos."
  [world [_eid pos face]]
  (if-let [st (lightable world pos)]
    (sounded world [[pos st]] [(charge-sound world pos)])
    (when-let [off (dir/face-offset face)]
      (fire-deltas world pos off #(charge-sound world %)))))

(defn- primable? [world eid pos]
  (and (tnt/tnt-state? (edit/block-at world pos))
       (get-in world [:rules :tnt-explodes] true)
       (not (get-in world [:entities eid :sneaking?]))
       (not ((tnt/primed-origins world) pos))))

(defn- prime-deltas [world pos]
  (let [primed (tnt/primed pos [(:tick world) pos])]
    (into (edit/change-deltas world [[pos 0]])
          [[:spawn-entity primed]
           (out/all (out/sound :tnt/primed (:pos primed) 1.0 1.0))])))

(defn flint-deltas
  "Returns the deltas of flint and steel used on the block at pos."
  [world [eid pos face]]
  (when-let [off (dir/face-offset face)]
    (cond
      (lightable world pos)
      (sounded world [[pos (lightable world pos)]]
               [(flint-sound world eid pos)])
      (primable? world eid pos) (prime-deltas world pos)
      :else (fire-deltas world pos off #(flint-sound world eid %)))))

(defn- meal-drops [world pos drops]
  (map-indexed
    (fn [i stack]
      [:spawn-entity (items/popped world pos stack [:meal i])])
    drops))

(defn- bonemealed [world pos]
  (let [st (edit/block-at world pos)
        salted #(random/of-key (:tick world) pos :meal %)]
    (grow/bonemeal (:chunks world) pos st salted (:dim world))))

(defn- crop-deltas [world pos]
  (when-let [{:keys [changes drops]} (bonemealed world pos)]
    (concat
      (when (seq changes) (edit/change-deltas world changes))
      (meal-drops world pos drops)
      [(out/all (out/bonemeal pos))])))

(def ^:private ^:table coral-biomes
  (delay (let [tag "produces_corals_from_bonemeal"]
           (set (data/tag-values "worldgen/biome" tag)))))

(defn- corals-at [world]
  (fn [p]
    (let [biome (biome/at (:dim world) p)]
      (contains? @coral-biomes (:name biome)))))

(defn- seabed-deltas [world pos face]
  (let [side (dir/from-index face)
        at (mapv + pos (dir/offset side))
        roll #(random/of-key (:tick world) at :seabed %)
        corals? (corals-at world)
        grow #(underwater/meal (:chunks world) at side roll corals?)]
    (when (block/face-sturdy? (edit/block-at world pos) side)
      (when-let [cs (grow)]
        (concat (edit/change-deltas world cs)
                [(out/all (out/bonemeal at))])))))

(defn bonemeal-deltas
  "Returns the deltas of bone meal used on the block at pos.
  On a floor under water it sprouts seagrass and coral."
  [world [eid pos face _ _]]
  (let [e (get-in world [:entities eid])]
    (when-let [ds (or (crop-deltas world pos)
                      (seabed-deltas world pos face))]
      (concat ds (items/consume-deltas eid e (:use-hand e) 1)
              [[:award eid :used/bone-meal 1]]))))

(def ^:private plant-heads
  #{:kelp :weeping-vines :twisting-vines :cave-vines})

(defn- grown-tip [st]
  (let [props (assoc (block/props-of st) :age :25)]
    (block/state (block/block-of st) props)))

(defn shear-deltas
  "Returns the deltas of shears used on the tip of a growing plant.
  The tip stops growing."
  [world [eid pos _ _ _]]
  (let [st (edit/block-at world pos)
        e (get-in world [:entities eid])
        kind :block.growing-plant.crop
        snd (out/block-sound kind pos 1.0 1.0 :blocks)]
    (when (and (contains? plant-heads (block/type-of st))
               (not= :25 (:age (block/props-of st))))
      (concat
        [(out/except eid snd)]
        (edit/change-deltas world [[pos (grown-tip st)]])
        (items/hurt-item-deltas eid e (:use-hand e) 1)
        [[:award eid :used/shears 1]]))))

(defn- tracker [world pos]
  {:target {:dimension (:dim world) :pos pos} :tracked true})

(defn- bound-compass [world eid e stack t]
  (let [one (stack/put (assoc stack :count 1) :lodestone-tracker t)]
    (concat (items/consume-deltas eid e (:use-hand e) 1)
            (items/kept world eid e one))))

(defn- lock-sound [pos]
  (out/block-sound :item.lodestone-compass.lock pos 1.0 1.0
                   :players))

(defn compass-deltas
  "Returns the deltas of a compass used on a lodestone.
  The compass points at it from then on."
  [world [eid pos _ _ _]]
  (when (= :lodestone (block/block-of (edit/block-at world pos)))
    (let [e (get-in world [:entities eid])
          hand (:use-hand e)
          stack (state/hand-stack e hand)
          t (tracker world pos)]
      (concat
        [(out/all (lock-sound pos))]
        (if (and (not (state/infinite-materials? e))
                 (= 1 (stack/size stack)))
          [[:set-slot eid (state/hand-slot e hand)
            (stack/put stack :lodestone-tracker t)]]
          (bound-compass world eid e stack t))
        [[:award eid :used/compass 1]]))))

(defn- air-above? [world pos]
  (zero? (edit/block-at world (mapv + pos [0 1 0]))))

(defn- freed-drop [world pos freed]
  (let [stack {:item freed :count 1}]
    [[:spawn-entity (items/popped world pos stack :till)]]))

(defn till-deltas
  "Returns the deltas of a hoe used on the block at pos."
  [world [_eid pos _ _ _]]
  (let [cur (edit/block-at world pos)]
    (when-let [[to freed] (grow/tilled (block/block-of cur))]
      (when (air-above? world pos)
        (concat
          (edit/change-deltas world [[pos (block/state to)]])
          (when freed (freed-drop world pos freed))
          [(out/all (out/block-sound :hoe/till pos 1.0 1.0))])))))

(defn- flattened-state [world pos cur]
  (when (and (contains? grow/flattened (block/block-of cur))
             (air-above? world pos))
    (block/state :dirt-path)))

(defn flatten-deltas
  "Returns the deltas of a shovel used on the block at pos."
  [world [_eid pos face _ _]]
  (let [cur (edit/block-at world pos)]
    (when (not= 0 (long face))
      (if-let [st (flattened-state world pos cur)]
        (let [snd (out/block-sound :shovel/flatten pos 1.0 1.0)]
          (sounded world [[pos st]] [(out/all snd)]))
        (edit/campfire-out-deltas world pos)))))

(defn- door-partner [world pos cur]
  (when (contains? block/door-types (block/type-of cur))
    (connect/partner (:chunks world) pos cur)))

(defn- half-changes [world pos ^long st]
  (let [cur (edit/block-at world pos)]
    (if-let [[ppos pst] (door-partner world pos cur)]
      (let [props (block/props-of pst)]
        [[pos st] [ppos (block/state (block/block-of st) props)]])
      [[pos st]])))

(defn- copper-fx [pos snd particles]
  (conj (if snd [(out/all (out/block-sound snd pos 1.0 1.0))] [])
        (out/all (out/level-event particles pos))))

(defn wax-deltas
  "Returns the deltas of honeycomb used on the block at pos."
  [world [_ pos _ _ _]]
  (when-let [st (block/waxed (edit/block-at world pos))]
    (sounded world (half-changes world pos st)
             (copper-fx pos nil
                        out/particles-and-sound-wax-on))))

(defn- copper-axe-deltas [world pos cur]
  (if-let [st (block/weathered-prev cur)]
    (sounded world (half-changes world pos st)
             (copper-fx pos :axe/scrape out/particles-scrape))
    (when-let [st (block/unwaxed cur)]
      (sounded world (half-changes world pos st)
               (copper-fx pos :axe/wax-off out/particles-wax-off)))))

(defn axe-deltas
  "Returns the deltas of an axe used on the block at pos."
  [world [_ pos _ _ _]]
  (let [cur (edit/block-at world pos)]
    (if-let [st (block/stripped cur)]
      (sounded world [[pos st]]
               [(out/all (out/block-sound :axe/strip pos 1.0 1.0))])
      (copper-axe-deltas world pos cur))))

(defn- egg-pitch ^double [t pos]
  (let [p (- (random/of-key t pos :p1) (random/of-key t pos :p2))]
    (+ 1.0 (* 0.2 p))))

(defn- hatch-deltas [world pos mob at]
  (let [t (:tick world)
        hatched (mobs/egg-mob mob at [t pos] t (:dim world))
        pitch (egg-pitch t pos)]
    (cons [:spawn-entity hatched]
          (when-let [say (mobs/sound-of hatched :say)]
            [(out/all (out/sound say at 1.0 pitch))]))))

(defn- egg-deltas [world eid pos mob at]
  (let [e (get-in world [:entities eid])]
    (concat (hatch-deltas world pos mob at)
            (items/consume-deltas eid e (:use-hand e) 1))))

(defn spawn-egg-deltas
  "Returns the deltas of a spawn egg used on a block face.
  The mob hatches next to the block at pos."
  [world [eid pos face item]]
  (when-let [off (dir/face-offset face)]
    (when-let [mob (mobs/egg-type item)]
      (let [[x y z] (mapv + pos off)
            at [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]]
        (when (chunk/in-range? y)
          (concat (egg-deltas world eid pos mob at)
                  [[:award eid (keyword "used" (name item)) 1]]))))))

(defn fluid-egg-deltas
  "Returns the deltas of a spawn egg used at a liquid source in
  view. The mob hatches in the liquid."
  [world eid e item]
  (when-let [mob (mobs/egg-type item)]
    (when-let [{:keys [pos]} (reach/clip world e :source-only)]
      (when (block/liquid? (edit/block-at world pos))
        (let [[x y z] pos
              at [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]]
          (concat (egg-deltas world eid pos mob at)
                  [[:award eid (keyword "used" (name item)) 1]]))))))

(defn- carve-facing [world eid face]
  (if (<= (long face) 1)
    (let [yaw (get-in world [:entities eid :yaw] 0.0)]
      (dir/opposite (dir/player-direction yaw)))
    (get {2 :north 3 :south 4 :west 5 :east} face)))

(defn- seeds-drop [world [x y z :as pos] [ox _ oz]]
  (let [t (:tick world)
        ox (long ox) oz (long oz)
        at [(+ (long x) 0.5 (* 0.65 ox))
            (+ (long y) 0.1)
            (+ (long z) 0.5 (* 0.65 oz))]
        vel [(+ (* 0.05 ox) (* 0.02 (random/of-key t pos :sx)))
             0.05
             (+ (* 0.05 oz) (* 0.02 (random/of-key t pos :sz)))]]
    (entity/item at vel {:item :pumpkin-seeds :count 4})))

(defn carve-deltas
  "Returns the deltas of shears carving the pumpkin at pos."
  [world eid pos face]
  (let [dir (carve-facing world eid face)
        carved (block/state :carved-pumpkin {:facing dir})]
    (concat
      (edit/change-deltas world [[pos carved]])
      [[:spawn-entity (seeds-drop world pos (dir/offset dir))]
       (out/all (out/block-sound :pumpkin/carve pos 1.0 1.0))])))

(def ^:private ^:table mud-blocks
  (delay (set (get-in (data/tags) ["block" "convertable_to_mud"]))))

(defn- muddable? [world pos]
  (contains? @mud-blocks (block/block-of (edit/block-at world pos))))

(defn mud-deltas
  "Returns the deltas of a water bottle poured on the block at pos."
  [world eid pos face]
  (when (and (not= 0 (long face))
             (muddable? world pos)
             (cauldron/water-bottle? (edit/held-stack world eid)))
    (concat (edit/change-deltas world [[pos (block/state :mud)]])
            [(out/all (out/block-sound :splash pos 1.0 1.0 :blocks))
             (out/all (out/block-sound :bottle/empty pos 1.0 1.0))]
            (items/filled-result-deltas
              world eid {:item :glass-bottle :count 1}))))
