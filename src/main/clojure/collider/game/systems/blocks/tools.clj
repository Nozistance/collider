(ns collider.game.systems.blocks.tools
  "Using tools and other items on blocks."
  (:require [collider.data :as data]
            [collider.game.block.tnt :as tnt]
            [collider.game.changes :as changes]
            [collider.game.entity :as entity]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.variant :as variant]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.game.player :as player]
            [collider.game.reach :as reach]
            [collider.game.systems.blocks.edit :as edit]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.block.oxidation :as oxidation]
            [collider.world.blocks.halves :as halves]
            [collider.world.blocks.fire :as fire]
            [collider.world.blocks.grow :as grow]
            [collider.world.blocks.grow.underwater :as underwater]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.biome :as biome]))

(set! *warn-on-reflection* true)

(defn- tool-set [tag] (delay (set (data/tag-values "item" tag))))

(def ^:private ^:table axe-items (tool-set "axes"))

(def ^:private ^:table hoe-items (tool-set "hoes"))

(def ^:private ^:table shovel-items (tool-set "shovels"))

(defn axes
  []
  @axe-items)

(defn hoes
  []
  @hoe-items)

(defn shovels
  []
  @shovel-items)

(def ^:private lightable-types #{:candle :candle-cake :campfire})

(defn- lightable [world pos]
  (let [cur (changes/block-at world pos) props (block/props-of cur)]
    (when (and (contains? lightable-types (block/type-of cur))
               (= :false (:lit props))
               (not= :true (:waterlogged props)))
      (block/with cur :lit :true))))

(defn- fire-deltas [world pos off snd]
  (let [[_ y' _ :as pos'] (mapv + pos off)
        st (fire/state-for (:chunks world) pos')]
    (when (and (chunk/in-level? world y')
               (zero? (changes/block-at world pos'))
               (support/supported? (:chunks world) pos' st))
      (conj (changes/change-deltas world [[pos' st]]) (snd pos')))))

(defn- flint-sound [world eid pos]
  (let [pitch (random/pitch (:tick world) pos :flint)]
    (out/except eid (out/block-sound :fire/ignite pos 1.0 pitch))))

(defn- charge-sound [world pos]
  (let [t (:tick world)
        a (random/of-key t pos :charge-a)
        b (random/of-key t pos :charge-b)
        pitch (random/triangle 1.0 0.2 a b)]
    (out/all (out/block-sound :firecharge/use pos 1.0 pitch))))

(defn- changed-with [world changes fx]
  (concat (changes/change-deltas world changes) fx))

(defn firecharge-deltas
  [world [_eid pos face]]
  (if-let [st (lightable world pos)]
    (changed-with world [[pos st]] [(charge-sound world pos)])
    (when-let [off (dir/face-offset face)]
      (fire-deltas world pos off #(charge-sound world %)))))

(defn- primable? [world eid pos]
  (and (tnt/tnt-state? (changes/block-at world pos))
       (get-in world [:rules :tnt-explodes] true)
       (not (get-in world [:entities eid :sneaking?]))
       (not ((tnt/primed-origins world) pos))))

(defn- prime-deltas [world eid pos]
  (let [primed (-> (tnt/primed pos [(:tick world) pos])
                   (assoc :owner eid))]
    (into (changes/change-deltas world [[pos 0]])
          [[:spawn-entity primed]
           (out/all (out/sound :tnt/primed (:pos primed) 1.0 1.0))])))

(defn prime-tnt-deltas
  "Returns the deltas of player eid lighting the tnt at pos, or nil
  when it does not light."
  [world eid pos]
  (when (primable? world eid pos)
    (prime-deltas world eid pos)))

(defn flint-deltas
  [world [eid pos face]]
  (when-let [off (dir/face-offset face)]
    (if-let [st (lightable world pos)]
      (changed-with world [[pos st]] [(flint-sound world eid pos)])
      (fire-deltas world pos off #(flint-sound world eid %)))))

(defn- meal-drops [world pos drops]
  (map-indexed
    (fn [i stack]
      [:spawn-entity (item/popped world pos stack [:meal i])])
    drops))

(defn- bonemealed [world pos]
  (let [st (changes/block-at world pos)
        salted #(random/of-key (:tick world) pos :meal %)]
    (grow/bonemeal (:chunks world) pos st salted (:dim world))))

(defn- crop-deltas [world pos]
  (when-let [{:keys [changes drops]} (bonemealed world pos)]
    (concat
      (when (seq changes) (changes/change-deltas world changes))
      (meal-drops world pos drops)
      [(out/all (out/bonemeal pos))])))

(def ^:private ^:table coral-biomes
  (delay (let [tag "produces_corals_from_bonemeal"]
           (set (data/tag-values "worldgen/biome" tag)))))

(defn- coral-biome? [world p]
  (contains? @coral-biomes (:name (biome/at (:dim world) p))))

(defn- seabed-deltas [world pos face]
  (let [side (dir/from-index face)
        at (dir/toward pos side)
        roll #(random/of-key (:tick world) at :seabed %)
        corals? #(coral-biome? world %)
        grow #(underwater/meal (:chunks world) at side roll corals?)]
    (when (block/face-sturdy? (changes/block-at world pos) side)
      (when-let [cs (grow)]
        (concat (changes/change-deltas world cs)
                [(out/all (out/bonemeal at))])))))

(defn bonemeal-deltas
  "Returns the deltas of bone meal used on the block at pos.
  On a floor under water it sprouts seagrass and coral."
  [world [eid pos face _ _]]
  (let [e (get-in world [:entities eid])]
    (when-let [ds (or (crop-deltas world pos)
                      (seabed-deltas world pos face))]
      (concat ds (inventory/consume-deltas eid e (:use-hand e) 1)
              [[:award eid :used/bone-meal 1]]))))

(def ^:private plant-heads
  #{:kelp :weeping-vines :twisting-vines :cave-vines})

(def ^:private tip-max-age :25)

(defn- grown-tip [st] (block/with st :age tip-max-age))

(defn shear-deltas
  "Returns the deltas of shears used on the tip of a growing plant.
  The tip stops growing."
  [world [eid pos _ _ _]]
  (let [st (changes/block-at world pos)
        e (get-in world [:entities eid])
        kind :block.growing-plant.crop
        snd (out/block-sound kind pos 1.0 1.0 :blocks)]
    (when (and (contains? plant-heads (block/type-of st))
               (not= tip-max-age (:age (block/props-of st))))
      (concat
        [(out/except eid snd)]
        (changes/change-deltas world [[pos (grown-tip st)]])
        (inventory/hurt-item-deltas (:tick world) eid e (:use-hand e) 1)
        [[:award eid :used/shears 1]]))))

(defn- tracker [world pos]
  {:target {:dimension (:dim world) :pos pos} :tracked true})

(defn- bound-compass [world eid e stack t]
  (let [one (stack/put (assoc stack :count 1) :lodestone-tracker t)]
    (concat (inventory/consume-deltas eid e (:use-hand e) 1)
            (inventory/kept world eid e one))))

(defn- lock-sound [pos]
  (out/block-sound :item.lodestone-compass.lock pos 1.0 1.0
                   :players))

(defn- tracked-deltas [world eid e t]
  (let [hand (:use-hand e)
        stack (player/hand-stack e hand)]
    (if (and (not (player/infinite-materials? e))
             (= 1 (stack/size stack)))
      [[:set-slot eid (player/hand-slot e hand)
        (stack/put stack :lodestone-tracker t)]]
      (bound-compass world eid e stack t))))

(defn compass-deltas
  "Returns the deltas of a compass used on a lodestone.
  After this the compass points at the lodestone."
  [world [eid pos _ _ _]]
  (when (= :lodestone (block/block-of (changes/block-at world pos)))
    (let [e (get-in world [:entities eid])]
      (concat
        [(out/all (lock-sound pos))]
        (tracked-deltas world eid e (tracker world pos))
        [[:award eid :used/compass 1]]))))

(defn- air-above? [world pos]
  (zero? (changes/block-at world (dir/up pos))))

(defn- freed-drop [world pos freed]
  (let [stack {:item freed :count 1}]
    [[:spawn-entity (item/popped world pos stack :till)]]))

(defn till-deltas
  [world [_eid pos _ _ _]]
  (let [cur (changes/block-at world pos)]
    (when-let [[to freed] (grow/tilled (block/block-of cur))]
      (when (or freed (air-above? world pos))
        (concat
          (changes/change-deltas world [[pos (block/state to)]])
          (when freed (freed-drop world pos freed))
          [(out/all (out/block-sound :hoe/till pos 1.0 1.0))])))))

(defn- flattened-state [world pos cur]
  (when (and (contains? grow/flattened (block/block-of cur))
             (air-above? world pos))
    (block/state :dirt-path)))

(defn flatten-deltas
  [world [_eid pos face _ _]]
  (let [cur (changes/block-at world pos)]
    (when (not= 0 (long face))
      (if-let [st (flattened-state world pos cur)]
        (let [snd (out/block-sound :shovel/flatten pos 1.0 1.0)]
          (changed-with world [[pos st]] [(out/all snd)]))
        (changes/campfire-out-deltas world pos)))))

(defn- door-partner [world pos cur]
  (when (contains? block/door-types (block/type-of cur))
    (halves/partner (:chunks world) pos cur)))

(defn- half-changes [world pos ^long st]
  (let [cur (changes/block-at world pos)]
    (if-let [[ppos pst] (door-partner world pos cur)]
      (let [props (block/props-of pst)]
        [[pos st] [ppos (block/state (block/block-of st) props)]])
      [[pos st]])))

(defn- copper-fx [pos snd particles]
  (conj (if snd [(out/all (out/block-sound snd pos 1.0 1.0))] [])
        (out/all (out/level-event particles pos))))

(defn wax-deltas
  [world [_ pos _ _ _]]
  (when-let [st (oxidation/waxed (changes/block-at world pos))]
    (let [fx (copper-fx pos nil out/particles-and-sound-wax-on)]
      (changed-with world (half-changes world pos st) fx))))

(defn- copper-axe-deltas [world pos cur]
  (if-let [st (oxidation/scraped cur)]
    (let [fx (copper-fx pos :axe/scrape out/particles-scrape)]
      (changed-with world (half-changes world pos st) fx))
    (when-let [st (oxidation/unwaxed cur)]
      (let [fx (copper-fx pos :axe/wax-off out/particles-wax-off)]
        (changed-with world (half-changes world pos st) fx)))))

(defn axe-deltas
  [world [_ pos _ _ _]]
  (let [cur (changes/block-at world pos)]
    (if-let [st (block/stripped cur)]
      (let [snd (out/block-sound :axe/strip pos 1.0 1.0)]
        (changed-with world [[pos st]] [(out/all snd)]))
      (copper-axe-deltas world pos cur))))

(defn- egg-pitch ^double [t pos]
  (random/triangle 1.0 0.2 (random/of-key t pos :p1)
                   (random/of-key t pos :p2)))

(defn- hatch-deltas [world pos mob at]
  (let [t (:tick world)
        place (variant/place world (:dim world) at)
        hatched (mobs/egg-mob mob at [t pos] t place)
        pitch (egg-pitch t pos)]
    (cons [:spawn-entity hatched]
          (when-let [say (mobs/sound-of hatched :say)]
            [(out/all (out/sound say at 1.0 pitch))]))))

(defn- egg-deltas [world eid pos mob at]
  (let [e (get-in world [:entities eid])]
    (concat (hatch-deltas world pos mob at)
            (inventory/consume-deltas eid e (:use-hand e) 1))))

(defn spawn-egg-deltas
  "Returns the deltas of a spawn egg used on a block face.
  The mob hatches next to the block at pos."
  [world [eid pos face item]]
  (when-let [off (dir/face-offset face)]
    (when-let [mob (mobs/egg-type item)]
      (let [[_ y _ :as cell] (mapv + pos off)
            at (v/bottom-centre cell)]
        (when (chunk/in-range? y)
          (concat (egg-deltas world eid pos mob at)
                  [[:award eid (keyword "used" (name item)) 1]]))))))

(defn- hatches-at? [world e {:keys [pos face]}]
  (and (block/liquid? (changes/block-at world pos))
       (let [behind (dir/toward pos (dir/opposite face))]
         (edit/may-use-at? world e behind))))

(defn fluid-egg-deltas
  "Returns the deltas of a spawn egg used at a liquid source.
  The mob hatches in the liquid source in view."
  [world eid e item]
  (when-let [mob (mobs/egg-type item)]
    (when-let [{:keys [pos] :as hit}
               (reach/clip world e :source-only)]
      (when (hatches-at? world e hit)
        (let [at (v/bottom-centre pos)]
          (concat (egg-deltas world eid pos mob at)
                  [[:award eid (keyword "used" (name item)) 1]]))))))

(defn- carve-facing [world eid face]
  (if (<= (long face) 1)
    (let [yaw (get-in world [:entities eid :yaw] 0.0)]
      (dir/opposite (dir/player-direction yaw)))
    (get dir/horizontal-face face)))

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
  [world eid pos face]
  (let [facing (carve-facing world eid face)
        carved (block/state :carved-pumpkin {:facing facing})]
    (concat
      (changes/change-deltas world [[pos carved]])
      [[:spawn-entity (seeds-drop world pos (dir/offset facing))]
       (out/all (out/block-sound :pumpkin/carve pos 1.0 1.0))])))

(def ^:private ^:table mud-blocks
  (delay (set (get-in (data/tags) ["block" "convertable_to_mud"]))))

(defn- muddable? [world pos]
  (contains? @mud-blocks
             (block/block-of (changes/block-at world pos))))

(defn- mud-splashes
  "Returns the five splashes over a block made mud, each at a random
  spot of its top."
  [world [x y z :as pos]]
  (for [i (range 5)
        :let [r #(random/of-key (:tick world) pos [:mud-splash i %])]]
    (out/all (out/particles :splash nil
                            [(+ (double x) (double (r 0))) (+ (long y) 1.0)
                             (+ (double z) (double (r 1)))]
                            1 1.0))))

(defn mud-deltas
  "Returns the deltas of a water bottle poured on the block at pos."
  [world eid pos face]
  (when (and (not= 0 (long face))
             (muddable? world pos)
             (stack/water-bottle? (player/use-stack world eid)))
    (concat (changes/change-deltas world [[pos (block/state :mud)]])
            (mud-splashes world pos)
            [(out/all (out/block-sound :splash pos 1.0 1.0 :blocks))
             (out/all (out/block-sound :bottle/empty pos 1.0 1.0))]
            (inventory/filled-result-deltas
              world eid {:item :glass-bottle :count 1}))))
