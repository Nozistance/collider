(ns collider.game.systems.blocks.use
  "The use of a block, by hand or with an item in hand."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.campfire :as campfire]
            [collider.game.block.container :as container]
            [collider.game.block.jukebox :as jukebox]
            [collider.game.block.lectern :as lectern]
            [collider.game.block.sign :as sign]
            [collider.game.changes :as changes]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.num :as num]
            [collider.game.systems.blocks.cauldron :as cauldron]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.tools :as tools]
            [collider.game.systems.containers :as containers]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.dragonegg :as dragonegg]
            [collider.world.blocks.lectern :as block-lectern]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- heard [kind pos pitch]
  (out/all (out/block-sound kind pos 1.0 pitch)))

(defn- potted-block [item] (get-in (data/blocks) [item :pot]))

(defn- set-at [world pos st]
  (changes/change-deltas world [[pos st]]))

(defn- stack-deltas [world eid stack]
  (inventory/kept world eid (get-in world [:entities eid]) stack))

(defn- consume-deltas [world eid]
  (let [e (get-in world [:entities eid])]
    (inventory/consume-deltas eid e (:use-hand e) 1)))

(defn- plant-deltas [world eid pos plant]
  (concat (set-at world pos (block/state plant))
          [[:award eid :custom/pot-flower 1]]
          (consume-deltas world eid)))

(defn- unpot-deltas [world eid pos n]
  (let [plant (:potted (get (data/blocks) n))]
    (concat (set-at world pos (block/state :flower-pot))
            (stack-deltas world eid {:item plant :count 1}))))

(defn- candle-item? [item]
  (= :candle (:type (get (data/blocks) item))))

(defn- candle-cake-deltas [world eid pos item]
  (let [cur (changes/block-at world pos)
        cake (keyword (str (name item) "-cake"))]
    (when (and (zero? (block/prop-long cur :bites))
               (contains? (data/blocks) cake))
      (concat (consume-deltas world eid)
              (set-at world pos (block/state cake))
              [(heard :cake/add-candle pos 1.0)
               [:award eid (keyword "used" (name item)) 1]]))))

(defn- main-hand?
  "Returns true when player eid uses the main hand. Only the main
  hand uses a block without an item."
  [world eid]
  (not= :off (get-in world [:entities eid :use-hand])))

(defn- pot-deltas
  "Returns the deltas of a flower pot used with item. A plant goes
  into an empty pot. The main hand takes the plant out of a full pot."
  [world eid pos item]
  (let [n (block/block-of (changes/block-at world pos))
        plant (potted-block item)
        empty? (= :flower-pot n)]
    (cond
      (and plant empty?) (plant-deltas world eid pos plant)
      plant []
      (not (main-hand? world eid)) nil
      empty? []
      :else (unpot-deltas world eid pos n))))

(def ^:private ^:const full-food 20)

(defn- eats?
  "Returns true when player eid eats.
  It eats when its mode keeps it from harm or when it is hungry."
  [world eid]
  (let [e (get-in world [:entities eid])]
    (and (main-hand? world eid)
         (or (game-mode/invulnerable? e)
             (< (long (:food e full-food)) full-food)))))

(def ^:private ^:const max-bites 6)

(defn- bitten ^long [^long st]
  (let [bites (block/prop-long st :bites)]
    (if (< bites max-bites)
      (block/with-long st :bites (inc bites))
      0)))

(defn- eat-deltas [world eid pos cake fx]
  (when (eats? world eid)
    (cons [:award eid :custom/eat-cake-slice 1]
          (changes/change-deltas world [[pos (bitten cake) fx]]))))

(defn- cake-use [{:keys [world eid pos item]}]
  (or (when (candle-item? item)
        (candle-cake-deltas world eid pos item))
      (eat-deltas world eid pos (changes/block-at world pos) nil)))

(defn- candle-hit? [cursor]
  (> (double (nth cursor 1)) 8.0))

(defn- lit? [st] (= :true (:lit (block/props-of st))))

(defn- candle-cake-use [{:keys [world eid pos item cursor]}]
  (let [cur (changes/block-at world pos)]
    (cond
      (#{:flint-and-steel :fire-charge} item) nil
      (and (nil? item) (candle-hit? cursor) (lit? cur))
      (changes/candle-out-deltas world pos)
      :else
      (eat-deltas world eid pos (block/state :cake) [[:drop cur]]))))

(defn- berries-deltas [world pos]
  (let [cur (changes/block-at world pos)
        props (block/props-of cur)
        pitch (random/pitch (:tick world) pos :berries)
        berries {:item :glow-berries :count 1}
        picked (block/with cur :berries :false)]
    (when (= :true (:berries props))
      (concat
        (set-at world pos picked)
        [[:spawn-entity (item/popped world pos berries :berries)]
         (heard :cave-vines/pick-berries pos pitch)]))))

(def ^:private ^:const bush-max-age 3)

(def ^:private ^:const dust-plume-particles 7)

(def ^:private ^:const berry-rolls 2.0)

(defn- picks-berries? [^long cur item]
  (and (= :sweet-berry-bush (block/type-of cur))
       (> (block/prop-long cur :age) 1)
       (not (and (= :bone-meal item)
                 (< (block/prop-long cur :age) bush-max-age)))))

(defn- bush-stacks [world pos ^long a]
  (let [roll (random/of-key (:tick world) pos :bush-count)
        n (inc (num/floor (* berry-rolls (double roll))))
        rolled {:item :sweet-berries :count n}]
    (if (= bush-max-age a)
      [{:item :sweet-berries :count 1} rolled]
      [rolled])))

(defn- bush-deltas [world pos]
  (let [cur (changes/block-at world pos)
        stacks (bush-stacks world pos (block/prop-long cur :age))
        pitch (random/pitch (:tick world) pos :bush-pitch)]
    (concat
      (set-at world pos (block/state :sweet-berry-bush {:age :1}))
      (map-indexed (fn [i stack]
                     [:spawn-entity
                      (item/popped world pos stack [:bush i])])
                   stacks)
      [(heard :sweet-berry-bush/pick-berries pos pitch)])))

(def ^:private statue-types
  #{:copper-golem-statue :weathering-copper-golem-statue})

(def ^:private next-pose
  {:standing :sitting :sitting :running :running :star
   :star :standing})

(defn- poses? [cur item]
  (and (contains? statue-types (block/type-of cur))
       (some? item)
       (not ((tools/axes) item))))

(defn- pose-deltas [{:keys [world pos]}]
  (let [cur (changes/block-at world pos)
        pose (next-pose (:copper-golem-pose (block/props-of cur)))]
    (concat (set-at world pos
                    (block/with cur :copper-golem-pose pose))
            [(heard :copper-golem/statue pos 1.0)])))

(defn- compost-took? [world pos ^long lvl item]
  (or (zero? lvl)
      (< (random/of-key (:tick world) pos :compost)
         (double (data/compost item)))))

(defn- compost-fill-deltas [world pos ^long lvl item]
  (let [took? (compost-took? world pos lvl item)
        level (keyword (str (inc lvl)))
        st (block/state :composter {:level level})
        fill (out/level-event out/composter-fill pos (if took? 1 0))]
    (concat (when took? (set-at world pos st))
            [(out/all fill)])))

(defn- compost-empty-deltas [world pos]
  (let [bone-meal {:item :bone-meal :count 1}
        at (dir/up pos)]
    (concat (set-at world pos (block/state :composter {:level :0}))
            [[:spawn-entity
              (item/popped world at bone-meal :compost)]
             (heard :composter/empty pos 1.0)])))

(defn- compost-deltas [{:keys [world pos item]}]
  (let [cur (changes/block-at world pos)
        lvl (block/prop-long cur :level)]
    (cond
      (and item (< lvl 8) (data/compost item))
      (when (< lvl 7) (compost-fill-deltas world pos lvl item))
      (and (nil? item) (= lvl 8)) (compost-empty-deltas world pos))))

(defn- campfire-use-deltas [{:keys [world pos item]}]
  (when-let [e (be/at world pos)]
    (when-let [e' (and item (campfire/place-food e item))]
      (changes/be-changed pos e'))))

(defn- sign-busy? [world eid e]
  (and (:editor e) (not= eid (:editor e))
       (get-in world [:entities (:editor e)])))

(defn- sign-apply-deltas [pos e front? item]
  (when-let [[e' sound] (sign/applied e front? item)]
    (concat [[:set-block-entity pos e']
             (out/all (out/block-entity pos))]
            [(out/all
               (if (= :wax sound)
                 (out/level-event out/particles-and-sound-wax-on pos)
                 (out/block-sound sound pos 1.0 1.0)))])))

(defn- sign-hand-deltas [world eid pos e front? busy?]
  (when (main-hand? world eid)
    (cond
      (:waxed? e) [(heard :sign/waxed pos 1.0)]
      (not busy?) [[:set-block-entity pos (assoc e :editor eid)]
                   (out/to eid (out/sign-editor pos front?))])))

(defn- chains?
  "Returns true when a hanging sign item places a new sign.
  The item is used on face of the sign st."
  [^long st face item]
  (let [d (dir/from-index (long face))
        across? (not= (dir/axis d) (dir/axis (block/facing-of st)))]
    (and (= :ceiling-hanging-sign (:type (get (data/blocks) item)))
         (case (block/type-of st)
           :ceiling-hanging-sign (= :down d)
           :wall-hanging-sign across?
           false))))

(defn- sign-use-deltas
  "Returns the deltas of a sign used with item. A dye, ink or
  honeycomb changes the side the player faces. The main hand opens
  the editor."
  [{:keys [world eid pos face item]}]
  (let [st (changes/block-at world pos) e (be/at world pos)
        at (get-in world [:entities eid :pos])
        front? (sign/front? st pos at)
        busy? (sign-busy? world eid e)]
    (when (and e (not (chains? st face item)))
      (or (when (and item (not (:waxed? e)) (not busy?))
            (sign-apply-deltas pos e front? item))
          (sign-hand-deltas world eid pos e front? busy?)))))

(defn sign-update-deltas
  "Returns the deltas that write lines on one side of a sign.
  Only the editor eid writes the sign at pos, and not when waxed."
  [world [eid pos front? lines]]
  (let [e (be/at world pos)]
    (when (and e (not (:waxed? e)) (= eid (:editor e)))
      [[:set-block-entity pos (sign/written e front? lines)]
       (out/all (out/block-entity pos))])))

(defn- pot-insertable? [e item]
  (and item
       (let [cur (:item e)]
         (or (nil? cur)
             (and (= (:item cur) item)
                  (< (long (:count cur)) (data/max-stack item)))))))

(defn- pot-insert-deltas [pos e item]
  (let [stack (if (:item e)
                (update (:item e) :count inc)
                {:item item :count 1})
        full (/ (double (:count stack)) (data/max-stack item))
        pitch (+ 0.7 (* 0.5 full))
        [x y z] pos
        plume [(+ (long x) 0.5) (+ (long y) 1.2) (+ (long z) 0.5)]
        dust (out/particles
               :dust-plume nil plume dust-plume-particles 0.0)]
    (concat (changes/be-changed pos (assoc e :item stack))
            [(out/all (out/block-event pos 1 0))
             (heard :decorated-pot/insert pos pitch)
             (out/all dust)])))

(defn- pot-use-deltas [{:keys [world pos item]}]
  (when-let [e (be/at world pos)]
    (if (pot-insertable? e item)
      (pot-insert-deltas pos e item)
      [(heard :decorated-pot/insert-fail pos 1.0)
       (out/all (out/block-event pos 1 1))])))

(defn- jukebox-eject-deltas [world pos e]
  (let [cur (changes/block-at world pos)
        at (dir/up pos)]
    (concat
      (set-at world pos (block/with cur :has-record :false))
      (changes/be-changed
        pos (assoc e :record nil :song nil :started nil))
      [[:spawn-entity (item/popped world at (:record e) :jukebox)]
       (out/all
         (out/level-event out/sound-stop-jukebox-song pos 0))])))

(defn- jukebox-insert-deltas [world pos e item]
  (let [cur (changes/block-at world pos)
        song (jukebox/song-of item)
        e' (assoc e :record {:item item :count 1} :song song
                    :started (:tick world))
        id (jukebox/song-id song)
        play (out/level-event out/sound-play-jukebox-song pos id)]
    (concat
      (set-at world pos (block/with cur :has-record :true))
      (changes/be-changed pos e')
      [(out/all play)])))

(defn- jukebox-use-deltas [{:keys [world pos item]}]
  (let [e (be/at world pos)
        props (block/props-of (changes/block-at world pos))
        has? (= :true (:has-record props))]
    (cond
      (nil? e) nil
      has? (when (:record e) (jukebox-eject-deltas world pos e))
      (jukebox/song-of item)
      (jukebox-insert-deltas world pos e item))))

(defn- shelf-swap-deltas [world eid pos e slot]
  (let [removed (get-in e [:items slot])
        stack (player/use-stack world eid)
        e' (assoc-in e [:items slot] stack)
        taken (if stack :shelf/single-swap :shelf/take-item)]
    (cond
      removed (concat (changes/be-changed pos e')
                      [[:set-slot eid (player/use-slot world eid)
                        removed]
                       (heard taken pos 1.0)])
      (nil? stack) nil
      :else (concat (changes/be-changed pos e')
                    [(heard :shelf/place-item pos 1.0)]))))

(defn- shelf-use-deltas [{:keys [world eid pos face cursor]}]
  (let [st (changes/block-at world pos) e (be/at world pos)
        slot (edit/hit-slot st face cursor 1 3)
        powered? (signal/has-neighbor-signal? (:chunks world) pos)]
    (when (and e slot (not powered?))
      (shelf-swap-deltas world eid pos e slot))))

(def ^:private ^:table bookshelf-books
  (delay (set (get-in (data/tags) ["item" "bookshelf_books"]))))

(defn- book-item? [item]
  (contains? @bookshelf-books item))

(defn- slot-prop [^long i]
  (keyword (str "slot-" i "-occupied")))

(defn- bookshelf-state [^long st items]
  (let [occupied (fn [m ^long i]
                   (assoc m (slot-prop i)
                     (if (nth items i) :true :false)))
        props (reduce occupied (block/props-of st) (range 6))]
    (block/state (block/block-of st) props)))

(defn- occupied? [^long st slot]
  (= :true (get (block/props-of st) (slot-prop slot))))

(defn- shelved [world pos e slot stack]
  (let [items (assoc (:items e) slot stack)
        st (bookshelf-state (changes/block-at world pos) items)]
    (concat (set-at world pos st)
            [[:set-block-entity pos
              (assoc e :items items :last-slot slot)]])))

(defn- one-held [world eid item]
  (let [held (player/use-stack world eid)]
    (assoc (if (= item (:item held)) held {:item item}) :count 1)))

(defn- bookshelf-add-deltas [world eid pos e slot item]
  (let [stack (one-held world eid item)
        sound (if (= :enchanted-book item)
                :bookshelf/insert-enchanted
                :bookshelf/insert)]
    (concat [[:award eid (keyword "used" (name item)) 1]]
            (consume-deltas world eid)
            (shelved world pos e slot stack)
            [(heard sound pos 1.0)])))

(defn- bookshelf-take-deltas [world eid pos e slot]
  (let [stack (nth (:items e) slot)
        sound (if (= :enchanted-book (:item stack))
                :bookshelf/pickup-enchanted
                :bookshelf/pickup)]
    (concat (when stack (shelved world pos e slot nil))
            [(heard sound pos 1.0)]
            (when stack (stack-deltas world eid stack)))))

(defn- bookshelf-hand-deltas
  "Returns the deltas of a bookshelf slot used by hand. An empty slot
  takes the click and does nothing."
  [world eid pos e slot full?]
  (when (and slot (main-hand? world eid))
    (if full? (bookshelf-take-deltas world eid pos e slot) [])))

(defn- bookshelf-use-deltas [{:keys [world eid pos face item cursor]}]
  (let [st (changes/block-at world pos) e (be/at world pos)
        slot (edit/hit-slot st face cursor 2 3)
        full? (and slot (occupied? st slot))]
    (when e
      (cond
        (not (book-item? item))
        (bookshelf-hand-deltas world eid pos e slot full?)
        (nil? slot) nil
        full? (bookshelf-hand-deltas world eid pos e slot full?)
        :else (bookshelf-add-deltas world eid pos e slot item)))))

(defn- bell-side? [^long st side]
  (let [same? (= (dir/axis (block/facing-of st)) (dir/axis side))]
    (case (:attachment (block/props-of st))
      :floor same?
      (:single_wall :double_wall) (not same?)
      :ceiling true
      false)))

(def ^:private ^:const bell-hit-top 0.8124)

(defn- bell-hit? [^long st face ^double cursor-y]
  (let [side (dir/from-index (long face))]
    (and (contains? dir/horizontal-offset side)
         (not (> (/ cursor-y 16.0) bell-hit-top))
         (bell-side? st side))))

(defn- bell-use-deltas [{:keys [world pos face cursor]}]
  (let [st (changes/block-at world pos)
        side (get dir/index (dir/from-index (long face)))]
    (when (bell-hit? st face (double (nth cursor 1)))
      [(out/all (out/block-event pos 1 side))
       (out/all (out/block-sound :bell/use pos 2.0 1.0))])))

(defn- dragon-egg-deltas [{:keys [world pos]}]
  (let [st (changes/block-at world pos)
        roll (fn [k] (random/of-key (:tick world) pos :egg k))]
    (when-let [target (dragonegg/teleport-target
                        (:chunks world) pos roll)]
      (changes/change-deltas world [[pos 0] [target st]]))))

(def ^:private ^:const light-levels 16)

(defn- light-deltas [{:keys [world pos]}]
  (let [st (changes/block-at world pos)
        lvl (rem (inc (block/prop-long st :level)) light-levels)]
    (set-at world pos (block/with-long st :level lvl))))

(defn- lectern-use-deltas
  "Returns the deltas of a lectern used with item. The main hand
  reads a book on it. A book goes on an empty lectern."
  [{:keys [world eid pos item]}]
  (let [st (changes/block-at world pos)
        stack (player/use-stack world eid)
        main? (main-hand? world eid)]
    (cond
      (block-lectern/has-book? st)
      (when main? (containers/open-deltas world eid pos))
      (lectern/book? stack)
      (concat (lectern/place-book-deltas world pos st stack)
              (consume-deltas world eid))
      (and main? item) [])))

(defn- flower-pot-use [{:keys [world eid pos item]}]
  (pot-deltas world eid pos item))

(defn- candle-use [{:keys [world pos item]}]
  (when (nil? item) (changes/candle-out-deltas world pos)))

(defn- berries-use [{:keys [world pos item]}]
  (when (nil? item) (berries-deltas world pos)))

(defn- bush-use [{:keys [world pos item]}]
  (when (picks-berries? (changes/block-at world pos) item)
    (bush-deltas world pos)))

(def ^:private by-type
  {:flower-pot flower-pot-use
   :candle candle-use
   :candle-cake candle-cake-use
   :cake cake-use
   :cave-vines berries-use
   :cave-vines-plant berries-use
   :sweet-berry-bush bush-use
   :composter compost-deltas
   :decorated-pot pot-use-deltas
   :jukebox jukebox-use-deltas
   :campfire campfire-use-deltas
   :shelf shelf-use-deltas
   :chiseled-book-shelf bookshelf-use-deltas
   :bell bell-use-deltas
   :lectern lectern-use-deltas
   :dragon-egg dragon-egg-deltas
   :light light-deltas})

(defn- game-master-use
  "Returns an empty use when a game master uses a game master block.
  The client opens its screen itself."
  [{:keys [world eid pos]}]
  (when (and (main-hand? world eid) (be/at world pos)
             (edit/game-master? (get-in world [:entities eid])))
    []))

(def ^:private menu-pending-stats
  {:enchantment-table nil
   :beacon :custom/interact-with-beacon
   :cartography-table :custom/interact-with-cartography-table
   :hopper :custom/inspect-hopper
   :dispenser :custom/inspect-dispenser
   :dropper :custom/inspect-dropper
   :crafter nil})

(defn- menu-use [{:keys [world eid pos]}]
  (when (main-hand? world eid)
    (let [t (block/type-of (changes/block-at world pos))]
      (if-let [stat (menu-pending-stats t)]
        [[:award eid stat 1]]
        []))))

(defn- open-use [{:keys [world eid pos]}]
  (when (main-hand? world eid)
    (containers/open-deltas world eid pos)))

(defn- cauldron-use [{:keys [world eid pos item]}]
  (let [held (player/use-stack world eid)]
    (cauldron/cauldron-deltas world eid pos item held)))

(defn- carve-use [{:keys [world eid pos face]}]
  (tools/carve-deltas world eid pos face))

(defn- carves? [cur item]
  (and (= :pumpkin (block/block-of cur)) (= :shears item)))

(defn- handler [cur item]
  (let [t (block/type-of cur)]
    (or (by-type t)
        (cond
          (poses? cur item) pose-deltas
          (contains? block/cauldron-types t) cauldron-use
          (edit/game-master-block? cur) game-master-use
          (sign/kind cur) sign-use-deltas
          (contains? container/menu-types t) open-use
          (contains? menu-pending-stats t) menu-use
          (carves? cur item) carve-use))))

(defn deltas
  "Returns the deltas of player eid using the block at pos.
  The player clicks face at cursor with item in hand. Returns nil
  when the block does nothing."
  [world eid pos face item cursor]
  (let [cur (changes/block-at world pos)]
    (when-let [h (handler cur item)]
      (h {:world world :eid eid :pos pos :face face :item item
          :cursor cursor}))))
