(ns collider.game.systems.blocks.use
  "The use of a block, by hand or with an item in hand."
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.block.jukebox :as jukebox]
            [collider.game.block.sign :as sign]
            [collider.game.game-mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.systems.blocks.cauldron :as cauldron]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.tools :as tools]
            [collider.game.systems.campfires :as campfires]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.dragonegg :as dragonegg]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- heard [kind pos pitch]
  (out/all (out/block-sound kind pos 1.0 pitch)))

(defn- potted-block [item] (get-in (data/blocks) [item :pot]))

(defn- set-at [world pos st]
  (edit/change-deltas world [[pos st]]))

(defn- with-props [cur props]
  (block/state (block/block-of cur) props))

(defn- inventory [world eid]
  (get-in world [:entities eid :inventory]))

(defn- stack-deltas [world eid stack]
  (let [[changes left] (items/add-stack (inventory world eid) stack)]
    (concat (for [[slot s] changes] [:set-slot eid slot s])
            (when left
              [[:spawn-entity (items/dropped world eid left)]]))))

(defn- pot-deltas [world eid pos item]
  (let [n (block/block-of (edit/block-at world pos))]
    (cond
      (and (= :flower-pot n) item (potted-block item))
      (set-at world pos (block/state (potted-block item)))
      (and (not= :flower-pot n) (nil? item))
      (let [plant (:potted (get (data/blocks) n))]
        (concat (set-at world pos (block/state :flower-pot))
                (stack-deltas world eid {:item plant :count 1}))))))

(defn- candle-item? [item]
  (= :candle (:type (get (data/blocks) item))))

(defn- candle-cake-deltas [world eid pos item]
  (let [cur (edit/block-at world pos)
        cake (keyword (str (name item) "-cake"))
        e (get-in world [:entities eid])]
    (when (and (zero? (block/prop-long cur :bites))
               (contains? (data/blocks) cake))
      (concat (items/consume-deltas eid e (:use-hand e) 1)
              (set-at world pos (block/state cake))
              [(heard :cake/add-candle pos 1.0)
               [:award eid (keyword "used" (name item)) 1]]))))

(def ^:private ^:const max-bites 6)

(defn- eats? [world eid]
  (let [e (get-in world [:entities eid])]
    (and (not= :off (:use-hand e))
         (:invulnerable? (game-mode/abilities e)))))

(defn- bitten ^long [^long st]
  (let [bites (block/prop-long st :bites)]
    (if (< bites max-bites)
      (with-props st {:bites (keyword (str (inc bites)))})
      0)))

(defn- eat-deltas [world eid pos cake fx]
  (when (eats? world eid)
    (cons [:award eid :custom/eat-cake-slice 1]
          (edit/change-deltas world [[pos (bitten cake) fx]]))))

(defn- cake-use [w eid pos _ item _]
  (or (when (candle-item? item) (candle-cake-deltas w eid pos item))
      (eat-deltas w eid pos (edit/block-at w pos) nil)))

(defn- candle-hit? [cursor]
  (> (double (nth cursor 1)) 8.0))

(defn- lit? [st] (= :true (:lit (block/props-of st))))

(defn- candle-cake-use [w eid pos _ item cursor]
  (let [cur (edit/block-at w pos)]
    (cond
      (#{:flint-and-steel :fire-charge} item) nil
      (and (nil? item) (candle-hit? cursor) (lit? cur))
      (edit/candle-out-deltas w pos)
      :else
      (eat-deltas w eid pos (block/state :cake) [[:drop cur]]))))

(defn- berries-deltas [world pos]
  (let [cur (edit/block-at world pos)
        props (block/props-of cur)
        pitch (random/pitch (:tick world) pos :berries)
        berries {:item :glow-berries :count 1}
        picked (with-props cur (assoc props :berries :false))]
    (when (= :true (:berries props))
      (concat
        (set-at world pos picked)
        [[:spawn-entity (items/popped world pos berries :berries)]
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
        n (inc (long (Math/floor (* berry-rolls roll))))]
    (cond-> []
      (= bush-max-age a) (conj {:item :sweet-berries :count 1})
      true (conj {:item :sweet-berries :count n}))))

(defn- bush-deltas [world pos]
  (let [cur (edit/block-at world pos)
        stacks (bush-stacks world pos (block/prop-long cur :age))
        pitch (random/pitch (:tick world) pos :bush-pitch)]
    (concat
      (set-at world pos (block/state :sweet-berry-bush {:age :1}))
      (map-indexed (fn [i stack]
                     [:spawn-entity
                      (items/popped world pos stack [:bush i])])
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

(defn- pose-deltas [world pos]
  (let [cur (edit/block-at world pos)
        props (update (block/props-of cur) :copper-golem-pose
                      next-pose)]
    (concat (set-at world pos (with-props cur props))
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
        at (mapv + pos [0 1 0])]
    (concat (set-at world pos (block/state :composter {:level :0}))
            [[:spawn-entity
              (items/popped world at bone-meal :compost)]
             (heard :composter/empty pos 1.0)])))

(defn- compost-deltas [world pos item]
  (let [cur (edit/block-at world pos)
        lvl (block/prop-long cur :level)]
    (cond
      (and item (< lvl 8) (data/compost item))
      (when (< lvl 7) (compost-fill-deltas world pos lvl item))
      (and (nil? item) (= lvl 8)) (compost-empty-deltas world pos))))

(defn- campfire-use-deltas [world _eid pos _face item _cursor]
  (when-let [e (be/at world pos)]
    (when-let [e' (and item (campfires/place-food e item))]
      (edit/be-changed pos e'))))

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

(defn- sign-use-deltas [world eid pos item]
  (let [st (edit/block-at world pos) e (sign/at world pos)
        at (get-in world [:entities eid :pos])
        front? (sign/front? st pos at)
        busy? (sign-busy? world eid e)]
    (cond
      (nil? e) nil
      (and item (not (:waxed? e)) (not busy?))
      (sign-apply-deltas pos e front? item)
      (some? item) nil
      (:waxed? e) [(heard :sign/waxed pos 1.0)]
      (not busy?) [[:set-block-entity pos (assoc e :editor eid)]
                   (out/to eid (out/sign-editor pos front?))])))

(defn sign-update-deltas
  "Returns the deltas that write lines on one side of the sign at pos,
  when player eid edits it and it is not waxed."
  [world [eid pos front? lines]]
  (let [e (sign/at world pos)]
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
    (concat (edit/be-changed pos (assoc e :item stack))
            [(out/all (out/block-event pos 1 0))
             (heard :decorated-pot/insert pos pitch)
             (out/all dust)])))

(defn- pot-use-deltas [world pos item]
  (when-let [e (be/at world pos)]
    (if (pot-insertable? e item)
      (pot-insert-deltas pos e item)
      [(heard :decorated-pot/insert-fail pos 1.0)
       (out/all (out/block-event pos 1 1))])))

(defn- jukebox-eject-deltas [world pos e]
  (let [cur (edit/block-at world pos)
        at (mapv + pos [0 1 0])]
    (concat
      (set-at world pos (with-props cur {:has-record :false}))
      (edit/be-changed pos
                       (assoc e :record nil :song nil :started nil))
      [[:spawn-entity (items/popped world at (:record e) :jukebox)]
       (out/all
         (out/level-event out/sound-stop-jukebox-song pos 0))])))

(defn- jukebox-insert-deltas [world pos e item]
  (let [cur (edit/block-at world pos)
        song (jukebox/song-of item)
        e' (assoc e :record {:item item :count 1} :song song
                    :started (:tick world))
        id (jukebox/song-id song)
        play (out/level-event out/sound-play-jukebox-song pos id)]
    (concat
      (set-at world pos (with-props cur {:has-record :true}))
      (edit/be-changed pos e')
      [(out/all play)])))

(defn- jukebox-use-deltas [world pos item]
  (let [e (be/at world pos)
        props (block/props-of (edit/block-at world pos))
        has? (= :true (:has-record props))]
    (cond
      (nil? e) nil
      has? (when (:record e) (jukebox-eject-deltas world pos e))
      (jukebox/song-of item)
      (jukebox-insert-deltas world pos e item))))

(defn- shelf-swap-deltas [world eid pos e slot]
  (let [removed (get-in e [:items slot])
        stack (edit/held-stack world eid)
        e' (assoc-in e [:items slot] stack)
        taken (if stack :shelf/single-swap :shelf/take-item)]
    (cond
      removed (concat (edit/be-changed pos e')
                      [[:set-slot eid (edit/held-slot world eid)
                        removed]
                       (heard taken pos 1.0)])
      (nil? stack) nil
      :else (concat (edit/be-changed pos e')
                    [(heard :shelf/place-item pos 1.0)]))))

(defn- shelf-use-deltas [world eid pos face cursor]
  (let [st (edit/block-at world pos) e (be/at world pos)
        slot (edit/hit-slot st face cursor 1 3)
        powered? (signal/has-neighbor-signal? (:chunks world) pos)]
    (when (and e slot (not powered?))
      (shelf-swap-deltas world eid pos e slot))))

(def ^:private ^:table book-items
  (delay (set (get-in (data/tags) ["item" "bookshelf_books"]))))

(defn- book-item? [item]
  (contains? @book-items item))

(defn- slot-prop [^long i]
  (keyword (str "slot-" i "-occupied")))

(defn- bookshelf-state [^long st items]
  (let [occupied (fn [m ^long i]
                   (assoc m (slot-prop i)
                     (if (nth items i) :true :false)))
        props (reduce occupied (block/props-of st) (range 6))]
    (with-props st props)))

(defn- bookshelf-add-deltas [world pos e slot item]
  (let [st (edit/block-at world pos)
        items (assoc (:items e) slot {:item item :count 1})
        sound (if (= :enchanted-book item)
                :bookshelf/insert-enchanted
                :bookshelf/insert)]
    (concat (set-at world pos (bookshelf-state st items))
            [[:set-block-entity pos
              (assoc e :items items :last-slot slot)]
             (heard sound pos 1.0)])))

(defn- bookshelf-take-deltas [world eid pos e slot]
  (let [st (edit/block-at world pos)
        stack (nth (:items e) slot)
        items (assoc (:items e) slot nil)
        sound (if (= :enchanted-book (:item stack))
                :bookshelf/pickup-enchanted
                :bookshelf/pickup)]
    (concat (set-at world pos (bookshelf-state st items))
            [[:set-block-entity pos
              (assoc e :items items :last-slot slot)]
             (heard sound pos 1.0)]
            (stack-deltas world eid stack))))

(defn- bookshelf-use-deltas [world eid pos face item cursor]
  (let [st (edit/block-at world pos) e (be/at world pos)
        slot (edit/hit-slot st face cursor 2 3)]
    (when (and e slot)
      (let [taken (nth (:items e) slot)]
        (cond
          (and (book-item? item) (nil? taken))
          (bookshelf-add-deltas world pos e slot item)
          taken (bookshelf-take-deltas world eid pos e slot))))))

(defn- bell-side? [^long st dir]
  (let [same? (= (dir/axis (block/facing-of st)) (dir/axis dir))]
    (case (:attachment (block/props-of st))
      :floor same?
      (:single_wall :double_wall) (not same?)
      :ceiling true
      false)))

(defn- bell-hit? [^long st face ^double cursor-y]
  (let [dir (dir/from-index (long face))]
    (and (contains? dir/horizontal-offset dir)
         (not (> (/ cursor-y 16.0) 0.8124))
         (bell-side? st dir))))

(defn- bell-use-deltas [world pos face cursor]
  (let [st (edit/block-at world pos)
        side (get dir/index (dir/from-index (long face)))]
    (when (bell-hit? st face (double (nth cursor 1)))
      [(out/all (out/block-event pos 1 side))
       (out/all (out/block-sound :bell/use pos 2.0 1.0))])))

(defn- egg-deltas [world pos]
  (let [st (edit/block-at world pos)
        roll (fn [k] (random/of-key (:tick world) pos :egg k))]
    (when-let [target (dragonegg/teleport-target
                        (:chunks world) pos roll)]
      (edit/change-deltas world [[pos 0] [target st]]))))

(def ^:private ^:const light-levels 16)

(defn- light-deltas [world pos]
  (let [st (edit/block-at world pos)
        lvl (rem (inc (block/prop-long st :level)) light-levels)
        props (assoc (block/props-of st) :level (keyword (str lvl)))]
    (set-at world pos (with-props st props))))

(defn- lectern-use-deltas [world eid pos]
  (let [st (edit/block-at world pos)
        stack (edit/held-stack world eid)]
    (cond
      (lectern/has-book? st) (containers/open-deltas world eid pos)
      (container/book? stack)
      (container/place-book-deltas world pos st stack))))

(defn- candle-use [w _ pos _ item _]
  (when (nil? item) (edit/candle-out-deltas w pos)))

(defn- berries-use [w _ pos _ item _]
  (when (nil? item) (berries-deltas w pos)))

(defn- bush-use [w _ pos _ item _]
  (when (picks-berries? (edit/block-at w pos) item)
    (bush-deltas w pos)))

(def ^:private by-type
  {:flower-pot (fn [w eid pos _ item _] (pot-deltas w eid pos item))
   :candle candle-use
   :candle-cake candle-cake-use
   :cake cake-use
   :cave-vines berries-use
   :cave-vines-plant berries-use
   :sweet-berry-bush bush-use
   :composter (fn [w _ pos _ item _] (compost-deltas w pos item))
   :decorated-pot (fn [w _ pos _ item _] (pot-use-deltas w pos item))
   :jukebox (fn [w _ pos _ item _] (jukebox-use-deltas w pos item))
   :campfire campfire-use-deltas
   :shelf (fn [w eid pos face _ cursor]
            (shelf-use-deltas w eid pos face cursor))
   :chiseled-book-shelf bookshelf-use-deltas
   :bell (fn [w _ pos face _ cursor]
           (bell-use-deltas w pos face cursor))
   :lectern (fn [w eid pos _ _ _] (lectern-use-deltas w eid pos))
   :dragon-egg (fn [w _ pos _ _ _] (egg-deltas w pos))
   :light (fn [w _ pos _ _ _] (light-deltas w pos))})

(defn- cauldron-use [w eid pos _ item _]
  (cauldron/cauldron-deltas w eid pos item (edit/held-stack w eid)))

(defn- handler [cur item]
  (let [t (block/type-of cur)]
    (or (by-type t)
        (cond
          (poses? cur item) (fn [w _ pos _ _ _] (pose-deltas w pos))
          (contains? block/cauldron-types t) cauldron-use
          (sign/kind cur)
          (fn [w eid pos _ item _] (sign-use-deltas w eid pos item))
          (contains? container/menu-types t)
          (fn [w eid pos _ _ _] (containers/open-deltas w eid pos))
          (and (= :pumpkin (block/block-of cur)) (= :shears item))
          (fn [w eid pos face _ _]
            (tools/carve-deltas w eid pos face))))))

(defn deltas
  "Returns the deltas of player eid using the block at pos on face,
  with item in hand and the cursor at the hit point, or nil when the
  block does nothing."
  [world eid pos face item cursor]
  (let [cur (edit/block-at world pos)]
    (when-let [h (handler cur item)]
      (h world eid pos face item cursor))))
