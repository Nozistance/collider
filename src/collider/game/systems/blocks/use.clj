(ns collider.game.systems.blocks.use
  (:require [collider.data :as data]
            [collider.game.block.blockentity :as be]
            [collider.game.block.container :as container]
            [collider.game.block.jukebox :as jukebox]
            [collider.game.block.sign :as sign]
            [collider.game.out :as out]
            [collider.game.systems.blocks.cauldron :as cauldron]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.tools :as tools]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- potted-block [item]
  (let [b (keyword (str "potted-" (name item)))]
    (when (contains? @data/blocks b) b)))

(defn- pot-deltas [world eid pos item]
  (let [cur (edit/block-at world pos) n (block/block-of cur)]
    (cond
      (and (= :flower-pot n) item (potted-block item))
      (edit/change-deltas world [[pos (block/state (potted-block item))]])
      (and (not= :flower-pot n) (nil? item))
      (let [plant (keyword (subs (name n) 7))
            [changes left] (items/add-stack (get-in world [:entities eid :inventory]) {:item plant :count 1})]
        (concat (edit/change-deltas world [[pos (block/state :flower-pot)]])
                (for [[slot s] changes] [:set-slot eid slot s])
                (when left [[:spawn-entity (items/dropped world eid left)]]))))))

(defn- candle-deltas [world pos]
  (let [cur (edit/block-at world pos)]
    (when (= :true (:lit (block/props-of cur)))
      (concat (edit/change-deltas world [[pos (block/state (block/block-of cur) (assoc (block/props-of cur) :lit :false))]])
              [(out/all (out/sound :candle/extinguish pos 1.0 1.0))]))))

(defn- candle-item? [item]
  (= :candle (:type (get @data/blocks item))))

(defn- candle-cake-deltas [world pos item]
  (let [cur (edit/block-at world pos) cake (keyword (str (name item) "-cake"))]
    (when (and (= :0 (:bites (block/props-of cur))) (contains? @data/blocks cake))
      (concat (edit/change-deltas world [[pos (block/state cake)]])
              [(out/all (out/sound :cake/add-candle pos 1.0 1.0))]))))

(defn- berries-deltas [world pos]
  (let [cur (edit/block-at world pos)]
    (when (= :true (:berries (block/props-of cur)))
      (concat (edit/change-deltas world [[pos (block/state (block/block-of cur) (assoc (block/props-of cur) :berries :false))]])
              [[:spawn-entity (items/popped world pos {:item :glow-berries :count 1} :berries)]
               (out/all (out/sound :cave-vines/pick-berries pos 1.0
                                   (random/pitch [(:tick world) pos :berries])))]))))

(defn- picks-berries? [^long cur item]
  (and (= :sweet-berry-bush (block/type-of cur)) (> (block/prop-long cur :age) 1)
       (not (and (= :bone-meal item) (< (block/prop-long cur :age) 3)))))

(defn- bush-stacks [world pos ^long a]
  (let [n (inc (long (Math/floor (* 2.0 (random/of-key [(:tick world) pos :bush-count])))))]
    (cond-> [] (= 3 a) (conj {:item :sweet-berries :count 1})
            true (conj {:item :sweet-berries :count n}))))

(defn- bush-deltas [world pos]
  (let [cur (edit/block-at world pos)]
    (concat (edit/change-deltas world [[pos (block/state :sweet-berry-bush {:age :1})]])
            (map-indexed (fn [i stack] [:spawn-entity (items/popped world pos stack [:bush i])])
                         (bush-stacks world pos (block/prop-long cur :age)))
            [(out/all (out/sound :sweet-berry-bush/pick-berries pos 1.0
                                 (random/pitch [(:tick world) pos :bush-pitch])))])))

(def ^:private statue-types #{:copper-golem-statue :weathering-copper-golem-statue})
(def ^:private next-pose {:standing :sitting :sitting :running :running :star :star :standing})

(defn- poses? [cur item]
  (and (contains? statue-types (block/type-of cur))
       (some? item)
       (not (@tools/axes item))))

(defn- pose-deltas [world pos]
  (let [cur (edit/block-at world pos) props (block/props-of cur)]
    (concat (edit/change-deltas world [[pos (block/state (block/block-of cur) (update props :copper-golem-pose next-pose))]])
            [(out/all (out/sound :copper-golem/statue pos 1.0 1.0))])))

(defn- compost-deltas [world pos item]
  (let [cur (edit/block-at world pos) lvl (block/prop-long cur :level)]
    (cond
      (and item (< lvl 8) (data/compost item))
      (when (< lvl 7)
        (let [took? (or (zero? lvl) (< (random/of-key [(:tick world) pos :compost]) (double (data/compost item))))
              st (if took? (block/state :composter {:level (keyword (str (inc lvl)))}) cur)]
          (concat (when took? (edit/change-deltas world [[pos st]]))
                  [(out/all (out/level-event 1500 pos (if took? 1 0)))
                   (out/all (out/sound (if took? :composter/fill-success :composter/fill) pos 1.0 1.0))])))
      (and (nil? item) (= lvl 8))
      (concat (edit/change-deltas world [[pos (block/state :composter {:level :0})]])
              [[:spawn-entity (items/popped world (mapv + pos [0 1 0]) {:item :bone-meal :count 1} :compost)]
               (out/all (out/sound :composter/empty pos 1.0 1.0))]))))

(defn- sign-use-deltas [world eid pos item]
  (let [st (edit/block-at world pos) e (sign/at world pos)
        front? (sign/front? st pos (get-in world [:entities eid :pos]))
        busy? (and (:editor e) (not= eid (:editor e)) (get-in world [:entities (:editor e)]))]
    (cond
      (nil? e) nil
      (and item (not (:waxed? e)) (not busy?))
      (when-let [[e' sound] (sign/applied e front? item)]
        (concat [[:set-block-entity pos e'] (out/all (out/block-entity pos))]
                (if (= :wax sound)
                  [(out/all (out/level-event 3003 pos))]
                  [(out/all (out/sound sound pos 1.0 1.0))])))
      (some? item) nil
      (:waxed? e) [(out/all (out/sound :sign/waxed pos 1.0 1.0))]
      (not busy?) [[:set-block-entity pos (assoc e :editor eid)]
                   (out/to eid (out/sign-editor pos front?))])))

(defn sign-update-deltas [world [eid pos front? lines]]
  (let [e (sign/at world pos)]
    (when (and e (not (:waxed? e)) (= eid (:editor e)))
      [[:set-block-entity pos (sign/written e front? lines)]
       (out/all (out/block-entity pos))])))

(defn- pot-insertable? [e item]
  (and item
       (let [cur (:item e)]
         (or (nil? cur)
             (and (= (:item cur) item) (< (long (:count cur)) (data/max-stack item)))))))

(defn- pot-insert-deltas [pos e item]
  (let [stack (if (:item e) (update (:item e) :count inc) {:item item :count 1})
        pitch (+ 0.7 (* 0.5 (/ (double (:count stack)) (data/max-stack item))))
        [x y z] pos
        plume [(+ (long x) 0.5) (+ (long y) 1.2) (+ (long z) 0.5)]]
    (concat (edit/be-changed pos (assoc e :item stack))
            [(out/all (out/block-event pos 1 0))
             (out/all (out/sound :decorated-pot/insert pos 1.0 pitch))
             (out/all (out/particles :dust-plume nil plume 7 0.0))])))

(defn- pot-use-deltas [world pos item]
  (when-let [e (be/at world pos)]
    (if (pot-insertable? e item)
      (pot-insert-deltas pos e item)
      [(out/all (out/sound :decorated-pot/insert-fail pos 1.0 1.0))
       (out/all (out/block-event pos 1 1))])))

(defn- jukebox-eject-deltas [world pos e]
  (let [cur (edit/block-at world pos)]
    (concat (edit/change-deltas world [[pos (block/state (block/block-of cur) {:has-record :false})]])
            (edit/be-changed pos (assoc e :record nil :song nil :started nil))
            [[:spawn-entity (items/popped world (mapv + pos [0 1 0]) (:record e) :jukebox)]
             (out/all (out/level-event 1011 pos 0))])))

(defn- jukebox-insert-deltas [world pos e item]
  (let [cur (edit/block-at world pos) song (jukebox/song-of item)]
    (concat (edit/change-deltas world [[pos (block/state (block/block-of cur) {:has-record :true})]])
            (edit/be-changed pos (assoc e :record {:item item :count 1} :song song :started (:tick world)))
            [(out/all (out/level-event 1010 pos (jukebox/song-id song)))])))

(defn- jukebox-use-deltas [world pos item]
  (let [e (be/at world pos)
        has? (= :true (:has-record (block/props-of (edit/block-at world pos))))]
    (cond
      (nil? e) nil
      has? (when (:record e) (jukebox-eject-deltas world pos e))
      (jukebox/song-of item) (jukebox-insert-deltas world pos e item))))

(defn- shelf-swap-deltas [world eid pos e slot]
  (let [removed (get-in e [:items slot])
        stack (edit/held-stack world eid)
        e' (assoc-in e [:items slot] stack)]
    (cond
      removed (concat (edit/be-changed pos e')
                      [[:set-slot eid (edit/held-slot world eid) removed]
                       (out/all (out/sound (if stack :shelf/single-swap :shelf/take-item) pos 1.0 1.0))])
      (nil? stack) nil
      :else (concat (edit/be-changed pos e')
                    [(out/all (out/sound :shelf/place-item pos 1.0 1.0))]))))

(defn- shelf-use-deltas [world eid pos face cursor]
  (let [st (edit/block-at world pos) e (be/at world pos)
        slot (edit/hit-slot st face cursor 1 3)]
    (when (and e slot (not (signal/has-neighbor-signal? (:chunks world) pos)))
      (shelf-swap-deltas world eid pos e slot))))

(def ^:private book-items (delay (set (get-in @data/tags ["item" "bookshelf_books"]))))

(defn- book-item? [item]
  (contains? @book-items item))

(defn- bookshelf-state [^long st items]
  (let [props (reduce (fn [m ^long i]
                        (assoc m (keyword (str "slot-" i "-occupied"))
                                 (if (nth items i) :true :false)))
                      (block/props-of st) (range 6))]
    (block/state (block/block-of st) props)))

(defn- bookshelf-add-deltas [world pos e slot item]
  (let [st (edit/block-at world pos)
        items (assoc (:items e) slot {:item item :count 1})
        sound (if (= :enchanted-book item) :bookshelf/insert-enchanted :bookshelf/insert)]
    (concat (edit/change-deltas world [[pos (bookshelf-state st items)]])
            [[:set-block-entity pos (assoc e :items items :last-slot slot)]
             (out/all (out/sound sound pos 1.0 1.0))])))

(defn- bookshelf-take-deltas [world eid pos e slot]
  (let [st (edit/block-at world pos)
        stack (nth (:items e) slot)
        items (assoc (:items e) slot nil)
        sound (if (= :enchanted-book (:item stack)) :bookshelf/pickup-enchanted :bookshelf/pickup)
        [changes left] (items/add-stack (get-in world [:entities eid :inventory]) stack)]
    (concat (edit/change-deltas world [[pos (bookshelf-state st items)]])
            [[:set-block-entity pos (assoc e :items items :last-slot slot)]
             (out/all (out/sound sound pos 1.0 1.0))]
            (for [[s v] changes] [:set-slot eid s v])
            (when left [[:spawn-entity (items/dropped world eid left)]]))))

(defn- bookshelf-use-deltas [world eid pos face item cursor]
  (let [st (edit/block-at world pos) e (be/at world pos)
        slot (edit/hit-slot st face cursor 2 3)]
    (when (and e slot)
      (let [taken (nth (:items e) slot)]
        (cond
          (and (book-item? item) (nil? taken)) (bookshelf-add-deltas world pos e slot item)
          taken (bookshelf-take-deltas world eid pos e slot))))))

(defn- bell-hit? [^long st face ^double cursor-y]
  (let [dir (dir/from-index (long face))]
    (and (contains? dir/horizontal-offset dir) (not (> (/ cursor-y 16.0) 0.8124))
         (let [same? (= (dir/axis (block/facing-of st)) (dir/axis dir))]
           (case (:attachment (block/props-of st))
             :floor same?
             (:single_wall :double_wall) (not same?)
             :ceiling true
             false)))))

(defn- bell-use-deltas [world pos face cursor]
  (let [st (edit/block-at world pos)]
    (when (bell-hit? st face (double (nth cursor 1)))
      [(out/all (out/block-event pos 1 (get dir/index (dir/from-index (long face)))))
       (out/all (out/sound :bell/use pos 2.0 1.0))])))

(defn- lectern-use-deltas [world eid pos]
  (let [st (edit/block-at world pos)
        stack (edit/held-stack world eid)]
    (cond
      (lectern/has-book? st) (containers/open-deltas world eid pos)
      (container/book? stack) (container/place-book-deltas world pos st stack))))

(def ^:private by-type
  {:flower-pot          (fn [w eid pos _ item _] (pot-deltas w eid pos item))
   :candle              (fn [w _ pos _ item _] (when (nil? item) (candle-deltas w pos)))
   :candle-cake         (fn [w _ pos _ item _] (when (nil? item) (candle-deltas w pos)))
   :cake                (fn [w _ pos _ item _] (when (and item (candle-item? item)) (candle-cake-deltas w pos item)))
   :cave-vines          (fn [w _ pos _ item _] (when (nil? item) (berries-deltas w pos)))
   :cave-vines-plant    (fn [w _ pos _ item _] (when (nil? item) (berries-deltas w pos)))
   :sweet-berry-bush    (fn [w _ pos _ item _] (when (picks-berries? (edit/block-at w pos) item) (bush-deltas w pos)))
   :composter           (fn [w _ pos _ item _] (compost-deltas w pos item))
   :decorated-pot       (fn [w _ pos _ item _] (pot-use-deltas w pos item))
   :jukebox             (fn [w _ pos _ item _] (jukebox-use-deltas w pos item))
   :shelf               (fn [w eid pos face _ cursor] (shelf-use-deltas w eid pos face cursor))
   :chiseled-book-shelf (fn [w eid pos face item cursor] (bookshelf-use-deltas w eid pos face item cursor))
   :bell                (fn [w _ pos face _ cursor] (bell-use-deltas w pos face cursor))
   :lectern             (fn [w eid pos _ _ _] (lectern-use-deltas w eid pos))})

(defn- handler [cur item]
  (let [t (block/type-of cur)]
    (or (by-type t)
        (cond
          (poses? cur item) (fn [w _ pos _ _ _] (pose-deltas w pos))
          (contains? block/cauldron-types t)
          (fn [w eid pos _ item _] (cauldron/cauldron-deltas w pos item (edit/held-stack w eid)))
          (sign/kind cur) (fn [w eid pos _ item _] (sign-use-deltas w eid pos item))
          (contains? container/container-types t)
          (fn [w eid pos _ _ _] (containers/open-deltas w eid pos))
          (and (= :pumpkin (block/block-of cur)) (= :shears item))
          (fn [w eid pos face _ _] (tools/carve-deltas w eid pos face))))))

(defn deltas [world eid pos face item cursor]
  (let [cur (edit/block-at world pos)]
    (when-let [h (handler cur item)]
      (h world eid pos face item cursor))))
