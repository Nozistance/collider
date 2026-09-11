(ns collider.game.systems.blocks
  (:require [clojure.string :as str]
            [collider.random :as random]
            [collider.data :as data]
            [collider.game.blockentity :as be]
            [collider.game.container :as container]
            [collider.game.systems.containers :as containers]
            [collider.game.jukebox :as jukebox]
            [collider.game.mobs :as mobs]
            [collider.game.sign :as sign]
            [collider.game.out :as out]
            [collider.game.sense :as sense]
            [collider.game.tnt :as tnt]
            [collider.game.systems.daynight :as daynight]
            [collider.game.systems.items :as items]
            [collider.game.systems.sleep :as sleep]
            [collider.vec :as v]
            [collider.world.bed :as bed]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.connect :as connect]
            [collider.world.gen :as gen]
            [collider.world.grow :as grow]
            [collider.world.lectern :as lectern]
            [collider.world.fire :as fire]
            [collider.world.liquid :as liquid]
            [collider.world.moss :as moss]
            [collider.world.signal :as signal]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(defn- place-sound [item]
  (let [n (name item)]
    (cond
      (= "chiseled-bookshelf" n) :place/bookshelf
      (str/ends-with? n "-shelf") :place/shelf
      (= "bell" n) :place/anvil
      (some #(str/ends-with? n %) ["-planks" "-log" "-wood" "-fence" "-fence-gate" "-door" "-trapdoor" "-sign" "-banner" "bookshelf" "crafting-table" "chest" "ladder" "jukebox"]) :place/wood
      (#{"grass-block" "short-grass" "tnt" "sponge" "vine" "moss-block"} n) :place/grass
      (some #(str/ends-with? n %) ["-leaves"]) :place/grass
      (#{"dirt" "gravel" "farmland" "clay" "coarse-dirt" "rooted-dirt" "mud"} n) :place/gravel
      (some #(str/ends-with? n %) ["sand" "soul-sand"]) :place/sand
      (some #(str/ends-with? n %) ["-wool" "-carpet" "-bed"]) :place/cloth
      (some #(str/includes? n %) ["glass" "ice" "glowstone" "sea-lantern"]) :place/glass
      (= "powder-snow-bucket" n) :bucket/empty-snow
      (#{"snow" "snow-block" "powder-snow"} n) :place/snow
      :else :place/stone)))

(defn- block-at ^long [world pos]
  (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos))

(defn- builder-box [e]
  (case (:type e)
    :player [0.3 (if (and (:sneaking? e) (not (:flying e))) 1.5 1.8)]
    (:tnt :falling-block) [0.49 0.98]
    :item nil
    (when-let [m (get mobs/types (:type e))]
      (let [s (if (mobs/baby? e) 0.5 1.0)]
        [(* s (double (:half m))) (* s (double (:height m)))]))))

(defn- box-hits? [[x1 y1 z1 x2 y2 z2] [px py pz] [half h]]
  (let [px (double px) py (double py) pz (double pz) half (double half) h (double h)]
    (and (> (+ px half) (double x1)) (< (- px half) (double x2))
         (> (+ py h) (double y1)) (< py (double y2))
         (> (+ pz half) (double z1)) (< (- pz half) (double z2)))))

(defn- obstructed? [world [x y z] state]
  (let [boxes (map (fn [[a b c d e f]]
                     [(+ (long x) (/ (double a) 16.0)) (+ (long y) (/ (double b) 16.0)) (+ (long z) (/ (double c) 16.0))
                      (+ (long x) (/ (double d) 16.0)) (+ (long y) (/ (double e) 16.0)) (+ (long z) (/ (double f) 16.0))])
                   (block/collision-boxes state))]
    (some (fn [[_ e]]
            (when-let [dims (builder-box e)]
              (some #(box-hits? % (:pos e) dims) boxes)))
          (:entities world))))

(defn- own-change [world eid pos]
  (out/to eid (out/blocks-changed (chunk/block-chunk pos) [[pos (block-at world pos)]])))

(defn- reject-deltas [world eid pos pos']
  (cond-> [(own-change world eid pos)]
    pos' (conj (own-change world eid pos'))))

(defn- change-deltas [world changes]
  (let [chunks' (chunk/chunks-set-blocks (:chunks world) gen/flat-chunk changes)
        all     (into (vec changes) (connect/derived-changes chunks' (map first changes) (:tick world)))
        chunks'' (chunk/chunks-set-blocks chunks' gen/flat-chunk all)
        mixed   (liquid/mix-changes chunks'' gen/flat-chunk (map first all))]
    (into [[:set-blocks (into all mixed) (dec (long (:tick world)))]]
          (map (fn [[p _]] (out/all (out/fizz p))))
          mixed)))

(defn- placed-deltas
  ([world eid pos state item] (placed-deltas world eid [[pos state]] item))
  ([world eid changes item]
   (conj (change-deltas world changes)
         (out/except eid (out/sound (place-sound item) (ffirst changes) 1.0 0.8)))))

(def ^:private openable-types (into #{:fence-gate} (concat block/door-types block/trapdoor-types)))
(defn- potted-block [item]
  (let [b (keyword (str "potted-" (name item)))]
    (when (contains? @data/blocks b) b)))

(defn- pot-deltas [world eid pos item]
  (let [cur (block-at world pos) n (block/block-of cur)]
    (cond
      (and (= :flower-pot n) item (potted-block item))
      (change-deltas world [[pos (block/state (potted-block item))]])
      (and (not= :flower-pot n) (nil? item))
      (let [plant (keyword (subs (name n) 7))
            [changes left] (items/add-stack (get-in world [:entities eid :inventory]) {:item plant :count 1})]
        (concat (change-deltas world [[pos (block/state :flower-pot)]])
                (for [[slot s] changes] [:set-slot eid slot s])
                (when left [[:spawn-entity (items/dropped world eid left)]]))))))

(defn- candle-deltas [world pos]
  (let [cur (block-at world pos)]
    (when (= :true (:lit (block/props-of cur)))
      (concat (change-deltas world [[pos (block/state (block/block-of cur) (assoc (block/props-of cur) :lit :false))]])
              [(out/all (out/sound :candle/extinguish pos 1.0 1.0))]))))

(defn- candle-item? [item]
  (= :candle (:type (get @data/blocks item))))

(defn- candle-cake-deltas [world pos item]
  (let [cur (block-at world pos) cake (keyword (str (name item) "-cake"))]
    (when (and (= :0 (:bites (block/props-of cur))) (contains? @data/blocks cake))
      (concat (change-deltas world [[pos (block/state cake)]])
              [(out/all (out/sound :cake/add-candle pos 1.0 1.0))]))))

(defn- berries-deltas [world pos]
  (let [cur (block-at world pos)]
    (when (= :true (:berries (block/props-of cur)))
      (let [pitch (+ 0.8 (* 0.4 (random/of-key [(:tick world) pos :berries])))]
        (concat (change-deltas world [[pos (block/state (block/block-of cur) (assoc (block/props-of cur) :berries :false))]])
                [[:spawn-entity (items/popped world pos {:item :glow-berries :count 1} :berries)]
                 (out/all (out/sound :cave-vines/pick-berries pos 1.0 pitch))])))))

(defn- bush-age ^long [^long st] (Long/parseLong (name (:age (block/props-of st)))))
(defn- picks-berries? [^long cur item]
  (and (= :sweet-berry-bush (block/type-of cur)) (> (bush-age cur) 1)
       (not (and (= :bone-meal item) (< (bush-age cur) 3)))))

(defn- bush-stacks [world pos ^long a]
  (let [n (inc (long (Math/floor (* 2.0 (random/of-key [(:tick world) pos :bush-count])))))]
    (cond-> [] (= 3 a) (conj {:item :sweet-berries :count 1})
            true (conj {:item :sweet-berries :count n}))))

(defn- bush-deltas [world pos]
  (let [cur (block-at world pos)
        pitch (+ 0.8 (* 0.4 (random/of-key [(:tick world) pos :bush-pitch])))]
    (concat (change-deltas world [[pos (block/state :sweet-berry-bush {:age :1})]])
            (map-indexed (fn [i stack] [:spawn-entity (items/popped world pos stack [:bush i])])
                         (bush-stacks world pos (bush-age cur)))
            [(out/all (out/sound :sweet-berry-bush/pick-berries pos 1.0 pitch))])))

(def ^:private statue-types #{:copper-golem-statue :weathering-copper-golem-statue})
(def ^:private next-pose {:standing :sitting :sitting :running :running :star :star :standing})
(defn- poses? [cur item]
  (and (contains? statue-types (block/type-of cur))
       (some? item)
       (not (str/ends-with? (name item) "-axe"))))

(defn- pose-deltas [world pos]
  (let [cur (block-at world pos) props (block/props-of cur)]
    (concat (change-deltas world [[pos (block/state (block/block-of cur) (update props :copper-golem-pose next-pose))]])
            [(out/all (out/sound :copper-golem/statue pos 1.0 1.0))])))

(def ^:private cauldron-types #{:cauldron :layered-cauldron :lava-cauldron})
(defn- cauldron-level ^long [st] (Long/parseLong (name (:level (block/props-of st) :0))))
(defn- cauldron-filled [world pos item]
  (let [under-water? (= :water (liquid/liquid-class (block-at world (mapv + pos [0 1 0]))))]
    (case item
      :water-bucket [(block/state :water-cauldron {:level :3}) :bucket/empty]
      :lava-bucket (when-not under-water? [(block/state :lava-cauldron) :bucket/empty-lava])
      :powder-snow-bucket (when-not under-water? [(block/state :powder-snow-cauldron {:level :3}) :bucket/empty-snow])
      nil)))

(defn- cauldron-scooped [cur]
  (let [n (block/block-of cur)]
    (cond
      (= :lava-cauldron n) :bucket/fill-lava
      (and (= :water-cauldron n) (= 3 (cauldron-level cur))) :bucket/fill
      (and (= :powder-snow-cauldron n) (= 3 (cauldron-level cur))) :bucket/fill-snow)))

(defn- cauldron-lowered ^long [cur]
  (let [lvl (cauldron-level cur)]
    (if (= 1 lvl) (block/state :cauldron) (block/state (block/block-of cur) {:level (keyword (str (dec lvl)))}))))

(defn- bottle-deltas [world pos]
  (let [cur (block-at world pos)]
    (when (= :water-cauldron (block/block-of cur))
      (concat (change-deltas world [[pos (cauldron-lowered cur)]])
              [(out/all (out/sound :bottle/fill pos 1.0 1.0))]))))

(defn water-bottle? [stack]
  (and (= :potion (:item stack))
       (= :water (get-in stack [:components :potion-contents :potion]))))

(defn- cauldron-raised [cur]
  (if (= :cauldron (block/block-of cur))
    (block/state :water-cauldron)
    (let [lvl (cauldron-level cur)]
      (when (< lvl 3)
        (block/state (block/block-of cur) {:level (keyword (str (inc lvl)))})))))

(defn- pour-bottle-deltas [world pos]
  (let [cur (block-at world pos)]
    (when (contains? #{:cauldron :water-cauldron} (block/block-of cur))
      (when-let [st (cauldron-raised cur)]
        (concat (change-deltas world [[pos st]])
                [(out/all (out/sound :bottle/empty pos 1.0 1.0))])))))

(defn- cauldron-deltas [world pos item stack]
  (let [cur (block-at world pos)]
    (cond
      (and (= :potion item) (water-bottle? stack)) (pour-bottle-deltas world pos)
      (= :bucket item)
      (when-let [sound (cauldron-scooped cur)]
        (concat (change-deltas world [[pos (block/state :cauldron)]]) [(out/all (out/sound sound pos 1.0 1.0))]))
      (= :glass-bottle item) (bottle-deltas world pos)
      :else
      (when-let [[st sound] (cauldron-filled world pos item)]
        (concat (change-deltas world [[pos st]]) [(out/all (out/sound sound pos 1.0 1.0))])))))

(defn- carve-deltas [world eid pos face]
  (let [dir (if (<= (long face) 1)
              (block/opposite-facing (block/player-direction (get-in world [:entities eid :yaw] 0.0)))
              (get {2 :north 3 :south 4 :west 5 :east} face))
        [ox _ oz] (block/facing-offset dir)
        [x y z] pos
        t (:tick world)]
    (concat
     (change-deltas world [[pos (block/state :carved-pumpkin {:facing dir})]])
     [[:spawn-entity {:type :item :pos [(+ (long x) 0.5 (* 0.65 (long ox))) (+ (long y) 0.1) (+ (long z) 0.5 (* 0.65 (long oz)))]
                      :vel [(+ (* 0.05 (long ox)) (* 0.02 (random/of-key [t pos :sx]))) 0.05 (+ (* 0.05 (long oz)) (* 0.02 (random/of-key [t pos :sz])))]
                      :yaw 0.0 :pitch 0.0 :on-ground false
                      :stack {:item :pumpkin-seeds :count 4} :age 0 :pickup-delay 10}]
      (out/all (out/sound :pumpkin/carve pos 1.0 1.0))])))

(defn- compost-deltas [world pos item]
  (let [cur (block-at world pos) lvl (cauldron-level cur)]
    (cond
      (and item (< lvl 8) (grow/compostables item))
      (when (< lvl 7)
        (let [took? (or (zero? lvl) (< (random/of-key [(:tick world) pos :compost]) (double (grow/compostables item))))
              st (if took? (block/state :composter {:level (keyword (str (inc lvl)))}) cur)]
          (concat (when took? (change-deltas world [[pos st]]))
                  [(out/all (out/level-event 1500 pos (if took? 1 0)))
                   (out/all (out/sound (if took? :composter/fill-success :composter/fill) pos 1.0 1.0))])))
      (and (nil? item) (= lvl 8))
      (concat (change-deltas world [[pos (block/state :composter {:level :0})]])
              [[:spawn-entity (items/popped world (mapv + pos [0 1 0]) {:item :bone-meal :count 1} :compost)]
               (out/all (out/sound :composter/empty pos 1.0 1.0))]))))

(defn- sign-use-deltas [world eid pos item]
  (let [st (block-at world pos) e (sign/at world pos)
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

(defn- sign-update-deltas [world [eid pos front? lines]]
  (let [e (sign/at world pos)]
    (when (and e (not (:waxed? e)) (= eid (:editor e)))
      [[:set-block-entity pos (sign/written e front? lines)]
       (out/all (out/block-entity pos))])))

(defn- be-changed [pos e]
  [[:set-block-entity pos e] (out/all (out/block-entity pos))])

(defn- held-slot ^long [world eid]
  (+ 36 (long (or (get-in world [:entities eid :held-slot]) 0))))

(defn- held-stack [world eid]
  (get-in world [:entities eid :inventory (held-slot world eid)]))

(def ^:private face-dir {0 :down 1 :up 2 :north 3 :south 4 :west 5 :east})

(defn- hit-uv [face [cx cy cz]]
  (let [x (/ (double cx) 16.0) y (/ (double cy) 16.0) z (/ (double cz) 16.0)]
    (case (long face)
      2 [(- 1.0 x) y]
      3 [x y]
      4 [z y]
      5 [(- 1.0 z) y]
      nil)))

(defn- section ^long [^double rel ^long n]
  (min (dec n) (max 0 (long (Math/floor (* rel n))))))

(defn- hit-slot [st face cursor rows cols]
  (when (= (block/facing-of st) (get face-dir (long face)))
    (when-let [[u v] (hit-uv face cursor)]
      (+ (section (double u) (long cols)) (* (long cols) (section (- 1.0 (double v)) (long rows)))))))

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
    (concat (be-changed pos (assoc e :item stack))
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
  (let [cur (block-at world pos)]
    (concat (change-deltas world [[pos (block/state (block/block-of cur) {:has-record :false})]])
            (be-changed pos (assoc e :record nil :song nil :started nil))
            [[:spawn-entity (items/popped world (mapv + pos [0 1 0]) (:record e) :jukebox)]
             (out/all (out/level-event 1011 pos 0))])))

(defn- jukebox-insert-deltas [world pos e item]
  (let [cur (block-at world pos) song (jukebox/song-of item)]
    (concat (change-deltas world [[pos (block/state (block/block-of cur) {:has-record :true})]])
            (be-changed pos (assoc e :record {:item item :count 1} :song song :started (:tick world)))
            [(out/all (out/level-event 1010 pos (jukebox/song-id song)))])))

(defn- jukebox-use-deltas [world pos item]
  (let [e (be/at world pos)
        has? (= :true (:has-record (block/props-of (block-at world pos))))]
    (cond
      (nil? e) nil
      has? (when (:record e) (jukebox-eject-deltas world pos e))
      (jukebox/song-of item) (jukebox-insert-deltas world pos e item))))

(defn- shelf-swap-deltas [world eid pos e slot]
  (let [removed (get-in e [:items slot])
        stack (held-stack world eid)
        e' (assoc-in e [:items slot] stack)]
    (cond
      removed (concat (be-changed pos e')
                      [[:set-slot eid (held-slot world eid) removed]
                       (out/all (out/sound (if stack :shelf/single-swap :shelf/take-item) pos 1.0 1.0))])
      (nil? stack) nil
      :else (concat (be-changed pos e')
                    [(out/all (out/sound :shelf/place-item pos 1.0 1.0))]))))

(defn- shelf-use-deltas [world eid pos face cursor]
  (let [st (block-at world pos) e (be/at world pos)
        slot (hit-slot st face cursor 1 3)]
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
  (let [st (block-at world pos)
        items (assoc (:items e) slot {:item item :count 1})
        sound (if (= :enchanted-book item) :bookshelf/insert-enchanted :bookshelf/insert)]
    (concat (change-deltas world [[pos (bookshelf-state st items)]])
            [[:set-block-entity pos (assoc e :items items :last-slot slot)]
             (out/all (out/sound sound pos 1.0 1.0))])))

(defn- bookshelf-take-deltas [world eid pos e slot]
  (let [st (block-at world pos)
        stack (nth (:items e) slot)
        items (assoc (:items e) slot nil)
        sound (if (= :enchanted-book (:item stack)) :bookshelf/pickup-enchanted :bookshelf/pickup)
        [changes left] (items/add-stack (get-in world [:entities eid :inventory]) stack)]
    (concat (change-deltas world [[pos (bookshelf-state st items)]])
            [[:set-block-entity pos (assoc e :items items :last-slot slot)]
             (out/all (out/sound sound pos 1.0 1.0))]
            (for [[s v] changes] [:set-slot eid s v])
            (when left [[:spawn-entity (items/dropped world eid left)]]))))

(defn- bookshelf-use-deltas [world eid pos face item cursor]
  (let [st (block-at world pos) e (be/at world pos)
        slot (hit-slot st face cursor 2 3)]
    (when (and e slot)
      (let [taken (nth (:items e) slot)]
        (cond
          (and (book-item? item) (nil? taken)) (bookshelf-add-deltas world pos e slot item)
          taken (bookshelf-take-deltas world eid pos e slot))))))

(def ^:private axis-of {:north :z :south :z :west :x :east :x})

(defn- bell-hit? [^long st face ^double cursor-y]
  (let [dir (get face-dir (long face))]
    (and dir (contains? axis-of dir) (not (> (/ cursor-y 16.0) 0.8124))
         (let [same? (= (get axis-of (block/facing-of st)) (get axis-of dir))]
           (case (:attachment (block/props-of st))
             :floor same?
             (:single_wall :double_wall) (not same?)
             :ceiling true
             false)))))

(def ^:private dir-index {:down 0 :up 1 :north 2 :south 3 :west 4 :east 5})

(defn- bell-use-deltas [world pos face cursor]
  (let [st (block-at world pos)]
    (when (bell-hit? st face (double (nth cursor 1)))
      [(out/all (out/block-event pos 1 (get dir-index (get face-dir (long face)))))
       (out/all (out/sound :bell/use pos 2.0 1.0))])))

(defn- lectern-use-deltas
  "LecternBlock.useItemOn with a book, then useWithoutItem on the book already
   there. The book itself is never consumed: a creative player has
   hasInfiniteMaterials."
  [world eid pos]
  (let [st (block-at world pos)
        stack (held-stack world eid)]
    (cond
      (lectern/has-book? st) (containers/open-deltas world eid pos)
      (container/book? stack) (container/place-book-deltas world pos st stack))))

(defn- uses-block? [world eid pos item use-item?]
  (and (not use-item?)
       (not (and item (get-in world [:entities eid :sneaking?])))
       (let [cur (block-at world pos)]
         (or (contains? #{:flower-pot :candle :candle-cake :cake :composter :cave-vines :cave-vines-plant
                          :decorated-pot :jukebox :shelf :chiseled-book-shelf :bell :lectern}
                        (block/type-of cur))
             (contains? container/container-types (block/type-of cur))
             (picks-berries? cur item)
             (poses? cur item)
             (contains? cauldron-types (block/type-of cur))
             (some? (sign/kind cur))
             (and (= :pumpkin (block/block-of cur)) (= :shears item))))))

(defn- use-deltas [world eid pos face item cursor]
  (let [cur (block-at world pos) t (block/type-of cur)]
    (cond
      (= :flower-pot t) (pot-deltas world eid pos item)
      (= :candle t) (when (nil? item) (candle-deltas world pos))
      (= :candle-cake t) (when (nil? item) (candle-deltas world pos))
      (= :cake t) (when (and item (candle-item? item)) (candle-cake-deltas world pos item))
      (contains? statue-types t) (pose-deltas world pos)
      (contains? #{:cave-vines :cave-vines-plant} t) (when (nil? item) (berries-deltas world pos))
      (= :sweet-berry-bush t) (bush-deltas world pos)
      (= :composter t) (compost-deltas world pos item)
      (contains? cauldron-types t) (cauldron-deltas world pos item (held-stack world eid))
      (sign/kind cur) (sign-use-deltas world eid pos item)
      (= :decorated-pot t) (pot-use-deltas world pos item)
      (= :jukebox t) (jukebox-use-deltas world pos item)
      (= :shelf t) (shelf-use-deltas world eid pos face cursor)
      (= :chiseled-book-shelf t) (bookshelf-use-deltas world eid pos face item cursor)
      (= :bell t) (bell-use-deltas world pos face cursor)
      (= :lectern t) (lectern-use-deltas world eid pos)
      (contains? container/container-types t) (containers/open-deltas world eid pos)
      (= :pumpkin (block/block-of cur)) (carve-deltas world eid pos face))))

(defn- scaffold-target [world eid pos face]
  (let [sneaking? (get-in world [:entities eid :sneaking?])
        dir (cond
              sneaking? (get {0 :down 1 :up 2 :north 3 :south 4 :west 5 :east} face)
              (= 1 (long face)) (block/player-direction (get-in world [:entities eid :yaw] 0.0))
              :else :up)
        off (get {:down [0 -1 0] :up [0 1 0]} dir (block/facing-offset dir))
        horizontal? (contains? #{:north :south :west :east} dir)]
    (loop [p (mapv + pos off) n 0]
      (let [[_ y _] p]
        (when (and (chunk/in-range? y) (< n 7))
          (let [st (block-at world p)]
            (cond
              (= :scaffolding (block/type-of st)) (recur (mapv + p off) (if horizontal? (inc n) n))
              (block/can-be-replaced? st) p
              :else nil)))))))

(defn- by-hand? [state]
  (not (str/starts-with? (name (block/block-of state)) "iron-")))

(defn- open-sound [state open?]
  (let [n (name (block/block-of state))
        wood (cond
               (str/includes? n "copper") "copper"
               (or (str/starts-with? n "crimson") (str/starts-with? n "warped")) "nether-wood"
               (str/starts-with? n "bamboo") "bamboo"
               (str/starts-with? n "cherry") "cherry"
               :else "wooden")
        kind (case (block/type-of state)
               (:door :weathering-copper-door) (if (#{"bamboo" "cherry"} wood) "wooden-door" (str wood "-door"))
               (:trapdoor :weathering-copper-trapdoor) (if (#{"bamboo" "cherry"} wood) "wooden-trapdoor" (str wood "-trapdoor"))
               :fence-gate (if (#{"wooden" "copper"} wood) "fence-gate" (str wood "-wood-fence-gate")))]
    (keyword (str "block." kind "." (if open? "open" "close")))))

(defn- toggled [world eid pos state]
  (let [props (block/props-of state)
        self (block/block-of state)
        open? (= :true (:open props))]
    (case (block/type-of state)
      (:door :weathering-copper-door) (let [st' (block/state self (assoc props :open (if open? :false :true)))
                  other (mapv + pos (if (= :lower (:half props)) [0 1 0] [0 -1 0]))
                  ost (block-at world other)]
              (cond-> [[pos st']]
                (= self (block/block-of ost))
                (conj [other (block/state self (assoc (block/props-of ost) :open (if open? :false :true)))])))
      (:trapdoor :weathering-copper-trapdoor) [[pos (block/state self (assoc props :open (if open? :false :true)))]]
      :fence-gate (let [dir (block/player-direction (get-in world [:entities eid :yaw] 0.0))
                        facing (if (and (not open?) (= (:facing props) (block/opposite-facing dir))) dir (:facing props))]
                    [[pos (block/state self (assoc props :open (if open? :false :true) :facing facing))]]))))

(defn- toggle-deltas [world eid pos state]
  (let [changes (toggled world eid pos state)
        open? (= :true (:open (block/props-of (second (first changes)))))
        t (:tick world)
        pitch (+ 0.9 (* 0.1 (random/of-key [t pos :door])))]
    (conj (change-deltas world changes)
          (out/except eid (out/sound (open-sound state open?) pos 1.0 pitch)))))

(defn- bed-head-effect [world eid pos old]
  (when (and (= :bed (block/type-of old)) (= :foot (:part (block/props-of old))))
    (let [head-pos (mapv + pos (connect/bed-partner-offset old))
          head     (block-at world head-pos)]
      (when (and (= (block/block-of old) (block/block-of head))
                 (= :head (:part (block/props-of head))))
        (out/except eid (out/break-effect head-pos head))))))

(defn- jukebox-break-deltas [world pos]
  (let [e (be/at world pos)]
    (when (and (= :jukebox (:kind e)) (:record e))
      [[:spawn-entity (items/popped world (mapv + pos [0 1 0]) (:record e) :jukebox)]
       (out/all (out/level-event 1011 pos 0))])))

(defn- shulker-break-deltas
  "ShulkerBoxBlock.playerWillDestroy: a creative player still gets the box
   itself when it is not empty, with its contents in the item."
  [world pos]
  (let [e (be/at world pos)
        [x y z] pos]
    (when (= :shulker-box (:kind e))
      (concat
       (when (container/animation world pos) [[:shulker-anim pos nil]])
       (when (some some? (:items e))
         (let [item (block/block-of (block-at world pos))
               r (fn [k] (random/of-key [(:tick world) pos :shulker k]))]
           [[:spawn-entity {:type :item
                            :pos [(+ (double x) 0.5) (+ (double y) 0.5) (+ (double z) 0.5)]
                            :vel [(- (* 0.2 (r :vx)) 0.1) 0.2 (- (* 0.2 (r :vz)) 0.1)]
                            :yaw 0.0 :pitch 0.0 :on-ground false
                            :stack (be/to-stack item e) :age 0 :pickup-delay 10}]]))))))

(defn- lectern-break-deltas
  "LecternBlockEntity.preRemoveSideEffects: the book falls out."
  [world pos]
  (for [e (container/dropped-book world pos)] [:spawn-entity e]))

(defn- spill-deltas [world pos]
  (let [e (be/at world pos)]
    (when (contains? be/spill-kinds (:kind e))
      (mapcat (fn [[i stack]]
                (when stack
                  (map (fn [part] [:spawn-entity (items/popped world pos part [:spill i])])
                       (items/split-drop world pos stack [:spill i]))))
              (map-indexed vector (:items e))))))

(defn- dig-deltas [world [eid status pos _face]]
  (let [old (block-at world pos)]
    (when (or (= 0 status) (= 2 status))
      (if (pos? old)
        (cond-> (into (vec (concat (jukebox-break-deltas world pos)
                                   (shulker-break-deltas world pos)
                                   (lectern-break-deltas world pos)
                                   (spill-deltas world pos)))
                      (conj (change-deltas world [[pos (block/emptied old)]])
                            (out/except eid (out/break-effect pos old))))
          (fire/fire-state? old) (conj (out/all (out/extinguish pos)))
          (bed-head-effect world eid pos old) (conj (bed-head-effect world eid pos old)))
        [(own-change world eid pos)]))))

(defn- snow-layers ^long [st]
  (Long/parseLong (name (:layers (block/props-of st)))))

(defn- replaceable?
  ([world pos] (replaceable? world pos nil))
  ([world pos item]
   (let [cur (block-at world pos)]
     (cond
       (zero? cur) true
       (liquid/liquid-state? cur) true
       (fire/fire-state? cur) true
       (= :snow-layer (block/type-of cur)) (let [n (snow-layers cur)]
                                            (if (= item :snow) (< n 8) (= n 1)))
       (block/stackable? cur item) true
       :else (block/can-be-replaced? cur)))))

(defn- stacked [world pos' state item]
  (let [cur (block-at world pos')]
    (cond
      (and (= item :snow) (= :snow-layer (block/type-of cur)))
      (block/state :snow {:layers (keyword (str (min 8 (inc (snow-layers cur)))))})
      (and (block/stackable? cur item) (block/stack-props (block/type-of cur))) (block/stacked cur)
      :else state)))

(defn- candle-lit [world pos]
  (let [cur (block-at world pos) props (block/props-of cur)]
    (when (and (contains? #{:candle :candle-cake} (block/type-of cur))
               (= :false (:lit props)) (not= :true (:waterlogged props)))
      (block/state (block/block-of cur) (assoc props :lit :true)))))

(defn- fire-deltas [world eid pos off]
  (let [[_ y' _ :as pos'] (mapv + pos off)
        st (fire/state-for (:chunks world) pos')]
    (when (and (chunk/in-range? y')
               (zero? (block-at world pos'))
               (support/supported? (:chunks world) gen/flat-chunk pos' st))
      [[:set-blocks [[pos' st]] (dec (long (:tick world)))]
       (out/except eid (out/sound :fire/ignite pos' 1.0 (+ 0.8 (* 0.4 (random/of-key [(:tick world) pos' :flint])))))])))

(defn- flint-deltas [world [eid pos face]]
  (when-let [off (block/face-offsets face)]
    (cond
      (candle-lit world pos)
      (change-deltas world [[pos (candle-lit world pos)]])
      (and (tnt/tnt-state? (block-at world pos))
           (get-in world [:rules :tnt-explodes] true)
           (not (get-in world [:entities eid :sneaking?]))
           (not ((tnt/primed-origins world) pos)))
      [[:spawn-entity (tnt/primed pos [(:tick world) pos])]
       (out/all (out/sound :tnt/primed pos 1.0 1.0))]
      :else (fire-deltas world eid pos off))))

(defn- slab-merge [world pos pos' face item]
  (let [clicked (block-at world pos)
        part    (block/slab-part clicked)
        lower?  (and (= 1 (long face)) (= :bottom part))
        upper?  (and (= 0 (long face)) (= :top part))]
    (cond
      (and (block/same-slab? clicked item) (or lower? upper?))
      [pos (block/double-slab item)]
      (and pos' (block/same-slab? (block-at world pos') item)
           (not= :double (block/slab-part (block-at world pos'))))
      [pos' (block/double-slab item)])))

(defn- waterlogged [world pos' state]
  (if (and (= :water (liquid/liquid-class (block-at world pos')))
           (contains? (block/props-of state) :waterlogged))
    (block/state (block/block-of state) (assoc (block/props-of state) :waterlogged :true))
    state))

(def ^:private water-plant-types #{:kelp :seagrass})

(defn- water-plant-ok? [world [_ y _ :as pos'] ^long state]
  (let [cur (block-at world pos')]
    (and (= :water (liquid/liquid-class cur))
         (contains? #{0 8} (liquid/level cur))
         (pos? (long y))
         (support/supported? (:chunks world) gen/flat-chunk pos' state))))

(declare door-place-deltas bed-place-deltas pair-place-deltas block-entity-place-deltas)

(defn- carpet-place-deltas [world eid pos state item]
  (let [chunks (chunk/chunks-set-blocks (:chunks world) gen/flat-chunk [[pos state]])
        side? (fn [dir] (< (double (random/of-key [(:tick world) pos :moss dir])) 0.5))]
    (if-let [topper (moss/carpet-topper chunks pos side?)]
      (placed-deltas world eid [[pos state] [(mapv + pos [0 1 0]) topper]] item)
      (placed-deltas world eid pos state item))))

(defn- solid-place-deltas [world [eid pos face item cursor]]
  (when-let [off (block/face-offsets face)]
    (when-let [state (block/placement item face (get-in world [:entities eid :yaw] 0.0) (nth cursor 1)
                                      (replaceable? world pos))]
      (let [pile (if (get-in world [:entities eid :sneaking?]) nil item)
            [_ y' _ :as target] (if (replaceable? world pos pile) pos (mapv + pos off))
            pos'   (when (chunk/in-range? y') target)
            state  (or (when (and pos' (contains? connect/placed-types (block/type-of state)))
                         (connect/reshape (:chunks world) pos' state (:tick world)))
                       state)
            state  (if pos'
                     (support/fitted (:chunks world) gen/flat-chunk pos' state face
                                     (or (get-in world [:entities eid :yaw]) 0.0) (or (get-in world [:entities eid :pitch]) 0.0)
                                     (boolean (get-in world [:entities eid :sneaking?]))
                                     (:tick world)
                                     (= pos' pos))
                     state)
            state  (if (and pos' state (contains? container/container-types (block/type-of state)))
                     (container/placed-state (:chunks world) pos' state face
                                             (boolean (get-in world [:entities eid :sneaking?]))
                                             (or (get-in world [:entities eid :yaw]) 0.0)
                                             (or (get-in world [:entities eid :pitch]) 0.0))
                     state)
            state  (if (and pos' state) (waterlogged world pos' state) state)
            state  (if pos' (stacked world pos' state item) state)
            merged (slab-merge world pos pos' face item)]
        (cond
          merged
          (let [[mp ms] merged]
            (if (obstructed? world mp ms)
              (reject-deltas world eid pos pos')
              (placed-deltas world eid mp ms item)))
          (nil? pos') nil
          (nil? state)
          (reject-deltas world eid pos pos')
          (not (replaceable? world pos' item))
          (reject-deltas world eid pos pos')
          (and (contains? water-plant-types (block/type-of state))
               (not (water-plant-ok? world pos' state)))
          (reject-deltas world eid pos pos')
          (obstructed? world pos' state)
          (reject-deltas world eid pos pos')
          (contains? block/door-types (block/type-of state))
          (door-place-deltas world eid pos pos' state item cursor)
          (= :bed (block/type-of state))
          (bed-place-deltas world eid pos pos' state item)
          (= :mossy-carpet (block/type-of state))
          (carpet-place-deltas world eid pos' state item)
          (contains? connect/pair-types (block/type-of state))
          (pair-place-deltas world eid pos pos' state item)
          (be/kind state)
          (block-entity-place-deltas world eid pos' state item)
          :else
          (placed-deltas world eid pos' state item))))))

(defn- block-entity-place-deltas [world eid pos state item]
  (concat (placed-deltas world eid pos state item)
          [[:set-block-entity pos (be/from-stack (be/fresh (be/kind state) eid)
                                                 (held-stack world eid))]]
          (when (sign/kind state) [(out/to eid (out/sign-editor pos true))])))

(defn- door-place-deltas [world eid pos pos' state item [cx _ cz]]
  (let [above (mapv + pos' [0 1 0])
        hinge (connect/door-hinge (:chunks world) pos' (block/facing-of state) cx cz)
        lower (block/state (block/block-of state) (assoc (block/props-of state) :hinge hinge))
        upper (block/state (block/block-of state) (assoc (block/props-of lower) :half :upper))]
    (if (and (chunk/in-range? (above 1))
             (replaceable? world above)
             (not (obstructed? world above upper))
             (block/face-sturdy? (block-at world (mapv + pos' [0 -1 0])) :up))
      (placed-deltas world eid [[pos' lower] [above upper]] item)
      (reject-deltas world eid pos pos'))))

(defn- eye-pos [e]
  (let [p (:pos e) crouch? (and (:sneaking? e) (not (:flying e)))]
    [(v/x p) (+ (v/y p) (if crouch? 1.27 1.62)) (v/z p)]))

(def ^:private ^:const block-interaction-range 6.0)

(defn- axis-gap ^double [^double eye ^double lo]
  (max (- lo eye) (- eye (+ lo 1.0)) 0.0))

(defn- in-reach? [e pos]
  (let [[ex ey ez] (eye-pos e)
        dx (axis-gap ex (double (nth pos 0)))
        dy (axis-gap ey (double (nth pos 1)))
        dz (axis-gap ez (double (nth pos 2)))]
    (< (+ (* dx dx) (* dy dy) (* dz dz))
       (* block-interaction-range block-interaction-range))))

(defn- look-dir [e]
  (let [yaw   (Math/toRadians (double (:yaw e)))
        pitch (Math/toRadians (double (:pitch e)))]
    [(- (* (Math/sin yaw) (Math/cos pitch)))
     (- (Math/sin pitch))
     (* (Math/cos yaw) (Math/cos pitch))]))

(defn- box-entry [[fx fy fz] [dx dy dz] [x0 y0 z0 x1 y1 z1]]
  (let [axis (fn [f d lo hi neg pos]
               (cond (pos? (double d)) [(/ (- (double lo) (double f)) (double d)) (/ (- (double hi) (double f)) (double d)) neg]
                     (neg? (double d)) [(/ (- (double hi) (double f)) (double d)) (/ (- (double lo) (double f)) (double d)) pos]
                     :else [(if (<= (double lo) (double f) (double hi)) Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY) Double/POSITIVE_INFINITY nil]))
        [ax bx facex] (axis fx dx x0 x1 :west :east)
        [ay by facey] (axis fy dy y0 y1 :down :up)
        [az bz facez] (axis fz dz z0 z1 :north :south)
        t-in (max (double ax) (double ay) (double az))
        t-out (min (double bx) (double by) (double bz))
        face (cond (= t-in (double ax)) facex (= t-in (double ay)) facey :else facez)]
    (when (and (<= t-in t-out) (< 0.0 t-in 1.0) face)
      [t-in face])))

(defn- cell-boxes [world [x y z :as pos] fluids]
  (let [st (block-at world pos)
        abs (fn [[a b c d e f]] [(+ (long x) (/ (double a) 16.0)) (+ (long y) (/ (double b) 16.0)) (+ (long z) (/ (double c) 16.0))
                                 (+ (long x) (/ (double d) 16.0)) (+ (long y) (/ (double e) 16.0)) (+ (long z) (/ (double f) 16.0))])
        fluid (when (and (not= :none fluids) (liquid/fluid-height-of (:chunks world) gen/flat-chunk pos st fluids))
                [[(long x) (long y) (long z) (inc (long x)) (+ (long y) (double (liquid/fluid-height-of (:chunks world) gen/flat-chunk pos st fluids))) (inc (long z))]])]
    (concat (when (and (pos? st) (not (liquid/liquid-state? st))) (map abs (block/outline-boxes st)))
            fluid)))

(defn- clip [world e fluids]
  (let [from (eye-pos e) dir (look-dir e)
        d (mapv #(* 5.0 (double %)) dir)
        step (fn [c dc] (if (neg? (double dc)) -1 1))
        next-t (fn [f dc c] (if (zero? (double dc)) Double/POSITIVE_INFINITY
                                (/ (- (if (pos? (double dc)) (inc (long c)) (double c)) (double f)) (double dc))))]
    (loop [[cx cy cz :as cell] (mapv #(long (Math/floor (double %))) from)
           tx (next-t (from 0) (d 0) cx) ty (next-t (from 1) (d 1) cy) tz (next-t (from 2) (d 2) cz)
           n 0]
      (let [hit (when (chunk/in-range? cy)
                  (first (sort-by first (keep #(box-entry from d %) (cell-boxes world cell fluids)))))]
        (cond
          hit {:pos cell :face (second hit)}
          (or (> n 24) (every? #(> (double %) 1.0) [tx ty tz])) nil
          (and (<= tx ty) (<= tx tz)) (recur [(+ cx (step cx (d 0))) cy cz] (+ tx (/ 1.0 (Math/abs (double (d 0))))) ty tz (inc n))
          (<= ty tz) (recur [cx (+ cy (step cy (d 1))) cz] tx (+ ty (/ 1.0 (Math/abs (double (d 1))))) tz (inc n))
          :else (recur [cx cy (+ cz (step cz (d 2)))] tx ty (+ tz (/ 1.0 (Math/abs (double (d 2))))) (inc n)))))))

(def ^:private face-normal {:down [0 -1 0] :up [0 1 0] :north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]})
(defn- waterloggable? [st]
  (= :false (:waterlogged (block/props-of st))))

(defn- with-water [st logged?]
  (block/state (block/block-of st) (assoc (block/props-of st) :waterlogged (if logged? :true :false))))

(defn- pour-deltas [world eid pos state relative]
  (let [cur (block-at world pos) water? (= :water (liquid/liquid-class state))
        may-replace? (or (block/can-be-replaced? cur) (not (block/blocks-motion? cur)))
        holds? (and water? (waterloggable? cur))
        shift? (get-in world [:entities eid :sneaking?])
        sound (if water? :bucket/empty :bucket/empty-lava)]
    (cond
      (not (or (zero? cur) (and (or may-replace? holds?) (or (not shift?) (nil? relative)))))
      (when relative (pour-deltas world eid relative state nil))
      holds? (concat (change-deltas world [[pos (with-water cur true)]]) [(out/except eid (out/sound sound pos 1.0 1.0))])
      :else (concat
             (when (and may-replace? (pos? cur) (not (liquid/liquid-state? cur)) (get-in world [:rules :block-drops] true))
               (map-indexed (fn [i stack] [:spawn-entity (items/popped world pos stack [:bucket i])])
                            (block/drops cur (fn [salt] (random/of-key [(:tick world) pos salt])))))
             (change-deltas world [[pos state]])
             [(out/except eid (out/sound sound pos 1.0 1.0))]))))

(defn- add [world eid e state]
  (when-let [{:keys [pos face]} (clip world e :none)]
    (let [relative (mapv + pos (face-normal face))
          hit (block-at world pos)
          target (if (and (contains? (block/props-of hit) :waterlogged) (= :water (liquid/liquid-class state))) pos relative)]
      (when (chunk/in-range? (target 1))
        (pour-deltas world eid target state (when (= target pos) relative))))))

(defn- scoop-target [world e]
  (when-let [{:keys [pos]} (clip world e :source-only)]
    (let [st (block-at world pos)]
      (cond
        (= :powder-snow (block/type-of st)) [:powder-snow pos]
        (liquid/bubble-column? st) [:bubble-column pos]
        (liquid/source-state? st) [:source pos]
        (= :true (:waterlogged (block/props-of st))) [:waterlogged pos]))))

(defn- scoop-deltas [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [st (block-at world pos)
          sound (cond
                  (= :powder-snow kind) :bucket/fill-snow
                  (= :lava (liquid/liquid-class st)) :bucket/fill-lava
                  :else :bucket/fill)]
      (concat (case kind
                (:source :bubble-column) [[:set-blocks [[pos 0]] (dec (long (:tick world)))]]
                :powder-snow (change-deltas world [[pos 0]])
                :waterlogged (change-deltas world [[pos (with-water st false)]]))
              (when (= :powder-snow kind) [(out/all (out/level-event 2001 pos st))])
              [(out/except eid (out/sound sound pos 1.0 1.0))]))))

(defn- lily-deltas [world eid e]
  (when-let [[kind pos] (scoop-target world e)]
    (let [[_ y' _ :as above] (mapv + pos [0 1 0])
          st (block/state :lily-pad)]
      (when (and (= :source kind)
                 (= :water (liquid/liquid-class (block-at world pos)))
                 (chunk/in-range? y')
                 (block/can-be-replaced? (block-at world above))
                 (not (obstructed? world above st))
                 (support/supported? (:chunks world) gen/flat-chunk above st))
        (placed-deltas world eid above st :lily-pad)))))

(defn- bonemeal-deltas [world [eid pos _ _ _]]
  (let [st (block-at world pos)]
    (when-let [{:keys [changes drops]} (grow/bonemeal (:chunks world) pos st (fn [salt] (random/of-key [(:tick world) pos :meal salt])))]
      (concat
       (when (seq changes) (change-deltas world changes))
       (map-indexed (fn [i stack] [:spawn-entity (items/popped world pos stack [:meal i])]) drops)
       [(out/all (out/bonemeal pos))]))))

(defn- till-deltas [world [eid pos _ _ _]]
  (let [cur (block-at world pos)]
    (when-let [[to freed] (grow/tilled (block/block-of cur))]
      (when (zero? (block-at world (mapv + pos [0 1 0])))
        (concat
         (change-deltas world [[pos (block/state to)]])
         (when freed [[:spawn-entity (items/popped world pos {:item freed :count 1} :till)]])
         [(out/all (out/sound :hoe/till pos 1.0 1.0))])))))

(defn- flatten-deltas [world [eid pos face _ _]]
  (let [cur (block-at world pos)]
    (when (and (not= 0 (long face))
               (contains? grow/flattened (block/block-of cur))
               (zero? (block-at world (mapv + pos [0 1 0]))))
      (concat
       (change-deltas world [[pos (block/state :dirt-path)]])
       [(out/all (out/sound :shovel/flatten pos 1.0 1.0))]))))

(defn- half-changes [world pos ^long st]
  (if-not (contains? block/door-types (block/type-of st))
    [[pos st]]
    (let [lower? (= :lower (:half (block/props-of st)))
          other  (mapv + pos (if lower? [0 1 0] [0 -1 0]))
          ost    (block-at world other)]
      (cond-> [[pos st]]
        (contains? block/door-types (block/type-of ost))
        (conj [other (block/state (block/block-of st)
                                  (assoc (block/props-of ost) :half (if lower? :upper :lower)))])))))

(defn- wax-deltas [world [_ pos _ _ _]]
  (when-let [st (block/waxed (block-at world pos))]
    (concat (change-deltas world (half-changes world pos st))
            [(out/all (out/sound :honeycomb/wax-on pos 1.0 1.0)) (out/all (out/level-event 3003 pos))])))

(defn- axe-deltas [world [_ pos _ _ _]]
  (let [cur (block-at world pos)]
    (if-let [st (block/stripped cur)]
      (concat (change-deltas world [[pos st]]) [(out/all (out/sound :axe/strip pos 1.0 1.0))])
      (if-let [st (block/weathered-prev cur)]
        (concat (change-deltas world (half-changes world pos st))
                [(out/all (out/sound :axe/scrape pos 1.0 1.0)) (out/all (out/level-event 3005 pos))])
        (when-let [st (block/unwaxed cur)]
          (concat (change-deltas world (half-changes world pos st))
                  [(out/all (out/sound :axe/wax-off pos 1.0 1.0)) (out/all (out/level-event 3004 pos))]))))))

(defn- axe? [item] (str/ends-with? (name item) "-axe"))
(defn- hoe? [item] (str/ends-with? (name item) "-hoe"))
(defn- shovel? [item] (str/ends-with? (name item) "-shovel"))
(def ^:private armor-slot
  {"helmet" 5 "chestplate" 6 "leggings" 7 "boots" 8})

(defn- armor-slot-of [item]
  (some (fn [[suffix slot]] (when (str/ends-with? (name item) suffix) slot)) armor-slot))

(defn- equip-armor-deltas [world eid item slot]
  (let [e (get-in world [:entities eid])]
    (when (and e (nil? (get-in e [:inventory slot])))
      (let [held  (+ 36 (long (or (:held-slot e) 0)))
            stack (or (get-in e [:inventory held])
                      {:item item :count 1})]
        [[:set-slot eid slot stack]
         [:set-slot eid held nil]]))))

(defn- spawn-egg-deltas [world [_ pos face item]]
  (when-let [off (block/face-offsets face)]
    (when-let [mob (mobs/egg-type item)]
      (let [[x y z] (mapv + pos off)
            t (:tick world)
            at [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
            pitch (+ 1.0 (* 0.2 (- (random/of-key [t pos :p1]) (random/of-key [t pos :p2]))))]
        (when (chunk/in-range? y)
          (cons [:spawn-entity (mobs/egg-mob mob at [t pos] t)]
                (when-let [say (mobs/say-sound mob)]
                  [(out/all (out/sound say at 1.0 pitch))])))))))

(defn- opens? [world eid pos item use-item?]
  (let [cur (block-at world pos)]
    (and (not use-item?)
         (contains? openable-types (block/type-of cur))
         (by-hand? cur)
         (not (and item (get-in world [:entities eid :sneaking?]))))))

(defn- bed-place-deltas [world eid pos pos' state item]
  (let [head-pos (mapv + pos' (connect/bed-partner-offset state))
        head     (block/state (block/block-of state) (assoc (block/props-of state) :part :head))]
    (if (and (replaceable? world head-pos item)
             (not (obstructed? world head-pos head)))
      (placed-deltas world eid [[pos' state] [head-pos head]] item)
      (reject-deltas world eid pos pos'))))

(defn- scaffold-place-deltas [world eid pos face]
  (if-let [target (scaffold-target world eid pos face)]
    (let [st (support/scaffold-state (:chunks world) gen/flat-chunk target (block/state :scaffolding))]
      (if (not (obstructed? world target st))
        (placed-deltas world eid target (waterlogged world target st) :scaffolding)
        (reject-deltas world eid pos target)))
    (reject-deltas world eid pos nil)))

(defn- pair-place-deltas [world eid pos pos' state item]
  (let [above (mapv + pos' [0 1 0])
        props (block/props-of state)
        upper (block/state (block/block-of state) (assoc props :half :upper))
        upper (if (contains? props :waterlogged)
                (with-water upper (= :water (liquid/liquid-class (block-at world above))))
                upper)]
    (if (and (chunk/in-range? (above 1))
             (block/can-be-replaced? (block-at world above))
             (not (obstructed? world above upper)))
      (placed-deltas world eid [[pos' state] [above upper]] item)
      (reject-deltas world eid pos pos'))))

(defn- uses-bed? [world eid pos item use-item?]
  (and (not use-item?)
       (= :bed (block/type-of (block-at world pos)))
       (not (and item (get-in world [:entities eid :sneaking?])))))

(defn- bed-in-range? [world eid head]
  (let [p  (get-in world [:entities eid :pos])
        st (block-at world head)
        foot (mapv + head (connect/bed-partner-offset st))]
    (some (fn [[x y z]]
            (and (<= (Math/abs (- (v/x p) (+ (double x) 0.5))) 3.0)
                 (<= (Math/abs (- (v/y p) (double y))) 2.0)
                 (<= (Math/abs (- (v/z p) (+ (double z) 0.5))) 3.0)))
          [head foot])))

(defn- bed-blocked? [world head]
  (let [st   (block-at world head)
        above (mapv + head [0 1 0])]
    (or (block/full-cube? (block-at world above))
        (block/full-cube? (block-at world (mapv + above (connect/bed-partner-offset st)))))))

(defn- spawn-deltas [world eid head]
  (when (not= head (get-in world [:entities eid :spawn]))
    [[:merge-entity eid {:spawn head}]
     (out/to eid (out/system-chat [{:translate "block.minecraft.set_spawn"}]))]))

(defn- sleep-deltas [world eid pos]
  (let [head (bed/head-pos (:chunks world) pos)
        st   (when head (block-at world head))
        say  (fn [k] [(out/to eid (out/overlay [{:translate k}]))])]
    (cond
      (nil? head) nil
      (= :true (:occupied (block/props-of st))) (say "block.minecraft.bed.occupied")
      (get-in world [:entities eid :sleeping]) nil
      (not (bed-in-range? world eid head)) (say "block.minecraft.bed.too_far_away")
      (bed-blocked? world head) (say "block.minecraft.bed.obstructed")
      (not (daynight/dark? (:time-of-day world 0)))
      (concat (spawn-deltas world eid head) (say "block.minecraft.bed.no_sleep"))
      :else
      (let [[x y z] head
            lie [(+ (long x) 0.5) (+ (long y) 0.6875) (+ (long z) 0.5)]]
        (concat (spawn-deltas world eid head)
                (change-deltas world [[head (block/state (block/block-of st) (assoc (block/props-of st) :occupied :true))]])
                [[:merge-entity eid {:sleeping {:pos head :since (:tick world)} :pos (v/v3 lie)
                                     :vel [0.0 0.0 0.0] :client-vel [0.0 0.0 0.0] :leave-bed? nil}]
                 (sleep/announcement world (inc (count (sleep/sleepers world))))])))))

(def ^:private mud-blocks (delay (set (get-in @data/tags ["block" "convertable_to_mud"]))))

(defn- mud-deltas [world eid pos face]
  (when (and (not= 0 (long face))
             (contains? @mud-blocks (block/block-of (block-at world pos)))
             (water-bottle? (held-stack world eid)))
    (concat (change-deltas world [[pos (block/state :mud)]])
            [(out/all (out/sound :splash pos 1.0 1.0))
             (out/all (out/sound :bottle/empty pos 1.0 1.0))])))

(defn- place-deltas [world [eid pos face item cursor] origin]
  (let [item      (or item (sense/held-of (get-in world [:entities eid])))
        args      [eid pos face item cursor]
        at        (merge (get-in world [:entities eid]) origin)
        world     (assoc-in world [:entities eid] at)
        use-item? (= 255 (bit-and (long face) 0xFF))
        pour      (liquid/bucket->state item)]
    (cond
      (opens? world eid pos item use-item?) (toggle-deltas world eid pos (block-at world pos))
      (uses-bed? world eid pos item use-item?) (sleep-deltas world eid pos)
      (and (uses-block? world eid pos item use-item?) (use-deltas world eid pos face item cursor))
      (use-deltas world eid pos face item cursor)
      (and (= :scaffolding item) (not use-item?) (= :scaffolding (block/type-of (block-at world pos))))
      (scaffold-place-deltas world eid pos face)
      (nil? item)                  nil
      pour                         (when use-item? (add world eid at pour))
      (= :flint-and-steel item)    (when-not use-item? (flint-deltas world args))
      (= :bucket item)             (when use-item? (scoop-deltas world eid at))
      (= :lily-pad item)           (when use-item? (lily-deltas world eid at))
      (= :potion item)             (when-not use-item? (mud-deltas world eid pos face))
      (= :bone-meal item)          (when-not use-item? (bonemeal-deltas world args))
      (hoe? item)                  (when-not use-item? (till-deltas world args))
      (= :honeycomb item)          (when-not use-item? (wax-deltas world args))
      (axe? item)                  (when-not use-item? (or (axe-deltas world args) (solid-place-deltas world args)))
      (shovel? item)               (when-not use-item? (flatten-deltas world args))
      (mobs/egg-type item)         (when-not use-item? (spawn-egg-deltas world args))
      (armor-slot-of item)         (when use-item?
                                     (equip-armor-deltas world eid item (armor-slot-of item)))
      :else                        (solid-place-deltas world args))))

(def ^:private face-offsets
  {0 [0 -1 0] 1 [0 1 0] 2 [0 0 -1] 3 [0 0 1] 4 [-1 0 0] 5 [1 0 0]})

(defn- sequence-of [tag args]
  (case tag
    :dig (when (#{0 1 2} (long (first args))) (nth args 3 nil))
    :place (nth args 4 nil)
    nil))

(defn- acted-at [world eid origin pos]
  (in-reach? (merge (get-in world [:entities eid]) origin) pos))

(defn- use-ack-deltas [world events origins]
  (mapcat (fn [[i [tag eid pos face]]]
            (when-let [off (and (= :place tag) (face-offsets (bit-and (long face) 0xFF)))]
              (when (and (chunk/in-range? (nth pos 1))
                         (acted-at world eid (get origins i) pos))
                (let [pos' (mapv + pos off)]
                  (cond-> [(own-change world eid pos)]
                    (chunk/in-range? (nth pos' 1)) (conj (own-change world eid pos')))))))
          (map-indexed vector events)))

(defn ack-deltas [world events]
  (let [latest (reduce (fn [m [tag eid & args]]
                         (if-let [sq (sequence-of tag args)]
                           (update m eid (fnil max -1) (long sq))
                           m))
                       {} events)]
    (concat (map (fn [[eid sq]] (out/to eid (out/block-ack sq))) latest)
            (use-ack-deltas world events (:use-origins world)))))

(defn- with-edits [world deltas]
  (let [changes (into [] (mapcat (fn [[tag recs]] (when (= tag :set-blocks) recs))) deltas)]
    (if (empty? changes)
      world
      (update world :chunks chunk/chunks-set-blocks gen/flat-chunk changes))))

(defn- block-edits-deltas [world events]
  (let [[_ edits] (reduce (fn [[w acc] [i [tag & args]]]
                            (let [ds (case tag
                                       :dig   (dig-deltas w args)
                                       :place (place-deltas w args (get-in world [:use-origins i]))
                                       :sign-update (sign-update-deltas w args)
                                       nil)]
                              [(with-edits w ds) (into acc ds)]))
                          [world []]
                          (map-indexed vector events))]
    edits))

(defn block-edits [world events]
  [#(block-edits-deltas world events)])
