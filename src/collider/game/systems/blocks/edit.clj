(ns collider.game.systems.blocks.edit
  "Block edit checks and change deltas."
  (:require [collider.data :as data]
            [collider.game.block.tnt :as tnt]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.campfire :as campfire]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.attribute :as attribute]))

(set! *warn-on-reflection* true)

(defn block-at ^long [world pos]
  (chunk/chunks-get-block (:chunks world) pos))

(def ^:private ^:const player-half 0.3)

(def ^:private ^:const player-height 1.8)

(def ^:private ^:const crouching-height 1.5)

(def ^:private ^:const tnt-half 0.49)

(def ^:private ^:const tnt-height 0.98)

(defn builder-box
  "Returns the half width and height an entity blocks with.
  Returns nil when it never blocks placement."
  [e]
  (case (:type e)
    :player [player-half
             (if (and (:sneaking? e) (not (:flying e)))
               crouching-height
               player-height)]
    (:tnt :falling-block) [tnt-half tnt-height]
    :item nil
    (when-let [m (get mobs/types (:type e))]
      (let [s (if (mobs/baby? e) 0.5 1.0)]
        [(* s (double (:half m))) (* s (double (:height m)))]))))

(defn- box-hits-at? [^doubles a ^long n [px py pz] [half h]]
  (let [px (double px) py (double py) pz (double pz)
        half (double half) h (double h)]
    (loop [i 0]
      (if (= i n)
        false
        (let [o (* i 6)]
          (if (and (> (+ px half) (aget a o))
                   (< (- px half) (aget a (+ o 3)))
                   (> (+ py h) (aget a (+ o 1)))
                   (< py (aget a (+ o 4)))
                   (> (+ pz half) (aget a (+ o 2)))
                   (< (- pz half) (aget a (+ o 5))))
            true
            (recur (inc i))))))))

(defn- abs-boxes ^doubles [^long x ^long y ^long z state]
  (let [bs (block/collision-boxes state)
        a (double-array (* 6 (count bs)))]
    (reduce (fn [^long i b]
              (let [o (* i 6)]
                (aset a o (+ x (/ (double (nth b 0)) 16.0)))
                (aset a (+ o 1) (+ y (/ (double (nth b 1)) 16.0)))
                (aset a (+ o 2) (+ z (/ (double (nth b 2)) 16.0)))
                (aset a (+ o 3) (+ x (/ (double (nth b 3)) 16.0)))
                (aset a (+ o 4) (+ y (/ (double (nth b 4)) 16.0)))
                (aset a (+ o 5) (+ z (/ (double (nth b 5)) 16.0)))
                (inc i)))
            0 bs)
    a))

(defn obstructed?
  "Returns true when a block of that state at the cell would
  overlap an entity that stops building there."
  [world [x y z] state]
  (let [^doubles a (abs-boxes (long x) (long y) (long z) state)
        n (quot (alength a) 6)
        hit (fn [_ _ e]
              (if-let [dims (builder-box e)]
                (if (box-hits-at? a n (:pos e) dims)
                  (reduced true)
                  false)
                false))]
    (and (pos? n)
         (boolean (reduce-kv hit false (:entities world))))))

(defn own-change
  "Returns the effect that shows one player the true block at pos."
  [world eid pos]
  (let [at (chunk/block-chunk pos)
        changed [[pos (block-at world pos)]]]
    (out/to eid (out/blocks-changed at changed))))

(defn build-limit
  "Returns the red line above the hotbar that names the height
  limit of building, the top one when high? is true."
  [eid high? ^long y]
  (let [k (if high? "build.tooHigh" "build.tooLow")
        text {:translate k :with [y] :color "red"}]
    (out/to eid (out/overlay text))))

(def ^:const ^:private sponge-dries 2009)

(defn- drying? [world [_ st]]
  (and (= :wet-sponge (block/block-of (long st)))
       (attribute/water-evaporates? (:dim world))))

(defn- dried-fx [world pos]
  (let [roll (random/of-key (:tick world) pos :sponge-dries)
        pitch (* (+ 1.0 (* (double roll) 0.2)) 0.7)]
    [(out/all (out/level-event sponge-dries pos 0))
     (out/all (out/sound :wet-sponge/dries pos 1.0 pitch))]))

(defn dried
  "Returns the changes with each wet sponge dried where water
  evaporates, and the effects of its drying."
  [world changes]
  (let [dry? #(drying? world %)
        sponge (block/state :sponge)]
    [(mapv (fn [[pos :as c]] (if (dry? c) [pos sponge] c)) changes)
     (into [] (comp (filter dry?) (map first)
                    (mapcat #(dried-fx world %)))
           changes)]))

(defn- dropped [world pos [_ old]]
  (when (get-in world [:rules :block-drops] true)
    (let [salt (fn [salt] (random/of-key (:tick world) pos salt))]
      (for [[i stack] (map-indexed vector (block/drops old salt))]
        [:spawn-entity (items/popped world pos stack i)]))))

(defn- falling-block [[x y z :as pos] [_ st]]
  [[:spawn-entity
    {:type :falling-block
     :pos [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
     :vel [0.0 0.0 0.0] :yaw 0.0 :pitch 0.0 :on-ground false
     :block (block/without-water st) :start pos :time 0}]])

(defn- primed [world pos]
  (when (get-in world [:rules :tnt-explodes] true)
    (let [e (tnt/primed pos [(:tick world) pos])]
      [[:spawn-entity e]
       (out/all (out/sound :tnt/primed (:pos e) 1.0 1.0))])))

(def ^:private drip-events
  {:water out/sound-drip-water-into-cauldron
   :lava out/sound-drip-lava-into-cauldron})

(def ^:private change-effects
  "What each effect a change names does, by its kind."
  {:fizz (fn [_ pos _] [(out/all (out/fizz pos))])
   :break (fn [_ pos [_ old]] [(out/all (out/break-effect pos old))])
   :drop dropped
   :fall (fn [_ pos e] (falling-block pos e))
   :prime (fn [world pos _] (primed world pos))
   :dry (fn [world pos _] (dried-fx world pos))
   :sound (fn [_ pos [_ kind volume pitch]]
            [(out/all (out/sound kind pos volume pitch))])
   :drip (fn [_ pos [_ fluid]]
           [(out/all (out/level-event (drip-events fluid) pos 0))])
   :schedule (fn [_ _ [_ at-ids]] [[:schedule-ticks at-ids]])})

(defn change-fx
  "Returns the deltas of the effects the changes carry. A change
  [pos st fx] names them in fx, where each is a kind, as :fizz,
  or a kind with its data, as [:drop st]. A change [pos st] has
  none."
  [world changes]
  (for [[pos _ fx] changes
        e fx
        :let [e (if (keyword? e) [e] e)]
        d ((change-effects (first e)) world pos e)]
    d))

(defn block-changes
  "Returns the changes as [pos st], without their effects."
  [changes]
  (mapv (fn [[pos st]] [pos st]) changes))

(defn- mixed-deltas [world all fx mixed]
  (-> [[:set-blocks (into all (block-changes mixed))
        (dec (long (:tick world)))]]
      (into (change-fx world mixed))
      (into fx)))

(defn change-deltas
  "Returns the deltas for the changes.
  It also covers the changes they cause in the blocks
  around them."
  [world changes]
  (let [[changes fx] (dried world changes)
        chunks' (chunk/chunks-set-blocks (:chunks world) changes)
        derived (connect/derived-changes
                  chunks' (map first changes) (:tick world))
        all (into (vec changes) derived)
        chunks'' (chunk/chunks-set-blocks chunks' all)]
    (mixed-deltas world all fx
                  (liquid/mix-changes chunks'' (map first all)))))

(defn held-slot
  "Returns the inventory slot of the item the player holds."
  ^long [world eid]
  (let [e (get-in world [:entities eid])]
    (state/hand-slot e (:use-hand e))))

(defn held-stack [world eid]
  (get-in world [:entities eid :inventory (held-slot world eid)]))

(def ^:private eruption-sounds
  {:erupting :block.potent-sulfur.geyser-eruption
   :continuous :block.potent-sulfur.geyser-continuous-eruption})

(defn sulfur-placed-fx
  "Returns the effects of potent sulfur taking state st at pos,
  PotentSulfurBlock.onPlace: a geyser that starts is heard and
  runs its block event."
  [[x y z :as pos] ^long st]
  (when-let [kind (eruption-sounds (geyser/phase st))]
    (let [at [(+ (long x) 0.5) (+ (long y) 0.5) (+ (long z) 0.5)]]
      [(out/all (out/sound kind at 1.0 1.0))
       (out/all (out/block-event pos 0 0))])))

(defn- ghast-placed-fx [pos ^long state]
  (let [kind (if (block/waterlogged? state)
               :block.dried-ghast.place-in-water
               :block.dried-ghast.place)]
    [(out/all (out/sound kind pos 1.0 1.0))]))

(defn- placed-by-fx [pos ^long state]
  (case (block/type-of state)
    :dried-ghast (ghast-placed-fx pos state)
    :potent-sulfur (sulfur-placed-fx pos state)
    nil))

(defn- place-sound [world eid pos state]
  (let [item (:item (held-stack world eid))
        {:keys [kind volume pitch]}
        (data/placed-sound (block/block-of state) item)]
    (out/except eid (out/sound kind pos volume pitch))))

(defn placed-deltas
  "Returns the deltas of a player placing blocks, with the place
  sound for everyone else."
  ([world eid pos state] (placed-deltas world eid [[pos state]]))
  ([world eid changes]
   (let [deltas (change-deltas world changes)
         [[pos state]] (first (dried world changes))]
     (-> deltas
         (into (placed-by-fx pos state))
         (conj (place-sound world eid pos state))))))

(defn be-changed
  "Returns the deltas that set the block entity at pos and show it."
  [pos e]
  [[:set-block-entity pos e] (out/all (out/block-entity pos))])

(defn hit-uv
  "Returns where a click landed on a face, across and up.
  Both run from zero to one."
  [face [cx cy cz]]
  (let [x (/ (double cx) 16.0)
        y (/ (double cy) 16.0)
        z (/ (double cz) 16.0)]
    (case (long face)
      2 [(- 1.0 x) y]
      3 [x y]
      4 [z y]
      5 [(- 1.0 z) y]
      nil)))

(defn section
  "Returns which of n equal parts a fraction falls in."
  ^long [^double rel ^long n]
  (min (dec n) (max 0 (long (Math/floor (* rel n))))))

(defn hit-slot
  "Returns which slot of a grid drawn on the block face was clicked."
  [st face cursor rows cols]
  (when (= (block/facing-of st) (dir/from-index (long face)))
    (when-let [[u v] (hit-uv face cursor)]
      (let [cols (long cols)
            col (section (double u) cols)
            row (section (- 1.0 (double v)) (long rows))]
        (+ col (* cols row))))))

(defn waterloggable?
  "Returns true when the state can take water and holds none."
  [st]
  (= :false (:waterlogged (block/props-of st))))

(defn with-water
  "Returns the state with water in it, or without when logged? is
  false."
  [st logged?]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :waterlogged
                      (if logged? :true :false))))

(def ^:private full-water-types
  "The classes that take a fall as well as a source, isFull.
  Everywhere else a placed block holds only source water."
  #{:conduit :coral-plant :coral-fan :coral-wall-fan
    :base-coral-plant :base-coral-fan :base-coral-wall-fan})

(defn- placed-wet? [st state]
  (if (contains? full-water-types (block/type-of state))
    (block/full-water? st)
    (block/water-source? st)))

(defn waterlogged
  "Returns the state to place at pos', with water in it when it
  takes the water that stands there."
  [world pos' state]
  (if (and (placed-wet? (block-at world pos') state)
           (contains? (block/props-of state) :waterlogged)
           (not= :light (block/type-of state)))
    (with-water state true)
    state))

(defn- unlit [^long st]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :lit :false)))

(defn candle-out-deltas
  "Returns the deltas that put out a lit candle at pos.
  Returns nil when it is already unlit."
  [world pos]
  (let [cur (block-at world pos)]
    (when (= :true (:lit (block/props-of cur)))
      (let [deltas (change-deltas world [[pos (unlit cur)]])
            snuff (out/sound :candle/extinguish pos 1.0 1.0)]
        (concat deltas [(out/all snuff)])))))

(defn campfire-out-deltas
  "Returns the deltas that dowse a campfire at pos.
  The deltas carry its level event."
  [world pos]
  (when-let [st (campfire/dowsed (block-at world pos))]
    (concat (change-deltas world [[pos st]])
            [(out/all (out/level-event
                        out/sound-extinguish-fire pos))])))

(defn dowse-deltas
  "Returns the deltas of a candle or a campfire dowsed by water."
  [world pos]
  (case (block/type-of (block-at world pos))
    (:candle :candle-cake) (candle-out-deltas world pos)
    :campfire (campfire-out-deltas world pos)
    nil))
