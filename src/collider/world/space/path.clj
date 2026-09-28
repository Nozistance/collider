(ns collider.world.space.path
  "Ground paths of mobs, the types of cells and the search over them."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world.space Path PathTarget)))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-visited-nodes 256)

(def path-types
  "The path types in the order the game declares them."
  [:blocked :open :walkable :walkable-door :trapdoor :powder-snow
   :on-top-of-powder-snow :fence :lava :water :water-border :rail
   :unpassable-rail :fire-in-neighbor :fire :damaging-in-neighbor
   :damaging :door-open :door-wood-closed :door-iron-closed :breach
   :leaves :sticky-honey :cocoa :damage-cautious :on-top-of-trapdoor
   :big-mobs-close-to-danger])

(def default-malus
  "The malus every path type carries when a mob keeps no own one."
  {:blocked -1.0 :open 0.0 :walkable 0.0 :walkable-door 0.0
   :trapdoor 0.0 :powder-snow -1.0 :on-top-of-powder-snow 0.0
   :fence -1.0 :lava -1.0 :water 8.0 :water-border 8.0 :rail 0.0
   :unpassable-rail -1.0 :fire-in-neighbor 8.0 :fire 16.0
   :damaging-in-neighbor 8.0 :damaging -1.0 :door-open 0.0
   :door-wood-closed -1.0 :door-iron-closed -1.0 :breach 4.0
   :leaves -1.0 :sticky-honey 8.0 :cocoa 0.0 :damage-cautious 0.0
   :on-top-of-trapdoor 0.0 :big-mobs-close-to-danger 4.0})

(def ^:private type-index (zipmap path-types (range)))

(defn path-type-malus
  "Returns the malus mob puts on a path type."
  ^double [mob type]
  (double (get (:malus mob) type (get default-malus type))))

(def cow
  "The pathfinding parameters of a cow, sheep or mooshroom."
  {:width  0.9 :height 1.4 :max-up-step 0.6 :max-fall 3
   :float? true :open-doors? false :pass-doors? true
   :walk-over-fences? false
   :malus  {:fire-in-neighbor 16.0 :fire -1.0}})

(defn- class-of [^long st]
  (:class (get (data/blocks) (block/block-of st))))

(def ^:private door-classes
  #{:door-block :weathering-copper-door-block})

(def ^:private trapdoor-classes
  #{:trap-door-block :weathering-copper-trap-door-block})

(def ^:private rail-classes
  #{:rail-block :powered-rail-block :detector-rail-block})

(def ^:private leaves-classes
  #{:mangrove-leaves-block :tinted-particle-leaves-block
    :untinted-particle-leaves-block})

(def ^:private unpathfindable
  #{:abstract-cauldron-block :anvil-block :azalea-block
    :bamboo-stalk-block :bed-block :bell-block :brewing-stand-block
    :cactus-block :cake-block :calibrated-sculk-sensor-block
    :campfire-block :candle-cake-block :cauldron-block :chain-block
    :chest-block :chorus-plant-block :cocoa-block :composter-block
    :conduit-block :copper-chest-block :copper-golem-statue-block
    :decorated-pot-block :dirt-path-block :dragon-egg-block
    :dried-ghast-block :enchanting-table-block :end-portal-frame-block
    :end-rod-block :ender-chest-block :farmland-block :fence-block
    :flower-pot-block :grindstone-block :heavy-core-block
    :hopper-block :iron-bars-block :lantern-block :lava-cauldron-block
    :layered-cauldron-block :lectern-block :lightning-rod-block
    :moving-piston-block :mud-block :piglin-wall-skull-block
    :piston-base-block :piston-head-block :player-head-block
    :player-wall-head-block :pointed-dripstone-block
    :respawn-anchor-block :sculk-sensor-block :sea-pickle-block
    :shelf-block :skull-block :slab-block :sniffer-egg-block
    :soul-sand-block :stained-glass-pane-block :stair-block
    :stonecutter-block :sulfur-spike-block :trapped-chest-block
    :wall-block :wall-hanging-sign-block :wall-skull-block
    :weathering-copper-bars-block :weathering-copper-chain-block
    :weathering-copper-chest-block
    :weathering-copper-golem-statue-block
    :weathering-copper-slab-block :weathering-copper-stair-block
    :weathering-lantern-block :weathering-lightning-rod-block
    :wither-skull-block :wither-wall-skull-block})

(defn- open-prop? [^long st] (= :true (:open (block/props-of st))))

(defn- pathfindable? [^long st]
  (let [c (class-of st)]
    (cond
      (or (door-classes c) (trapdoor-classes c)
          (= :fence-gate-block c)) (open-prop? st)
      (= :snow-layer-block c) (< (block/prop-long st :layers) 5)
      (= :liquid-block c) (not (block/lava? st))
      (= :powder-snow-block c) true
      (unpathfindable c) false
      :else (not (block/full-cube? st)))))

(defn- burning? [^long st]
  (let [b (block/block-of st)]
    (or (block/tagged? st "fire") (= :lava b) (= :magma-block b)
        (and (= :campfire-block (class-of st))
             (= :true (:lit (block/props-of st))))
        (= :lava-cauldron b))))

(defn- plant-type [^long st]
  (let [b (block/block-of st)]
    (cond
      (block/air? st) :open
      (or (block/tagged? st "trapdoors") (= :lily-pad b)
          (= :big-dripleaf b)) :trapdoor
      (= :powder-snow b) :powder-snow
      (or (= :cactus b) (= :sweet-berry-bush b)) :damaging
      (= :honey-block b) :sticky-honey
      (= :cocoa b) :cocoa
      (or (= :wither-rose b)
          (block/tagged? st "speleothems")) :damage-cautious)))

(defn- door-type [^long st]
  (cond
    (open-prop? st) :door-open
    (:hand? (get (data/blocks) (block/block-of st))) :door-wood-closed
    :else :door-iron-closed))

(defn- fence-type? [^long st]
  (or (block/tagged? st "fences") (block/tagged? st "walls")
      (and (= :fence-gate-block (class-of st))
           (not (open-prop? st)))))

(defn- solid-type [^long st]
  (let [c (class-of st)]
    (cond
      (block/lava? st) :lava
      (burning? st) :fire
      (door-classes c) (door-type st)
      (rail-classes c) :rail
      (leaves-classes c) :leaves
      (fence-type? st) :fence
      (not (pathfindable? st)) :blocked
      (block/water? st) :water
      :else :open)))

(def ^:private ^:table type-arr
  (delay
    (let [of (fn [st] (or (plant-type st) (solid-type st)))]
      (object-array (map of (range (data/block-state-count)))))))

(defn type-of-state
  "Returns the path type of a block state.
  A cell of an absent chunk reads as air, which is :open."
  [^long st]
  (if (< -1 st (data/block-state-count))
    (aget ^objects @type-arr st)
    :open))

(defn- neighbour-type [t]
  (case t
    :damaging :damaging-in-neighbor
    (:fire :lava) :fire-in-neighbor
    :water :water-border
    :damage-cautious :damage-cautious
    nil))

(def ^:private ^:table type-ids
  (delay (int-array (map type-index @type-arr))))

(defn- forced-id [t] (type-index (neighbour-type t) -1))

(def ^:private ^:table forced-ids
  (delay (int-array (map forced-id @type-arr))))

(def ^:private ^:table water-arr
  (delay
    (boolean-array
      (map block/water? (range (data/block-state-count))))))

(defn type-static
  "Returns the path type of the cell x y z of level lv for a mob one
  cell tall."
  [lv x y z]
  (nth path-types
       (Path/staticType (:chunks lv) @type-ids @forced-ids
                        (chunk/level-min-y lv) (long x) (long y)
                        (long z))))

(defn- malus-arr ^doubles [mob]
  (double-array (map #(path-type-malus mob %) path-types)))

(def ^:private base-malus (malus-arr {}))

(def ^:private cow-malus (malus-arr cow))

(defn- flag ^long [x] (if x 1 0))

(defn- malus-of ^doubles [mob]
  (if (identical? (:malus mob) (:malus cow))
    cow-malus
    (malus-arr mob)))

(defn- sizes ^doubles [mob]
  (let [[px py pz] (:pos mob)]
    (double-array [px py pz (:width mob) (:height mob)
                   (:max-up-step mob)])))

(defn- flags ^longs [mob]
  (long-array [(:max-fall mob) (flag (:float? mob))
               (flag (:open-doors? mob)) (flag (:pass-doors? mob))
               (flag (:walk-over-fences? mob))]))

(defn- search-of [lv mob]
  (Path. (:chunks lv) (chunk/level-min-y lv) @type-ids @forced-ids
         @water-arr (block/collision-arr) (malus-of mob) base-malus
         (sizes mob) (flags mob)))

(defn context
  "Returns what one search over level lv knows about its mob.
  The mob carries its size, its own malus and where it stands."
  [lv mob]
  {:chunks (:chunks lv)
   :min-y  (chunk/level-min-y lv)
   :mob    mob
   :search (search-of lv mob)})

(defn type-of-mob
  "Returns the path type of the cell x y z for the mob of ctx.
  One search types each cell once, as WalkNodeEvaluator caches it."
  [ctx x y z]
  (nth path-types
       (Path/typeOf (:search ctx) (long x) (long y) (long z))))

(defn- water-start? [^long st]
  (or (= :water (block/block-of st))
      (and (block/water? st) (zero? (block/liquid-level st)))))

(defn- water-top ^long [ctx ^long x ^long y ^long z]
  (loop [cy y]
    (if (water-start? (chunk/block-state (:chunks ctx) x cy z))
      (recur (inc cy))
      cy)))

(defn- stands-on-fluid? [mob ^long st]
  (contains? (:stand-on-fluid? mob) (block/liquid-class st)))

(defn- fluid-top ^long [ctx ^long x ^long y ^long z]
  (let [mob (:mob ctx) chunks (:chunks ctx)]
    (loop [cy y]
      (if (stands-on-fluid? mob (chunk/block-state chunks x cy z))
        (recur (inc cy))
        cy))))

(defn- air-drop ^long [ctx ^long x ^double py ^long z]
  (let [chunks (:chunks ctx) lo (long (:min-y ctx))]
    (loop [cy (long (Math/floor (+ py 1.0)))
           best (long (Math/floor py))]
      (if (<= cy lo)
        best
        (let [st (chunk/block-state chunks x (dec cy) z)]
          (if (or (block/air? st) (pathfindable? st))
            (recur (dec cy) cy)
            cy))))))

(defn- start-y ^long [ctx]
  (let [mob (:mob ctx) [px py pz] (:pos mob)
        x (long (Math/floor (double px)))
        z (long (Math/floor (double pz)))
        y0 (long (Math/floor (double py)))]
    (cond
      (stands-on-fluid? mob (chunk/block-state (:chunks ctx) x y0 z))
      (dec (fluid-top ctx x y0 z))
      (and (:float? mob) (:in-water? mob))
      (dec (water-top ctx x y0 z))
      (:on-ground? mob) (long (Math/floor (+ (double py) 0.5)))
      :else (air-drop ctx x (double py) z))))

(defn- can-start-at? [ctx x y z]
  (let [t (type-of-mob ctx x y z)]
    (and (not= :open t) (>= (path-type-malus (:mob ctx) t) 0.0))))

(defn- cell-of ^long [c] (long (Math/floor (double c))))

(defn- corners [mob]
  (let [[px _ pz] (:pos mob) w (/ (double (:width mob)) 2.0)
        x0 (cell-of (- (double px) w))
        x1 (cell-of (+ (double px) w))
        z0 (cell-of (- (double pz) w))
        z1 (cell-of (+ (double pz) w))]
    [[x0 z0] [x0 z1] [x1 z0] [x1 z1]]))

(defn start-node
  "Returns the node the mob of ctx stands on."
  [ctx]
  (let [mob (:mob ctx)
        [px _ pz] (:pos mob)
        bx (cell-of px) bz (cell-of pz)
        y (start-y ctx)
        ok? (fn [[x z]] (can-start-at? ctx x y z))
        [sx sz] (or (when-not (can-start-at? ctx bx y bz)
                      (first (filter ok? (corners mob))))
                    [bx bz])]
    (Path/startAt (:search ctx) (long sx) y (long sz))))

(defn- targets-of [goals]
  (mapv (fn [[x y z]] (PathTarget. (long x) (long y) (long z)))
        goals))

(defn- node-map [^longs c ^long i]
  {:x (aget c i) :y (aget c (+ i 1)) :z (aget c (+ i 2))
   :type (nth path-types (aget c (+ i 3)))})

(defn- reconstruct [t reached?]
  (let [c (Path/cells t)]
    {:nodes          (mapv #(node-map c %) (range 0 (alength c) 4))
     :target         (vec (Path/goal t))
     :reached?       reached?
     :dist-to-target (Path/gap t)}))

(defn- pick [targets reached?]
  (let [ps (mapv (fn [t] (reconstruct t reached?)) targets)
        len (fn [p] (count (:nodes p)))]
    (first (if reached?
             (sort-by len ps)
             (sort-by (juxt :dist-to-target len) ps)))))

(defn- search [lv mob goals maxlen reach mult]
  (let [ctx (context lv mob)
        from (start-node ctx)
        targets (targets-of goals)
        maxv (long (int (* (float max-visited-nodes) (float mult))))
        arr (into-array PathTarget targets)
        hit (Path/run (:search ctx) from arr maxlen reach maxv)]
    (if hit (pick hit true) (pick targets false))))

(defn find-path
  "Returns the path of a mob over level lv to the closest of the goal
  cells. The path holds the nodes walked, whether a goal was reached
  and how far its last node stays from the goal."
  [lv mob goals max-path-length reach-range multiplier]
  (search lv mob goals (double max-path-length)
          (long reach-range) (double multiplier)))
