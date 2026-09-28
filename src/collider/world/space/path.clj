(ns collider.world.space.path
  "Ground paths of mobs, the types of cells and the search over them."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.world.space Path PathHeap PathNode PathTarget)
           (java.util HashMap)))

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

(def ^:private type-order (zipmap path-types (range)))

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

(defn- type-at [chunks ^long x ^long y ^long z]
  (type-of-state (chunk/block-state chunks x y z)))

(defn- neighbour-type [t]
  (case t
    :damaging :damaging-in-neighbor
    (:fire :lava) :fire-in-neighbor
    :water :water-border
    :damage-cautious :damage-cautious
    nil))

(def ^:private ^:table forced-arr
  (delay (object-array (map neighbour-type @type-arr))))

(defn check-neighbours
  "Returns the type the cells around x y z force upon it, else t."
  [chunks x y z t]
  (or (Path/forced chunks @forced-arr (long x) (long y) (long z)) t))

(defn- floor-type [chunks ^long x ^long y ^long z]
  (case (type-at chunks x (dec y) z)
    (:open :water :lava :walkable) :open
    :fire :fire
    :damaging :damaging
    :sticky-honey :sticky-honey
    :powder-snow :on-top-of-powder-snow
    :damage-cautious :damage-cautious
    :trapdoor :on-top-of-trapdoor
    (check-neighbours chunks x y z :walkable)))

(defn- static-type [chunks lo x y z]
  (let [x (long x) y (long y) z (long z)
        t (type-at chunks x y z)]
    (if (and (= :open t) (>= y (inc (long lo))))
      (floor-type chunks x y z)
      t)))

(defn type-static
  "Returns the path type of the cell x y z of level lv for a mob one
  cell tall."
  [lv x y z]
  (static-type (:chunks lv) (chunk/level-min-y lv) x y z))

(defn- bb-type [ctx ^long x ^long y ^long z]
  (let [{:keys [chunks mob]} ctx
        lo (:min-y ctx)
        t (static-type chunks lo x y z)
        [mx my mz] (:block-pos mob)]
    (cond
      (and (= :door-wood-closed t) (:open-doors? mob)
           (:pass-doors? mob)) :walkable-door
      (and (= :door-open t) (not (:pass-doors? mob))) :blocked
      (and (= :rail t)
           (not= :rail (static-type chunks lo mx my mz))
           (not= :rail (static-type chunks lo mx (dec (long my)) mz)))
      :unpassable-rail
      :else t)))

(defn- box-type [ctx x y z w h i]
  (let [w (long w) h (long h) i (long i) r (quot i w)]
    (bb-type ctx (+ (long x) (quot r h)) (+ (long y) (rem r h))
             (+ (long z) (rem i w)))))

(defn type-within-bb
  "Returns the set of path types the mob box at x y z covers."
  [ctx x y z]
  (let [mob (:mob ctx)
        w (long (:bb-w mob)) h (long (:bb-h mob))
        at (fn [s i] (conj! s (box-type ctx x y z w h i)))]
    (persistent! (reduce at (transient #{}) (range (* w w h))))))

(defn- highest-malus [mob types]
  (reduce (fn [[bt bm] t]
            (let [m (path-type-malus mob t)]
              (cond
                (neg? m) (reduced [t m true])
                (>= m (double bm)) [t m]
                :else [bt bm])))
          [:blocked (path-type-malus mob :blocked)]
          (sort-by type-order types)))

(defn- capped-type [ctx x y z t m]
  (let [mob (:mob ctx)
        cur (static-type (:chunks ctx) (:min-y ctx) x y z)]
    (if (> (long (:bb-w mob)) 1)
      (if (and (< (path-type-malus mob cur) m)
               (< (path-type-malus mob :big-mobs-close-to-danger) m))
        :big-mobs-close-to-danger
        t)
      (if (and (= :open cur) (not= :open t) (zero? m)) :open t))))

(defn- typed-for-mob [ctx x y z]
  (let [ts (type-within-bb ctx x y z)]
    (cond
      (= 1 (count ts)) (first ts)
      (ts :fence) :fence
      (ts :unpassable-rail) :unpassable-rail
      :else (let [[t m early?] (highest-malus (:mob ctx) ts)]
              (if early? t (capped-type ctx x y z t (double m)))))))

(defn type-of-mob
  "Returns the path type of the cell x y z for the mob of ctx.
  One search types each cell once, as WalkNodeEvaluator caches it."
  [ctx x y z]
  (if-let [types (:types ctx)]
    (let [x (long x) y (long y) z (long z)]
      (or (Path/cachedType types x y z)
          (Path/cacheType types x y z (typed-for-mob ctx x y z))))
    (typed-for-mob ctx x y z)))

(defn- node-at ^PathNode [ctx x y z]
  (Path/node (:nodes ctx) (long x) (long y) (long z)))

(defn- x-of ^long [^PathNode n] (.x n))

(defn- y-of ^long [^PathNode n] (.y n))

(defn- z-of ^long [^PathNode n] (.z n))

(defn- malus ^double [^PathNode n] (.malus n))

(defn- set-malus! [^PathNode n ^double v] (.setMalus n v))

(defn- closed? [^PathNode n] (.closed n))

(defn- close! [^PathNode n] (.close n))

(defn- came [^PathNode n] (.came n))

(defn- kind [^PathNode n] (.type n))

(defn- set-kind! [^PathNode n t] (.setType n t))

(defn- manhattan ^double [^PathNode n ^PathTarget t]
  (.manhattan n (.x t) (.y t) (.z t)))

(defn- shape-top ^double [chunks ^long x ^long y ^long z]
  (Path/shapeTop (block/collision-arr)
                 (chunk/block-state chunks x y z)))

(defn- floor-level ^double [ctx x y z]
  (let [x (long x) y (long y) z (long z)
        chunks (:chunks ctx)]
    (if (and (:float? (:mob ctx))
             (block/water? (chunk/block-state chunks x y z)))
      (+ y 0.5)
      (+ (dec y) (shape-top chunks x (dec y) z)))))

(defn- collides? [chunks ^doubles b]
  (Path/collides chunks (block/collision-arr) b))

(defn- can-reach? [ctx n]
  (let [mob (:mob ctx) [px py pz] (:pos mob)]
    (Path/canReach (:chunks ctx) (block/collision-arr) (double px)
                   (double py) (double pz) (double (:width mob))
                   (double (:height mob)) n)))

(defn- node-with-cost [ctx x y z t cost]
  (let [n (node-at ctx x y z)]
    (set-kind! n t)
    (set-malus! n (max (malus n) cost))
    n))

(defn- blocked-node [ctx x y z]
  (let [n (node-at ctx x y z)]
    (set-kind! n :blocked)
    (set-malus! n -1.0)
    n))

(defn- closed-node [ctx x y z t]
  (let [n (node-at ctx x y z)]
    (close! n)
    (set-kind! n t)
    (set-malus! n (double (get default-malus t)))
    n))

(defn- partial-collision? [t]
  (contains? #{:fence :door-wood-closed :door-iron-closed} t))

(defn- jump-height ^double [mob]
  (max 1.125 (double (:max-up-step mob))))

(defn- ground-below [ctx ^long x ^long y ^long z]
  (let [mob (:mob ctx) lo (long (:min-y ctx))]
    (loop [cy (dec y)]
      (cond
        (< cy lo) (blocked-node ctx x y z)
        (> (- y cy) (long (:max-fall mob))) (blocked-node ctx x cy z)
        :else
        (let [t (type-of-mob ctx x cy z)
              m (path-type-malus mob t)]
          (cond
            (= :open t) (recur (dec cy))
            (>= m 0.0) (node-with-cost ctx x cy z t m)
            :else (blocked-node ctx x cy z)))))))

(defn- non-water-below [ctx x y z best]
  (let [mob (:mob ctx) lo (long (:min-y ctx))]
    (loop [cy (dec y) best best]
      (if (<= cy lo)
        best
        (let [t (type-of-mob ctx x cy z)]
          (if (not= :water t)
            best
            (let [m (path-type-malus mob t)]
              (recur (dec cy) (node-with-cost ctx x cy z t m)))))))))

(def ^:private dir-by-2d [[0 1] [-1 0] [0 -1] [1 0]])

(def ^:private horizontal-order [2 3 0 1])

(def ^:private clockwise [1 2 3 0])

(declare accepted-node)

(defn- jump-box ^doubles [ctx x y z dir above]
  (let [mob (:mob ctx)
        [dx dz] (nth dir-by-2d dir)
        cx (- x (long dx)) cz (- z (long dz))
        hw (/ (double (:width mob)) 2.0)]
    (double-array
      [(- (+ cx 0.5) hw) (+ (floor-level ctx cx (inc y) cz) 0.001)
       (- (+ cz 0.5) hw) (+ (+ cx 0.5) hw)
       (- (+ (double (:height mob))
             (floor-level ctx (x-of above) (y-of above) (z-of above)))
          0.002)
       (+ (+ cz 0.5) hw)])))

(defn- try-jump-on [ctx x y z jump nh dir cur]
  (let [up (inc (long y))
        above (accepted-node ctx x up z (dec jump) nh dir cur)
        mob (:mob ctx)]
    (cond
      (nil? above) nil
      (>= (double (:width mob)) 1.0) above
      (not (contains? #{:open :walkable} (kind above))) above
      (collides? (:chunks ctx) (jump-box ctx x y z dir above)) nil
      :else above)))

(defn- best-at [ctx x y z cur]
  (let [mob (:mob ctx)
        t (type-of-mob ctx x y z)
        m (path-type-malus mob t)
        n (when (>= m 0.0) (node-with-cost ctx x y z t m))]
    (if (and (partial-collision? cur) n (>= (malus n) 0.0)
             (not (can-reach? ctx n)))
      [t nil]
      [t n])))

(defn- jumpable? [ctx t best ^long jump]
  (and (or (nil? best) (neg? (malus best))) (pos? jump)
       (or (not= :fence t) (:walk-over-fences? (:mob ctx)))
       (not= :unpassable-rail t) (not= :trapdoor t)
       (not= :powder-snow t)))

(defn- descend [ctx x y z jump nh dir cur t best]
  (cond
    (jumpable? ctx t best jump)
    (try-jump-on ctx x y z jump nh dir cur)
    (and (= :water t) (not (:float? (:mob ctx))))
    (non-water-below ctx x y z best)
    (= :open t) (ground-below ctx x y z)
    (and (partial-collision? t) (nil? best)) (closed-node ctx x y z t)
    :else best))

(defn- accepted-node [ctx x y z jump nh dir cur]
  (when (<= (- (floor-level ctx x y z) nh)
            (jump-height (:mob ctx)))
    (let [[t best] (best-at ctx x y z cur)]
      (if (= :walkable t)
        best
        (descend ctx x y z jump nh dir cur t best)))))

(defn- neighbor-valid? [n cur]
  (boolean (and n (not (closed? n))
                (or (>= (malus n) 0.0) (neg? (malus cur))))))

(defn- posts-gap? [ctx ew ns]
  (and (= :fence (kind ns)) (= :fence (kind ew))
       (< (double (:width (:mob ctx))) 0.5)))

(defn- corner-free? [ctx pos ew ns]
  (let [gap (posts-gap? ctx ew ns) y (y-of pos)]
    (and (or (< (y-of ns) y) (>= (malus ns) 0.0) gap)
         (or (< (y-of ew) y) (>= (malus ew) 0.0) gap))))

(defn- diagonal-ok? [ctx pos ew ns]
  (let [w (double (:width (:mob ctx)))]
    (cond
      (or (nil? ns) (nil? ew) (> (y-of ns) (y-of pos))
          (> (y-of ew) (y-of pos))) false
      (or (= :walkable-door (kind ew))
          (= :walkable-door (kind ns))) false
      (and (> w 1.0) (or (pos? (malus ew)) (pos? (malus ns)))) false
      :else (corner-free? ctx pos ew ns))))

(defn- diagonal-node-ok? [n]
  (boolean (and n (not (closed? n)) (not= :walkable-door (kind n))
                (>= (malus n) 0.0))))

(defn- jump-size ^long [ctx pos cur]
  (let [mob (:mob ctx)
        y (y-of pos)
        above (type-of-mob ctx (x-of pos) (inc y) (z-of pos))]
    (if (and (>= (path-type-malus mob above) 0.0)
             (not= :sticky-honey cur))
      (long (Math/floor (max 1.0 (double (:max-up-step mob)))))
      0)))

(defn- side-nodes [ctx pos js ph cur]
  (reduce (fn [a d]
            (let [[dx dz] (nth dir-by-2d d)
                  x (+ (x-of pos) (long dx))
                  z (+ (z-of pos) (long dz))
                  n (accepted-node ctx x (y-of pos) z js ph d cur)]
              (assoc a d n)))
          [nil nil nil nil] horizontal-order))

(defn- diagonal [ctx pos side js ph cur d]
  (let [cw (nth clockwise d)
        [dx dz] (nth dir-by-2d d)
        [cx cz] (nth dir-by-2d cw)]
    (when (diagonal-ok? ctx pos (nth side d) (nth side cw))
      (let [n (accepted-node
                ctx (+ (x-of pos) (long dx) (long cx)) (y-of pos)
                (+ (z-of pos) (long dz) (long cz)) js ph d cur)]
        (when (diagonal-node-ok? n) n)))))

(defn neighbors
  "Returns the nodes a mob standing on pos can step onto."
  [ctx pos]
  (let [cur (type-of-mob ctx (x-of pos) (y-of pos) (z-of pos))
        js (jump-size ctx pos cur)
        ph (floor-level ctx (x-of pos) (y-of pos) (z-of pos))
        side (side-nodes ctx pos js ph cur)]
    (into (filterv (fn [n] (neighbor-valid? n pos))
                   (mapv (fn [d] (nth side d)) horizontal-order))
          (keep (fn [d] (diagonal ctx pos side js ph cur d))
                horizontal-order))))

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

(defn- corners [mob]
  (let [[px _ pz] (:pos mob) w (/ (double (:width mob)) 2.0)
        x0 (long (Math/floor (- (double px) w)))
        x1 (long (Math/floor (+ (double px) w)))
        z0 (long (Math/floor (- (double pz) w)))
        z1 (long (Math/floor (+ (double pz) w)))]
    [[x0 z0] [x0 z1] [x1 z0] [x1 z1]]))

(defn start-node
  "Returns the node the mob of ctx stands on."
  [ctx]
  (let [mob (:mob ctx)
        [bx _ bz] (:block-pos mob)
        y (start-y ctx)
        ok? (fn [[x z]] (can-start-at? ctx x y z))
        [sx sz] (or (when-not (can-start-at? ctx bx y bz)
                      (first (filter ok? (corners mob))))
                    [bx bz])
        n (node-at ctx sx y sz)]
    (set-kind! n (type-of-mob ctx sx y sz))
    (set-malus! n (path-type-malus mob (kind n)))
    n))

(defn- targets-of ^"[Lcollider.world.space.PathTarget;" [goals]
  (into-array PathTarget
              (map (fn [[x y z]]
                     (PathTarget. (long x) (long y) (long z)))
                   goals)))

(defn- dist-to ^double [^PathNode a ^PathNode b]
  (.distTo a (.x b) (.y b) (.z b)))

(defn- heap-empty? [^PathHeap h] (.isEmpty h))

(defn- heap-pop! ^PathNode [^PathHeap h] (.pop h))

(defn- run-search [ctx heap from targets maxlen reach maxv]
  (loop [c 0]
    (if (or (heap-empty? heap) (>= (inc c) maxv))
      []
      (let [cur (heap-pop! heap)
            _ (close! cur)
            near? (fn [t] (<= (manhattan cur t) reach))
            hit (filterv near? targets)]
        (if (seq hit)
          hit
          (do (when (< (dist-to cur from) maxlen)
                (doseq [n (neighbors ctx cur)]
                  (Path/relax heap targets maxlen cur n)))
              (recur (inc c))))))))

(defn- node-map [n]
  {:x (x-of n) :y (y-of n) :z (z-of n) :type (kind n)})

(defn- reconstruct [^PathTarget t reached?]
  (let [ns (loop [n (.node t) acc ()]
             (if n (recur (came n) (conj acc n)) (vec acc)))]
    {:nodes          (mapv node-map ns)
     :target         [(.x t) (.y t) (.z t)]
     :reached?       reached?
     :dist-to-target (if (empty? ns)
                       (double Float/MAX_VALUE)
                       (manhattan (peek ns) t))}))

(defn- pick [targets reached?]
  (let [ps (mapv (fn [t] (reconstruct t reached?)) targets)
        len (fn [p] (count (:nodes p)))]
    (first (if reached?
             (sort-by len ps)
             (sort-by (juxt :dist-to-target len) ps)))))

(defn context
  "Returns what one search over level lv knows about its mob.
  The mob carries its size, its own malus and where it stands."
  [lv mob]
  (let [w (double (:width mob))
        [px py pz] (:pos mob)
        cell (fn [c] (long (Math/floor (double c))))]
    {:chunks (:chunks lv)
     :min-y  (chunk/level-min-y lv)
     :nodes  (HashMap.)
     :types  (HashMap.)
     :mob    (assoc mob
               :bb-w (long (Math/floor (+ w 1.0)))
               :bb-h (long (Math/floor (inc (double (:height mob)))))
               :block-pos [(cell px) (cell py) (cell pz)])}))

(defn- search [lv mob goals maxlen reach mult]
  (let [ctx (context lv mob)
        from (start-node ctx)
        targets (targets-of goals)
        maxv (long (int (* (float max-visited-nodes) (float mult))))
        heap (PathHeap.)]
    (Path/start heap targets from)
    (let [hit (run-search ctx heap from targets maxlen reach maxv)]
      (if (seq hit) (pick hit true) (pick targets false)))))

(defn find-path
  "Returns the path of a mob over level lv to the closest of the goal
  cells. The path holds the nodes walked, whether a goal was reached
  and how far its last node stays from the goal."
  [lv mob goals max-path-length reach-range multiplier]
  (search lv mob goals (double max-path-length)
          (long reach-range) (double multiplier)))
