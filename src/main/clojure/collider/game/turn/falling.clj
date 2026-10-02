(ns collider.game.turn.falling
  "The turn of a falling block such as sand, gravel or an anvil."
  (:require [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.world.blocks.fall :as fall]
            [collider.game.entity.size :as size]
            [collider.game.out :as out]
            [collider.game.areas :as areas]
            [collider.game.turn.landing :as landing]
            [collider.game.turn.overlay :as overlay]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys]
            [collider.world.blocks.support :as support])
  (:import (collider.world Move)))

(set! *warn-on-reflection* true)

(defn- half ^double [] (size/half :falling-block))

(defn- height ^double [] (size/height :falling-block))

(def ^:private ^:const max-time 600)

(def ^:private ^:const air-drag (double (float 0.98)))

(defn- block-at ^long [world [_ y _ :as pos]]
  (if (chunk/in-range? y)
    (chunk/chunks-get-block (:chunks world) pos)
    0))

(defn- cell-of [pos]
  [(long (Math/floor (v/x pos)))
   (long (Math/floor (v/y pos)))
   (long (Math/floor (v/z pos)))])

(defn- item-deltas [world eid e]
  (when (get-in world [:rules :entity-drops] true)
    (let [vel (entity/pop-velocity [(:tick world) eid])
          stack {:item (block/block-of (:block e)) :count 1}]
      [[:spawn-entity (entity/item (:pos e) vel stack)]])))

(defn- speleothem? [^long st]
  (= :pointed-dripstone (block/type-of st)))

(defn- anvil? [^long st] (= :anvil (block/type-of st)))

(defn- land-event [sound cell]
  (out/all (out/level-event sound cell 0)))

(defn- landed-state [world e cell cur concrete? stuck?]
  (let [st (:block e)
        free? (fall/free-below? (:chunks world) cell)
        continues? (and free? (not (and concrete? stuck?)))]
    (when (and (block/can-be-replaced? cur) (not continues?)
               (support/supported? (:chunks world) cell st))
      (let [in-water? (block/water? cur)
            st (if in-water? (block/with-water st) st)]
        (if (and concrete? in-water?) (block/concrete-of st) st)))))

(defn- broken-deltas [world eid e cell]
  (concat [[:remove-entity eid]]
          (when (speleothem? (:block e))
            [(land-event out/sound-pointed-dripstone-land cell)])
          (when (anvil? (:block e))
            [(land-event out/sound-anvil-broken cell)])
          (item-deltas world eid e)))

(defn- land-deltas [world eid e cell cur concrete? stuck?]
  (if-let [st (landed-state world e cell cur concrete? stuck?)]
    (cond-> (into [[:remove-entity eid]]
                  (delta/authored
                    (changes/set-deltas world [[cell st]])
                    (delta/entity-author eid e)))
            (anvil? st)
            (conj (land-event out/sound-anvil-land cell)))
    (broken-deltas world eid e cell)))

(defn- solid-here? [world cell]
  (let [st (block-at world cell)]
    (and (pos? st) (not (block/liquid? st))
         (block/blocks-motion? st))))

(defn- water-source-here? [world cell]
  (let [st (block-at world cell)]
    (or (block/waterlogged? st)
        (block/water-source? st))))

(defn- stops-here? [world cell]
  (or (solid-here? world cell) (water-source-here? world cell)))

(defn- axis-time [from d cell ^long i]
  (let [di (double (nth d i))
        ci (long (nth cell i))]
    (when-not (zero? di)
      (let [bound (if (pos? di) (inc ci) ci)]
        [(/ (- (double bound) (double (nth from i))) di) i]))))

(defn- soonest-axis [from d cell]
  (->> (range 3)
       (keep (fn [i] (axis-time from d cell i)))
       (remove (fn [[t _]] (< (double t) 0.0)))
       (sort-by first)
       first
       second))

(defn- next-cell [from d cell]
  (if-let [axis (soonest-axis from d cell)]
    (let [step (if (pos? (double (nth d axis))) 1 -1)]
      (update cell axis (fn [c] (+ (long c) step))))
    cell))

(defn- clip-cell [world from to]
  (let [d (mapv - to from) end (cell-of to)]
    (loop [cell (cell-of from) n 0]
      (cond
        (stops-here? world cell) cell
        (or (= cell end) (> n 64)) nil
        :else (recur (next-cell from d cell) (inc n))))))

(defn- clipped-cell [world e pos [mx my mz]]
  (let [mx (double mx) my (double my) mz (double mz)]
    (when (> (+ (* mx mx) (* my my) (* mz mz)) 1.0)
      (when-let [hit (clip-cell world (:pos e) pos)]
        (when (block/water? (block-at world hit)) hit)))))

(defn- landing [world e pos vel]
  (let [concrete? (= :concrete-powder (block/type-of (:block e)))
        clipped (when concrete? (clipped-cell world e pos vel))
        cell (or clipped (cell-of pos))
        cur (block-at world cell)]
    [cell cur concrete?
     (and concrete? (or (some? clipped) (block/water? cur)))]))

(defn- fall-move ^Move [world e]
  (let [[vx vy vz] (:vel e)
        d [(double vx) (- (double vy) 0.04) (double vz)]]
    (phys/move (:chunks world) (:pos e)
               (if (:stuck e) (mapv * d (:stuck e)) d) (half) (height)
               0.0 (phys/context e))))

(defn- drift-vel [stuck [mx my mz]]
  (if stuck
    [0.0 0.0 0.0]
    [(* (double mx) air-drag) (* (double my) air-drag)
     (* (double mz) air-drag)]))

(defn- stuck-now [world pos]
  (motion/stuck-speed (:chunks world) pos (half) (height)))

(defn- drift-deltas [eid e pos time vel stuck' fall]
  (let [stuck (:stuck e)
        m {:pos pos :on-ground false :time time
           :vel (drift-vel stuck vel)
           :fall (landing/kept e (if stuck' 0.0 fall))}
        m (cond-> m (or stuck' stuck) (assoc :stuck stuck'))]
    [[:merge-entity eid m]]))

(defn- drift [world eid e pos time vel fall]
  (drift-deltas eid e pos time vel (stuck-now world pos) fall))

(defn- expired? [world ^long time cell]
  (let [y (long (cell 1))]
    (or (> time max-time)
        (and (> time 100)
             (or (<= y (chunk/level-min-y world))
                 (> y (chunk/level-max-y world)))))))

(def ^:private sources
  {:anvil :falling-anvil :pointed-dripstone :falling-stalactite
   :sulfur-spike :falling-stalactite})

(defn- source [eid e]
  {:type (get sources (block/type-of (:block e)) :falling-block)
   :cause eid :direct eid :from (:pos e)})

(defn- hurt-amount
  "Returns the damage of FallingBlockEntity.causeFallDamage:258 of e
  after fall d, nil when it hurts nothing."
  [e ^double d]
  (when-let [per (:hurt e)]
    (let [n (long (Math/ceil (- d 1.0)))]
      (when-not (neg? n)
        [n (double (min (long (Math/floor (float (* (float n)
                                                    (float per)))))
                        (long (:hurt-max e))))]))))

(defn- box-of [e]
  (let [[half h] (entity/box e) p (:pos e) half (double half)]
    [(- (v/x p) half) (v/y p) (- (v/z p) half)
     (+ (v/x p) half) (+ (v/y p) (double h)) (+ (v/z p) half)]))

(defn- meet? [a b]
  (every? (fn [i]
            (and (< (double (nth a i)) (double (nth b (+ i 3))))
                 (> (double (nth a (+ i 3))) (double (nth b i)))))
          [0 1 2]))

(defn- victim?
  "Returns true when o is a living body alive in box, as
  EntitySelector.LIVING_ENTITY_STILL_ALIVE picks it."
  [box [_ o]]
  (and (or (= :player (:type o)) (mobs/mob-type? (:type o)))
       (pos? (double (:health o 0.0)))
       (meet? box (box-of o))))

(defn- hurt-deltas [world eid e ^double n]
  (let [src (source eid e)
        box (box-of e)
        hit (fn [[oid o]]
              (when-let [ds (hurt/damage-deltas world oid o n src)]
                (let [h (hurt/hurt-now world oid o ds)]
                  (into ds (hurt/report-deltas world oid h)))))]
    (into [] (comp (filter #(victim? box %)) (mapcat hit))
          (:entities world))))

(def ^:private ^:const chip-key 0x616e76)

(def ^:private chips
  {:anvil :chipped-anvil :chipped-anvil :damaged-anvil})

(defn- chip-chance
  "Returns 0.05F + i * 0.05F in float."
  ^double [^long i]
  (let [step (float 0.05)]
    (double (float (+ step (float (* (float i) step)))))))

(defn- chipped
  "Returns falling block e after its anvil took damage n from a fall
  of i blocks, as FallingBlockEntity.causeFallDamage:269 chips it;
  nil when it breaks."
  [world eid e i n]
  (let [st (:block e)
        r (random/of-longs (:tick world) eid chip-key)]
    (if (and (anvil? st) (pos? (double n)) (< r (chip-chance i)))
      (when-let [k (chips (block/block-of st))]
        (let [facing (select-keys (block/props-of st) [:facing])]
          (assoc e :block (block/state k facing))))
      e)))

(defn- honey-deltas [eid e]
  [(out/all (out/sound :block.honey-block.slide (:pos e) 1.0 1.0
                       :neutral))
   (out/all (out/status eid :honey-jump))])

(defn- fell-on
  "Returns falling block e that lands after fall f and the deltas of
  Block.fallOn:492 of the block under it, which hurt the living
  bodies in its box (FallingBlockEntity.causeFallDamage:253)."
  [world eid e ^double f]
  (if-not (pos? f)
    [e []]
    (let [ch (:chunks world) pos (:pos e)
          sup (phys/supporting-block ch pos (half) (phys/context e))
          st (chunk/at ch (fall/on-pos ch pos sup))
          hd (when (= :honey (block/type-of st)) (honey-deltas eid e))
          [i n] (when-let [l (fall/landing st f)]
                  (hurt-amount e (double (nth l 0))))]
      (if i
        [(chipped world eid e i n)
         (into (vec hd) (hurt-deltas world eid e n))]
        [e (vec hd)]))))

(defn- landed-deltas
  "Returns the deltas of falling block e that landed, or broke when
  its fall broke it (FallingBlockEntity.tick:183, cancelDrop)."
  [world eid e cell cur concrete? stuck?]
  (if e
    (land-deltas world eid e cell cur concrete? stuck?)
    [[:remove-entity eid]
     (land-event out/sound-anvil-broken cell)]))

(defn- fallen ^double [world e ^Move mv]
  (let [f (double (or (:fall e) 0.0))]
    (phys/fallen (landing/cleared world (:pos e) (phys/pos mv) f)
                 (phys/moved-y mv))))

(defn- step-deltas [world eid e]
  (let [^Move mv (fall-move world e)
        pos (phys/pos mv)
        time (inc (long (:time e)))
        vel (phys/vel mv)
        f (fallen world e mv)
        [cell cur concrete? stuck?] (landing world e pos vel)]
    (cond
      (phys/on-ground? mv)
      (let [[e hs] (fell-on world eid (assoc e :pos pos) f)]
        (into hs (landed-deltas world eid e cell cur concrete?
                                stuck?)))
      stuck?
      (land-deltas world eid (assoc e :pos pos)
                   cell cur concrete? stuck?)
      (expired? world time cell)
      (cons [:remove-entity eid]
            (item-deltas world eid (assoc e :pos pos)))
      :else (drift world eid e pos time vel f))))

(defn- turn [[world out] [eid e]]
  (let [ds (step-deltas (overlay/seen world eid) eid e)]
    [(overlay/wrote world eid ds) (into out ds)]))

(defn turns
  "Returns world with the blocks that falling blocks set in their
  turns, and the deltas of every falling block in an active chunk.
  Each lands with setBlock as FallingBlockEntity.tick:196, so the
  turns after it see the block."
  [world]
  (reduce turn [world []]
          (areas/active-of-types world [:falling-block])))
