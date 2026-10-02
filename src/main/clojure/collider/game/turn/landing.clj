(ns collider.game.turn.landing
  "The fall of a living entity: how far it falls in its move and
  what the block it lands on does with the fall."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.fall :as fall]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- fall-of ^double [e] (double (or (:fall e) 0.0)))

(defn kept
  "Returns fall distance f as entity e should hold it: nil for none,
  and the value e holds when it is the same, so that a body that
  does not fall keeps its entity."
  [e ^double f]
  (let [old (:fall e)]
    (cond (zero? f) nil
          (== f (double (or old 0.0))) old
          :else f)))

(defn- held? [e]
  (let [fx (:effects e)]
    (or (contains? fx :slow-falling) (contains? fx :levitation))))

(defn before-move
  "Returns the fall distance of living entity e when its move starts
  out of water in fluid f: lava halves it (Entity.baseTick:560),
  slow falling and levitation clear it (LivingEntity.travel:3139)."
  ^double [e f]
  (cond (held? e) 0.0
        (pos? (double (:lava f))) (* 0.5 (fall-of e))
        :else (fall-of e)))

(defn cleared
  "Returns fall f of a body that moved from pos to pos', or 0.0 when
  the move of a block or more passed a block that resets a fall or
  water (Entity.move:752)."
  ^double [world pos pos' ^double f]
  (if (and (not (zero? f)) (>= (v/dist-sq pos pos') 1.0)
           (fall/resets? (:chunks world) pos pos'))
    0.0
    f))

(defn- power ^double [e ^double f]
  (- (+ f 1.0E-6)
     (attribute/value e (:effects e) :safe-fall-distance)))

(defn- particle-at [pos [cx _ cz]]
  (let [x (v/x pos) z (v/z pos) bx (Math/floor x) bz (Math/floor z)
        cx (double cx) cz (double cz)]
    (if (and (== bx cx) (== bz cz))
      [x (v/y pos) z]
      (let [dx (- x cx 0.5) dz (- z cz 0.5)
            m (max (Math/abs dx) (Math/abs dz))]
        [(+ cx 0.5 (* (/ dx m) 0.5)) (v/y pos)
         (+ cz 0.5 (* (/ dz m) 0.5))]))))

(def ^:private ^:const least-scale (double (float 0.2)))

(defn- particles
  "Returns the block particles of LivingEntity.checkFallDamage:379
  for e landing at pos on block st at cell after fall f."
  [e pos st cell f]
  (let [p (double (max 0 (long (Math/floor (power e (double f))))))]
    (when (and (pos? p) (not (block/air? st)))
      (let [scale (min (+ least-scale (/ p 15.0)) 2.5)]
        [(out/all (out/particles :block st (particle-at pos cell)
                                 (long (* 150.0 scale)) 0.15))]))))

(def ^:private ^:table immune
  (delay (set (data/tag-values "entity_type" "fall_damage_immune"))))

(defn- damage-of
  "Returns the fall damage of LivingEntity.calculateFallDamage:1858."
  ^long [e ^double d ^double m]
  (if (contains? @immune (:type e))
    0
    (let [k (get (attribute/base-values e) :fall-damage-multiplier)]
      (long (Math/floor (* (power e d) (double (float m))
                           (double (or k 1.0))))))))

(defn- sound [e kind ^double vol ^double pitch]
  (out/all (out/sound kind (:pos e) vol pitch :neutral)))

(defn- fall-sound [e big?]
  (sound e (if big? :entity.generic.big-fall
               :entity.generic.small-fall) 1.0 1.0))

(defn- block-sound [e ^long st]
  (let [s (get (data/sounds) (:sound (data/info (block/block-of st))))
        f (fn [k x] (double (float (* (float (get s k)) (float x)))))]
    (sound e (:fall s) (f :volume 0.5) (f :pitch 0.75))))

(def ^:private srcs
  {:fall {:type :fall} :stalagmite {:type :stalagmite}})

(def ^:private ^:const sound-offset (double (float 0.2)))

(defn- floor-of ^long [^double a] (long (Math/floor a)))

(defn- block-fall-sound
  "Returns the sound of LivingEntity.playBlockFallSound:1871."
  [world e]
  (let [p (:pos e)
        y (floor-of (- (v/y p) sound-offset))
        st (chunk/at (:chunks world)
                     [(floor-of (v/x p)) y (floor-of (v/z p))])]
    (when-not (block/air? st) [(block-sound e st)])))

(defn- hurt-deltas
  "Returns the deltas of LivingEntity.causeFallDamage:1799 of e for
  a fall d with modifier m from source type k."
  [world eid e [d m k]]
  (let [n (damage-of e (double d) (double m))]
    (when (pos? n)
      (-> [(fall-sound e (> n 4))]
          (into (block-fall-sound world e))
          (conj [:damage eid (double n) (srcs k)])))))

(defn- honey-deltas [eid e ^long st hs]
  (cond-> (into [(sound e :block.honey-block.slide 1.0 1.0)
                 (out/all (out/status eid :honey-jump))]
                hs)
    (seq hs) (conj (block-sound e st))))

(defn- snow-deltas [e ^double f]
  (when (>= f 4.0) [(fall-sound e (>= f 7.0))]))

(def ^:private ^:const trample-key 0x74726d)

(defn- tramples? [world eid e ^double f]
  (let [[half h] (entity/box e)
        w (float (* 2.0 (double half)))
        size (float (* (float (* w w)) (float h)))]
    (and (< (random/of-longs (:tick world) eid trample-key) (- f 0.5))
         (get-in world [:rules :mob-griefing] true)
         (> size (float 0.512)))))

(defn- top-of
  "Returns how high the collision shape of block st reaches."
  ^double [^long st]
  (let [tops (map #(nth % 4) (block/collision-boxes st))]
    (/ (double (reduce max 0 tops)) 16.0)))

(defn- lifted-y
  "Returns the height a body at y rises to out of the block at cy
  that became a whole one, as Block.pushEntitiesUp moves it."
  ^double [^double y ^long cy]
  (let [off (max -1.0 (- (+ (double cy) 1.0) (+ y 1.0)))]
    (+ y (+ 1.0 off))))

(defn- in-top?
  "Returns true when the box of o meets the cell from height y0 of
  the cell up."
  [o [cx cy cz] ^double y0]
  (let [[half h] (entity/box o) p (:pos o) half (double half)
        x (double cx) z (double cz) y0 (+ (double cy) y0)]
    (and (< (- (v/x p) half) (inc x)) (> (+ (v/x p) half) x)
         (< (v/y p) (inc (double cy))) (> (+ (v/y p) (double h)) y0)
         (< (- (v/z p) half) (inc z)) (> (+ (v/z p) half) z))))

(defn- lift [o cell]
  (let [p (:pos o)]
    (v/v3 (v/x p) (lifted-y (v/y p) (long (nth cell 1))) (v/z p))))

(defn- lifted-deltas [world eid cell y0]
  (into [] (keep (fn [[oid o]]
                   (when (and (not= oid eid) (not= :player (:type o))
                              (in-top? o cell y0))
                     [:merge-entity oid {:pos (lift o cell)}])))
        (:entities world)))

(defn- trampled
  "Returns e and the deltas of FarmlandBlock.fallOn:109 when e
  tramples farmland st at cell to dirt: the bodies in the top of
  the cell rise out of it (FarmlandBlock.turnToDirt:122)."
  [world eid e st cell f]
  (if (tramples? world eid e f)
    (let [y0 (top-of st)]
      [(cond-> e (in-top? e cell y0) (assoc :pos (lift e cell)))
       (into (delta/authored
               (changes/set-deltas world [[cell (block/state :dirt)]])
               (delta/entity-author eid e))
             (lifted-deltas world eid cell y0))])
    [e nil]))

(defn- fell-on
  "Returns e and the deltas of Block.fallOn:492 of block st at cell
  for living entity e after fall f."
  [world eid e st cell f]
  (let [f (double f)
        [e ts] (if (= :farmland (block/type-of st))
                 (trampled world eid e st cell f)
                 [e nil])
        hs (when-let [l (fall/landing st f)]
             (hurt-deltas world eid e l))]
    [e (case (block/type-of st)
         :honey (honey-deltas eid e st hs)
         :powder-snow (snow-deltas e f)
         (into (vec ts) hs))]))

(defn landed
  "Returns the position and the deltas of living entity e that lands
  at pos on block sup, or on nothing, with fall f0 before its move
  and f after it: the particles of LivingEntity.checkFallDamage:379,
  then what the block under it does with the fall
  (Entity.checkFallDamage:1589)."
  [world eid e pos sup f0 f]
  (let [f0 (double f0) f (double f)
        e (assoc e :pos pos)
        cell (fall/on-pos (:chunks world) pos sup)
        st (chunk/at (:chunks world) cell)
        ps (when (pos? f0) (particles e pos st cell f0))]
    (if (pos? f)
      (let [[e ds] (fell-on world eid e st cell f)]
        [(:pos e) (into (vec ps) ds)])
      [pos ps])))
