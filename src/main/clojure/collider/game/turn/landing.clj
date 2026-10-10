(ns collider.game.turn.landing
  "Falls of living entities and what the block they land on does
  with each fall."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.climb :as climb]
            [collider.world.blocks.fall :as fall]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- fall-of ^double [e] (double (or (:fall e) 0.0)))

(defn kept
  "Returns fall distance f as entity e should hold it. It is nil for
  none, and the value e already holds when equal, so a body that
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
  out of water in fluid f. Lava halves it, and slow falling and
  levitation clear it."
  ^double [e f]
  (cond (held? e) 0.0
        (pos? (double (:lava f))) (* 0.5 (fall-of e))
        :else (fall-of e)))

(defn cleared
  "Returns fall f of a body that moved from pos to pos'. It is 0.0
  when a move of a block or more passed water or a block that resets
  a fall."
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
  "Returns the block particles of e landing at pos on block st at
  cell after fall f."
  [e pos st cell f]
  (let [p (double (max 0 (long (Math/floor (power e (double f))))))]
    (when (and (pos? p) (not (block/air? st)))
      (let [scale (min (+ least-scale (/ p 15.0)) 2.5)
            n (long (* 150.0 scale))
            at (particle-at pos cell)]
        [(out/all (out/particles :block st at n 0.15))]))))

(def ^:private ^:table immune
  (delay (set (data/tag-values "entity_type" "fall_damage_immune"))))

(defn- reduction ^long [e]
  (long (get-in mobs/types [(:type e) :fall-reduction] 0)))

(defn- damage-of ^long [e ^double d ^double m]
  (if (contains? @immune (:type e))
    0
    (let [a :fall-damage-multiplier
          k (when (a (attribute/base-values e))
              (attribute/value e (:effects e) a))
          scaled (* (power e d) (double (float m)))]
      (- (long (Math/floor (* scaled (double (or k 1.0)))))
         (reduction e)))))

(defn- player? [e] (= :player (:type e)))

(defn- sound [e kind ^double vol ^double pitch]
  (if (player? e)
    (out/except (:eid e) (out/sound kind (:pos e) vol pitch :players))
    (out/all (out/sound kind (:pos e) vol pitch :neutral))))

(defn- fall-sound [e big?]
  (sound e (cond (player? e) (if big? :entity.player.big-fall
                                 :entity.player.small-fall)
                 big? :entity.generic.big-fall
                 :else :entity.generic.small-fall)
         1.0 1.0))

(defn- block-sound [e ^long st]
  (let [s (get (data/sounds) (:sound (data/info (block/block-of st))))
        f (fn [k x] (double (float (* (float (get s k)) (float x)))))]
    (sound e (:fall s) (f :volume 0.5) (f :pitch 0.75))))

(def ^:private srcs
  {:fall {:type :fall} :stalagmite {:type :stalagmite}})

(def ^:private ^:const sound-offset (double (float 0.2)))

(defn- floor-of ^long [^double a] (long (Math/floor a)))

(defn- block-fall-sound
  "Returns the fall sound of the block under e, or nil over air."
  [world e]
  (let [p (:pos e)
        y (floor-of (- (v/y p) sound-offset))
        st (chunk/at (:chunks world)
                     [(floor-of (v/x p)) y (floor-of (v/z p))])]
    (when-not (block/air? st) [(block-sound e st)])))

(defn- fall-stat [eid e ^double d]
  (when (and (player? e) (>= d 2.0))
    [[:award eid :custom/fall-one-cm (Math/round (* d 100.0))]]))

(def ^:private forgotten {:impulse-at nil :impulse-grace 0})

(defn- after-impulse
  "Returns the fall of e that counts after a gust caught it, and
  whether the gust is forgotten before the damage."
  [e ^double d]
  (if-let [at (:impulse-at e)]
    (let [f (min d (- (double (v/y at)) (double (v/y (:pos e)))))]
      [f (or (<= f 0.0) (zero? (long (:impulse-grace e 0))))])
    [d false]))

(defn- hurt-deltas
  "Returns the deltas of the fall damage of e for a fall d with
  modifier m from source type k. A body that may fly takes none."
  [world eid e [d m k]]
  (when-not (game-mode/may-fly? e)
    (let [[f gone?] (after-impulse e (double d))
          n (damage-of e f (double m))]
      (concat
        (fall-stat eid e (double d))
        (when (or gone? (and (pos? n) (:impulse-at e)))
          [[:merge-entity eid forgotten]])
        (when (pos? n)
          (-> [(fall-sound e (> n 4))]
              (into (block-fall-sound world e))
              (into (hurt/damage-deltas world eid e (double n)
                                        (srcs k)))))))))

(defn grace-deltas
  "Returns the deltas of the grace after a gust that player eid, e,
  loses in its turn."
  [_ [eid e]]
  (let [g (long (:impulse-grace e 0))]
    (when (pos? g) [[:merge-entity eid {:impulse-grace (dec g)}]])))

(defn settled-deltas
  "Returns the deltas that forget the gust of player eid, e, whose
  move left it on the ground, in a liquid or on a ladder after its
  grace ran out."
  [world eid e]
  (when (and (:impulse-at e) (zero? (long (:impulse-grace e 0)))
             (or (:on-ground e) (:in-water? e) (:in-lava? e)
                 (game-mode/spectator? e) (:fall-flying e)
                 (climb/on-climbable? (:chunks world) (:pos e))))
    [[:merge-entity eid forgotten]]))

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
         (or (player? e) (get-in world [:rules :mob-griefing] true))
         (> size (float 0.512)))))

(defn- top-of
  "Returns how high the collision shape of block st reaches."
  ^double [^long st]
  (let [tops (map #(nth % 4) (block/collision-boxes st))]
    (/ (double (reduce max 0 tops)) 16.0)))

(defn- lifted-y
  "Returns the height a body at y rises to out of the block at cy that
  became a whole one."
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

(defn- lifted-delta [eid cell y0 [oid o]]
  (when (and (not= oid eid) (not= :player (:type o))
             (in-top? o cell y0))
    [:merge-entity oid {:pos (lift o cell)}]))

(defn- lifted-deltas [world eid cell y0]
  (into [] (keep #(lifted-delta eid cell y0 %)) (:entities world)))

(defn- trampled
  "Returns e and the deltas of farmland st at cell that e tramples
  to dirt. The bodies in the top of the cell rise out of it."
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
  "Returns e and the deltas of block st at cell that living entity e
  falls on after fall f."
  [world eid e st cell f]
  (let [f (double f)
        [e ts] (if (= :farmland (block/type-of st))
                 (trampled world eid e st cell f)
                 [e nil])
        l (if (and (:sneaking? e) (= :slime (block/type-of st)))
            [f 1.0 :fall]
            (fall/landing st f))
        hs (when l (hurt-deltas world eid e l))]
    [e (case (block/type-of st)
         :honey (honey-deltas eid e st hs)
         :powder-snow (snow-deltas e f)
         (into (vec ts) hs))]))

(defn landed
  "Returns the position and the deltas of living entity e that lands
  at pos on block sup, after fall f0 before its move and f after
  it."
  [world eid e pos sup f0 f]
  (let [f0 (double f0) f (double f)
        e (assoc e :pos pos :eid eid)
        cell (fall/on-pos (:chunks world) pos sup)
        st (chunk/at (:chunks world) cell)
        ps (when (pos? f0) (particles e pos st cell f0))]
    (if (pos? f)
      (let [[e ds] (fell-on world eid e st cell f)]
        [(:pos e) (into (vec ps) ds)])
      [pos ps])))
