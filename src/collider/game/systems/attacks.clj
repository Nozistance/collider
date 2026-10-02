(ns collider.game.systems.attacks
  "Players hitting entities, as ServerGamePacketListenerImpl
  .handleAttack runs Player.attack when the packet comes."
  (:require [collider.data :as data]
            [collider.game.apply :as apply]
            [collider.game.attribute :as attribute]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.systems.damage :as damage]
            [collider.vec :as v]
            [collider.world.blocks.climb :as climb]))

(set! *warn-on-reflection* true)

(defn- f32 ^double [x] (double (float x)))

(def ^:private ^:const weak-floor (double (float 0.2)))

(def ^:private ^:const strong-share (double (float 0.8)))

(def ^:private ^:const full-strength (double (float 0.9)))

(def ^:private ^:const crit-multiplier 1.5)

(def ^:private ^:const sprint-knockback 0.5)

(def ^:private ^:const sweep-knockback (double (float 0.4)))

(def ^:private ^:const range-buffer 3.0)

(def ^:private ^:const degree (double (float (/ Math/PI 180.0))))

(defn- held [a] (:item (player/hand-stack a :main)))

(defn- attr
  "Returns attribute k of player a with the main hand modifier of
  the item it holds."
  ^double [a k]
  (let [m (get-in (data/items) [(held a) k])]
    (attribute/value a (:effects a) k (when m [[:item m 0]]))))

(defn- strength
  "Returns Player.getAttackStrengthScale(0.5F):1835 at tick t. The
  ticker counts from the tick in :strength-from."
  ^double [a ^long t]
  (let [n (if-let [from (:strength-from a)]
            (- t (long from))
            Integer/MAX_VALUE)
        delay (f32 (* (/ 1.0 (attr a :attack-speed)) 20.0))
        s (f32 (/ (f32 (+ (f32 n) 0.5)) delay))]
    (max 0.0 (min 1.0 s))))

(defn- scaled
  "Returns damage base scaled by strength s, as
  Player.baseDamageScaleFactor:1206."
  ^double [^double base ^double s]
  (f32 (* base (f32 (+ weak-floor (f32 (* (f32 (* s s))
                                          strong-share)))))))

(defn- crit?
  "Player.canCriticalAttack:1013, of player a at target t."
  [world a t]
  (and (pos? (double (or (:fall a) 0.0))) (not (:on-ground a))
       (not (climb/on-climbable? (:chunks world) (:pos a)))
       (not (:in-water? a)) (not (:blindness (:effects a)))
       (some? (:health t)) (not (:sprinting? a))))

(defn- sweep?
  "Player.isSweepAttack:1037 of a full hit that is neither a crit
  nor a sprint knockback."
  [a]
  (let [c (or (:client-vel a) [0.0 0.0 0.0])
        m (* (f32 (attribute/value a (:effects a) :movement-speed))
             2.5)]
    (and (:on-ground a) (player/sword? (held a))
         (< (+ (* (v/x c) (v/x c)) (* (v/z c) (v/z c))) (* m m)))))

(defn- blow
  "Returns what Player.attack:945 decides for player a at target t
  at tick t0."
  [world a t ^long t0]
  (let [s (strength a t0)
        full? (> s full-strength)
        knock? (and full? (boolean (:sprinting? a)))
        crit? (and full? (crit? world a t))
        d (scaled (f32 (attr a :attack-damage)) s)]
    {:s s :full? full? :knock? knock? :crit? crit?
     :sweep? (and full? (not crit?) (not knock?) (sweep? a))
     :damage (if crit? (f32 (* d crit-multiplier)) d)}))

(defn- source [eid a]
  {:type :player-attack :cause eid :direct eid :from (:pos a)
   :player? true})

(defn- sound [a k]
  (out/all (out/sound k (:pos a) 1.0 1.0 :players)))

(defn- facing
  "Returns the sine and the negated cosine of the yaw of player a,
  as Player.causeExtraKnockback:1123 turns it."
  [a]
  (let [r (f32 (* (f32 (:yaw a)) degree))]
    [(v/sin r) (- (v/cos r))]))

(defn- hurt-of
  "Returns target eid of world after damage d from src."
  [world eid e d src]
  (if-let [ds (damage/damage-deltas world eid e d src)]
    (damage/hurt-now world eid e ds)
    e))

(defn- extra-knock
  "Returns the deltas of Player.causeExtraKnockback:1116 on living
  target tid when the blow knocks."
  [eid a tid knock?]
  (let [kb (f32 (/ (f32 (get (attribute/base-values a)
                              :attack-knockback 0.0))
                   2.0))
        k (f32 (+ kb (if knock? sprint-knockback 0.0)))
        [sx cz] (facing a)]
    (when (pos? k)
      [[:knockback tid k sx cz]
       [:merge-entity eid {:sprinting? false}]])))

(defn- swept? [a [lo hi] [oid o] eid tid]
  (and (not= oid eid) (not= oid tid) (some? (:health o))
       (or (mobs/mob-type? (:type o)) (= :player (:type o)))
       (let [[h ht] (entity/box o) p (:pos o)
             h (double h)]
         (and (< (- (v/x p) h) (v/x hi)) (> (+ (v/x p) h) (v/x lo))
              (< (v/y p) (v/y hi))
              (> (+ (v/y p) (double ht)) (v/y lo))
              (< (- (v/z p) h) (v/z hi)) (> (+ (v/z p) h) (v/z lo))
              (< (v/dist-sq (:pos a) p) 9.0)))))

(defn- sweep-box
  "Returns the box of target t grown by 1, 0.25 and 1."
  [t]
  (let [[h ht] (entity/box t) p (:pos t)
        g (+ (double h) 1.0)]
    [[(- (v/x p) g) (- (v/y p) 0.25) (- (v/z p) g)]
     [(+ (v/x p) g) (+ (v/y p) (double ht) 0.25) (+ (v/z p) g)]]))

(defn- swept-deltas [world src s [sx cz] [oid o]]
  (let [d [:damage oid s src]
        h (hurt-of world oid o s src)]
    (if (entity/taken? o h)
      (into [d [:knockback oid sweep-knockback sx cz]]
            (damage/report-deltas world oid h))
      (when-not (identical? o h) [d]))))

(defn- sweep-particle [a]
  (let [[sx cz] (facing a) p (:pos a)
        [_ ht] (entity/box a)
        at [(- (v/x p) sx) (+ (v/y p) (* (double ht) 0.5))
            (+ (v/z p) (- cz))]]
    (out/all (out/particles :sweep-attack nil at 0 0.0
                            [(- sx) 0.0 (- cz)]))))

(defn- sweep-deltas
  "Returns the deltas of Player.doSweepAttack:1201 around target t."
  [world eid a tid t src s]
  (let [box (sweep-box t) dir (facing a)
        near (filter #(swept? a box % eid tid) (:entities world))]
    (concat [(sound a :entity.player.attack.sweep)]
            (mapcat #(swept-deltas world src (f32 s) dir %) near)
            [(sweep-particle a)])))

(defn- visual-deltas
  "Player.attackVisualEffects:1049: the crit to the attacker and
  those who see it, or the sound of a plain blow."
  [eid a tid {:keys [crit? sweep? full?]}]
  (let [crit (out/animation tid :crit)]
    (cond crit? [(sound a :entity.player.attack.crit)
                 (out/all (assoc crit :via eid)) (out/to eid crit)]
          sweep? nil
          :else [(sound a (if full? :entity.player.attack.strong
                            :entity.player.attack.weak))])))

(defn- hearts
  "Player.damageStatsAndHearts:1071: the damage indicator over
  target t that lost more than 2 health, h after the blow."
  [t h]
  (let [lost (f32 (- (double (:health t)) (double (:health h))))
        [_ ht] (entity/box t) p (:pos t)
        at [(v/x p) (+ (v/y p) (* (double ht) 0.5)) (v/z p)]]
    (when (> lost 2.0)
      [(out/all (out/particles :damage-indicator nil at
                               (long (* lost 0.5)) 0.2
                               [0.1 0.0 0.1]))])))

(defn- landed-deltas [world eid a tid t h b src]
  (concat (damage/report-deltas world tid h)
          (extra-knock eid a tid (:knock? b))
          (when (:sweep? b)
            (sweep-deltas world eid a tid t src (:s b)))
          (visual-deltas eid a tid b)
          (hearts t h)))

(defn- struck-deltas
  "Returns the deltas of player eid's blow b at target tid."
  [world eid a tid t b]
  (let [src (source eid a)
        h (hurt-of world tid t (:damage b) src)
        hit (when-not (identical? t h)
              [[:damage tid (:damage b) src]])]
    (concat (when (:knock? b)
              [(sound a :entity.player.attack.knockback)])
            hit
            (if (entity/taken? t h)
              (landed-deltas world eid a tid t h b src)
              [(sound a :entity.player.attack.nodamage)]))))

(defn- attack
  "Returns the deltas of Player.attack:945 of player eid at target
  tid: the ticker restarts, then the blow lands when it has damage."
  [world eid a tid t]
  (let [t0 (long (:tick world))
        b (blow world a t t0)]
    (cons [:merge-entity eid {:strength-from t0}]
          (when (pos? (double (:damage b)))
            (struck-deltas world eid a tid t b)))))

(defn- gap ^double [^double p ^double lo ^double hi]
  (max (- lo p) (- p hi) 0.0))

(defn- in-range?
  "Returns true when the eye of player a is within its attack range
  of the box of target t, as AttackRange.isInRange:112 with a
  buffer of 3."
  [a t]
  (let [[h ht] (entity/box t) h (double h) p (:pos t) q (:pos a)
        ey (+ (v/y q) (entity/eye-height a))
        dx (gap (v/x q) (- (v/x p) h) (+ (v/x p) h))
        dy (gap ey (v/y p) (+ (v/y p) (double ht)))
        dz (gap (v/z q) (- (v/z p) h) (+ (v/z p) h))
        d (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
    (<= d (+ (f32 (player/entity-reach a)) range-buffer))))

(defn- target? [eid tid t]
  (and t (not= eid tid) (some? (:health t))
       (not (contains? #{:item :experience-orb} (:type t)))))

(defn- attack-deltas
  "Returns the deltas of ServerGamePacketListenerImpl.handleAttack
  :1808 of player eid at target tid."
  [world [_ eid tid]]
  (let [a (get-in world [:entities eid])
        t (get-in world [:entities tid])]
    (when (and a (not (game-mode/spectator? a)) (target? eid tid t)
               (in-range? a t))
      (attack world eid a tid t))))

(defn- swing-deltas
  "ServerPlayer.swing:1996 restarts the attack ticker."
  [world [_ eid]]
  (when (get-in world [:entities eid])
    [[:merge-entity eid {:strength-from (long (:tick world))}]]))

(defn- event-deltas [world ev]
  (if (identical? :attack (nth ev 0))
    (attack-deltas world ev)
    (swing-deltas world ev)))

(defn attacks
  "Returns the deltas of the players hitting entities and swinging
  their arms this tick, one packet after another."
  {:wake {:events #{:attack :swing}}}
  [world d]
  (let [evs (filterv #(contains? #{:attack :swing} (nth % 0))
                     (:input d))]
    (deltas/of-vec (when (seq evs)
                     (apply/fold-events world evs event-deltas)))))

(defn wielded
  "Returns the deltas of Player.tick:266-274 for player eid: the
  attack ticker restarts the tick after its main hand takes another
  item."
  [world [eid e]]
  (let [k (held e)]
    (when-not (= k (:wielded e))
      [[:merge-entity eid
        {:wielded k :strength-from (inc (long (:tick world)))}]])))
