(ns collider.game.systems.attacks
  "Players hitting entities when the attack packet comes."
  (:require [collider.data :as data]
            [collider.game.apply :as apply]
            [collider.game.attribute :as attribute]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.enchantment :as enchantment]
            [collider.game.entity.hurt :as hurt]
            [collider.game.inventory :as inventory]
            [collider.game.mob.clock :as clock]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.blocks.climb :as climb]))

(set! *warn-on-reflection* true)

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
  ^double [a k]
  (attribute/value a (:effects a) k))

(defn- strength
  "Returns how far the attack of player a has charged at tick t,
  from 0 to 1."
  ^double [a ^long t]
  (let [n (if-let [from (:strength-from a)]
            (- t (long from))
            Integer/MAX_VALUE)
        delay (num/f32 (* (/ 1.0 (attr a :attack-speed)) 20.0))
        s (num/f32 (/ (num/f32 (+ (num/f32 n) 0.5)) delay))]
    (max 0.0 (min 1.0 s))))

(defn- strength-factor ^double [^double s]
  (let [sq (num/f32 (* s s))]
    (num/f32 (+ weak-floor (num/f32 (* sq strong-share))))))

(defn- scaled
  ^double [^double base ^double s]
  (num/f32 (* base (strength-factor s))))

(defn- crit?
  [world a t]
  (and (pos? (double (or (:fall a) 0.0))) (not (:on-ground a))
       (not (climb/on-climbable? (:chunks world) (:pos a)))
       (not (:in-water? a)) (not (:blindness (:effects a)))
       (some? (:health t)) (not (:sprinting? a))))

(defn- sweep?
  "Returns true when a full hit of player a sweeps, given it is
  neither a crit nor a sprint knockback."
  [a]
  (let [c (or (:client-vel a) [0.0 0.0 0.0])
        speed (attribute/value a (:effects a) :movement-speed)
        m (* (num/f32 speed) 2.5)]
    (and (:on-ground a) (player/sword? (held a))
         (< (+ (* (v/x c) (v/x c)) (* (v/z c) (v/z c))) (* m m)))))

(defn- blow
  [world a target ^long t0]
  (let [s (strength a t0)
        full? (> s full-strength)
        knock? (and full? (boolean (:sprinting? a)))
        crit? (and full? (crit? world a target))
        d (scaled (num/f32 (attr a :attack-damage)) s)
        bonus (enchantment/damage-bonus (player/hand-stack a :main)
                                        (:type target))
        magic (num/f32 (* s bonus))
        base (if crit? (num/f32 (* d crit-multiplier)) d)]
    {:s s :full? full? :knock? knock? :crit? crit?
     :sweep? (and full? (not crit?) (not knock?) (sweep? a))
     :magic magic
     :damage (num/f32 (+ base magic))}))

(defn- source [eid a]
  {:type :player-attack :cause eid :direct eid :from (:pos a)
   :player? true :attacker a :direct-attacker a})

(defn- sound [a k]
  (out/all (out/sound k (:pos a) 1.0 1.0 :players)))

(defn- facing
  "Returns the sine and the negated cosine of the yaw of player a."
  [a]
  (let [r (num/f32 (* (num/f32 (:yaw a)) degree))]
    [(v/sin r) (- (v/cos r))]))

(defn- hurt-of
  [world eid e d src]
  (if-let [ds (hurt/damage-deltas world eid e d src)]
    (hurt/hurt-now world eid e ds)
    e))

(defn- base-knockback ^double [a]
  (num/f32 (+ (num/f32 (attribute/value a (:effects a) :attack-knockback))
              (enchantment/knockback-bonus (player/hand-stack a :main)))))

(defn- extra-knock
  [eid a tid knock?]
  (let [kb (num/f32 (/ (base-knockback a) 2.0))
        k (num/f32 (+ kb (if knock? sprint-knockback 0.0)))
        [sx cz] (facing a)]
    (when (pos? k)
      [[:knockback tid k sx cz]
       [:merge-entity eid {:sprinting? false}]])))

(defn- body-box [o]
  (let [[h ht] (entity/box o) p (:pos o) h (double h)]
    [(- (v/x p) h) (v/y p) (- (v/z p) h)
     (+ (v/x p) h) (+ (v/y p) (double ht)) (+ (v/z p) h)]))

(defn- swept? [a box [oid o] eid tid]
  (and (not= oid eid) (not= oid tid) (some? (:health o))
       (entity/living? o)
       (v/boxes-meet? (body-box o) box)
       (< (v/dist-sq (:pos a) (:pos o)) 9.0)))

(defn- sweep-box
  [target]
  (let [[h ht] (entity/box target) p (:pos target)
        g (+ (double h) 1.0)]
    [(- (v/x p) g) (- (v/y p) 0.25) (- (v/z p) g)
     (+ (v/x p) g) (+ (v/y p) (double ht) 0.25) (+ (v/z p) g)]))

(defn- swept-deltas [world src s [sx cz] [oid o]]
  (let [d [:damage oid s src]
        h (hurt-of world oid o s src)]
    (if (entity/taken? o h)
      (into [d [:knockback oid sweep-knockback sx cz]]
            (hurt/report-deltas world oid h))
      (when-not (identical? o h) [d]))))

(defn- sweep-particle [a]
  (let [[sx cz] (facing a) p (:pos a)
        [_ ht] (entity/box a)
        at [(- (v/x p) sx) (+ (v/y p) (* (double ht) 0.5))
            (+ (v/z p) (- cz))]
        spread [(- sx) 0.0 (- cz)]]
    (out/all (out/particles :sweep-attack nil at 0 0.0 spread))))

(defn- sweep-damage
  "Returns the damage the sweep of blow b of player a deals each
  entity near."
  ^double [a b]
  (let [r (num/f32 (attr a :sweeping-damage-ratio))
        d (num/f32 (+ 1.0 (* r (double (:damage b)))))]
    (* d (double (:s b)))))

(defn- sweep-deltas
  [world eid a tid target src b]
  (let [s (sweep-damage a b) box (sweep-box target) dir (facing a)
        near (filter #(swept? a box % eid tid) (:entities world))]
    (concat [(sound a :entity.player.attack.sweep)]
            (mapcat #(swept-deltas world src (num/f32 s) dir %) near)
            [(sweep-particle a)])))

(defn- plain-sound [a full?]
  (sound a (if full?
             :entity.player.attack.strong
             :entity.player.attack.weak)))

(defn- visual-deltas
  [eid a tid {:keys [crit? sweep? full?]}]
  (let [crit (out/animation tid :crit)]
    (cond crit? [(sound a :entity.player.attack.crit)
                 (out/all (assoc crit :via eid)) (out/to eid crit)]
          sweep? nil
          :else [(plain-sound a full?)])))

(defn- lost-health ^double [target h]
  (num/f32 (- (double (:health target)) (double (:health h)))))

(defn- hearts
  [target h]
  (let [lost (lost-health target h)
        [_ ht] (entity/box target) p (:pos target)
        at [(v/x p) (+ (v/y p) (* (double ht) 0.5)) (v/z p)]
        n (long (* lost 0.5)) spread [0.1 0.0 0.1]
        fx (out/particles :damage-indicator nil at n 0.2 spread)]
    (when (> lost 2.0)
      [(out/all fx)])))

(defn- ignite-deltas [a tid target]
  (let [secs (enchantment/ignite-seconds (player/hand-stack a :main))
        n (long (Math/floor (* secs 20.0)))]
    (when (> n (long (or (:fire target) 0)))
      [[:merge-entity tid {:fire n :ticks-frozen 0}]])))

(defn- wear-deltas [world eid a]
  (let [stack (player/hand-stack a :main)]
    (when-let [n (get-in (data/items) [(:item stack) :per-attack])]
      (when-not (player/infinite-materials? a)
        (cons [:award eid (keyword "used" (name (:item stack))) 1]
              (inventory/hurt-item-deltas (:tick world) eid a :main
                                          (long n)))))))

(defn- landed-deltas [world eid a tid target h b src]
  (concat (hurt/report-deltas world tid h)
          (extra-knock eid a tid (:knock? b))
          (when (:sweep? b)
            (sweep-deltas world eid a tid target src b))
          (visual-deltas eid a tid b)
          (ignite-deltas a tid target)
          (wear-deltas world eid a)
          (hearts target h)))

(defn- struck-deltas
  [world eid a tid target b]
  (let [src (source eid a)
        h (hurt-of world tid target (:damage b) src)
        hit (when-not (identical? target h)
              [[:damage tid (:damage b) src]])]
    (concat (when (:knock? b)
              [(sound a :entity.player.attack.knockback)])
            hit
            (if (entity/taken? target h)
              (landed-deltas world eid a tid target h b src)
              [(sound a :entity.player.attack.nodamage)]))))

(defn- attack
  [world eid a tid target]
  (let [t0 (long (:tick world))
        b (blow world a target t0)]
    (cons [:merge-entity eid {:strength-from t0}]
          (when (pos? (double (:damage b)))
            (struck-deltas world eid a tid target b)))))

(defn- gap ^double [^double p ^double lo ^double hi]
  (max (- lo p) (- p hi) 0.0))

(defn- in-range?
  [a target]
  (let [[h ht] (entity/box target) h (double h)
        p (:pos target) q (:pos a)
        ey (+ (v/y q) (entity/eye-height a))
        dx (gap (v/x q) (- (v/x p) h) (+ (v/x p) h))
        dy (gap ey (v/y p) (+ (v/y p) (double ht)))
        dz (gap (v/z q) (- (v/z p) h) (+ (v/z p) h))
        d (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
    (<= d (+ (num/f32 (player/entity-reach a)) range-buffer))))

(defn- target? [eid tid target]
  (and target (not= eid tid) (some? (:health target))
       (not (contains? #{:item :experience-orb} (:type target)))))

(defn- attack-deltas
  [world [_ eid tid]]
  (let [a (get-in world [:entities eid])
        target (get-in world [:entities tid])]
    (when (and a (not (game-mode/spectator? a))
               (target? eid tid target) (in-range? a target))
      (if-let [m (clock/rebased target (:tick world))]
        (cons [:merge-entity tid m]
              (attack world eid a tid (merge target m)))
        (attack world eid a tid target)))))

(defn- swing-deltas
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
  "Returns the deltas that restart the attack ticker of player eid
  the tick after its main hand takes another item."
  [world [eid e]]
  (let [k (held e)]
    (when-not (= k (:wielded e))
      [[:merge-entity eid
        {:wielded k :strength-from (inc (long (:tick world)))}]])))
