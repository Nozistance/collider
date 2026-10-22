(ns collider.game.turn.arrow
  "The turn of an arrow in flight, in the ground and falling out of
  it."
  (:require [collider.game.block.press :as press]
            [collider.game.effect :as effect]
            [collider.game.effect.account :as account]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.potion :as potion]
            [collider.game.projectile :as projectile]
            [collider.game.team :as team]
            [collider.math :as math]
            [collider.parallel :as par]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:const drag
  "How much of its speed an arrow in the air keeps each tick."
  (math/f32 0.99))

(def ^:private ^:const water-drag (math/f32 0.6))

(def ^:const gravity
  "How much an arrow falls faster each tick."
  0.05)

(def ^:private ^:const set-back (math/f32 0.05))

(def ^:private ^:const shake-ticks 7)

(def ^:private ^:const lifetime 1200)

(def ^:private ^:const fall-margin 0.06)

(def ^:private still [0.0 0.0 0.0])

(defn- scaled [d ^double k]
  [(* k (v/x d)) (* k (v/y d)) (* k (v/z d))])

(defn- held? [st cell p]
  (some #(projectile/inside? (projectile/abs-box cell %) p)
        (block/collision-boxes st)))

(defn falls?
  "Returns true when no block holds arrow e where it stands."
  [world e]
  (let [[x y z] (:pos e) m fall-margin]
    (phys/box-free? (:chunks world)
                    [(- x m) (- y m) (- z m) (+ x m) (+ y m) (+ z m)]
                    (- y m))))

(defn- shaken ^long [e]
  (max 0 (dec (long (:shake e 0)))))

(defn- fire-met [world e from to]
  (hurt/met-fire world e (long (:fire e 0)) [[from to nil]]))

(defn- put-out [world eid e f]
  (when (and (pos? (long (:fire e 0))) (<= (long f) 0))
    [(hurt/put-out-sound world eid e)]))

(def ^:private ^:const potion-ticks 600)

(defn- worn-off? [e ^long t]
  (and (>= t potion-ticks) (= :arrow (:type e))
       (potion/tipped? (:stack e))))

(defn- lain [eid e m]
  (let [t (inc (long (:ground-time e 0)))
        m (assoc m :ground-time t)]
    (if (worn-off? e t)
      [[:merge-entity eid (assoc m :stack {:item :arrow :count 1})]
       (out/all (out/status eid :potion-puff))]
      [[:merge-entity eid m]])))

(defn- lying-deltas [world eid e st]
  (let [f (fire-met world e (:pos e) (:pos e))
        out (put-out world eid e f)
        m {:in-ground? true :vel still :shake (shaken e)
           :age (inc (long (:age e 0))) :needs-sync? false
           :fire f :burning? (pos? f)}
        life (inc (long (:life e 0)))]
    (cond
      (and (not= (:last-state e) st) (falls? world e))
      (let [m (assoc m :in-ground? false :life 0 :ground-time 0)]
        (into [[:merge-entity eid m]] out))
      (>= life lifetime) [[:remove-entity eid]]
      :else (into (lain eid e (assoc m :life life)) out))))

(defn- hit-sound [world eid e]
  (let [r (math/f32 (random/of-key (:tick world) eid :arrow-hit))
        d (math/f32 (+ (math/f32 (* r (math/f32 0.2))) (math/f32 0.9)))]
    (out/all (out/sound (:hit-sound e :arrow/hit) (:pos e) 1.0
                        (math/f32 (/ (math/f32 1.2) d))))))

(defn- stuck-at [at d]
  (let [back #(- (double %1) (* set-back (Math/signum (double %2))))]
    (mapv back at d)))

(defn- after-move [e d]
  (cond-> d
    (not (:in-water? e)) (scaled drag)
    (not (:in-ground? e)) (v/add [0.0 (- gravity) 0.0])))

(defn- wet [world e]
  (let [[half h] (entity/box e)
        f (liquid/fluid-info (:chunks world) (:pos e) half h
                             (:vel e))]
    (cond-> {:in-water? (pos? (double (:water f)))}
      (not (:in-ground? e)) (assoc :vel (v/add (:vel e) (:push f))))))

(defn- settled [world e]
  (let [e (assoc e :vel (after-move e (:vel e)))]
    (merge e (wet world e))))

(defn- stuck [world e at d cell]
  (assoc e :pos (stuck-at at d) :vel still :in-ground? true
         :last-state (chunk/at (:chunks world) cell)
         :shake shake-ticks :crit? false :needs-sync? true))

(defn- searched [e d]
  (let [[half h] (entity/box e)
        b (projectile/box-of (:pos e) half h)
        grow (fn [^long i ^double x]
               (+ (double (b i))
                  (if (< i 3) (min x 0.0) (max x 0.0))))
        b (mapv grow (range 6) (concat d d))]
    (projectile/inflated b 1.0 1.0 1.0)))

(defn- harmed? [world e o]
  (let [a (get-in world [:entities (:owner e)])]
    (not (and (entity/player? o) (entity/player? a)
              (team/spares? (:teams world) o a)))))

(defn- target? [world eid e oid o]
  (and (not= (long oid) (long eid)) (projectile/hittable? o)
       (pos? (double (:health o 1.0))) (not (game-mode/spectator? o))
       (harmed? world e o)))

(defn- centre [b]
  (mapv #(/ (+ (double (b %)) (double (b (+ % 3)))) 2.0) [0 1 2]))

(defn- grazed [world bb m from to]
  (when-let [out (projectile/box-clip (projectile/inflated bb m m m)
                                      from to)]
    (let [d (projectile/span out (mapv - (centre bb) out))
          b (projectile/block-clip world out d)
          towards (if b (projectile/point out d (b 0)) (centre bb))]
      (projectile/box-clip bb out towards))))

(defn- met-at [world m from to o]
  (let [bb (projectile/target-box o)]
    (or (projectile/box-clip bb from to)
        (when (pos? m) (grazed world bb m from to)))))

(defn- bodies
  "Returns the bodies arrow e meets on its way from to to, each
  [oid o at], nearest first by where they stand."
  [world eid e to]
  (let [from (:pos e) area (searched e (:vel e))
        m (projectile/margin e)
        met (for [[oid o] (par/keyed (:entities world))
                  :when (and (target? world eid e oid o)
                             (projectile/overlaps?
                               area (projectile/target-box o)))
                  :let [at (met-at world m from to o)]
                  :when at]
              [oid o at])]
    (sort-by #(v/dist-sq from (:pos (nth % 1))) met)))

(defn- damage ^long [world eid e]
  (let [pow (math/f32 (v/length (:vel e)))
        n (long (Math/ceil (min (* pow 2.0) 2.147483647E9)))]
    (if (:crit? e)
      (let [bound (+ 2 (quot n 2))
            r (random/of-key (:tick world) eid :crit)]
        (+ n (long (* bound r))))
      n)))

(defn- arrow-source [world eid e]
  (let [o (some->> (:owner e) (get (:entities world)))]
    {:type :arrow :cause (if o (:owner e) eid) :direct eid
     :along (:vel e) :player? (entity/player? o) :attacker (or o e)
     :direct-attacker e}))

(defn- told-owner [world e oid o]
  (let [a (get-in world [:entities (:owner e)])]
    (when (and (entity/player? o) (entity/player? a)
               (not= (long oid) (long (:owner e))))
      [(out/to (:owner e) (out/arrow-hit-player))])))

(def ^:private ^:const ignite-ticks 100)

(def ^:private glowing [[:glowing (effect/instance 200 0)]])

(defn- laid [e]
  (if (= :spectral-arrow (:type e))
    glowing
    (let [st (:stack e) s (potion/duration-scale st)]
      (for [[k i] (potion/instances st)] [k (effect/scaled i s)]))))

(defn- effect-deltas [e oid o]
  (let [fx (laid e)]
    (when (and (seq fx) (account/living? o))
      (let [land (fn [acc [k i]] (account/land acc k i))
            acc (reduce land (account/account oid o) fx)]
        (account/deltas acc o)))))

(defn- hit-deltas [world eid e oid o n src]
  (let [ds (hurt/damage-deltas world oid o n src)]
    (-> (vec ds)
        (into (hurt/report-deltas world oid
                                  (hurt/hurt-now world oid o ds)))
        (conj [:merge-entity oid
               (cond-> {:arrow-count (inc (long (:arrow-count o 0)))}
                 (pos? (long (:fire e 0)))
                 (assoc :fire (max (long (:fire o 0))
                                   ignite-ticks)))])
        (into (effect-deltas e oid o))
        (into (told-owner world e oid o))
        (conj (hit-sound world eid e) [:remove-entity eid]))))

(def ^:private ^:const bounce-turn (math/f32 20.0))

(defn- bounced [world eid e]
  (let [r (math/f32 (random/of-key (:tick world) eid :bounce))
        turn (math/f32 (+ (math/f32 170.0)
                         (math/f32 (* r bounce-turn))))]
    (assoc e :vel (scaled (scaled (:vel e) -0.5) 0.2)
           :yaw (math/f32 (+ (math/f32 (:yaw e 0.0)) turn)))))

(defn- dropped [world eid e]
  (when (= :allowed (:pickup e))
    (let [[x y z] (:pos e)
          vel (entity/pop-velocity [(:tick world) eid :drop])]
      [[:spawn-entity
        (entity/item [x (+ (double y) (double (math/f32 0.1))) z]
                     vel (:stack e))]])))

(defn- struck [world eid {:keys [e ds] :as acc} [oid o]]
  (let [n (damage world eid e) src (arrow-source world eid e)]
    (if (hurt/hurts? world o n src)
      (let [hit (hit-deltas world eid e oid o n src)]
        (reduced (assoc acc :gone? true :ds (into ds hit))))
      (let [e (bounced world eid e)]
        (if (< (v/dot (:vel e) (:vel e)) 1.0E-7)
          (reduced (assoc acc :gone? true
                          :ds (-> ds (into (dropped world eid e))
                                  (conj [:remove-entity eid]))))
          (assoc acc :e e))))))

(def ^:private flown-keys
  [:pos :vel :yaw :pitch :left-owner? :shake :age :in-ground?
   :in-water? :last-state :crit? :needs-sync? :fire :burning?])

(defn- burnt-down [e]
  (let [f (long (:fire e 0))
        f (if (pos? f) (dec f) f)]
    (assoc e :fire f :burning? (pos? f))))

(defn- ended [world eid e ds]
  (if (< (v/y (:pos e)) (chunk/void-y world))
    [[:remove-entity eid]]
    (into [[:merge-entity eid
            (select-keys (burnt-down (settled world e)) flown-keys)]]
          ds)))

(defn- body-flight [world eid e met]
  (let [to (nth (first met) 2)
        f (fire-met world e (:pos e) to)
        out (into (vec (press/trip world eid e (:pos e) to))
                  (put-out world eid e f))
        e (assoc e :pos to :needs-sync? true :fire f)
        {:keys [e ds gone?]} (reduce #(struck world eid %1 %2)
                                     {:e e :ds (vec out)} met)]
    (if gone? ds (ended world eid e ds))))

(defn- block-landed [world eid e at b]
  (if b
    (let [cell (nth b 2) e (stuck world e at (:vel e) cell)]
      (ended world eid e
             (into [(hit-sound world eid e)]
                   (projectile/block-hit-deltas
                     world e {:at at :face (nth b 1) :cell cell}))))
    (ended world eid (assoc e :pos at :needs-sync? false) nil)))

(defn- block-flight [world eid e at b]
  (let [f (fire-met world e (:pos e) at)
        out (put-out world eid e f)]
    (-> (vec (press/trip world eid e (:pos e) at))
        (into (block-landed world eid (assoc e :fire f) at b))
        (into out))))

(defn- flight-deltas [world eid e]
  (let [move (:vel e)
        d (if (:in-water? e) (scaled move water-drag) move)
        [yaw pitch] (projectile/rotated e move)
        span (projectile/span (:pos e) move)
        b (projectile/block-clip world (:pos e) span)
        at (if b
             (projectile/point (:pos e) span (b 0))
             (v/add (:pos e) move))
        e (assoc e :vel d :yaw yaw :pitch pitch
                 :left-owner? (projectile/left-owner? world e d)
                 :shake (shaken e))
        met (bodies world eid e at)
        e (update e :age #(inc (long (or % 0))))]
    (if (seq met)
      (body-flight world eid e met)
      (block-flight world eid e at b))))

(defn- doused [world e]
  (if (or (:in-water? e) (hurt/rained-on? world e))
    (update e :fire #(min 0 (long (or % 0))))
    e))

(defn step-deltas
  "Returns the deltas of the turn of arrow e of eid."
  [world eid e]
  (let [e (doused world e)
        p (:pos e) cell (v/cell p)
        st (if (chunk/in-range? (nth cell 1))
             (chunk/at (:chunks world) cell)
             0)]
    (if (or (:in-ground? e) (held? st cell p))
      (lying-deltas world eid e st)
      (flight-deltas world eid e))))

(defn reach
  "Returns the box [x0 z0 x1 z1] that arrow e sweeps in its move
  this tick."
  [e]
  (let [p (:pos e) q (if (:in-ground? e) p (v/add p (:vel e)))]
    [(min (v/x p) (v/x q)) (min (v/z p) (v/z q))
     (max (v/x p) (v/x q)) (max (v/z p) (v/z q))]))
