(ns collider.game.turn.mob
  "The turn of a mob: its base tick, thinking, movement and sounds."
  (:require [collider.game.deltas :as deltas]
            [clojure.core.reducers :as r]
            [collider.game.attribute :as attribute]
            [collider.game.mode :as game-mode]
            [collider.random :as random]
            [collider.game.entity :as entity]
            [collider.vec :as v]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.control :as control]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.sheep :as sheep]
            [collider.game.mob.cow :as cow]
            [collider.game.mob.chicken :as chicken]
            [collider.game.mob.pig :as pig]
            [collider.game.mob.rabbit :as rabbit]
            [collider.game.mob.mooshroom :as mooshroom]
            [collider.game.mob.push :as push]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.turn.landing :as landing]
            [collider.game.turn.living :as living]
            [collider.game.turn.overlay :as overlay]
            [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.player :as player]
            [collider.game.out :as out]
            [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.game.reach :as reach]
            [collider.world.env.signal :as signal]
            [collider.world.phys :as phys])
  (:import (collider.game.entity.records Mob)
           (collider.game.mob Islands Steer Turns)
           (collider.world Move)))

(set! *warn-on-reflection* true)

(def ^:private brains
  {:sheep sheep/brain :cow cow/brain
   :mooshroom mooshroom/brain :pig pig/brain :chicken chicken/brain
   :rabbit rabbit/brain})

(def ^:private specs
  {:sheep sheep/spec :cow cow/spec :mooshroom mooshroom/spec
   :pig pig/spec :chicken chicken/spec :rabbit rabbit/spec})

(def ^:private ai-steps
  {:chicken chicken/ai-step :rabbit rabbit/ai-step})

(defn- think [world eid e t tempters]
  (if-let [b (brains (:type e))]
    (b world eid e t tempters)
    [e nil]))

(def ^:private ^:const reach-buffer 3.0)

(defn- in-reach? [p e]
  (let [[half height] (mobs/box-of e)
        [ex ey ez] (reach/eye-pos p)
        [x y z] (:pos e)
        w (* 2.0 (double half))
        dx (reach/axis-gap ex (- (double x) (double half)) w)
        dy (reach/axis-gap ey (double y) (double height))
        dz (reach/axis-gap ez (- (double z) (double half)) w)
        r (+ (player/entity-reach p) reach-buffer)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- ctx-of
  [world t [_ peid target hand sneaking?]]
  (let [p (get-in world [:entities peid])
        e (get-in world [:entities target])
        hand (if (#{:off 1} hand) :off :main)]
    (when (and p e (mobs/mob-type? (:type e)))
      (let [p (assoc p :sneaking? (boolean sneaking?))]
        (when (in-reach? p e)
          {:world world :t t :peid peid :p p :eid target :e e
           :hand hand :item (sense/in-hand p hand)})))))

(defn- name-tag-result [_ctx]
  nil)

(defn- leash-result [_ctx]
  nil)

(defn- equippable-result [_ctx]
  nil)

(defn- species-result [ctx]
  (let [mushroom [mooshroom/bowl-result mooshroom/shear-result
                  mooshroom/flower-result cow/milk-result
                  animal/feed-result]
        fs (case (:type (:e ctx))
             :sheep [sheep/shear-result animal/feed-result]
             :cow [cow/milk-result animal/feed-result]
             :mooshroom mushroom
             (:pig :chicken :rabbit) [animal/feed-result]
             nil)]
    (some (fn [f] (f ctx)) fs)))

(def ^:private chain
  [name-tag-result (partial animal/egg-result specs) leash-result
   species-result sheep/dye-result equippable-result])

(defn- answered
  [{:keys [peid p e hand t]} {:keys [result deltas]}]
  (cond-> (vec deltas)
    (not= :pass result)
    (into (signal/game-event :entity-interact (:pos e) peid))
    (= :success-server result)
    (into (player/swing-deltas peid p hand t true))))

(defn- spectating? [world ev]
  (game-mode/spectator? (get-in world [:entities (nth ev 1)])))

(defn- interact [world ev t]
  (if-let [ctx (when-not (spectating? world ev) (ctx-of world t ev))]
    (answered ctx (or (some (fn [f] (f ctx)) chain) {:result :pass}))
    []))

(defn- interact-deltas [world events t]
  (apply/fold-events world (filter #(= :interact (first %)) events)
                     (fn [w ev] (interact w ev t))))

(defn- fmul
  ^double [^double a ^double b] (double (float (* a b))))

(defn- fsub ^double [^double a ^double b] (double (float (- a b))))

(defn- fdiv ^double [^double a ^double b] (double (float (/ a b))))

(def ^:private ^:const deg->rad (double (float (/ Math/PI 180.0))))

(defn- yaw-radians ^double [^double yaw]
  (double (float (* (double (float yaw)) deg->rad))))

(defn- modified-friction ^double [^double f]
  (Math/clamp (fsub 1.0 (fsub 1.0 f)) 0.0 1.0))

(def ^:private ^:const gravity 0.08)

(def ^:private ^:const slow-fall-gravity 0.01)

(def ^:private ^:const levitation-rise 0.05)

(def ^:private ^:const jump-boost (double (float 0.1)))

(def ^:private ^:const jump-strength (double (float 0.42)))

(def ^:private ^:const min-jump (double (float 1.0E-5)))

(def ^:private ^:const fluid-jump (double (float 0.04)))

(def ^:private ^:const fluid-drive (double (float 0.02)))

(def ^:private ^:const flying-speed (double (float 0.02)))

(def ^:private ^:const water-slowdown (double (float 0.8)))

(def ^:private ^:const out-of-fluid (double (float 0.3)))

(def ^:private ^:const out-of-fluid-reach (double (float 0.6)))

(def ^:private ^:const walk-drive (double (float 0.21600002)))

(def ^:private ^:const air-drag
  (modified-friction (double (float 0.91))))

(def ^:private ^:const vertical-drag
  (modified-friction (double (float 0.98))))

(def ^:private ^:const jump-threshold 0.4)

(defn- below-state ^long [world pos sup]
  (motion/below-state (:chunks world) pos sup))

(defn- feet-state [world pos]
  (sense/block-at world (long (Math/floor (v/x pos)))
                  (long (Math/floor (v/y pos)))
                  (long (Math/floor (v/z pos)))))

(defn- below-friction ^double [world pos sup]
  (modified-friction
    (motion/friction (below-state world pos sup))))

(defn- speed-factor ^double [world pos sup]
  (motion/block-speed-factor (:chunks world) pos sup))

(defn- jump-factor ^double [world pos sup]
  (let [here (motion/jump-factor (feet-state world pos))]
    (if (== here 1.0)
      (motion/jump-factor (below-state world pos sup))
      here)))

(defn- look-pitch ^double [e height o]
  (let [pos (:pos e) opos (:pos o)
        oh (double (nth (or (mobs/box-of o) [0.0 1.0]) 1))
        eye (+ (v/y pos) (* 0.95 (double height)))
        oeye (+ (v/y opos)
                (case (:type o) :player 1.62 :point 0.0 (* 0.95 oh)))
        dh (Math/sqrt (v/dist-xz-sq pos opos))]
    (- (Math/toDegrees (Math/atan2 (- oeye eye) dh)))))

(defn- active-look [e ^long t]
  (let [look (:look e)]
    (when (and look (> (long (or (:until look) 0)) t)) look)))

(defn- head-same? [e look ^double hy ^double hp]
  (and (identical? look (:look e))
       (let [oh (:head-yaw e)] (and oh (== (double oh) hy)))
       (let [op (:pitch e)] (and op (== (double op) hp)))))

(defn- look-aim [world e height look]
  (let [oid (:target look)
        at (:at look)
        o (cond oid (get (:entities world) oid)
                at {:pos at :type :point})]
    [(cond o (v/yaw-toward (:pos e) (:pos o))
           (and look (:yaw look)) (:yaw look)
           :else (control/body-yaw e))
     (if o (look-pitch e height o) 0.0)]))

(defn- look-of
  "Returns the head yaw, head pitch and look of mob e turned towards
  what it looks at, or nil when its head stays."
  [world e height t]
  (let [look (active-look e (long t))
        [dyaw dpitch] (look-aim world e height look)
        y0 (double (or (:head-yaw e) (:yaw e)))
        p0 (double (or (:pitch e) 0.0))
        hy (v/limit-angle y0 (double dyaw) 10.0)
        hp (v/limit-angle p0 (double dpitch) 40.0)]
    (when-not (head-same? e look (double hy) (double hp))
      [hy hp look])))

(defn- looked [world e height t]
  (if-let [[hy hp look] (look-of world e height t)]
    (entity/mob-looked e hy hp look)
    e))

(defn- fall-gravity
  "Returns the gravity mob e falls by at vertical speed vy. This is
  LivingEntity.getEffectiveGravity."
  ^double [e ^double vy]
  (if (and (<= vy 0.0) (contains? (:effects e) :slow-falling))
    (Math/min gravity slow-fall-gravity)
    gravity))

(defn- levitation [e] (get (:effects e) :levitation))

(defn- lifted
  "Returns vertical speed vy of mob e after gravity or levitation,
  as LivingEntity.travelInAir does."
  ^double [e ^double vy]
  (if-let [l (levitation e)]
    (let [a (double (inc (long (:amplifier l))))]
      (+ vy (* (- (* levitation-rise a) vy) 0.2)))
    (- vy (fall-gravity e vy))))

(def ^:private rest-vel
  (v/v3 0.0 (* (- 0.0 gravity) vertical-drag) 0.0))

(defn- rest-vel-of [e]
  (if (:effects e)
    (v/v3 0.0 (* (- 0.0 (fall-gravity e 0.0)) vertical-drag) 0.0)
    rest-vel))

(defn- dead-band ^double [^double a]
  (if (< (Math/abs a) 0.003) 0.0 a))

(defn- at-rest? [world e half moving? fluid? vel]
  (let [pos (:pos e)
        x (v/x pos) y (v/y pos) z (v/z pos)
        still? (and (not moving?) (boolean (:on-ground e))
                    (not fluid?) (not (:jump e))
                    (zero? (v/x vel)) (zero? (v/z vel))
                    (neg? (v/y vel)))]
    (and still? (nil? (levitation e))
         (phys/standing-on-cubes? (:chunks world) x y z half))))

(defn- speed-of ^double [e] (double (:speed (:move e) 0.0)))

(defn- driven [e vel ^double speed]
  (let [zza (double (:zza (:move e) 0.0))
        l (* zza zza)]
    (if (< l 1.0E-7)
      vel
      (let [f (* (if (> l 1.0) (Math/signum zza) zza) speed)
            r (yaw-radians (double (:yaw e)))]
        (v/v3 (- (v/x vel) (* f (v/sin r)))
              (v/y vel)
              (+ (v/z vel) (* f (v/cos r))))))))

(defn- friction-speed ^double [og? ^double bf ^double speed]
  (if og?
    (if (> bf 0.6)
      (fmul speed (fdiv walk-drive (fmul (fmul bf bf) bf)))
      speed)
    flying-speed))

(defn- hit-wall? [drive vel]
  (or (not= (v/x drive) (v/x vel)) (not= (v/z drive) (v/z vel))))

(defn- fluid-fall ^double [^double g falling? ^double vy]
  (if (zero? g)
    vy
    (if (and falling? (>= (Math/abs (- vy 0.005)) 0.003)
             (< (Math/abs (- vy (/ g 16.0))) 0.003))
      -0.003
      (- vy (/ g 16.0)))))

(defn- stepped ^Move [world e vel half height c]
  (phys/move (:chunks world) (:pos e) vel half height
             (mobs/step-height (:type e)) (boolean (:on-ground e)) c))

(defn- supported [world e ^Move mv half c]
  (if-not (phys/on-ground? mv)
    [nil false]
    (let [ch (:chunks world) p (phys/pos mv) o (:pos e)
          sb (phys/supporting-block ch p half c)
          s (or sb (when-not (:no-blocks? e)
                     (phys/supporting-block
                       ch (v/v3 (v/x o) (v/y p) (v/z o)) half c)))]
      [(if (= s (:support e)) (:support e) s) (nil? sb)])))

(defn- wet-state? [^long st]
  (and (pos? st)
       (or (some? (block/liquid-class st)) (block/waterlogged? st))))

(defn- any-liquid? [world lo hi]
  (let [xa (long (Math/floor (v/x lo))) xb (long (Math/ceil (v/x hi)))
        ya (long (Math/floor (v/y lo))) yb (long (Math/ceil (v/y hi)))
        za (long (Math/floor (v/z lo)))
        zb (long (Math/ceil (v/z hi)))]
    (loop [x xa y ya z za]
      (cond (>= x xb) false
            (>= y yb) (recur (inc x) ya za)
            (>= z zb) (recur x (inc y) za)
            (wet-state? (sense/block-at world x y z)) true
            :else (recur x y (inc z))))))

(defn- climb-free? [world e pos vel half height oy]
  (let [up (+ (v/y vel) out-of-fluid-reach
              (- (double oy) (v/y pos)))
        x (v/x pos) y (v/y pos) z (v/z pos) half (double half)
        dx (v/x vel) dz (v/z vel)]
    (and (phys/free? (:chunks world) pos half height dx up dz
                     (phys/context e))
         (not (any-liquid?
                world
                (v/v3 (+ (- x half) dx) (+ y up) (+ (- z half) dz))
                (v/v3 (+ (+ x half) dx) (+ (+ y (double height)) up)
                      (+ (+ z half) dz)))))))

(defn- jumped-out [world e ^Move mv vel half height oy hit?]
  (if (and hit?
           (climb-free? world e (phys/pos mv) vel half height oy))
    (v/v3 (v/x vel) out-of-fluid (v/z vel))
    vel))

(defn- in-fluid? [{:keys [water lava]}]
  (or (pos? (double water)) (pos? (double lava))))

(defn- same-span? [^double lo ^double hi ^double lo' ^double hi']
  (and (== (Math/floor lo) (Math/floor lo'))
       (== (Math/ceil hi) (Math/ceil hi'))))

(defn- same-cells? [from to ^double half ^double height]
  (let [a (- half 0.001) x (v/x from) y (v/y from) z (v/z from)
        x' (v/x to) y' (v/y to) z' (v/z to)]
    (and (<= y y')
         (same-span? (- x a) (+ x a) (- x' a) (+ x' a))
         (same-span? (- z a) (+ z a) (- z' a) (+ z' a))
         (same-span? (+ y 0.001) (- (+ y height) 0.001)
                     (+ y' 0.001) (- (+ y' height) 0.001)))))

(defn- dry-still? [e ^Move mv half height f]
  (and (not (in-fluid? f))
       (same-cells? (:pos e) (phys/pos mv) half height)))

(defn- current-axis ^double [^double asked ^double got ^double p]
  (if (== got asked) (+ got p) (* (- (+ asked p)) 0.0)))

(defn- carried [d u p]
  (v/v3 (current-axis (v/x d) (v/x u) (double (nth p 0)))
        (current-axis (v/y d) (v/y u) (double (nth p 1)))
        (current-axis (v/z d) (v/z u) (double (nth p 2)))))

(def ^:private dry {:water 0.0 :lava 0.0 :push [0.0 0.0 0.0]})

(defn- fluid-after [world pos h ht v]
  (let [ch (:chunks world)]
    (if (phys/dry? ch pos h ht)
      dry
      (liquid/fluid-info ch pos h ht v (:dim world)))))

(defn- fluid-of [world e half height]
  (fluid-after world (:pos e) half height (:vel e)))

(defn- moved-fluid
  "Returns the fluid over mob e after the move mv by d and its
  velocity with the push of that fluid. A mob out of water looks
  again after it moves, as LivingEntity.checkFallDamage does."
  [world e d ^Move mv half height f]
  (if (dry-still? e mv half height f)
    [f (phys/vel mv)]
    (let [g (fluid-after world (phys/pos mv) half height d)]
      [g (carried d (phys/vel mv) (:push g))])))

(defn- travel-air [world e vel half height og? f]
  (let [bf (if og? (below-friction world (:pos e) (:support e)) 1.0)
        d (driven e vel (friction-speed og? bf (speed-of e)))
        c (phys/context e)
        ^Move mv (stepped world e d half height c)
        [sup nb?] (supported world e mv half c)
        sf (speed-factor world (phys/pos mv) sup)
        k (fmul bf air-drag)
        [h u] (moved-fluid world e d mv half height f)]
    [(phys/pos mv)
     (v/v3 (* (* (v/x u) sf) k)
           (* (lifted e (v/y u)) vertical-drag)
           (* (* (v/z u) sf) k))
     (phys/on-ground? mv) sup nb? h d (phys/vel mv) mv]))

(defn- travel-water [world e vel half height]
  (let [oy (v/y (:pos e)) falling? (<= (v/y vel) 0.0)
        g (fall-gravity e (v/y vel))
        d (driven e vel fluid-drive)
        c (phys/context e)
        ^Move mv (stepped world e d half height c)
        [sup nb?] (supported world e mv half c)
        sf (speed-factor world (phys/pos mv) sup)
        u (phys/vel mv)
        vy (fluid-fall g falling? (* (v/y u) water-slowdown))
        w (v/v3 (* (* (v/x u) sf) water-slowdown) vy
                (* (* (v/z u) sf) water-slowdown))]
    [(phys/pos mv)
     (jumped-out world e mv w half height oy (hit-wall? d u))
     (phys/on-ground? mv) sup nb? nil d u mv]))

(defn- lava-slowed [x y z g falling? shallow?]
  (let [x (* (double x) 0.5) y (double y) z (* (double z) 0.5)]
    (if shallow?
      (v/v3 x (fluid-fall g falling? (* y water-slowdown)) z)
      (v/v3 x (* y 0.5) z))))

(defn- travel-lava [world e vel half height f]
  (let [oy (v/y (:pos e)) falling? (<= (v/y vel) 0.0)
        g (fall-gravity e (v/y vel))
        d (driven e vel fluid-drive) c (phys/context e)
        ^Move mv (stepped world e d half height c)
        [sup nb?] (supported world e mv half c)
        sf (speed-factor world (phys/pos mv) sup)
        [h u] (moved-fluid world e d mv half height f)
        shallow? (<= (double (:lava h)) (double (:threshold f)))
        w (lava-slowed (* (v/x u) sf) (v/y u) (* (v/z u) sf)
                       g falling? shallow?)
        w (v/v3 (v/x w) (- (v/y w) (/ g 4.0)) (v/z w))]
    [(phys/pos mv)
     (jumped-out world e mv w half height oy (hit-wall? d u))
     (phys/on-ground? mv) sup nb? h d (phys/vel mv) mv]))

(defn- travelled
  [world e vel half height og? {:keys [water lava] :as f}]
  (cond (pos? (double water)) (travel-water world e vel half height)
        (pos? (double lava)) (travel-lava world e vel half height f)
        :else (travel-air world e vel half height og? f)))

(defn- boost-power ^double [e]
  (if-let [b (get (:effects e) :jump-boost)]
    (fmul jump-boost (double (float (inc (long (:amplifier b))))))
    0.0))

(defn- jump-share ^double [e]
  (if (identical? :rabbit (:type e)) (rabbit/jump-share e) 1.0))

(defn- jump-power
  "Returns how hard mob e jumps. This is LivingEntity.getJumpPower."
  ^double [world e]
  (let [f (jump-factor world (:pos e) (:support e))
        s (fmul jump-strength (jump-share e))]
    (double (float (+ (fmul s f) (boost-power e))))))

(defn- jump-off [vel ^double p]
  (if (<= p min-jump)
    vel
    (v/v3 (v/x vel) (Math/max p (v/y vel)) (v/z vel))))

(defn- fluid-jumped [vel]
  (v/v3 (v/x vel) (+ (v/y vel) fluid-jump) (v/z vel)))

(defn- hop-off [e vel]
  (if (identical? :rabbit (:type e)) (rabbit/hopped e vel) vel))

(defn- jumping-vel
  [world e vel og? {:keys [water lava threshold]} ready?]
  (let [wh (double water) lh (double lava) thr (double threshold)
        lava? (pos? lh) fh (if lava? lh wh)
        in-w? (and (pos? wh) (pos? fh))
        float-w? (and in-w? (or (not og?) (> fh thr)))
        float-l? (and lava? (or (not og?) (not (<= lh thr))))]
    (cond
      float-w? [(fluid-jumped vel) false]
      float-l? [(fluid-jumped vel) false]
      (and (or og? (and in-w? (<= fh thr))) ready?)
      [(hop-off e (jump-off vel (jump-power world e))) true]
      :else [vel false])))

(defn- shifted? [from to]
  (let [dx (- (v/x to) (v/x from)) dz (- (v/z to) (v/z from))]
    (> (+ (* dx dx) (* dz dz)) 2.5000003E-7)))

(defn- jump-delay [e ^long t jumped?]
  (cond jumped? (+ t 10) (:jump e) (:jump-cd e)))

(defn- physics-move [world e vel half height f]
  (let [t (long (:tick world))
        og? (boolean (:on-ground e))
        ready? (>= t (long (or (:jump-cd e) 0)))
        [v jumped?] (if (:jump e)
                      (jumping-vel world e vel og? f ready?)
                      [vel false])
        [pos w ground? sup nb? h d u mv]
        (travelled world e v half height og? f)
        vy (liquid/bubble-push (:chunks world) pos (v/y w))]
    [pos (v/v3 (v/x w) vy (v/z w)) ground? (jump-delay e t jumped?)
     sup nb? h f false d u mv]))

(defn- rest-move [e f]
  [(:pos e) (rest-vel-of e) true nil (:support e) (:no-blocks? e) f
   f true])

(defn- eye-height ^double [^double height]
  (* 0.85 height))

(defn- fluid-threshold ^double [^double height]
  (if (< (eye-height height) 0.4) 0.0 jump-threshold))

(defn- flag [old now]
  (if (= (boolean old) now) old now))

(defn- pushed [vel push]
  (v/v3 (dead-band (+ (v/x vel) (double (nth push 0))))
        (dead-band (+ (v/y vel) (double (nth push 1))))
        (dead-band (+ (v/z vel) (double (nth push 2))))))

(defn- taken
  [vel [_ dx dz]]
  (v/v3 (+ (v/x vel) (double dx)) (v/y vel)
        (+ (v/z vel) (double dz))))

(defn- shoved [world e vel shoves live?]
  (if (and (seq shoves) (push/alive? e))
    (let [es (:entities world)
          live? (or live? #(push/alive? (get es %)))]
      (reduce (fn [v sh] (if (live? (nth sh 0)) (taken v sh) v))
              vel shoves))
    vel))

(defn- fluid-at [world e half height]
  (assoc (fluid-of world e half height)
         :threshold (fluid-threshold height)))

(defn- own-vel [index eid e half height f]
  (let [shoves (when (push/alive? e)
                 (push/before index eid e half height))]
    (pushed (reduce taken (:vel e) shoves) (:push f))))

(defn- walked-to ^double [e pos]
  (+ (double (or (:walked e) 0.0))
     (* 0.6 (Math/sqrt (v/dist-sq (:pos e) pos)))))

(defn- walk-of [e prev pos]
  (let [w (if prev (walked-to prev pos) 0.0)]
    (if (and prev (not (== w (double (or (:walked prev) 0.0)))))
      w
      (:walked e))))

(defn- travel-of
  "Returns the position, speed, ground, jump cooldown, support and
  no-blocks flag of mob e after its move this tick, the fluid over
  it after the move when the move looked, then the fluid it stood in
  and whether it rested."
  [world index eid e h ht]
  (let [f (fluid-at world e h ht)
        moving? (not (zero? (double (:zza (:move e) 0.0))))
        vel (own-vel index eid e h ht f)
        rest? (at-rest? world e h moving? (in-fluid? f) vel)]
    (if rest? (rest-move e f) (physics-move world e vel h ht f))))

(defn- settled
  [e [_ _ og cd sup nb?] pos fall v g rest? [yaw hy body] look
   walked came]
  (let [w (pos? (double (:water g))) l (pos? (double (:lava g)))]
    (entity/with e {:pos pos :vel v :on-ground og :jump-cd cd
                    :fall fall
                    :arrived came
                    :support sup :no-blocks? nb?
                    :wet? (flag (if rest? false (:wet? e)) w)
                    :in-lava? (flag (:in-lava? e) l)
                    :yaw yaw :head-yaw hy :body body
                    :pitch (if look (nth look 1) (:pitch e))
                    :look (if look (nth look 2) (:look e))
                    :walked walked})))

(defn- body-of-move [world e pos look]
  (let [moved? (shifted? (:pos e) pos)
        head (if look (nth look 0) (:head-yaw e))]
    (control/body-turn e head moved? (:tick world))))

(defn- pushed-back
  "Returns the speed of mob e after it shoves the bodies it touches
  at pos and takes their push back, the shoves, and whether cram
  hurt it first; live? says which bodies are alive now."
  [world index eid e [half height pos vel] cram live?]
  (let [hurt (when cram (cram e pos))
        shoves (push/shoves-at index eid pos half height)
        v (shoved world (or hurt e) vel shoves live?)]
    [v shoves (some? hurt)]))

(defn- moved-y ^double [tr]
  (if-let [mv (nth tr 11 nil)] (phys/moved-y mv) 0.0))

(defn- dry?
  "Returns true when the mob that moved by tr was out of water when
  its move began and is out of it after, as Entity.baseTick and
  LivingEntity.checkFallDamage:375 look."
  [tr]
  (and (zero? (double (:water (nth tr 7))))
       (zero? (double (:water (nth tr 6))))))

(defn- fall-before ^double [world e tr]
  (if (dry? tr)
    (landing/cleared world (:pos e) (nth tr 0)
                     (landing/before-move e (nth tr 7)))
    0.0))

(defn- fall-after ^double [tr ^double f0]
  (if (dry? tr) (phys/fallen f0 (moved-y tr)) 0.0))

(defn- lands? [tr ^double f0 ^double f1]
  (and (nth tr 2) (or (pos? f0) (pos? f1))))

(defn- physics-shoves
  "Returns mob e after one tick of movement, the shoves it gave,
  whether cramming hurt it, as pushed-back gives them, and the
  deltas of its landing."
  [world index eid e half height [look prev cram live?]]
  (let [h (double (float half)) ht (double (float height))
        tr (travel-of world index eid e h ht)
        f0 (fall-before world e tr) f1 (fall-after tr f0)
        ls (when (lands? tr f0 f1)
             (landing/landed world eid e (nth tr 0) (nth tr 4) f0 f1))
        pos (if ls (nth ls 0) (nth tr 0))
        fall (landing/kept e (if (nth tr 2) 0.0 f1))
        box [half height pos (nth tr 1)]
        [v shoves hit?] (pushed-back world index eid e box cram live?)
        g (or (nth tr 6) (fluid-after world pos h ht v))
        hd (body-of-move world e pos look)
        e2 (settled e tr pos fall v g (nth tr 8) hd look
                    (walk-of e prev pos)
                    (push/arrived e pos (:tick world) eid))]
    [(cond-> e2 (identical? :rabbit (:type e))
       (rabbit/bumped (nth tr 9 nil) (nth tr 10 nil)))
     shoves hit? (when ls (nth ls 1))]))

(defn- physics [world index eid e half height]
  (nth (physics-shoves world index eid e half height nil) 0))

(def ^:private ^:const say-rest 120)

(def ^:private ^:const say-mean 40)

(defn- sound-pitch ^double [e ^long t ^long eid]
  (let [base (if (mobs/baby? e) 1.5 1.0)]
    (+ base (* 0.2 (- (random/of-longs t eid (hash :p1))
                      (random/of-longs t eid (hash :p2)))))))

(defn- wide-pitch ^double [^long t ^long eid kind]
  (+ 1.0 (* 0.4 (- (random/of-longs t eid (hash kind) (hash :w1))
                   (random/of-longs t eid (hash kind) (hash :w2))))))

(defn- water-vol ^double [vel3 k]
  (let [vx (v/x vel3) vy (v/y vel3) vz (v/z vel3)]
    (min 1.0 (* (Math/sqrt (+ (* vx vx 0.2) (* vy vy) (* vz vz 0.2)))
                (double k)))))

(defn- next-say ^long [^long t ^long eid]
  (+ t say-rest (mobs/exp-delay say-mean t eid :say)))

(defn- said [eid e t st]
  (let [t (long t) eid (long eid) say (mobs/sound-of e :say)]
    (cond (nil? say) [e nil]
          (nil? st) [(assoc e :say-tick (next-say t eid)) nil]
          :else
          (let [p (sound-pitch e t eid)
                s (out/sound say (:pos e) 1.0 p)]
            [(assoc e :say-tick (next-say t eid)) [(out/all s)]]))))

(defn- ambient [eid e t]
  (let [st (:say-tick e)]
    (if (and st (< (long t) (long st)))
      [e nil]
      (said eid e t st))))

(defn- step-state ^long [world e]
  (let [ch (:chunks world) p (:pos e)
        st (motion/below-state ch p (:support e) 0.2)
        [x _ z] (or (:support e) (mapv #(Math/floor (double %)) p))
        y (inc (long (Math/floor (- (v/y p) 0.2))))
        up (sense/block-at world (long x) y (long z))]
    (if (or (block/tagged? up "inside_step_sound_blocks")
            (block/tagged? up "combination_step_sound_blocks"))
      up
      st)))

(defn- block-step [world e]
  (let [st (step-state world e)]
    (when-not (block/air? st)
      (let [info (data/info (block/block-of st))
            s (get (data/sounds) (:sound info))
            vol (double (float (* (float (:volume s)) (float 0.15))))]
        (out/all (out/sound (:step s) (:pos e) vol (:pitch s)))))))

(defn- step-sound-delta [world e t eid]
  (cond
    (:wet? e)
    (let [vol (water-vol (:vel e) 0.35)
          pitch (wide-pitch (long t) (long eid) :swm)]
      (out/all (out/sound :swim (:pos e) vol pitch)))
    (not (:on-ground e)) nil
    (:block-steps? (mobs/types (:type e))) (block-step world e)
    :else
    (when-let [snd (mobs/sound-of e :step)]
      (out/all (out/sound snd (:pos e) 0.15 1.0)))))

(defn- splash-delta [e t eid]
  (out/all (out/sound :splash (:pos e) (water-vol (:vel e) 0.2)
                      (wide-pitch (long t) (long eid) :spl))))

(defn- movement-sounds
  [world acc e was-wet? old-walked new-walked t eid]
  (let [acc (if (and (:wet? e) (not was-wet?))
              (conj acc (splash-delta e t eid))
              acc)]
    (if (> (long (Math/floor (double new-walked)))
           (long (Math/floor (double old-walked))))
      (if-let [d (step-sound-delta world e t eid)] (conj acc d) acc)
      acc)))

(defn- diff-step [o n a c c' k]
  (let [g (symbol (str ".-" (name k)))]
    [c' `(if (v/same? (~g ~n) (~g ~o))
           ~c
           (do (aset ~a ~c ~k)
               (aset ~a (unchecked-inc ~c) (~g ~n))
               (unchecked-add ~c 2)))]))

(defn- diff-size [o n k]
  (let [g (symbol (str ".-" (name k)))]
    `(if (v/same? (~g ~n) (~g ~o)) 0 2)))

(defmacro ^:private diff-fields [old new & ks]
  (let [o (with-meta (gensym "o") {:tag 'Mob})
        n (with-meta (gensym "n") {:tag 'Mob})
        a (with-meta (gensym "a") {:tag 'objects})
        cs (vec (repeatedly (inc (count ks)) #(gensym "c")))
        step (fn [i k] (diff-step o n a (cs i) (cs (inc i)) k))
        add (fn [acc k] `(unchecked-add ~acc ~(diff-size o n k)))
        size (reduce add 0 ks)]
    `(let [~o ~old ~n ~new ~a (object-array ~size)
           ~(cs 0) 0 ~@(mapcat step (range) ks)]
       (clojure.lang.PersistentArrayMap. ~a))))

(defn- mob-changes [old new]
  (diff-fields old new :pos :vel :yaw :pitch :on-ground :task :follow
               :no-action :baby-until :tempt-cooldown-until :say-tick
               :stick-cooldown-until :egg-at
               :walked :head-yaw :look :jump-cd :wet? :sheared? :nav
               :move :jump :body :follow-at :in-lava? :float? :support
               :no-blocks? :arrived :hop :fall))

(defn- age-up [e t]
  (if (and (mobs/baby? e) (>= (long t) (long (:baby-until e))))
    (assoc e :baby-until nil)
    e))

(defn- brain-step [world eid e t tempters dead?]
  (let [[e1 deltas] (if dead? [e nil] (think world eid e t tempters))
        e1 (age-up e1 t)
        [e1 say-deltas] (if dead? [e1 nil] (ambient eid e1 t))]
    [e1 deltas say-deltas]))

(defn- steered [world e speed half]
  (let [[e nav m] (nav/aim world e)]
    (control/tick world e speed (* 2.0 (double half)) nav m)))

(defn- sensed [world e speed half height t]
  (looked world (steered world e speed half) height t))

(defn- spent-jump [e dead?]
  (cond-> e
    (:jump e) (entity/with {:jump false})
    dead? (entity/with {:move (Steer/halted (:move e))})))

(defn- move-speed ^double [e]
  (if-let [fx (not-empty (:effects e))]
    (attribute/value e fx :movement-speed)
    (mobs/speed (:type e))))

(defn- joined-into [acc more]
  (if (zero? (count more)) acc (into acc more)))

(defn- stepped-deltas [world eid e e2 [pre ds say-ds] t]
  (let [changes (mob-changes e e2)
        acc (if (pos? (count changes))
              (conj (vec pre) [:merge-entity eid changes])
              (vec pre))
        acc (-> acc (joined-into ds) (joined-into say-ds))]
    (movement-sounds world acc e2 (boolean (:wet? e))
                     (double (or (:walked e) 0.0))
                     (double (or (:walked e2) 0.0)) t eid)))

(defn- minded
  "Returns mob e after its base tick, after it thought and steered
  this tick, the head it turns, and its deltas and sounds. Other
  bodies do not move it yet."
  [world tempters eid e t]
  (let [[e pre] (living/based world eid e)
        [half height] (mobs/box-of e)
        speed (move-speed e)
        dead? (not (pos? (double (:health e))))
        e0 (spent-jump e dead?)
        [e1 ds say-ds] (brain-step world eid e0 t tempters dead?)
        e1 (cond dead? e1
                 (identical? :rabbit (:type e1))
                 (rabbit/steered world eid e1 speed half)
                 :else (steered world e1 speed half))
        look (when-not dead? (look-of world e1 height t))]
    [e1 look ds say-ds pre]))

(defn- step-mob
  ([world index eid e mind t]
   (step-mob world index eid e mind t nil nil))
  ([world index eid e [e1 look ds say-ds pre] t cram live?]
   (let [[half height] (mobs/box-of e)
         more [look e cram live?]
         [e2 shoves hit? ls]
         (physics-shoves world index eid e1 half height more)
         [e2 own] (if-let [f (ai-steps (:type e))]
                    (f eid e2 t)
                    [e2 nil])
         ds (stepped-deltas world eid e e2 [pre ds say-ds] t)]
     [e2 (joined-into ds own) shoves hit? ls])))

(defn- hand
  "Returns the delta that hands the shove sh to mob e in slot j, whose
  speed this tick so far is in vels, or nil when e takes no shove."
  [e ^objects vels j [eid dx dz]]
  (when (mobs/mob-type? (:type e))
    (let [j (int j)
          vel (or (aget vels j) (:vel e))
          v (v/v3 (- (v/x vel) (double dx)) (v/y vel)
                  (- (v/z vel) (double dz)))]
      (aset vels j v)
      [:merge-entity eid {:vel v}])))

(defn- takes-now? [^booleans ticking ^long i ^long j]
  (or (< j i) (not (aget ticking j))))

(defn- taker [ticking slots es i sh]
  (let [j (push/slot slots (nth sh 0))]
    (when (and (>= j 0) (takes-now? ticking i j)
               (push/alive? (nth (nth es j) 1)))
      j)))

(defn- handing [ticking vels slots es i shoves]
  (let [f (fn [acc sh]
            (let [j (taker ticking slots es i sh)
                  d (when j (hand (nth (nth es j) 1) vels j sh))]
              (if d (conj acc d) acc)))]
    (reduce f [] shoves)))

(defn- takers
  "Returns the slot and the shove of each body that takes a shove
  of mob i now."
  [ticking slots es i shoves]
  (let [f (fn [acc sh]
            (if-let [j (taker ticking slots es i sh)]
              (conj (or acc []) [j sh])
              acc))]
    (reduce f nil shoves)))

(def ^:private ^:const cramming-damage 6.0)

(def ^:private ^:const cramming-key 0x63726d)

(defn- max-cramming ^long [world]
  (long (get (:rules world) :max-entity-cramming 24)))

(defn- live-of [slots es]
  (fn [o] (push/alive? (nth (nth es (push/slot slots o)) 1))))

(defn- crowd [index slots es eid e]
  (let [[half height] (mobs/box-of e)
        alive? (live-of slots es)
        f (fn [^long n o] (if (alive? o) (inc n) n))]
    (reduce f 0 (push/touching index eid e half height))))

(defn- crammed?
  "Returns true when mob e, crowded, takes cramming damage this tick,
  as LivingEntity.pushEntities."
  [world index slots es eid e pos t]
  (and (push/alive? e)
       (< (random/of-longs t eid cramming-key) 0.25)
       (let [m (max-cramming world)
             e (assoc e :pos pos)]
         (and (pos? m) (> (crowd index slots es eid e) (dec m))))))

(def ^:private crush {:type :cramming})

(defn- cram-of
  "Returns the fn that hurts mob eid of es when it stands crammed,
  as LivingEntity.pushEntities does before it pushes."
  [world index slots es eid t]
  (fn [e pos]
    (when (crammed? world index slots es eid e pos t)
      (entity/hurt (assoc e :pos pos) cramming-damage crush t eid))))

(defn- stepping? [^booleans ticking es ^long i]
  (and (aget ticking i) (mobs/mob-type? (:type (nth (nth es i) 1)))))

(defn- run-turn
  "Returns mob i of es after its step from mind, as minded returns
  it, and its cramming, its deltas and the shoves it gave."
  [world index t slots es i mind]
  (let [[eid e] (nth es i)
        cram (cram-of world index slots es eid t)
        [e2 ds shoves hit? ls]
        (step-mob world index eid e mind t cram (live-of slots es))
        cs (when hit? [[:damage eid cramming-damage crush]])
        [e2 ds] (living/touched world eid e2 (:wet? e) ds ls cs)]
    [e2 ds shoves]))

(defn- live
  "Returns world as mob eid sees it in its turn: with the blocks the
  turns before it wrote this tick."
  [world ^long eid]
  (let [w (overlay/seen world eid)]
    (if (identical? w world)
      w
      (assoc w :watchers (:watchers world)))))

(defn- turn [world ticking vels tempters t index slots es i]
  (let [[eid e] (nth es i)
        world (live world eid)
        [e2 ds shoves]
        (if (stepping? ticking es i)
          (run-turn world index t slots es i
                    (minded world tempters eid e t))
          [e nil nil])
        es (assoc! es i [eid e2])
        hs (handing ticking vels slots es i shoves)
        from (when-not (identical? (:pos e) (:pos e2)) (:pos e))]
    [es (if (seq hs) (into (vec ds) hs) ds) from]))

(defn- reindexed [index es i from]
  (if from
    (let [[eid e] (nth es i)] (push/moved index eid from e))
    index))

(defn- island
  "Returns the island of herd h: its bodies with their index, their
  slots and whether each ticks."
  [{:keys [es bodies at]}]
  {:es es :index (push/grid-of bodies at)
   :ticking (push/ticks-of bodies at)
   :slots (push/slots-of bodies at)})

(defn- walk
  "Returns the deltas of the island isl stepped in the order of its
  bodies."
  [world tempters t {:keys [es index ticking slots]}]
  (let [vels (object-array (count es))]
    (loop [i 0 es (transient es) index index acc (transient [])]
      (if (= i (count es))
        (persistent! acc)
        (let [[es ds from]
              (turn world ticking vels tempters t index slots es i)]
          (recur (inc i) es (reindexed index es i from)
                 (reduce conj! acc ds)))))))

(defn- step-island [world tempters t h]
  (walk world tempters t (island h)))

(def ^:private ^:const step-reach 0.5)

(defn- mind-of
  "Returns the fn that keeps in minds what mob i of the island isl
  thinks."
  [world tempters t {:keys [es ticking]} ^objects minds]
  (fn [i]
    (let [i (int i)]
      (when (stepping? ticking es i)
        (let [[eid e] (nth es i)
              m (minded (live world eid) tempters eid e t)]
          (aset minds i m))))))

(defn- placed! [index ^objects cur i eid e e2]
  (when-not (identical? (:pos e) (:pos e2))
    (push/moved index eid (:pos e) e2))
  (aset cur i [eid e2]))

(defn- body-of
  "Returns the fn that moves mob i of the island isl as it thought,
  and keeps its run in runs. It marks out when ok? finds the mob out
  of reach."
  [world t {:keys [ticking index slots]}
   [^objects minds cur ^objects runs] out ok?]
  (fn [i]
    (let [i (int i) m (aget minds i)]
      (when m
        (let [[eid e] (nth cur i)
              [e2 ds shoves]
              (run-turn (live world eid) index t slots cur i m)]
          (when-not (ok? e e2) (aset ^booleans out 0 true))
          (placed! index cur i eid e e2)
          (aset runs i [ds (takers ticking slots cur i shoves)]))))))

(defn- joiner
  "Returns the fn that adds to collector c the run of mob i of runs,
  then the shoves it hands to the bodies before it, which end the
  tick as cur."
  [c ^objects cur ^objects runs]
  (let [vels (object-array (alength runs))
        f (fn [_ [j sh]]
            (when-let [d (hand (nth (aget cur (int j)) 1) vels j sh)]
              (deltas/collect! c d)))]
    (fn [i]
      (when-let [run (aget runs (int i))]
        (deltas/collect-all! c (nth run 0))
        (reduce f nil (nth run 1))))))

(defn- turn-runs
  "Returns the deltas of the island isl in the order of its mobs,
  each run with the shoves it hands to the bodies before it, or nil
  when ok? finds a mob out of reach. Each mob thinks in parallel and
  moves as soon as the mobs it could meet before it moved. The array
  cur ends with the bodies after the tick."
  [world tempters t isl cur ok?]
  (let [n (count (:es isl)) minds (object-array n)
        runs (object-array n) out (boolean-array 1)
        c (deltas/collecting)]
    (push/turns (push/pinned (:index isl) step-reach) step-reach
                (mind-of world tempters t isl minds)
                (body-of world t isl [minds cur runs] out ok?)
                (joiner c cur runs))
    (when-not (aget out 0) (deltas/collected c))))

(defn- near-start? [e e2] (push/within? e e2 step-reach))

(defn- mob? [[_ e]] (mobs/mob-type? (:type e)))

(defn- ahead-island
  "Returns the deltas of herd h as step-island steps it, and the
  count of runs that ran again. Mobs that cannot meet run in
  parallel, and the herd keeps the runs when ok? finds every mob in
  reach; else it steps again in order."
  [world tempters t h ok?]
  (let [isl (island h) cur (object-array (:es isl))
        d (turn-runs world tempters t isl cur ok?)]
    (if d
      [d 0]
      [(deltas/of-vec (step-island world tempters t h))
       (count (filter mob? (:es h)))])))

(def ^:private ^:const ahead-bodies 64)

(defn- ahead? [h]
  (and (>= (count (:es h)) ahead-bodies)
       (> (Islands/threads @r/pool) 1)))

(defn- island-deltas [world tempters t h]
  (if (ahead? h)
    (nth (ahead-island world tempters t h near-start?) 0)
    (deltas/of-vec (step-island world tempters t h))))

(def ^:private ^:const batch-bodies 32)

(defn- batched [[acc b ^long n] h]
  (let [b (conj b h) n (+ n (count (:es h)))]
    (if (>= n batch-bodies) [(conj acc b) [] 0] [acc b n])))

(defn- batches [islands]
  (let [[acc b] (reduce batched [[] [] 0] islands)]
    (cond-> acc (seq b) (conj b))))

(defn- island-batch [world tempters t batch]
  (let [f (fn [acc h]
            (->> (island-deltas world tempters t h)
                 (deltas/merge acc)))]
    (reduce f deltas/empty-deltas batch)))

(def ^:private biters {:sheep sheep/biting? :rabbit rabbit/raiding?})

(defn- biting? [active t [_ e]]
  (when-let [f (biters (:type e))]
    (and (f e t) (areas/active-at? active (:pos e)))))

(defn- ended? [active [_ e]]
  (and (mobs/mob-type? (:type e)) (mobs/death-ends? e)
       (areas/active-at? active (:pos e))))

(defn- kept! [^objects acc i entry]
  (aset acc i (conj! (aget acc i) entry)))

(defn- scan
  "Returns the fn that adds entry to the bodies, the biters or the
  endings in acc."
  [held active t]
  (fn [^objects acc [_ e :as entry]]
    (cond
      (push/body? held entry)
      (let [ticks? (areas/active-at? active (:pos e))]
        (push/add-body (aget acc 0) entry ticks?)
        (when (biting? active t entry) (kept! acc 1 entry)))
      (ended? active entry) (kept! acc 2 entry))
    acc))

(defn- scanned ^objects []
  (object-array [(push/bodies) (transient []) (transient [])]))

(defn- herd-of [b at]
  (let [es (push/entries-of b at)]
    (when (some mob? es) {:es es :bodies b :at at})))

(defn- herds
  "Returns the herds of world, the islands of bodies with a mob among
  them, then the mobs that bite this tick and the mobs whose death
  ends, all in id order."
  [world active t]
  (let [f (scan (areas/loaded-zone world) active t)
        ^objects acc (reduce f (scanned) (:entities world))
        b (aget acc 0)]
    [(into [] (keep #(herd-of b %)) (push/groups b))
     (persistent! (aget acc 1)) (persistent! (aget acc 2))]))

(defn- bitten [world tempters t [eid e]]
  (let [ds (nth (minded (live world eid) tempters eid e t) 2)]
    (overlay/wrote world eid ds)))

(defn- seen
  "Returns world with the bites of this tick in the overlay. A mob
  bites in its turn and sees the writes of each turn before it, as
  in the level."
  [world tempters t biters]
  (let [world (assoc world :watchers (animal/watchers world))
        bf (fn [w m] (bitten w tempters t m))]
    (reduce bf world biters)))

(defn- endings [ends]
  (into [] (mapcat (fn [[eid e]] (living/ended eid e))) ends))

(defn turns
  "Returns the deltas of the mobs in one tick, each in its turn,
  and the answers to the clicks of players on them. Each island of
  mobs steps on its own."
  [world d]
  (let [t (long (:tick world))
        active (areas/active-chunks world)
        tempters (sense/holders world)
        [hs biters ends] (herds world active t)
        world (seen world tempters t biters)
        clicks (interact-deltas world (:input d) t)]
    (deltas/merge
      (->> (batches hs)
           (deltas/fold-merged #(island-batch world tempters t %)))
      (deltas/of-vec (into (endings ends) clicks)))))
