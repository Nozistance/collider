(ns collider.game.systems.mobs
  "Mob thinking, movement and sounds."
  (:require [collider.data :as data]
            [collider.game.game-mode :as game-mode]
            [collider.random :as random]
            [collider.game.entity :as entity]
            [collider.vec :as v]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.control :as control]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.sheep :as sheep]
            [collider.game.mob.cow :as cow]
            [collider.game.mob.mooshroom :as mooshroom]
            [collider.game.mob.push :as push]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.game.systems.blocks.reach :as reach]
            [collider.world.env.signal :as signal]
            [collider.world.phys :as phys])
  (:import (collider.game.entity.records Mob)
           (collider.java Move)))

(set! *warn-on-reflection* true)

(def ^:private brains
  {:sheep sheep/brain :cow cow/brain
   :mooshroom mooshroom/brain})

(def ^:private specs
  {:sheep sheep/spec :cow cow/spec :mooshroom mooshroom/spec})

(defn- think [world eid e t tempters]
  (if-let [b (brains (:type e))]
    (b world eid e t tempters)
    [e nil]))

(def ^:private ^:const reach-buffer 3.0)

(defn- in-reach?
  "Returns true when the mob box is within the entity reach of p.
  The reach starts at the eye and allows a slack for the click."
  [p e]
  (let [[half height] (mobs/box-of e)
        [ex ey ez] (reach/eye-pos p)
        [x y z] (:pos e)
        w (* 2.0 (double half))
        dx (reach/axis-gap ex (- (double x) (double half)) w)
        dy (reach/axis-gap ey (double y) (double height))
        dz (reach/axis-gap ez (- (double z) (double half)) w)
        r (+ (state/entity-reach p) reach-buffer)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- ctx-of
  "Returns what an interact event gives its handlers. Returns nil
  when the click hits no mob or a mob out of reach."
  [world t [_ peid target hand sneaking?]]
  (let [p (get-in world [:entities peid])
        e (get-in world [:entities target])
        hand (if (#{:off 1} hand) :off :main)]
    (when (and p e (mobs/mob-type? (:type e)))
      (let [p (assoc p :sneaking? (boolean sneaking?))]
        (when (in-reach? p e)
          {:world world :t t :peid peid :p p :eid target :e e
           :hand hand :item (sense/in-hand p hand)})))))

(defn- name-tag-result
  "Returns nil, so the click goes on to the next handler. Name
  tags do not name mobs yet."
  [_ctx]
  nil)

(defn- leash-result
  "Returns nil, so the click goes on to the next handler. Leads
  do not attach yet."
  [_ctx]
  nil)

(defn- equippable-result
  "Returns nil, so the click goes on to the next handler. Mobs do
  not wear saddles or armour yet."
  [_ctx]
  nil)

(defn- species-result [ctx]
  (let [mushroom [mooshroom/bowl-result mooshroom/shear-result
                  mooshroom/flower-result cow/milk-result
                  animal/feed-result]
        fs (case (:type (:e ctx))
             :sheep [sheep/shear-result animal/feed-result]
             :cow [cow/milk-result animal/feed-result]
             :mooshroom mushroom
             nil)]
    (some (fn [f] (f ctx)) fs)))

(def ^:private chain
  [name-tag-result (partial animal/egg-result specs) leash-result
   species-result sheep/dye-result equippable-result])

(defn- answered
  "Returns the deltas that the chain leaves. A result that takes
  the click sends a game event. The server swings the arm of the
  player when the client does not."
  [{:keys [peid p e hand t]} {:keys [result deltas]}]
  (cond-> (vec deltas)
    (not= :pass result)
    (into (signal/game-event :entity-interact (:pos e) peid))
    (= :success-server result)
    (into (state/swing-deltas peid p hand t true))))

(defn- spectating? [world ev]
  (game-mode/spectator? (get-in world [:entities (nth ev 1)])))

(defn- interact
  "Returns the deltas of one player click on a mob. The handlers
  run in a fixed order. The first handler that does not pass takes
  the click. A click out of reach does nothing."
  [world ev t]
  (if-let [ctx (when-not (spectating? world ev) (ctx-of world t ev))]
    (answered ctx (or (some (fn [f] (f ctx)) chain) {:result :pass}))
    []))

(defn- interact-deltas [world events t]
  (state/fold-events world (filter #(= :interact (first %)) events)
                     (fn [w ev] (interact w ev t))))

(def ^:private ^:const sin-scale 10430.378350470453)

(def ^:private sin-table
  (let [a (float-array 65536)]
    (dotimes [i 65536]
      (aset a i (float (Math/sin (/ (double i) sin-scale)))))
    a))

(defn- mth-sin ^double [^double a]
  (aget ^floats sin-table
        (int (bit-and (long (* a sin-scale)) 65535))))

(defn- mth-cos ^double [^double a]
  (aget ^floats sin-table
        (int (bit-and (long (+ (* a sin-scale) 16384.0)) 65535))))

(defn- fmul
  "Returns the product of two floats as a float."
  ^double [^double a ^double b] (double (float (* a b))))

(defn- fsub ^double [^double a ^double b] (double (float (- a b))))

(defn- fdiv ^double [^double a ^double b] (double (float (/ a b))))

(def ^:private ^:const deg->rad (double (float (/ Math/PI 180.0))))

(defn- yaw-radians
  "Returns the yaw in radians with float precision."
  ^double [^double yaw]
  (double (float (* (double (float yaw)) deg->rad))))

(defn- modified-friction
  "Returns the friction that a plain mob feels on a block of
  friction f. The result is not always f."
  ^double [^double f]
  (Math/clamp (fsub 1.0 (fsub 1.0 f)) 0.0 1.0))

(def ^:private ^:const gravity 0.08)

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

(def ^:private ^:const below-offset (double (float 0.500001)))

(def ^:private ^:const jump-threshold 0.4)

(def ^:private ^:const max-up-step 0.6)

(defn- motion-table
  "Returns the factor that key k names for every block state, or
  default. Each factor has float precision."
  ^doubles [k ^double default]
  (let [a (double-array (data/block-state-count) default)]
    (doseq [[_ b] (data/blocks)
            :let [v (k b)] :when v
            i (range (reduce * 1 (map count (vals (:props b)))))]
      (aset a (+ (long (:first b)) (long i))
            (double (float (double v)))))
    a))

(def ^:private ^:table frictions
  (delay (motion-table :friction (double (float 0.6)))))

(def ^:private ^:table speed-factors
  (delay (motion-table :speed-factor 1.0)))

(def ^:private ^:table jump-factors
  (delay (motion-table :jump-factor 1.0)))

(defn- factor-of ^double [^doubles a ^long st]
  (if (< -1 st (alength a)) (aget a st) (aget a 0)))

(defn- below-state
  "Returns the block state whose motion the mob feels. It is below
  the mob in the column of sup, the block the mob rests on."
  ^long [world pos sup]
  (let [y (long (Math/floor (- (v/y pos) below-offset)))]
    (if sup
      (sense/block-at world (long (nth sup 0)) y (long (nth sup 2)))
      (sense/block-at world (long (Math/floor (v/x pos))) y
                      (long (Math/floor (v/z pos)))))))

(defn- feet-state [world pos]
  (sense/block-at world (long (Math/floor (v/x pos)))
                  (long (Math/floor (v/y pos)))
                  (long (Math/floor (v/z pos)))))

(defn- below-friction
  "Returns the friction of the block that carries the mob."
  ^double [world pos sup]
  (modified-friction
    (factor-of @frictions (below-state world pos sup))))

(defn- speed-factor
  "Returns the speed factor of the block the mob stands in. When
  that block is ordinary, returns the factor of the block below."
  ^double [world pos sup]
  (let [^doubles a @speed-factors
        st (feet-state world pos)
        here (factor-of a st)]
    (if (or (not (== here 1.0))
            (block/water? st) (liquid/bubble-column? st))
      here
      (factor-of a (below-state world pos sup)))))

(defn- jump-factor
  "Returns the jump factor of the block the mob stands in. When
  that block has none, returns the factor of the block below."
  ^double [world pos sup]
  (let [^doubles a @jump-factors
        here (factor-of a (feet-state world pos))]
    (if (== here 1.0)
      (factor-of a (below-state world pos sup))
      here)))

(defn- look-toward [e height o]
  (let [[_ y _] (:pos e)
        [_ oy _] (:pos o)
        oh (double (get-in mobs/types [(:type o) :height] 1.0))
        eye (+ (double y) (* 0.95 (double height)))
        oeye (+ (double oy)
                (if (= :player (:type o)) 1.62 (* 0.95 oh)))
        dh (Math/sqrt (v/dist-sq (:pos e) (:pos o)))]
    [(v/yaw-toward (:pos e) (:pos o))
     (- (Math/toDegrees (Math/atan2 (- oeye eye) dh)))]))

(defn- active-look [e ^long t]
  (let [look (:look e)]
    (when (and look (> (long (:until look 0)) t)) look)))

(defn- look-angles [world e height look]
  (let [oid (:target look)
        target (when oid (get-in world [:entities oid]))]
    (cond
      target (look-toward e height target)
      (and look (:yaw look)) [(:yaw look) 0.0]
      :else [(:yaw e) 0.0])))

(defn- head-same? [e look ^double hy ^double hp]
  (and (identical? look (:look e))
       (let [oh (:head-yaw e)] (and oh (== (double oh) hy)))
       (let [op (:pitch e)] (and op (== (double op) hp)))))

(defn- looked
  "Returns the mob with its head turned toward what it watches.
  The head turns before the mob travels."
  [world e height t]
  (let [look (active-look e (long t))
        [dyaw dpitch] (look-angles world e height look)
        y0 (double (or (:head-yaw e) (:yaw e)))
        p0 (double (or (:pitch e) 0.0))
        hy (v/limit-angle y0 (double dyaw) 10.0)
        hp (v/limit-angle p0 (double dpitch) 40.0)]
    (if (head-same? e look (double hy) (double hp))
      e
      (entity/mob-looked e hy hp look))))

(def ^:private rest-vel
  (v/v3 0.0 (* (- 0.0 gravity) vertical-drag) 0.0))

(defn- dead-band ^double [^double a]
  (if (< (Math/abs a) 0.003) 0.0 a))

(defn- at-rest?
  "Returns true when the tick leaves the mob where it stands. Such
  a mob has no drive, fluid or push and stands on whole blocks."
  [world e half moving? fluid? vel]
  (let [pos (:pos e)
        x (v/x pos) y (v/y pos) z (v/z pos)
        still? (and (not moving?) (boolean (:on-ground e))
                    (not fluid?) (not (:jump e))
                    (zero? (v/x vel)) (zero? (v/z vel))
                    (neg? (v/y vel)))]
    (and still?
         (phys/standing-on-cubes? (:chunks world) x y z half))))

(defn- rest-step [e t]
  (control/body-tick
    (entity/mob-moved e (:pos e) rest-vel true (:yaw e) false nil)
    false (long t)))

(defn- speed-of ^double [e] (double (:speed (:move e) 0.0)))

(defn- driven
  "Returns vel with the drive of the mob added. The drive points
  along the yaw. A drive stronger than one counts as one."
  [e vel ^double speed]
  (let [zza (double (:zza (:move e) 0.0))
        l (* zza zza)]
    (if (< l 1.0E-7)
      vel
      (let [f (* (if (> l 1.0) (Math/signum zza) zza) speed)
            r (yaw-radians (double (:yaw e)))]
        (v/v3 (- (v/x vel) (* f (mth-sin r)))
              (v/y vel)
              (+ (v/z vel) (* f (mth-cos r))))))))

(defn- friction-speed
  "Returns the drive speed that the block friction bf leaves. A
  body in the air has a fixed speed of its own."
  ^double [og? ^double bf ^double speed]
  (if og?
    (if (> bf 0.6)
      (fmul speed (fdiv walk-drive (fmul (fmul bf bf) bf)))
      speed)
    flying-speed))

(defn- hit-wall?
  "Returns true when the blocks stop the body sideways."
  [drive vel]
  (or (not= (v/x drive) (v/x vel)) (not= (v/z drive) (v/z vel))))

(defn- fluid-fall
  "Returns the vertical speed of a body that sinks in a fluid."
  ^double [^double g falling? ^double vy]
  (if (zero? g)
    vy
    (if (and falling? (>= (Math/abs (- vy 0.005)) 0.003)
             (< (Math/abs (- vy (/ g 16.0))) 0.003))
      -0.003
      (- vy (/ g 16.0)))))

(defn- stepped
  "Returns the move of a body that climbs steps like a walking mob."
  ^Move [world pos vel half height]
  (phys/move (:chunks world) pos vel half height max-up-step))

(defn- supported
  "Returns the block the box rests on after the move, and true
  when no block is under the box. When the move finds nothing, it
  looks again where the box came from."
  [world e ^Move mv half]
  (if-not (phys/on-ground? mv)
    [nil false]
    (let [ch (:chunks world) p (phys/pos mv) o (:pos e)
          sb (phys/supporting-block ch p half)
          s (or sb (when-not (:no-blocks? e)
                     (phys/supporting-block
                       ch (v/v3 (v/x o) (v/y p) (v/z o)) half)))]
      [(if (= s (:support e)) (:support e) s) (nil? sb)])))

(defn- climb-free? [world pos vel half height oy]
  (let [up (+ (v/y vel) out-of-fluid-reach
              (- (double oy) (v/y pos)))]
    (phys/free? (:chunks world) pos half height
                (v/x vel) up (v/z vel))))

(defn- jumped-out
  "Returns the velocity of a body that may climb out of a fluid.
  Only a body against a wall with room above it climbs."
  [world pos vel half height oy hit?]
  (if (and hit? (climb-free? world pos vel half height oy))
    (v/v3 (v/x vel) out-of-fluid (v/z vel))
    vel))

(defn- travel-air
  "Returns one tick of travel of a body out of any fluid."
  [world e vel half height og?]
  (let [bf (if og? (below-friction world (:pos e) (:support e)) 1.0)
        d (driven e vel (friction-speed og? bf (speed-of e)))
        ^Move mv (stepped world (:pos e) d half height)
        [sup nb?] (supported world e mv half)
        sf (speed-factor world (phys/pos mv) sup)
        f (fmul bf air-drag)
        u (phys/vel mv)]
    [(phys/pos mv)
     (v/v3 (* (* (v/x u) sf) f)
           (* (- (v/y u) gravity) vertical-drag)
           (* (* (v/z u) sf) f))
     (phys/on-ground? mv) sup nb?]))

(defn- travel-water
  "Returns one tick of travel of a body in water."
  [world e vel half height]
  (let [oy (v/y (:pos e)) falling? (<= (v/y vel) 0.0)
        d (driven e vel fluid-drive)
        ^Move mv (stepped world (:pos e) d half height)
        [sup nb?] (supported world e mv half)
        sf (speed-factor world (phys/pos mv) sup)
        u (phys/vel mv)
        vy (fluid-fall gravity falling? (* (v/y u) water-slowdown))
        w (v/v3 (* (* (v/x u) sf) water-slowdown) vy
                (* (* (v/z u) sf) water-slowdown))]
    [(phys/pos mv)
     (jumped-out world (phys/pos mv) w half height oy (hit-wall? d u))
     (phys/on-ground? mv) sup nb?]))

(defn- lava-slowed
  "Returns the velocity x y z slowed by lava. Lava halves the speed
  sideways. A shallow pool slows the fall as water does."
  [x y z falling? shallow?]
  (let [x (* (double x) 0.5) y (double y) z (* (double z) 0.5)]
    (if shallow?
      (v/v3 x (fluid-fall gravity falling? (* y water-slowdown)) z)
      (v/v3 x (* y 0.5) z))))

(defn- travel-lava
  "Returns one tick of travel of a body in lava."
  [world e vel half height shallow?]
  (let [oy (v/y (:pos e)) falling? (<= (v/y vel) 0.0)
        d (driven e vel fluid-drive)
        ^Move mv (stepped world (:pos e) d half height)
        [sup nb?] (supported world e mv half)
        sf (speed-factor world (phys/pos mv) sup)
        u (phys/vel mv)
        w (lava-slowed (* (v/x u) sf) (v/y u) (* (v/z u) sf)
                       falling? shallow?)
        w (v/v3 (v/x w) (- (v/y w) (/ gravity 4.0)) (v/z w))]
    [(phys/pos mv)
     (jumped-out world (phys/pos mv) w half height oy (hit-wall? d u))
     (phys/on-ground? mv) sup nb?]))

(defn- travelled
  "Returns one tick of travel in the fluid around the mob. Water
  wins over lava when the mob stands in both."
  [world e vel half height og? {:keys [water lava threshold]}]
  (cond (pos? (double water)) (travel-water world e vel half height)
        (pos? (double lava))
        (travel-lava world e vel half height
                     (<= (double lava) (double threshold)))
        :else (travel-air world e vel half height og?)))

(defn- jump-power
  "Returns the jump power that the block under the mob allows."
  ^double [world pos sup]
  (fmul jump-strength (jump-factor world pos sup)))

(defn- jump-off
  "Returns vel with a push up of p. The push never slows a body
  that rises faster."
  [vel ^double p]
  (if (<= p min-jump)
    vel
    (v/v3 (v/x vel) (Math/max p (v/y vel)) (v/z vel))))

(defn- fluid-jumped
  "Returns vel with the small push up that a mob gets in a fluid."
  [vel]
  (v/v3 (v/x vel) (+ (v/y vel) fluid-jump) (v/z vel)))

(defn- jumping-vel
  "Returns the velocity after the jump and whether the mob spent a
  jump off the ground on it."
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
      [(jump-off vel (jump-power world (:pos e) (:support e))) true]
      :else [vel false])))

(defn- shifted?
  "Returns true when the mob moves far enough sideways this tick
  to count as walking."
  [from to]
  (let [dx (- (v/x to) (v/x from)) dz (- (v/z to) (v/z from))]
    (> (+ (* dx dx) (* dz dz)) 2.5000003E-7)))

(defn- jump-delay [e ^long t jumped?]
  (cond jumped? (+ t 10) (:jump e) (:jump-cd e)))

(defn- physics-move [world e vel half height f]
  (let [t (long (:tick world))
        from (:pos e) og? (boolean (:on-ground e))
        ready? (>= t (long (or (:jump-cd e) 0)))
        [v jumped?] (if (:jump e)
                      (jumping-vel world e vel og? f ready?)
                      [vel false])
        [pos w ground? sup nb?]
        (travelled world e v half height og? f)
        vy (liquid/bubble-push (:chunks world) pos (v/y w))
        v2 (v/v3 (v/x w) vy (v/z w))
        cd (jump-delay e t jumped?)
        e (entity/mob-moved e pos v2 ground? (:yaw e) (:wet? e) cd)]
    (control/body-tick (assoc e :support sup :no-blocks? nb?)
                       (shifted? from pos) t)))

(defn- eye-height
  "Returns the eye height of a mob of that height. Kinds with their
  own eye height fall on the same side of the fluid threshold."
  ^double [^double height]
  (* 0.85 height))

(defn- fluid-threshold
  "Returns the fluid depth above which a jumping mob swims up. A
  mob with its eyes near the ground swims up in any fluid."
  ^double [^double height]
  (if (< (eye-height height) 0.4) 0.0 jump-threshold))

(defn- fluid-of [world e half height]
  (liquid/fluid-info (:chunks world) (:pos e) half height (:vel e)
                     (:dim world)))

(defn- in-fluid? [{:keys [water lava]}]
  (or (pos? (double water)) (pos? (double lava))))

(defn- flagged
  "Returns the mob with flags for the fluid it stands in now. Its
  goals and controls read the flags in the next tick. A mob at
  rest keeps the fluid reading of this tick."
  [world e half height kept]
  (let [f (or kept (fluid-of world e half height))
        w (pos? (double (:water f)))
        l (pos? (double (:lava f)))]
    (cond-> e
      (not= (boolean (:wet? e)) w) (assoc :wet? w)
      (not= (boolean (:in-lava? e)) l) (assoc :in-lava? l))))

(defn- pushed
  "Returns the velocity that a tick starts with. It adds the push
  of the fluid current to vel and ignores very small speeds."
  [vel push]
  (v/v3 (dead-band (+ (v/x vel) (double (nth push 0))))
        (dead-band (+ (v/y vel) (double (nth push 1))))
        (dead-band (+ (v/z vel) (double (nth push 2))))))

(defn- taken
  "Returns vel with one shove added."
  [vel [_ dx dz]]
  (v/v3 (+ (v/x vel) (double dx)) (v/y vel)
        (+ (v/z vel) (double dz))))

(defn- shoved
  "Returns the mob after its own shove. The mob takes one push
  from each body it meets, in order. Very small speeds stay until
  the next tick."
  [e shoves]
  (if (seq shoves)
    (assoc e :vel (reduce taken (:vel e) shoves))
    e))

(defn- physics [world index eid e half height]
  (let [t (long (:tick world))
        half (double (float half)) height (double (float height))
        f (assoc (fluid-of world e half height)
                 :threshold (fluid-threshold height))
        moving? (not (zero? (double (:zza (:move e) 0.0))))
        shoves (push/before index eid e half height)
        vel (pushed (reduce taken (:vel e) shoves) (:push f))
        rest? (at-rest? world e half moving? (in-fluid? f) vel)
        e1 (if rest?
             (rest-step e t)
             (physics-move world e vel half height f))
        e2 (shoved e1 (push/shoves index eid e1 half height))]
    (flagged world e2 half height (when rest? f))))

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

(defn- ambient [eid e t]
  (let [t (long t) eid (long eid) st (:say-tick e)
        say (mobs/sound-of e :say)]
    (cond (and st (< t (long st))) [e nil]
          (nil? say) [e nil]
          (nil? st) [(assoc e :say-tick (next-say t eid)) nil]
          :else
          (let [p (sound-pitch e t eid)
                s (out/sound say (:pos e) 1.0 p)]
            [(assoc e :say-tick (next-say t eid)) [(out/all s)]]))))

(defn- step-sound-delta [e t eid]
  (if (:wet? e)
    (let [vol (water-vol (:vel e) 0.35)
          pitch (wide-pitch (long t) (long eid) :swm)]
      (out/all (out/sound :swim (:pos e) vol pitch)))
    (when (:on-ground e)
      (when-let [snd (mobs/sound-of e :step)]
        (out/all (out/sound snd (:pos e) 0.15 1.0))))))

(defn- splash-delta [e t eid]
  (out/all (out/sound :splash (:pos e) (water-vol (:vel e) 0.2)
                      (wide-pitch (long t) (long eid) :spl))))

(defn- movement-sounds [acc e was-wet? old-walked new-walked t eid]
  (let [acc (if (and (:wet? e) (not was-wet?))
              (conj acc (splash-delta e t eid))
              acc)]
    (if (> (long (Math/floor (double new-walked)))
           (long (Math/floor (double old-walked))))
      (if-let [d (step-sound-delta e t eid)] (conj acc d) acc)
      acc)))

(defmacro ^:private diff-fields [old new & ks]
  (let [o (with-meta (gensym) {:tag 'Mob})
        n (with-meta (gensym) {:tag 'Mob})
        f (fn [k]
            (let [g (symbol (str ".-" (name k)))]
              [`(not (identical? (~g ~n) (~g ~o)))
               `(assoc ~k (~g ~n))]))]
    `(let [~o ~old ~n ~new]
       (cond-> {} ~@(mapcat f ks)))))

(def ^:private loose-keys
  [:nav :move :jump :body :follow-at :in-lava? :float? :support
   :no-blocks?])

(defn- mob-changes
  "Returns the parts of the mob that the tick changed."
  [old new]
  (let [changed (diff-fields
                  old new :pos :vel :yaw :pitch :on-ground :task
                  :follow :no-action :baby-until
                  :tempt-cooldown-until :say-tick :walked
                  :head-yaw :look :jump-cd :wet? :sheared?)]
    (reduce (fn [m k]
              (if (identical? (k old) (k new)) m (assoc m k (k new))))
            changed
            loose-keys)))

(defn- age-up [e t]
  (if (and (mobs/baby? e) (>= (long t) (long (:baby-until e))))
    (assoc e :baby-until nil)
    e))

(defn- brain-step [world eid e t tempters dead?]
  (let [[e1 deltas] (if dead? [e nil] (think world eid e t tempters))
        e1 (age-up e1 t)
        [e1 say-deltas] (if dead? [e1 nil] (ambient eid e1 t))]
    [e1 deltas say-deltas]))

(defn- walked-step [e prev now]
  (let [walked (double (or (:walked e) 0.0))
        d (Math/sqrt (v/dist3-sq (:pos prev) (:pos now)))
        walked' (+ walked (* 0.6 d))
        now (if (== walked walked') now (assoc now :walked walked'))]
    [now walked walked']))

(defn- sensed
  "Returns the mob after its navigation, controls and head run.
  The navigation aims the move in the same tick that the move
  reads it."
  [world e speed half height t]
  (let [width (* 2.0 (double half))
        n (control/tick world (nav/tick world e) speed width)]
    (looked world n height t)))

(defn- spent-jump
  "Returns the mob with its jump handed to the physics. A dead mob
  loses its drive."
  [e dead?]
  (cond-> e
    (:jump e) (assoc :jump false)
    dead? (assoc :move (assoc (:move e) :zza 0.0))))

(defn- step-mob [world index tempters eid e t]
  (let [[half height] (mobs/box-of e)
        speed (get-in mobs/types [(:type e) :speed])
        dead? (not (pos? (double (:health e))))
        e0 (spent-jump e dead?)
        [e1 ds say-ds] (brain-step world eid e0 t tempters dead?)
        e1 (if dead? e1 (sensed world e1 speed half height t))
        was-wet? (boolean (:wet? e))
        e2 (physics world index eid e1 half height)
        [e2 walked walked'] (walked-step e e1 e2)
        changes (mob-changes e e2)
        merged (when (seq changes) [[:merge-entity eid changes]])
        acc (cond-> (vec merged) ds (into ds) say-ds (into say-ds))]
    [e2 (movement-sounds acc e2 was-wet? walked walked' t eid)]))

(defn- ticks-at?
  "Returns true when the body stands where entity ticks run."
  [active [_ e]]
  (state/active-at? active (:pos e)))

(defn- handed
  "Returns the island after the body at j takes its part of a
  shove, the opposite of what the shoving body takes. Players do
  not take it, because their client moves them."
  [es acc j [eid dx dz]]
  (let [[_ e] (nth es j)]
    (if (mobs/mob-type? (:type e))
      (let [vel (:vel e)
            v (v/v3 (- (v/x vel) (double dx)) (v/y vel)
                    (- (v/z vel) (double dz)))]
        [(assoc es j [eid (assoc e :vel v)])
         (conj acc [:merge-entity eid {:vel v}])])
      [es acc])))

(defn- takes-now?
  "Returns true when the body at j takes the shove now. A body
  that already stepped ends its tick with the shove in its speed.
  A body in a chunk that does not tick gathers the shoves in its
  speed. A body still to step reads the shove on its own turn."
  [active es ^long i ^long j]
  (or (< j i) (not (ticks-at? active (nth es j)))))

(defn- steps?
  "Returns true when the body takes a turn of its own this tick.
  Only a mob in a chunk that runs entity ticks does."
  [active entry]
  (and (mobs/mob-type? (:type (nth entry 1)))
       (ticks-at? active entry)))

(defn- handing [index active slots es i]
  (let [[eid e] (nth es i)
        [half height] (mobs/box-of e)
        f (fn [[es acc] sh]
            (let [j (get slots (nth sh 0))]
              (if (and j (takes-now? active es i j))
                (handed es acc j sh)
                [es acc])))]
    (reduce f [es []] (push/shoves index eid e half height))))

(defn- handed-out
  "Returns the island after the body at i shoves every body it
  meets, as the last part of its tick. A body that did not step
  this tick shoves nobody."
  [index active slots es i]
  (if (steps? active (nth es i))
    (handing index active slots es i)
    [es nil]))

(defn- turn
  "Returns the island after the body at i takes its turn to step
  and shove. The result also holds the deltas of the turn and
  whether the body stayed in place."
  [world active tempters t index slots es i]
  (let [[eid e] (nth es i)
        [e2 ds] (if (steps? active (nth es i))
                  (step-mob world index tempters eid e t)
                  [e nil])
        es (assoc es i [eid e2])
        [es hs] (handed-out index active slots es i)]
    [es (into (vec ds) hs) (identical? (:pos e) (:pos e2))]))

(defn- step-island
  "Returns the deltas of one tick of an island. Its bodies step in
  eid order. Each body sees the earlier bodies where this tick
  left them, and gives out its shove on its turn. A body in a
  chunk that does not tick stands still and only takes shoves."
  [world active tempters t es]
  (let [slots (into {} (map-indexed (fn [i [eid _]] [eid i])) es)]
    (loop [i 0 es (vec es) index (push/index-of es) acc []]
      (if (= i (count es))
        acc
        (let [[es ds still?]
              (turn world active tempters t index slots es i)]
          (recur (inc i) es (if still? index (push/index-of es))
                 (into acc ds)))))))

(defn- herds
  "Returns the islands that have a mob to step. An island of
  players only moves nothing."
  [world]
  (filter (fn [es] (some (fn [[_ e]] (mobs/mob-type? (:type e))) es))
          (push/islands world (state/loaded-zone world))))

(defn- island-batch [world active tempters t batch]
  (into [] (mapcat (fn [es] (step-island world active tempters t es)))
        batch))

(defn mobs-system
  "Returns the tasks of one tick. Each island of mobs steps, and
  the clicks of players get answers."
  [world d]
  (let [events (:input d)
        t (long (:tick world))
        active (state/active-chunks world)
        tempters (sense/holders world)
        batches (partition-all 32 (herds world))]
    (conj (mapv (fn [batch]
                  #(island-batch world active tempters t batch))
                batches)
          #(interact-deltas world events t))))
