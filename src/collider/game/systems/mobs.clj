(ns collider.game.systems.mobs
  "Mob thinking, movement and sounds."
  (:require [collider.data :as data]
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
  (:import (collider.game.entity Mob)
           (collider.world.phys Move)))

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
  "Tells whether the mob's box is within the player's entity reach,
  measured from the eye and with the buffer a click may have."
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
  "Returns what an interact event gives its handlers, nil when the
  click reaches nothing: no mob, or one too far away."
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
  "The place of the name tag in the chain; naming waits for its own
  ticket and lets every other handler have the click."
  [_ctx]
  nil)

(defn- leash-result
  "The place of the lead in the chain; it waits for its own ticket."
  [_ctx]
  nil)

(defn- equippable-result
  "The place of wearable items in the chain; saddles and armour wait
  for their own ticket."
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
  "Returns the deltas the whole chain leaves behind. A result that
  takes the click is heard as a game event, and the server swings
  the arm for the player when the client will not."
  [{:keys [peid p e hand t]} {:keys [result deltas]}]
  (cond-> (vec deltas)
    (not= :pass result)
    (into (signal/game-event :entity-interact (:pos e) peid))
    (= :success-server result)
    (into (state/swing-deltas peid p hand t true))))

(defn- interact
  "Returns the deltas of one player click on a mob. The handlers run
  in a fixed order and the first one that does not pass has the
  click; one out of reach is dropped without a word."
  [world ev t]
  (if-let [ctx (ctx-of world t ev)]
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
  "The product of two floats. The double product of two floats is
  exact, so rounding it is the float product itself."
  ^double [^double a ^double b] (double (float (* a b))))

(defn- fsub ^double [^double a ^double b] (double (float (- a b))))

(defn- fdiv ^double [^double a ^double b] (double (float (/ a b))))

(def ^:private ^:const deg->rad (double (float (/ Math/PI 180.0))))

(defn- yaw-radians
  "The yaw in radians, rounded the way a float yaw rounds it."
  ^double [^double yaw]
  (double (float (* (double (float yaw)) deg->rad))))

(defn- modified-friction
  "The friction a plain mob feels underfoot. It is not the identity:
  the two float steps shift the value."
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
  "The factor k names for every block state, each of them a float
  widened again, as the value is written out in decimal."
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
  "The block whose motion the mob feels: under the block it rests
  on, not under its feet."
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
  "The friction of the block that carries the mob."
  ^double [world pos sup]
  (modified-friction
    (factor-of @frictions (below-state world pos sup))))

(defn- speed-factor
  "The block the mob stands in slows it, or the one below it when
  the one it stands in is an ordinary block."
  ^double [world pos sup]
  (let [^doubles a @speed-factors
        st (feet-state world pos)
        here (factor-of a st)]
    (if (or (not (== here 1.0))
            (block/water? st) (liquid/bubble-column? st))
      here
      (factor-of a (below-state world pos sup)))))

(defn- jump-factor
  "Honey underfoot or around shortens a jump."
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
  "The mob with its head turned toward what it watches. It turns
  with the other controls, before the mob travels."
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
  "Whether the tick would leave the mob exactly where it stands:
  no drive, no fluid, no push, whole blocks under its feet."
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
  "The velocity with the drive of the mob added. The yaw turns the
  drive, and a drive past one is cut back to one."
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
  "What block friction leaves of the drive. Plain ground is
  slippery enough to take the branch, and a body in the air has a
  speed of its own."
  ^double [og? ^double bf ^double speed]
  (if og?
    (if (> bf 0.6)
      (fmul speed (fdiv walk-drive (fmul (fmul bf bf) bf)))
      speed)
    flying-speed))

(defn- hit-wall?
  "Whether the blocks stopped the body sideways."
  [drive vel]
  (or (not= (v/x drive) (v/x vel)) (not= (v/z drive) (v/z vel))))

(defn- fluid-fall
  "The slow sink of a body left to itself in a fluid."
  ^double [^double g falling? ^double vy]
  (if (zero? g)
    vy
    (if (and falling? (>= (Math/abs (- vy 0.005)) 0.003)
             (< (Math/abs (- vy (/ g 16.0))) 0.003))
      -0.003
      (- vy (/ g 16.0)))))

(defn- stepped
  "The move of a body with the step height of a walking mob."
  ^Move [world pos vel half height]
  (phys/move (:chunks world) pos vel half height max-up-step))

(defn- supported
  "The block the box now rests on and whether nothing at all stood
  under it. A move that found nothing looks again where the box
  came from."
  [world e ^Move mv half]
  (if-not (.on-ground mv)
    [nil false]
    (let [ch (:chunks world) p (.pos mv) o (:pos e)
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
  "A body against a wall with room above it climbs out of the
  fluid."
  [world pos vel half height oy hit?]
  (if (and hit? (climb-free? world pos vel half height oy))
    (v/v3 (v/x vel) out-of-fluid (v/z vel))
    vel))

(defn- travel-air
  "One tick of drive, step, gravity and friction out of any fluid."
  [world e vel half height og?]
  (let [bf (if og? (below-friction world (:pos e) (:support e)) 1.0)
        d (driven e vel (friction-speed og? bf (speed-of e)))
        ^Move mv (stepped world (:pos e) d half height)
        [sup nb?] (supported world e mv half)
        sf (speed-factor world (.pos mv) sup)
        f (fmul bf air-drag)
        u (.vel mv)]
    [(.pos mv)
     (v/v3 (* (* (v/x u) sf) f)
           (* (- (v/y u) gravity) vertical-drag)
           (* (* (v/z u) sf) f))
     (.on-ground mv) sup nb?]))

(defn- travel-water
  "One tick of the slow drive, step, slowdown and sink in water."
  [world e vel half height]
  (let [oy (v/y (:pos e)) falling? (<= (v/y vel) 0.0)
        d (driven e vel fluid-drive)
        ^Move mv (stepped world (:pos e) d half height)
        [sup nb?] (supported world e mv half)
        sf (speed-factor world (.pos mv) sup)
        u (.vel mv)
        vy (fluid-fall gravity falling? (* (v/y u) water-slowdown))
        w (v/v3 (* (* (v/x u) sf) water-slowdown) vy
                (* (* (v/z u) sf) water-slowdown))]
    [(.pos mv)
     (jumped-out world (.pos mv) w half height oy (hit-wall? d u))
     (.on-ground mv) sup nb?]))

(defn- lava-slowed
  "The slowdown of lava: a shallow pool holds a body the way water
  does, a deep one halves every axis."
  [x y z falling? shallow?]
  (let [x (* (double x) 0.5) y (double y) z (* (double z) 0.5)]
    (if shallow?
      (v/v3 x (fluid-fall gravity falling? (* y water-slowdown)) z)
      (v/v3 x (* y 0.5) z))))

(defn- travel-lava
  "One tick of the slow drive, the step and the sink in lava."
  [world e vel half height shallow?]
  (let [oy (v/y (:pos e)) falling? (<= (v/y vel) 0.0)
        d (driven e vel fluid-drive)
        ^Move mv (stepped world (:pos e) d half height)
        [sup nb?] (supported world e mv half)
        sf (speed-factor world (.pos mv) sup)
        u (.vel mv)
        w (lava-slowed (* (v/x u) sf) (v/y u) (* (v/z u) sf)
                       falling? shallow?)
        w (v/v3 (v/x w) (- (v/y w) (/ gravity 4.0)) (v/z w))]
    [(.pos mv)
     (jumped-out world (.pos mv) w half height oy (hit-wall? d u))
     (.on-ground mv) sup nb?]))

(defn- travelled
  "The branch the fluids around the mob pick. Water wins over lava
  when the mob stands in both."
  [world e vel half height og? {:keys [water lava threshold]}]
  (cond (pos? (double water)) (travel-water world e vel half height)
        (pos? (double lava))
        (travel-lava world e vel half height
                     (<= (double lava) (double threshold)))
        :else (travel-air world e vel half height og?)))

(defn- jump-power
  "The jump of 0.42, shortened by the block under the mob."
  ^double [world pos sup]
  (fmul jump-strength (jump-factor world pos sup)))

(defn- jump-off
  "The push up that never slows a rising body."
  [vel ^double p]
  (if (<= p min-jump)
    vel
    (v/v3 (v/x vel) (Math/max p (v/y vel)) (v/z vel))))

(defn- fluid-jumped
  "The nudge upwards a mob that floats gets in a fluid."
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
  "Whether the mob moved far enough this tick to carry its body."
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
  "Where a mob of that height looks from. The kinds that name their
  own eye height name one well clear of the only threshold that
  reads this."
  ^double [^double height]
  (* 0.85 height))

(defn- fluid-threshold
  "A mob with its eyes near the ground is never held up by a fluid."
  ^double [^double height]
  (if (< (eye-height height) 0.4) 0.0 jump-threshold))

(defn- fluid-of [world e half height]
  (liquid/fluid-info (:chunks world) (:pos e) half height (:vel e)))

(defn- in-fluid? [{:keys [water lava]}]
  (or (pos? (double water)) (pos? (double lava))))

(defn- flagged
  "The mob that knows the fluid it stands in now: what its goals
  and controls read in the next tick. A mob that stayed put keeps
  the reading of this tick."
  [world e half height kept]
  (let [f (or kept (fluid-of world e half height))
        w (pos? (double (:water f)))
        l (pos? (double (:lava f)))]
    (cond-> e
      (not= (boolean (:wet? e)) w) (assoc :wet? w)
      (not= (boolean (:in-lava? e)) l) (assoc :in-lava? l))))

(defn- pushed
  "The velocity a tick starts with: the push of other mobs and of
  the fluid current, then the dead band that follows."
  [vel push [cx cz]]
  (v/v3 (dead-band (+ (v/x vel) (double (nth push 0)) (double cx)))
        (dead-band (+ (v/y vel) (double (nth push 1))))
        (dead-band (+ (v/z vel) (double (nth push 2)) (double cz)))))

(defn- shoved
  "The mob after it has shoved the others: the shove goes straight
  into the velocity, and the dead band only meets it next tick."
  [e [dx dz]]
  (if (and (zero? (double dx)) (zero? (double dz)))
    e
    (let [vel (:vel e)
          x (+ (v/x vel) (double dx))
          z (+ (v/z vel) (double dz))]
      (assoc e :vel (v/v3 x (v/y vel) z)))))

(defn- ticked
  "Marks the mob as having a tick behind it: before its first one
  no shove of a higher eid carries over."
  [e]
  (if (:ticked? e) e (assoc e :ticked? true)))

(defn- physics [world index eid e half height]
  (let [t (long (:tick world))
        f (assoc (fluid-of world e half height)
                 :threshold (fluid-threshold height))
        moving? (not (zero? (double (:zza (:move e) 0.0))))
        vel (pushed (:vel e) (:push f)
                    (push/before index eid e half height))
        rest? (at-rest? world e half moving? (in-fluid? f) vel)
        e1 (if rest?
             (rest-step e t)
             (physics-move world e vel half height f))
        shove (push/after index eid e1 half height)
        e2 (ticked (shoved e1 shove))]
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
    (out/all (out/sound :swim (:pos e)
                        (water-vol (:vel e) 0.35)
                        (wide-pitch (long t) (long eid) :swm)))
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
   :no-blocks? :ticked?])

(defn- mob-changes
  "The parts of a mob that one tick left different."
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
  "The mob after its navigation, its controls and its head have
  run. The navigation aims the move in the same tick the move
  reads it, and both run every tick."
  [world e speed half height t]
  (let [n (control/tick world (nav/tick world e) speed
                        (* 2.0 (double half)))]
    (looked world n height t)))

(defn- spent-jump
  "The mob with its jump handed over to the physics. A dead mob is
  left without a drive."
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

(defn- step-island
  "One tick of an island: its mobs step in eid order, each of them
  seeing the ones before it where this tick has left them."
  [world tempters t es]
  (loop [i 0 es (vec es) index (push/index-of es) acc []]
    (if (= i (count es))
      acc
      (let [[eid e] (nth es i)
            [e2 ds] (if (mobs/mob-type? (:type e))
                      (step-mob world index tempters eid e t)
                      [e nil])
            moved? (not (identical? (:pos e) (:pos e2)))
            es (if moved? (assoc es i [eid e2]) es)]
        (recur (inc i) es (if moved? (push/index-of es) index)
               (into acc ds))))))

(defn- herds
  "The islands that have a mob to step; one of players alone moves
  nothing."
  [world active]
  (filter (fn [es] (some (fn [[_ e]] (mobs/mob-type? (:type e))) es))
          (push/islands world active)))

(defn- island-batch [world tempters t batch]
  (into [] (mapcat (fn [es] (step-island world tempters t es)))
        batch))

(defn mobs-system
  "The tasks of one tick: every island of mobs steps, and the
  clicks players sent are answered."
  [world d]
  (let [events (:input d)
        t (long (:tick world))
        active (state/active-chunks world)
        tempters (sense/holders world)
        batches (partition-all 32 (herds world active))]
    (conj (mapv (fn [batch]
                  #(island-batch world tempters t batch))
                batches)
          #(interact-deltas world events t))))
