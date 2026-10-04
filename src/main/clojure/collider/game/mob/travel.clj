(ns collider.game.mob.travel
  "Mob moves in one tick, with walking, jumping, falling, swimming,
  shoving and landing."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.push :as push]
            [collider.game.mob.sense :as sense]
            [collider.game.mob.spec :as spec]
            [collider.game.turn.landing :as landing]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys])
  (:import (collider.world Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const deg->rad (double (float (/ Math/PI 180.0))))

(defn- yaw-radians ^double [^double yaw]
  (double (float (* (double (float yaw)) deg->rad))))

(defn- modified-friction ^double [^double f]
  (Math/clamp (num/fsub 1.0 (num/fsub 1.0 f)) 0.0 1.0))

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

(defn- fall-gravity
  "Returns the gravity mob e falls by at vertical speed vy. Slow
  falling lowers it only while e does not rise."
  ^double [e ^double vy]
  (if (and (<= vy 0.0) (contains? (:effects e) :slow-falling))
    (Math/min gravity slow-fall-gravity)
    gravity))

(defn- levitation [e] (get (:effects e) :levitation))

(defn- lifted
  "Returns vertical speed vy of mob e after one tick of gravity.
  Levitation takes the place of gravity."
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

(defn- cubed ^double [^double f]
  (num/fmul (num/fmul f f) f))

(defn- friction-speed ^double [og? ^double bf ^double speed]
  (if og?
    (if (> bf 0.6)
      (num/fmul speed (num/fdiv walk-drive (cubed bf)))
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
  velocity with the push of that fluid. A mob that stays in the same
  dry cells keeps the fluid it had."
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
        k (num/fmul bf air-drag)
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
    (num/fmul jump-boost (double (float (inc (long (:amplifier b))))))
    0.0))

(defn- jump-share ^double [e]
  (if-let [f (:jump-share (spec/of (:type e)))] (f e) 1.0))

(defn- jump-power
  "Returns the upward speed mob e jumps with. The block under it
  and jump boost change it."
  ^double [world e]
  (let [f (jump-factor world (:pos e) (:support e))
        s (num/fmul jump-strength (jump-share e))]
    (double (float (+ (num/fmul s f) (boost-power e))))))

(defn- jump-off [vel ^double p]
  (if (<= p min-jump)
    vel
    (v/v3 (v/x vel) (Math/max p (v/y vel)) (v/z vel))))

(defn- fluid-jumped [vel]
  (v/v3 (v/x vel) (+ (v/y vel) fluid-jump) (v/z vel)))

(defn- hop-off [e vel]
  (if-let [f (:hop (spec/of (:type e)))] (f e vel) vel))

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

(defn- kept-flag [old now]
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
  (if (and (seq shoves) (push/pushable? (:chunks world) e))
    (let [es (:entities world)
          live? (or live?
                    #(push/pushable? (:chunks world) (get es %)))]
      (reduce (fn [v sh] (if (live? (nth sh 0)) (taken v sh) v))
              vel shoves))
    vel))

(defn- fluid-at [world e half height]
  (assoc (fluid-of world e half height)
         :threshold (mobs/fluid-jump-threshold e)))

(defn- own-vel [world index eid e half height f]
  (let [shoves (when (push/pushable? (:chunks world) e)
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
  "Returns the travel of mob e this tick, a rest or a full move."
  [world index eid e h ht]
  (let [f (fluid-at world e h ht)
        moving? (not (zero? (double (:zza (:move e) 0.0))))
        vel (own-vel world index eid e h ht f)
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
                    :wet? (kept-flag (if rest? false (:wet? e)) w)
                    :in-lava? (kept-flag (:in-lava? e) l)
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
  hurt it first. The fn live? tells the bodies that are alive now."
  [world index eid e [half height pos vel] cram live?]
  (let [hurt (when cram (cram e pos))
        shoves (push/shoves-at index eid pos half height)
        v (shoved world (or hurt (assoc e :pos pos)) vel shoves
                  live?)]
    [v shoves (some? hurt)]))

(defn- moved-y ^double [tr]
  (if-let [mv (nth tr 11 nil)] (phys/moved-y mv) 0.0))

(defn- dry?
  "Returns true when the mob that moved by tr was out of water when
  its move began and is out of it after."
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

(defn- bumped [e e2 tr]
  (if-let [f (:bump (spec/of (:type e)))]
    (f e2 (nth tr 9 nil) (nth tr 10 nil))
    e2))

(defn- shoved-move
  [world index eid e tr box fall ls [look prev cram live?]]
  (let [[half height pos] box
        h (num/f32 half) ht (num/f32 height)
        [v shoves hit?] (pushed-back world index eid e box cram live?)
        g (or (nth tr 6) (fluid-after world pos h ht v))
        hd (body-of-move world e pos look)
        came (push/arrived e pos (:tick world) eid)
        e2 (settled e tr pos fall v g (nth tr 8) hd look
                    (walk-of e prev pos) came)]
    [(bumped e e2 tr) shoves hit? (when ls (nth ls 1))]))

(defn moved
  "Returns mob e after one tick of its move, the shoves it gave,
  whether cramming hurt it, and the deltas of its landing. The look
  it turns to, its start state, its cram and the bodies alive now
  come in more."
  [world index eid e half height more]
  (let [h (num/f32 half) ht (num/f32 height)
        tr (travel-of world index eid e h ht)
        f0 (fall-before world e tr) f1 (fall-after tr f0)
        ls (when (lands? tr f0 f1)
             (landing/landed world eid e (nth tr 0) (nth tr 4) f0 f1))
        pos (if ls (nth ls 0) (nth tr 0))
        fall (landing/kept e (if (nth tr 2) 0.0 f1))
        box [half height pos (nth tr 1)]]
    (shoved-move world index eid e tr box fall ls more)))
