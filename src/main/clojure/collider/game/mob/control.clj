(ns collider.game.mob.control
  "The move, jump, body and head controls of a mob."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.game.mob Steer)
           (collider.world.space Path)))

(set! *warn-on-reflection* true)

(def ^:private ^:const min-speed-sqr 2.5000003E-7)

(def ^:private ^:const max-turn 90.0)

(def ^:private ^:const max-head-y-rot 75.0)

(def ^:private ^:const head-stable-angle 15.0)

(def ^:private ^:const face-forward-delay 10)

(defn rotlerp
  "Returns a turned toward b by at most max degrees, brought back into
  one turn around the circle."
  ^double [^double a ^double b ^double max]
  (Steer/rotlerp a b max))

(defn shape-top
  "Returns how high the collision shape of the cell x y z reaches."
  ^double [chunks ^long x ^long y ^long z]
  (Path/shapeTop (block/collision-arr)
                 (chunk/block-state chunks x y z)))

(defn floor-level
  "Returns the height the floor under the cell x y z stands at."
  ^double [chunks ^long x ^long y ^long z]
  (+ (dec y) (shape-top chunks x (dec y) z)))

(defn wanted
  "Returns mob e told to walk to x y z at that speed.
  A jump under way keeps the mob jumping."
  [e x y z speed]
  (let [m (Steer/wanted (:move e) (double x) (double y) (double z)
                        (double speed))]
    (assoc e :move m)))

(defn in-liquid?
  "Returns true when the mob stands in water or in lava, either of
  which holds it up."
  [e]
  (boolean (or (:wet? e) (:in-lava? e))))

(defn- stuck-in-block? [chunks pos]
  (let [x (long (Math/floor (v/x pos)))
        y (long (Math/floor (v/y pos)))
        z (long (Math/floor (v/z pos)))
        st (chunk/block-state chunks x y z)]
    (and (seq (block/collision-boxes st))
         (< (v/y pos) (+ (shape-top chunks x y z) y))
         (not (block/tagged? st "doors"))
         (not (block/tagged? st "fences")))))

(defn- turned ^double [e ^double xd ^double zd]
  (Steer/turned (double (:yaw e)) xd zd max-turn))

(defn- jumps? [chunks e xd yd zd width]
  (let [xd (double xd) zd (double zd) w (double width)]
    (or (and (> (double yd) (mobs/step-height (:type e)))
             (< (+ (* xd xd) (* zd zd)) (Math/max 1.0 w)))
        (stuck-in-block? chunks (:pos e)))))

(defn- move-to-tick [world e nav m attr width]
  (let [pos (:pos e) attr (double attr)
        xd (- (double (:x m)) (v/x pos))
        yd (- (double (:y m)) (v/y pos))
        zd (- (double (:z m)) (v/z pos))]
    (if (< (+ (* xd xd) (* yd yd) (* zd zd)) min-speed-sqr)
      (entity/with e {:nav nav :move (Steer/arrived m)})
      (let [jump? (boolean (jumps? (:chunks world) e xd yd zd width))
            m (Steer/driven m (* (double (:mult m)) attr) jump?)
            yaw (turned e xd zd)]
        (if jump?
          (entity/with e {:nav nav :yaw yaw :move m :jump true})
          (entity/with e {:nav nav :yaw yaw :move m}))))))

(defn- jumping-tick [e nav m ^double attr]
  (let [landed? (boolean (or (:on-ground e) (in-liquid? e)))
        s (* (double (:mult m)) attr)]
    (entity/with e {:nav nav :move (Steer/jumped m s landed?)})))

(defn- waiting [e nav m]
  (let [h (Steer/halted m)]
    (if (and (identical? h (:move e)) (identical? nav (:nav e)))
      e
      (entity/with e {:nav nav :move h}))))

(defn tick
  "Returns mob e after one tick of its move and jump controls, for
  speed attribute attr and box width width. The path state nav and
  the move m the navigation aims at, when given, go in the same copy."
  ([world e attr width] (tick world e attr width nil nil))
  ([world e attr width nav m]
   (let [nav (or nav (:nav e)) m (or m (:move e))
         op (:op m :wait)]
     (cond
       (identical? op Steer/MOVE_TO)
       (move-to-tick world e nav m attr width)
       (identical? op Steer/JUMPING)
       (jumping-tick e nav m (double attr))
       :else (waiting e nav m)))))

(defn- flt ^double [^double a] (double (float a)))

(defn rotate-if-necessary
  "Returns target pulled back toward base by no more than max degrees.
  Each step rounds to a float."
  ^double [^double base ^double target ^double max]
  (let [d (flt (v/wrap-deg (flt (- target base))))]
    (flt (- target (Math/clamp d (- max) max)))))

(defn- faced-forward ^double [^double yaw ^double hy ^long stable]
  (if (> stable face-forward-delay)
    (let [n (- stable face-forward-delay)
          f (Math/clamp (flt (/ (double n) 10.0)) 0.0 1.0)
          r (flt (* max-head-y-rot (flt (- 1.0 f))))]
      (rotate-if-necessary yaw hy r))
    yaw))

(defn- carried [^double yaw ^double hy ^long t]
  (let [h (rotate-if-necessary hy yaw max-head-y-rot)]
    [yaw h {:head h :at t :yaw yaw}]))

(defn body-yaw
  "Returns the yaw of the body of mob e, which a standing mob turns
  after its head without turning itself."
  ^double [e]
  (double (or (:yaw (:body e)) (:yaw e))))

(defn- faced [e b ^double hy ^long t]
  (let [by (body-yaw e)
        f (faced-forward by hy (- t (long (:at b))))]
    (if (and (:yaw b) (== f by)) b (assoc b :yaw f))))

(defn- body-after [e ^double hy]
  (rotate-if-necessary (body-yaw e) hy max-head-y-rot))

(defn body-turn
  "Returns the yaw, head yaw and body of mob e after its move, for
  head yaw head. The flag moved? is true when the mob shifted in the
  XZ plane this tick. A walking mob carries its head. A standing one
  turns its body after its head."
  [e head moved? t]
  (let [yaw (double (:yaw e)) t (long t)
        hy (double (or head yaw))
        b (or (:body e) {:head 0.0 :at (dec t)})]
    (cond
      moved? (carried yaw hy t)
      (> (Math/abs (- hy (double (:head b)))) head-stable-angle)
      [yaw head {:head hy :at t :yaw (body-after e hy)}]
      :else [yaw head (faced e b hy t)])))

(defn- look-pitch ^double [e height o]
  (let [pos (:pos e) opos (:pos o)
        oh (double (nth (or (mobs/box-of o) [0.0 1.0]) 1))
        eye (+ (v/y pos) (* mobs/legacy-look-eye (double height)))
        oeye (+ (v/y opos)
                (case (:type o)
                  :player mobs/legacy-player-eye
                  :point 0.0
                  (* mobs/legacy-look-eye oh)))
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
           :else (body-yaw e))
     (if o (look-pitch e height o) 0.0)]))

(defn look-of
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
