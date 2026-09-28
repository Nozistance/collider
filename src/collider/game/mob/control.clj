(ns collider.game.mob.control
  "The move, jump and body rotation controls of a mob."
  (:require [collider.game.entity :as entity]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (collider.game.mob Steer)
           (collider.world.space Path)))

(set! *warn-on-reflection* true)

(def ^:private ^:const min-speed-sqr 2.5000003E-7)

(def ^:private ^:const max-turn 90.0)

(def ^:private ^:const max-up-step 0.6)

(def ^:private ^:const max-head-y-rot 75.0)

(def ^:private ^:const head-stable-angle 15.0)

(def ^:private ^:const face-forward-delay 10)

(defn rotlerp
  "Returns a turned toward b by at most max degrees, brought back into
  one turn around the circle."
  ^double [^double a ^double b ^double max]
  (let [d (Math/max (- max) (Math/min max (v/wrap-deg (- b a))))
        r (+ a d)]
    (cond (< r 0.0) (+ r 360.0)
          (> r 360.0) (- r 360.0)
          :else r)))

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
  (let [to (- (Math/toDegrees (Math/atan2 zd xd)) 90.0)]
    (rotlerp (double (:yaw e)) to max-turn)))

(defn- jumps? [chunks e xd yd zd width]
  (let [xd (double xd) zd (double zd) w (double width)]
    (or (and (> (double yd) max-up-step)
             (< (+ (* xd xd) (* zd zd)) (Math/max 1.0 w)))
        (stuck-in-block? chunks (:pos e)))))

(defn- move-to-tick [world e ^double attr ^double width]
  (let [m (:move e) pos (:pos e)
        xd (- (double (:x m)) (v/x pos))
        yd (- (double (:y m)) (v/y pos))
        zd (- (double (:z m)) (v/z pos))]
    (if (< (+ (* xd xd) (* yd yd) (* zd zd)) min-speed-sqr)
      (assoc e :move (Steer/arrived m))
      (let [jump? (boolean (jumps? (:chunks world) e xd yd zd width))
            m (Steer/driven m (* (double (:mult m)) attr) jump?)
            yaw (turned e xd zd)]
        (if jump?
          (entity/with e {:yaw yaw :move m :jump true})
          (entity/with e {:yaw yaw :move m}))))))

(defn- jumping-tick [e ^double attr]
  (let [m (:move e)
        landed? (boolean (or (:on-ground e) (in-liquid? e)))
        s (* (double (:mult m)) attr)]
    (assoc e :move (Steer/jumped m s landed?))))

(defn- waiting [e]
  (let [m (:move e) h (Steer/halted m)]
    (if (identical? m h) e (assoc e :move h))))

(defn tick
  "Returns mob e after one tick of its move and jump controls, for
  speed attribute attr and box width width."
  [world e attr width]
  (let [op (:op (:move e) :wait)]
    (cond
      (identical? op Steer/MOVE_TO)
      (move-to-tick world e (double attr) (double width))
      (identical? op Steer/JUMPING) (jumping-tick e (double attr))
      :else (waiting e))))

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

(defn- turned-body [e ^double yaw ^double hy ^long t]
  (entity/with e {:yaw (rotate-if-necessary yaw hy max-head-y-rot)
                  :body {:head hy :at t}}))

(defn- carried-head [e ^double yaw ^double hy ^long t]
  (let [h (rotate-if-necessary hy yaw max-head-y-rot)]
    (entity/with e {:head-yaw h :body {:head h :at t}})))

(defn- faced [e b yaw hy t]
  (let [stable (- (long t) (long (:at b)))
        yaw (faced-forward (double yaw) (double hy) stable)]
    (entity/with e {:body b :yaw yaw})))

(defn body-tick
  "Returns e with its body and head turned after its move.
  The flag moved? is true when the mob shifted in the XZ plane this
  tick. A walking mob carries its head. A standing one turns its body
  after its head."
  [e moved? ^long t]
  (let [hy (double (or (:head-yaw e) (:yaw e)))
        yaw (double (:yaw e))
        b (or (:body e) {:head 0.0 :at (dec t)})]
    (cond
      moved? (carried-head e yaw hy t)
      (> (Math/abs (- hy (double (:head b)))) head-stable-angle)
      (turned-body e yaw hy t)
      :else (faced e b yaw hy t))))
