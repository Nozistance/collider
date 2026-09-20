(ns collider.game.mob.control
  "The move, jump and body rotation controls of a mob."
  (:require [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const min-speed-sqr 2.5000003E-7)

(def ^:private ^:const max-turn 90.0)

(def ^:private ^:const max-up-step 0.6)

(def ^:private ^:const max-head-y-rot 75.0)

(def ^:private ^:const head-stable-angle 15.0)

(def ^:private ^:const face-forward-delay 10)

(defn rotlerp
  "Returns a turned toward b by at most max degrees, brought back
  into one turn around the circle."
  ^double [^double a ^double b ^double max]
  (let [d (Math/max (- max) (Math/min max (v/wrap-deg (- b a))))
        r (+ a d)]
    (cond (< r 0.0) (+ r 360.0)
          (> r 360.0) (- r 360.0)
          :else r)))

(defn shape-top
  "Returns how high the collision shape of the cell x y z reaches."
  ^double [chunks ^long x ^long y ^long z]
  (let [bs (block/collision-boxes (chunk/block-state chunks x y z))]
    (if (empty? bs)
      0.0
      (/ (double (reduce max (map (fn [b] (nth b 4)) bs))) 16.0))))

(defn floor-level
  "Returns the height the floor under the cell x y z stands at."
  ^double [chunks ^long x ^long y ^long z]
  (+ (dec y) (shape-top chunks x (dec y) z)))

(defn wanted
  "Returns mob e told to walk to x y z at that speed.
  A jump under way keeps the mob jumping."
  [e x y z speed]
  (let [m (:move e)
        op (if (= :jumping (:op m)) :jumping :move-to)
        m2 (assoc m :x (double x) :y (double y) :z (double z)
                  :mult (double speed) :op op)]
    (assoc e :move m2)))

(defn in-liquid?
  "Returns true when the mob stands in water or in lava, either of
  which holds it up."
  [e]
  (boolean (or (:wet? e) (:in-lava? e))))

(defn- sped
  "Returns the move control driving at s, which is also the speed
  it reports, rounded to a float."
  [m ^double s]
  (let [s (double (float s))] (assoc m :speed s :zza s)))

(defn- stuck-in-block?
  "Whether the block the mob stands in pushes it into a jump."
  [chunks pos]
  (let [x (long (Math/floor (v/x pos)))
        y (long (Math/floor (v/y pos)))
        z (long (Math/floor (v/z pos)))
        st (chunk/block-state chunks x y z)]
    (and (seq (block/collision-boxes st))
         (< (v/y pos) (+ (shape-top chunks x y z) y))
         (not (block/tagged? st "doors"))
         (not (block/tagged? st "fences")))))

(defn- turned [e ^double xd ^double zd]
  (let [to (- (Math/toDegrees (Math/atan2 zd xd)) 90.0)]
    (assoc e :yaw (rotlerp (double (:yaw e)) to max-turn))))

(defn- jumps? [chunks e [xd yd zd] ^double width]
  (let [xd (double xd) yd (double yd) zd (double zd)]
    (or (and (> yd max-up-step)
             (< (+ (* xd xd) (* zd zd)) (Math/max 1.0 width)))
        (stuck-in-block? chunks (:pos e)))))

(defn- move-to-tick [world e ^double attr ^double width]
  (let [m (:move e) pos (:pos e)
        xd (- (double (:x m)) (v/x pos))
        yd (- (double (:y m)) (v/y pos))
        zd (- (double (:z m)) (v/z pos))]
    (if (< (+ (* xd xd) (* yd yd) (* zd zd)) min-speed-sqr)
      (assoc e :move (assoc m :op :wait :zza 0.0))
      (let [jump? (jumps? (:chunks world) e [xd yd zd] width)
            m (assoc (sped m (* (double (:mult m)) attr))
                     :op (if jump? :jumping :wait))]
        (cond-> (assoc (turned e xd zd) :move m)
          jump? (assoc :jump true))))))

(defn- jumping-tick [e ^double attr]
  (let [m (sped (:move e) (* (double (:mult (:move e))) attr))
        landed? (or (:on-ground e) (in-liquid? e))]
    (assoc e :move (cond-> m landed? (assoc :op :wait)))))

(defn- waiting [e]
  (let [m (:move e)]
    (if (and m (not (zero? (double (:zza m 0.0)))))
      (assoc e :move (assoc m :zza 0.0))
      e)))

(defn tick
  "Returns mob e after one tick of its move and jump controls.
  attr is the movement speed of its kind, width its box width."
  [world e attr width]
  (case (:op (:move e) :wait)
    :move-to (move-to-tick world e (double attr) (double width))
    :jumping (jumping-tick e (double attr))
    (waiting e)))

(defn- flt ^double [^double a] (double (float a)))

(defn rotate-if-necessary
  "Returns target pulled back toward base by no more than max
  degrees. Every step rounds to a float, as the angles are floats."
  ^double [^double base ^double target ^double max]
  (let [d (flt (v/wrap-deg (flt (- target base))))]
    (flt (- target (Math/clamp d (- max) max)))))

(defn- faced-forward
  "Returns the yaw of a body that has stood still for that many
  ticks. After ten the body starts to turn toward the head, and
  ten ticks later it faces where the head looks."
  ^double [^double yaw ^double hy ^long stable]
  (if (> stable face-forward-delay)
    (let [n (- stable face-forward-delay)
          f (Math/clamp (flt (/ (double n) 10.0)) 0.0 1.0)
          r (flt (* max-head-y-rot (flt (- 1.0 f))))]
      (rotate-if-necessary yaw hy r))
    yaw))

(defn- turned-body [e ^double yaw ^double hy ^long t]
  (assoc e :yaw (rotate-if-necessary yaw hy max-head-y-rot)
         :body {:head hy :at t}))

(defn- carried-head [e ^double yaw ^double hy ^long t]
  (let [h (rotate-if-necessary hy yaw max-head-y-rot)]
    (assoc e :head-yaw h :body {:head h :at t})))

(defn body-tick
  "Returns e with its body and head turned after its move.
  moved? tells whether the mob shifted in the XZ plane this tick:
  a walking mob carries its head, a standing one turns its body
  after its head."
  [e moved? ^long t]
  (let [hy (double (or (:head-yaw e) (:yaw e)))
        yaw (double (:yaw e))
        b (or (:body e) {:head 0.0 :at (dec t)})
        stable (- t (long (:at b)))]
    (cond
      moved? (carried-head e yaw hy t)
      (> (Math/abs (- hy (double (:head b)))) head-stable-angle)
      (turned-body e yaw hy t)
      :else (assoc e :body b :yaw (faced-forward yaw hy stable)))))
