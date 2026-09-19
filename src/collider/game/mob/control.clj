(ns collider.game.mob.control
  "The move, jump and body rotation controls of a mob.
  MoveControl and JumpControl live in :move and :jump, the body
  rotation in :body."
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
  "Returns a turned toward b by at most max degrees.
  The answer is normalised the way MoveControl.rotlerp normalises."
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
  "Returns mob e told to walk to x y z, as setWantedPosition.
  A jump under way keeps the control jumping."
  [e x y z speed]
  (let [m (:move e)]
    (assoc e :move (assoc m :x (double x) :y (double y) :z (double z)
                            :mult (double speed)
                            :op (if (= :jumping (:op m))
                                  :jumping
                                  :move-to)))))

(defn in-liquid?
  "Whether the mob stands in a liquid, as Entity.isInLiquid.
  Both water and lava hold a mob up; :wet? and :in-lava? are the
  flags the physics leaves behind."
  [e]
  (boolean (or (:wet? e) (:in-lava? e))))

(defn- sped [m ^double s] (assoc m :speed s :zza s))

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
  (assoc e :yaw (rotlerp (double (:yaw e))
                         (- (Math/toDegrees (Math/atan2 zd xd)) 90.0)
                         max-turn)))

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
  (let [m (sped (:move e) (* (double (:mult (:move e))) attr))]
    (assoc e :move (cond-> m
                           (or (:on-ground e) (in-liquid? e))
                           (assoc :op :wait)))))

(defn- waiting [e]
  (let [m (:move e)]
    (if (and m (not (zero? (double (:zza m 0.0)))))
      (assoc e :move (assoc m :zza 0.0))
      e)))

(defn tick
  "Runs the move and jump controls of mob e for one tick.
  attr is the movement speed of its kind, width its box width."
  [world e attr width]
  (case (:op (:move e) :wait)
    :move-to (move-to-tick world e (double attr) (double width))
    :jumping (jumping-tick e (double attr))
    (waiting e)))

(defn- faced-forward ^double [^double yaw ^double hy ^long stable]
  (if (> stable face-forward-delay)
    (let [f (Math/min 1.0 (/ (double (- stable face-forward-delay))
                             (double face-forward-delay)))]
      (v/limit-angle yaw hy (* max-head-y-rot (- 1.0 f))))
    yaw))

(defn body-tick
  "Turns the body and head of e after its move.
  moved? tells whether the mob shifted in the XZ plane this tick:
  a walking mob carries its head, a standing one turns its body
  after its head, as BodyRotationControl."
  [e moved? ^long t]
  (let [hy (double (or (:head-yaw e) (:yaw e)))
        yaw (double (:yaw e))
        b (or (:body e) {:head 0.0 :at t})]
    (cond
      moved? (let [hy (v/limit-angle hy yaw max-head-y-rot)]
               (assoc e :head-yaw hy :body {:head hy :at t}))
      (> (Math/abs (- hy (double (:head b)))) head-stable-angle)
      (assoc e :yaw (v/limit-angle yaw hy max-head-y-rot)
               :body {:head hy :at t})
      :else (assoc e :body b
                     :yaw (faced-forward yaw hy
                                         (- t (long (:at b))))))))
