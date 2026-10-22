(ns collider.game.mob.steer
  "The move control of a mob. It holds what the mob does (:op), where
  it walks to, the speed modifier it was given and the speed and
  forward input it drives with."
  (:require [collider.math :as math]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn rotlerp
  "Returns a turned toward b by at most max degrees, brought back into
  one turn, in floats."
  ^double [^double a ^double b ^double max]
  (let [a (math/f32 a) m (math/f32 max)
        d (math/wrap-degrees (- (math/f32 b) a))
        d (if (> d m) m d)
        d (if (< d (- m)) (- m) d)
        r (math/f32 (+ a d))]
    (cond (< r 0.0) (math/f32 (+ r 360.0))
          (> r 360.0) (math/f32 (- r 360.0))
          :else r)))

(defn turned
  "Returns yaw turned toward the offset xd zd by at most max degrees,
  in floats."
  ^double [^double yaw ^double xd ^double zd ^double max]
  (let [d (math/f32 (math/degrees (math/atan2 zd xd)))
        to (math/f32 (- d 90.0))]
    (rotlerp yaw to max)))

(def ^:private idle
  {:op :wait :x 0.0 :y 0.0 :z 0.0 :mult 0.0 :speed 0.0 :zza 0.0})

(defn- kept
  "Returns control s when s' holds the same values bit for bit, else s'."
  [s s']
  (if (every? #(v/same? (get s %) (get s' %)) (keys idle)) s s'))

(defn wanted
  "Returns control m told to walk to x y z with speed modifier mult. A
  jump under way goes on."
  [m x y z mult]
  (let [s (or m idle)]
    (kept s (assoc s :op (if (= :jumping (:op s)) :jumping :move-to)
                   :x (double x) :y (double y) :z (double z)
                   :mult (double mult)))))

(defn arrived
  "Returns control m arrived, waiting with no forward input."
  [m]
  (let [s (or m idle)] (kept s (assoc s :op :wait :zza 0.0))))

(defn driven
  "Returns control m driving at speed, rounded to a float. It jumps
  when jump? is true and waits otherwise."
  [m speed jump?]
  (let [s (or m idle) f (math/f32 (double speed))]
    (kept s (assoc s :op (if jump? :jumping :wait) :speed f :zza f))))

(defn jumped
  "Returns control m in its jump at speed, rounded to a float. The
  jump ends on landing."
  [m speed landed?]
  (let [s (or m idle) f (math/f32 (double speed))]
    (kept s (assoc s :op (if landed? :wait (:op s)) :speed f :zza f))))

(defn halted
  "Returns control m with no forward input, nil for no control."
  [m]
  (when m
    (if (== 0.0 (double (:zza m))) m (kept m (assoc m :zza 0.0)))))
