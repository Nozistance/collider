(ns collider.game.systems.mobs

  (:require [collider.rnd :as rnd]
            [clojure.data.int-map :as im]
            [collider.game.entity :as entity]
            [collider.vec :as v]
            [collider.game.features.sheep :as sheep]
            [collider.game.crowd :as crowd]
            [collider.game.mobs :as mobs]
            [collider.game.sense :as sense]
            [collider.game.state :as state]
            [collider.proto.packets.play :as play]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]
            [collider.world.path :as path]
            [collider.world.phys :as phys])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private brains {:sheep sheep/brain})
(def ^:private feeders {:sheep sheep/feed-deltas})
(defn- think [world eid e t tempters]
  (if-let [b (brains (:type e))]
    (b world eid e t tempters)
    [e nil]))

(defn- feed-deltas [world events t]
  (into [] (mapcat (fn [f] (f world events t))) (vals feeders)))

(def ^:private zero3 (v/v3 0.0 0.0 0.0))
(def ^:private ^:const gravity 0.08)
(def ^:private ^:const jump-speed 0.42)
(def ^:private ^:const ground-friction 0.546)
(def ^:private ^:const air-friction 0.91)
(def ^:private ^:const water-friction 0.8)
(def ^:private ^:const air-accel 0.02)
(def ^:private ^:const repath-interval 10)

(defn- steer-target [world e]
  (case (get-in e [:task :kind])
    (:wander :panic) (let [[tx tz] (get-in e [:task :target])]
              [(v/v3 (double tx) (v/y (:pos e)) (double tz)) 0.4])
    :follow (when-let [p (get-in world [:entities (get-in e [:task :parent])])]
              [(:pos p) 1.5])
    :mate   (when-let [p (get-in world [:entities (get-in e [:task :partner])])]
              [(:pos p) 1.1])
    :tempt  (when-let [p (get-in world [:entities (get-in e [:task :player])])]
              [(:pos p) 2.5])
    nil))

(defn- task-speed-mult ^double [e]
  (case (get-in e [:task :kind])
    :panic 1.25
    (:tempt :follow) 1.1
    1.0))

(defn- ensure-path [world e t goal avoid-water?]
  (let [task (:task e)
        gc   (sense/feet-cell goal)]
    (if (or (and (:path task) (= gc (:path-goal task)))
            (> (long (:repath-at task 0)) (long t)))
      e
      (assoc e :task (assoc task
                            :path (path/find-path (:chunks world) gen/flat-chunk
                                                  (sense/feet-cell (:pos e)) gc avoid-water?)
                            :path-i 0
                            :path-goal gc
                            :repath-at (+ (long t) repath-interval))))))

(defn- advance-path
  ^long [e]
  (let [{:keys [path path-i]} (:task e)
        p (:pos e)]
    (loop [i (long (or path-i 0))]
      (if-let [[wx _ wz] (get path i)]
        (if (< (v/dist-sq p (+ (double wx) 0.5) (+ (double wz) 0.5)) 0.25)
          (recur (inc i))
          i)
        i))))

(defn- smooth-index [world e half pth pi]
  (let [pi (long pi)
        fy (long (Math/floor (double (nth (:pos e) 1))))]
    (loop [j (min (+ pi 3) (dec (count pth)))]
      (if (<= j pi)
        pi
        (let [w (pth j)]
          (if (and (= (long (w 1)) fy)
                   (path/direct? (:chunks world) gen/flat-chunk (:pos e) half w))
            j
            (recur (dec j))))))))

(defn- navigate [world e t half water?]
  (if-let [[goal stop] (steer-target world e)]
    (if (<= (v/dist-sq (:pos e) goal) (* (double stop) (double stop)))
      [e nil nil]
      (let [avoid? (and (not water?) (not= :tempt (get-in e [:task :kind])))
            e (ensure-path world e t goal avoid?)
            pth (get-in e [:task :path])]
        (if (nil? pth)
          [e nil goal]
          (let [pi (smooth-index world e half pth (advance-path e))
                e  (assoc-in e [:task :path-i] pi)
                wp (get pth pi)]
            [e wp (when wp (v/v3 (+ (double (wp 0)) 0.5) 0.0 (+ (double (wp 2)) 0.5)))]))))
    [e nil nil]))

(defn- in-water? [world p ^double height]
  (let [y (v/y p)
        cx (long (Math/floor (v/x p)))
        cz (long (Math/floor (v/z p)))
        y1 (long (Math/floor (- (+ y (double height)) 0.4)))]
    (loop [cy (long (Math/floor (+ y 0.4)))]
      (cond
        (> cy y1) false
        (= :water (liquid/liquid-class (sense/block-at world cx cy cz))) true
        :else (recur (inc cy))))))

(defn- water-above? [world p]
  (= :water (liquid/liquid-class
             (sense/block-at world (long (Math/floor (v/x p)))
                             (long (Math/floor (+ (v/y p) 0.6)))
                             (long (Math/floor (v/z p)))))))

(defn- heading [p tgt]
  (let [dx (- (v/x tgt) (v/x p))
        dz (- (v/z tgt) (v/z p))
        d  (Math/sqrt (+ (* dx dx) (* dz dz)))]
    (when (> d 1.0E-4) [(/ dx d) (/ dz d)])))

(defn- look-toward [e height o]
  (let [[_ y _] (:pos e)
        [_ oy _] (:pos o)
        eye  (+ (double y) (* 0.95 (double height)))
        oeye (+ (double oy)
                (if (= :player (:type o))
                  1.62
                  (* 0.95 (double (get-in mobs/types [(:type o) :height] 1.0)))))
        dh (Math/sqrt (v/dist-sq (:pos e) (:pos o)))]
    [(v/yaw-toward (:pos e) (:pos o))
     (- (Math/toDegrees (Math/atan2 (- oeye eye) dh)))]))

(defn- head-update [world e height t moving?]
  (let [look   (:look e)
        look   (when (and look (> (long (:until look 0)) (long t))) look)
        target (when-let [oid (:target look)] (get-in world [:entities oid]))
        [dyaw dpitch] (cond
                        target                 (look-toward e height target)
                        (and look (:yaw look)) [(:yaw look) 0.0]
                        :else                  [(:yaw e) 0.0])
        hy   (v/limit-angle (double (or (:head-yaw e) (:yaw e))) (double dyaw) 10.0)
        hp   (v/limit-angle (double (or (:pitch e) 0.0)) (double dpitch) 40.0)
        body (double (:yaw e))
        d    (v/wrap-deg (- hy body))
        hy   (cond (and moving? (> d 75.0))  (+ body 75.0)
                   (and moving? (< d -75.0)) (- body 75.0)
                   :else hy)
        hy   (v/wrap-deg hy)]
    (if (and (identical? look (:look e))
             (let [oh (:head-yaw e)] (and oh (== (double oh) hy)))
             (let [op (:pitch e)] (and op (== (double op) hp))))
      e
      (entity/mob-looked e hy hp look))))

(def ^:private rest-vel (v/v3 0.0 (* 0.98 (- 0.0 0.08)) 0.0))
(declare physics-move)

(defn- physics [world index eid e half height attr]
  (let [t (long (:tick world))
        pos (:pos e) x (v/x pos) ey (v/y pos) z (v/z pos)
        water? (in-water? world (:pos e) height)
        [e wp target] (navigate world e t half water?)
        moving? (some? target)
        og (boolean (:on-ground e))
        [cx cz] (crowd/crowd-push index eid e t half height)
        vel0 (:vel e)
        vx0 (let [a (v/x vel0)] (if (< (Math/abs a) 0.005) 0.0 a))
        vz0 (let [a (v/z vel0)] (if (< (Math/abs a) 0.005) 0.0 a))
        vy0 (v/y vel0)]
    (if (and (not moving?) og (not water?)
             (< (Math/abs (+ vx0 (double cx))) 0.005)
             (< (Math/abs (+ vz0 (double cz))) 0.005)
             (<= -0.0785 vy0 0.0)
             (phys/standing-on-cubes? (:chunks world) gen/flat-chunk x ey z half))
      (head-update world (entity/mob-moved e pos rest-vel true (:yaw e) false (:jump-cd e))
                   height t false)
      (physics-move world eid e water? wp target [vx0 vy0 vz0] [cx cz] half height attr))))

(defn- physics-move [world eid e water? wp target [vx0 vy0 vz0] [cx cz] half height attr]
  (let [t (long (:tick world))
        pos (:pos e) ey (v/y pos)
        moving? (some? target)
        [hx hz] (when moving? (heading pos target))
        og (boolean (:on-ground e))
        vx0 (double vx0) vy0 (double vy0) vz0 (double vz0)
        aispeed (* (double attr) (task-speed-mult e))
        fric  (cond water? water-friction og ground-friction :else air-friction)
        accel (if (and og (not water?)) (* aispeed aispeed) (* air-accel aispeed))
        wpush (if water?
                     (liquid/entity-push (:chunks world) gen/flat-chunk (:pos e) half height)
                     zero3)
        vx (let [a (+ vx0 (if hx (* (double hx) accel) 0.0) (double cx) (v/x wpush))]
             (if (and (not moving?) (not water?) (< (Math/abs a) 0.005)) 0.0 a))
        vz (let [a (+ vz0 (if hz (* (double hz) accel) 0.0) (double cz) (v/z wpush))]
             (if (and (not moving?) (not water?) (< (Math/abs a) 0.005)) 0.0 a))
        vy (if water?
             (+ vy0 (v/y wpush)
                (if (< (rnd/rnd3 t eid (hash :swim)) 0.8) 0.04 0.0))
             (double vy0))
        ^Move mv (phys/move (:chunks world) gen/flat-chunk (:pos e) (v/v3 vx vy vz) half height 0.6)
        pos (.pos mv) vel (.vel mv) on-ground (.on-ground mv)
        nx (v/x vel) ny (v/y vel) nz (v/z vel)
        bump? (and moving?
                   (or (and (not (zero? (double vx))) (zero? (double nx)))
                       (and (not (zero? (double vz))) (zero? (double nz)))))
        climb? (and moving? wp
                    (> (long (wp 1)) (long (Math/floor (double ey))))
                    (< (v/dist-sq (:pos e) target) 1.0))
        jump? (and on-ground climb? (>= t (long (or (:jump-cd e) 0))))
        ny (cond (and bump? water? (water-above? world (:pos e))) 0.3
                 jump?  jump-speed
                 water? (- (* water-friction (double ny)) 0.02)
                 :else  (* 0.98 (- (double ny) gravity)))
        yaw (if moving?
              (v/wrap-deg (v/limit-angle (double (:yaw e))
                                     (v/yaw-toward (:pos e) target)
                                     30.0))
              (:yaw e))
        e (entity/mob-moved e pos (v/v3 (* (double nx) fric) (double ny) (* (double nz) fric))
                            on-ground yaw water? (if jump? (+ t 10) (:jump-cd e)))]
    (head-update world e height t moving?)))

(def ^:private ^:const say-rest 120)
(def ^:private ^:const say-mean 40)
(defn- sound-pitch ^long [e ^long t ^long eid]

  (let [base (if (mobs/baby? e) 1.5 1.0)]
    (long (* 63.0 (+ base (* 0.2 (- (rnd/rnd3 t eid (hash :p1))
                                    (rnd/rnd3 t eid (hash :p2)))))))))

(defn- wide-pitch ^long [^long t ^long eid kind]
  (long (* 63.0 (+ 1.0 (* 0.4 (- (rnd/rnd4 t eid (hash kind) (hash :w1))
                                 (rnd/rnd4 t eid (hash kind) (hash :w2))))))))

(defn- water-vol ^double [vel3 k]
  (let [vx (v/x vel3) vy (v/y vel3) vz (v/z vel3)]
    (min 1.0 (* (Math/sqrt (+ (* vx vx 0.2) (* vy vy) (* vz vz 0.2)))
                (double k)))))

(defn- ambient [world eid e t]
  (if-let [say (mobs/say-sound (:type e))]
    (let [st (:say-tick e)
          next-say (+ (long t) say-rest (mobs/exp-delay say-mean (long t) (long eid) :say))]
      (cond
        (nil? st)         [(assoc e :say-tick next-say) nil]
        (>= (long t) (long st))
        [(assoc e :say-tick next-say)
         (state/broadcast world (play/entity-sound say (:pos e) 1.0
                                                   (sound-pitch e (long t) (long eid))))]
        :else [e nil]))
    [e nil]))

(defn- movement-sounds [world e was-wet? old-walked new-walked t eid]
  (concat
   (when (and (:wet? e) (not was-wet?))
     (state/broadcast world (play/entity-sound "game.neutral.swim.splash" (:pos e)
                                               (water-vol (:vel e) 0.2)
                                               (wide-pitch (long t) (long eid) :spl))))
   (when (> (long (Math/floor (double new-walked)))
            (long (Math/floor (double old-walked))))
     (if (:wet? e)
       (state/broadcast world (play/entity-sound "game.neutral.swim" (:pos e)
                                                 (water-vol (:vel e) 0.35)
                                                 (wide-pitch (long t) (long eid) :swm)))
       (when (:on-ground e)
         (when-let [snd (mobs/step-sound (:type e))]
           (state/broadcast world (play/entity-sound snd (:pos e) 0.15 63))))))))

(defn- dist3 ^double [[x1 y1 z1] [x2 y2 z2]]
  (let [dx (- (double x2) (double x1))
        dy (- (double y2) (double y1))
        dz (- (double z2) (double z1))]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(def ^:private mob-keys
  [:pos :vel :yaw :pitch :on-ground :task :pending :wake-tick :baby-until
   :tempt-cooldown-until :say-tick :walked :head-yaw :look :jump-cd :wet?])

(defmacro ^:private diff-keys
  "Build the map of the ks whose value in new is not identical? to old's.
   Expands to compiled keyword reads ((:k new) - an inline lookup thunk):
   a dynamic (get record k) goes through the record's case dispatch and
   hashes the keyword on every key of every sheep every tick."
  [old new & ks]
  (let [o (gensym) n (gensym)]
    `(let [~o ~old ~n ~new]
       (cond-> {}
         ~@(mapcat (fn [k]
                     [`(let [v# (~k ~n)] (not (identical? v# (~k ~o))))
                      `(assoc ~k (~k ~n))])
                   ks)))))

(defn- mob-changes [old new]
  (diff-keys old new
             :pos :vel :yaw :pitch :on-ground :task :pending :wake-tick
             :baby-until :tempt-cooldown-until :say-tick :walked :head-yaw
             :look :jump-cd :wet?))

(defn- step-mob [world index tempters eid e t]

  (let [{:keys [half height speed]} (mobs/types (:type e))
        dead?  (not (pos? (double (:health e))))
        [e1 deltas] (if dead? [e nil] (think world eid e t tempters))
        e1 (if (and (mobs/baby? e1) (>= (long t) (long (:baby-until e1))))
             (assoc e1 :baby-until nil)
             e1)
        [e1 say-deltas] (if dead? [e1 nil] (ambient world eid e1 t))
        was-wet? (boolean (:wet? e))
        e2 (physics world index eid e1 half height speed)
        walked  (double (or (:walked e) 0.0))
        walked' (+ walked (* 0.6 (dist3 (:pos e1) (:pos e2))))
        e2 (if (== walked walked') e2 (assoc e2 :walked walked'))
        changes (mob-changes e e2)]
    (concat (when (seq changes) [[:merge-entity eid changes]])
            deltas
            say-deltas
            (movement-sounds world e2 was-wet? walked walked' t eid))))

(defn mobs-system [world events]
  (let [t        (long (:tick world))
        active   (state/active-chunks world)
        index    (crowd/push-index world active)
        tempters (sense/holders world)
        herd     (into []
                       (filter (fn [[_ e]] (and (mobs/mob-type? (:type e))
                                                (state/active-at? active (:pos e)))))
                       (:entities world))]
    (conj (mapv (fn [batch]
                  #(into [] (mapcat (fn [me] (step-mob world index tempters (key me) (val me) t))) batch))
                (partition-all 32 herd))
          #(feed-deltas world events t))))
