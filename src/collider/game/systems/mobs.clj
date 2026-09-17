(ns collider.game.systems.mobs
  "Mob thinking, movement and sounds."
  (:require [collider.random :as random]
            [collider.game.entity :as entity]
            [collider.vec :as v]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.sheep :as sheep]
            [collider.game.mob.cow :as cow]
            [collider.game.mob.mooshroom :as mooshroom]
            [collider.game.mob.push :as push]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.space.path :as path]
            [collider.world.phys :as phys])
  (:import (collider.game.entity Mob)
           (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private brains {:sheep sheep/brain :cow cow/brain :mooshroom mooshroom/brain})
(def ^:private interactions [animal/feed-deltas cow/milk-deltas mooshroom/interact-deltas])
(defn- think [world eid e t tempters]
  (if-let [b (brains (:type e))]
    (b world eid e t tempters)
    [e nil]))

(defn- feed-deltas [world events t]
  (into [] (mapcat (fn [f] (f world events t))) interactions))

(def ^:private zero3 (v/v3 0.0 0.0 0.0))
(def ^:private ^:const gravity 0.08)
(def ^:private ^:const jump-speed 0.42)
(def ^:private ^:const ground-friction 0.546)
(def ^:private ^:const air-friction 0.91)
(def ^:private ^:const water-friction 0.8)
(def ^:private ^:const air-accel 0.02)
(def ^:private ^:const repath-interval 10)
(defn- steering [e] (if (:follow e) :follow (get-in e [:task :kind])))

(defn- steer-target [world e]
  (case (steering e)
    (:wander :panic) (let [[tx tz] (get-in e [:task :target])]
                       [(v/v3 (double tx) (v/y (:pos e)) (double tz)) 0.4])
    :follow (when-let [p (get-in world [:entities (:follow e)])]
              [(:pos p) 1.5])
    :mate (when-let [p (get-in world [:entities (get-in e [:task :partner])])]
            [(:pos p) 1.1])
    :tempt (when-let [p (get-in world [:entities (get-in e [:task :player])])]
             [(:pos p) 2.5])
    nil))

(defn- task-speed-mult ^double [e]
  (double (get-in mobs/types [(:type e) :speeds (steering e)] 1.0)))

(defn- ensure-path [world e t goal avoid-water?]
  (let [task (:task e)
        gc (sense/feet-cell goal)]
    (if (or (and (:path task) (= gc (:path-goal task)))
            (> (long (:repath-at task 0)) (long t)))
      e
      (assoc e :task (assoc task
                       :path (path/find-path (:chunks world)
                                             (sense/feet-cell (:pos e)) gc avoid-water?)
                       :path-i 0
                       :path-goal gc
                       :repath-at (+ (long t) repath-interval))))))

(defn- advance-path ^long [e]
  (let [{:keys [path path-i]} (:task e)
        p (:pos e)]
    (loop [i (long (or path-i 0))]
      (if-let [[wx _ wz] (get path i)]
        (if (< (v/dist-sq p (+ (double wx) 0.5) (+ (double wz) 0.5)) 0.25)
          (recur (inc i))
          i)
        i))))

(defn- smooth-index
  "Returns the furthest point of the route a mob can head straight for."
  [world e half pth pi]
  (let [pi (long pi)
        fy (long (Math/floor (double (nth (:pos e) 1))))]
    (loop [j (min (+ pi 3) (dec (count pth)))]
      (if (<= j pi)
        pi
        (let [w (pth j)]
          (if (and (= (long (w 1)) fy)
                   (path/direct? (:chunks world) (:pos e) half w))
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
                e (assoc-in e [:task :path-i] pi)
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
        d (Math/sqrt (+ (* dx dx) (* dz dz)))]
    (when (> d 1.0E-4) [(/ dx d) (/ dz d)])))

(defn- look-toward [e height o]
  (let [[_ y _] (:pos e)
        [_ oy _] (:pos o)
        eye (+ (double y) (* 0.95 (double height)))
        oeye (+ (double oy)
                (if (= :player (:type o))
                  1.62
                  (* 0.95 (double (get-in mobs/types [(:type o) :height] 1.0)))))
        dh (Math/sqrt (v/dist-sq (:pos e) (:pos o)))]
    [(v/yaw-toward (:pos e) (:pos o))
     (- (Math/toDegrees (Math/atan2 (- oeye eye) dh)))]))

(defn- active-look [e ^long t]
  (let [look (:look e)]
    (when (and look (> (long (:until look 0)) t)) look)))

(defn- look-angles [world e height look]
  (let [target (when-let [oid (:target look)] (get-in world [:entities oid]))]
    (cond
      target (look-toward e height target)
      (and look (:yaw look)) [(:yaw look) 0.0]
      :else [(:yaw e) 0.0])))

(defn- clamp-head ^double [^double hy ^double body moving?]
  (let [d (v/wrap-deg (- hy body))]
    (v/wrap-deg (cond (and moving? (> d 75.0)) (+ body 75.0)
                      (and moving? (< d -75.0)) (- body 75.0)
                      :else hy))))

(defn- head-same? [e look ^double hy ^double hp]
  (and (identical? look (:look e))
       (let [oh (:head-yaw e)] (and oh (== (double oh) hy)))
       (let [op (:pitch e)] (and op (== (double op) hp)))))

(defn- head-update [world e height t moving?]
  (let [look (active-look e (long t))
        [dyaw dpitch] (look-angles world e height look)
        hy (v/limit-angle (double (or (:head-yaw e) (:yaw e))) (double dyaw) 10.0)
        hp (v/limit-angle (double (or (:pitch e) 0.0)) (double dpitch) 40.0)
        hy (clamp-head (double hy) (double (:yaw e)) moving?)]
    (if (head-same? e look (double hy) (double hp))
      e
      (entity/mob-looked e hy hp look))))

(def ^:private rest-vel (v/v3 0.0 (* 0.98 (- 0.0 0.08)) 0.0))
(declare physics-move)

(defn- dead-band ^double [^double a]
  (if (< (Math/abs a) 0.005) 0.0 a))

(defn- at-rest? [world e half moving? water? [vx0 vy0 vz0] [cx cz]]
  (let [pos (:pos e)]
    (and (not moving?) (boolean (:on-ground e)) (not water?)
         (< (^[double] Math/abs (+ (double vx0) (double cx))) 0.005)
         (< (^[double] Math/abs (+ (double vz0) (double cz))) 0.005)
         (<= -0.0785 (double vy0) 0.0)
         (phys/standing-on-cubes? (:chunks world)
                                  (v/x pos) (v/y pos) (v/z pos) half))))

(defn- rest-step [world e height t]
  (head-update world
               (entity/mob-moved e (:pos e) rest-vel true (:yaw e) false (:jump-cd e))
               height t false))

(defn- physics [world index eid e half height attr]
  (let [t (long (:tick world))
        water? (in-water? world (:pos e) height)
        [e wp target] (navigate world e t half water?)
        moving? (some? target)
        push (push/push index eid e t half height)
        vel0 (:vel e)
        vel [(dead-band (v/x vel0)) (v/y vel0) (dead-band (v/z vel0))]]
    (if (at-rest? world e half moving? water? vel push)
      (rest-step world e height t)
      (physics-move world eid e water? wp target vel push half height attr))))

(defn- water-push [world e half height water?]
  (if water?
    (liquid/entity-push (:chunks world) (:pos e) half height (:vel e))
    zero3))

(defn- steer-axis ^double [^double v0 h ^double accel]
  (+ v0 (if h (* (double h) accel) 0.0)))

(defn- drop-axis ^double [drop? ^double a]
  (if (and drop? (< (Math/abs a) 0.005)) 0.0 a))

(defn- swim-bob ^double [^long t ^long eid]
  (if (< (random/of-longs t eid (hash :swim)) 0.8) 0.04 0.0))

(defn- steer-vel [world e t eid water? [hx hz] [vx0 vy0 vz0] [cx cz] half height attr moving?]
  (let [aispeed (* (double attr) (task-speed-mult e))
        og (boolean (:on-ground e))
        accel (if (and og (not water?)) (* aispeed aispeed) (* air-accel aispeed))
        wpush (water-push world e half height water?)
        drop? (and (not moving?) (not water?))
        ax (+ (steer-axis (double vx0) hx (double accel)) (double cx) (v/x wpush))
        az (+ (steer-axis (double vz0) hz (double accel)) (double cz) (v/z wpush))]
    (v/v3 (drop-axis drop? ax)
          (if water?
            (+ (double vy0) (v/y wpush) (swim-bob (long t) (long eid)))
            (double vy0))
          (drop-axis drop? az))))

(defn- bumped? [moving? vx vz nx nz]
  (and moving? (or (and (not (zero? (double vx))) (zero? (double nx)))
                   (and (not (zero? (double vz))) (zero? (double nz))))))

(defn- climbing? [e wp target ey moving?]
  (and moving? wp
       (> (long (wp 1)) (long (Math/floor (double ey))))
       (< (v/dist-sq (:pos e) target) 1.0)))

(defn- next-vy [world e ny water? bump? jump?]
  (let [ny (double ny)]
    (cond (and bump? water? (water-above? world (:pos e))) 0.3
          jump? jump-speed
          water? (- (* water-friction ny) 0.02)
          :else (* 0.98 (- ny gravity)))))

(defn- next-yaw [e target moving?]
  (if moving?
    (v/wrap-deg (v/limit-angle (double (:yaw e)) (v/yaw-toward (:pos e) target) 30.0))
    (:yaw e)))

(defn- friction ^double [og water?]
  (cond water? water-friction og ground-friction :else air-friction))

(defn- jump-now? [world e wp target ey moving? ^Move mv t]
  (and (.on-ground mv)
       (climbing? e wp target ey moving?)
       (>= (long t) (long (or (:jump-cd e) 0)))))

(defn- bubbled-vy [world e ^Move mv water? bump? jump?]
  (liquid/bubble-push (:chunks world) (.pos mv)
                      (next-vy world e (v/y (.vel mv)) water? bump? jump?)))

(defn- mob-stepped [e ^Move mv ny fric target moving? water? jump? t]
  (let [vel (.vel mv)]
    (entity/mob-moved e (.pos mv)
                      (v/v3 (* (double (v/x vel)) (double fric)) (double ny)
                            (* (double (v/z vel)) (double fric)))
                      (.on-ground mv) (next-yaw e target moving?) water?
                      (if jump? (+ (long t) 10) (:jump-cd e)))))

(defn- physics-move [world eid e water? wp target vel0 push half height attr]
  (let [t (long (:tick world))
        ey (v/y (:pos e))
        moving? (some? target)
        fric (friction (boolean (:on-ground e)) water?)
        drive (steer-vel world e t eid water? (when moving? (heading (:pos e) target))
                         vel0 push half height attr moving?)
        ^Move mv (phys/move (:chunks world) (:pos e) drive half height 0.6)
        vel (.vel mv)
        bump? (bumped? moving? (v/x drive) (v/z drive) (v/x vel) (v/z vel))
        jump? (jump-now? world e wp target ey moving? mv t)
        ny (bubbled-vy world e mv water? bump? jump?)
        e (mob-stepped e mv ny fric target moving? water? jump? t)]
    (head-update world e height t moving?)))

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
  (let [t (long t) eid (long eid) st (:say-tick e)]
    (if (and st (< t (long st)))
      [e nil]
      (if-let [say (mobs/say-sound (:type e))]
        (if (nil? st)
          [(assoc e :say-tick (next-say t eid)) nil]
          [(assoc e :say-tick (next-say t eid))
           [(out/all (out/sound say (:pos e) 1.0 (sound-pitch e t eid)))]])
        [e nil]))))

(defn- step-sound-delta [e t eid]
  (if (:wet? e)
    (out/all (out/sound :swim (:pos e)
                        (water-vol (:vel e) 0.35)
                        (wide-pitch (long t) (long eid) :swm)))
    (when (:on-ground e)
      (when-let [snd (mobs/step-sound (:type e))]
        (out/all (out/sound snd (:pos e) 0.15 1.0))))))

(defn- movement-sounds [acc e was-wet? old-walked new-walked t eid]
  (let [acc (if (and (:wet? e) (not was-wet?))
              (conj acc (out/all (out/sound :splash (:pos e)
                                            (water-vol (:vel e) 0.2)
                                            (wide-pitch (long t) (long eid) :spl))))
              acc)]
    (if (> (long (Math/floor (double new-walked)))
           (long (Math/floor (double old-walked))))
      (if-let [d (step-sound-delta e t eid)] (conj acc d) acc)
      acc)))

(def ^:private mob-keys
  [:pos :vel :yaw :pitch :on-ground :task :follow :no-action :baby-until
   :tempt-cooldown-until :say-tick :walked :head-yaw :look :jump-cd :wet? :sheared?])

(defmacro ^:private diff-fields
  [old new & ks]
  (let [o (with-meta (gensym) {:tag 'collider.game.entity.Mob})
        n (with-meta (gensym) {:tag 'collider.game.entity.Mob})]
    `(let [~o ~old ~n ~new]
       (cond-> {}
               ~@(mapcat (fn [k]
                           (let [f (symbol (str ".-" (name k)))]
                             [`(not (identical? (~f ~n) (~f ~o)))
                              `(assoc ~k (~f ~n))]))
                         ks)))))

(defn- mob-changes [old new]
  (diff-fields old new
               :pos :vel :yaw :pitch :on-ground :task :follow :no-action
               :baby-until :tempt-cooldown-until :say-tick :walked :head-yaw
               :look :jump-cd :wet? :sheared?))

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
        walked' (+ walked (* 0.6 (Math/sqrt (v/dist3-sq (:pos prev) (:pos now)))))]
    [(if (== walked walked') now (assoc now :walked walked')) walked walked']))

(defn- step-mob [world index tempters eid e t]
  (let [{:keys [half height speed]} (mobs/types (:type e))
        dead? (not (pos? (double (:health e))))
        [e1 deltas say-deltas] (brain-step world eid e t tempters dead?)
        was-wet? (boolean (:wet? e))
        e2 (physics world index eid e1 half height speed)
        [e2 walked walked'] (walked-step e e1 e2)
        changes (mob-changes e e2)]
    (-> (if (seq changes) [[:merge-entity eid changes]] [])
        (cond-> deltas (into deltas)
                say-deltas (into say-deltas))
        (movement-sounds e2 was-wet? walked walked' t eid))))

(defn mobs-system [world d]
  (let [events (:input d)
        t (long (:tick world))
        active (state/active-chunks world)
        index (push/push-index world active)
        tempters (sense/holders world)
        herd (into []
                   (filter (fn [[_ e]] (and (mobs/mob-type? (:type e))
                                            (state/active-at? active (:pos e)))))
                   (:entities world))]
    (conj (mapv (fn [batch]
                  #(into [] (mapcat (fn [me] (step-mob world index tempters (key me) (val me) t))) batch))
                (partition-all 32 herd))
          #(feed-deltas world events t))))
