(ns collider.game.features.sheep
  (:require [collider.game.mobs :as mobs]
            [collider.game.sense :as sense]
            [collider.game.state :as state]
            [collider.proto.packets.play :as play]
            [collider.rnd :as rnd]
            [collider.vec :as v]
            [collider.world.grass :as grass]))

(set! *warn-on-reflection* true)

(def ^:private ^:const eat-duration 40)
(def ^:private ^:const wander-timeout 200)
(def ^:private ^:const love-duration 600)
(def ^:private ^:const mate-together 60)
(def ^:private ^:const breed-cooldown 6000)
(def ^:private ^:const baby-growth 24000)
(def ^:private ^:const tallgrass-id 31)

(defn- decide [t eid e]
  (let [means (mobs/action-means (:type e))
        means (if (mobs/baby? e) (assoc means :eat 50) means)
        choices (map (fn [[kind mean]] [kind (mobs/exp-delay mean t eid kind)]) means)
        [kind d] (apply min-key second choices)]
    (assoc e :pending kind :wake-tick (+ (long t) (long d)))))

(defn- grass-target [world e]
  (let [[fx fy fz :as feet] (sense/feet-cell (:pos e))]
    (cond
      (= tallgrass-id (bit-shift-right (sense/block-at world feet) 4)) feet
      (= grass/grass-state (sense/block-at world [fx (dec fy) fz])) [fx (dec fy) fz]
      :else nil)))

(defn- start-eat [world eid e t]
  (if-let [cell (grass-target world e)]
    [(assoc e :pending nil
              :task {:kind :eat :until (+ (long t) eat-duration) :cell cell})
     (state/broadcast world (play/entity-status eid 10))]
    [(decide t eid (assoc e :pending nil)) nil]))

(defn- start-wander [world eid e t]
  (let [p (:pos e) x (v/x p) y (v/y p) z (v/z p)
        try-at (fn [i]
                 [(+ (double x) (- (* 20.0 (rnd/rnd4 t eid (hash :tx) i)) 10.0))
                  (+ (double z) (- (* 20.0 (rnd/rnd4 t eid (hash :tz) i)) 10.0))])
        weight (fn [[cx cz]]
                 (if (= grass/grass-state
                        (sense/block-at world [(long (Math/floor (double cx)))
                                               (dec (long (Math/floor (double y))))
                                               (long (Math/floor (double cz)))]))
                   10.0 0.0))
        target (reduce (fn [best c] (if (> (double (weight c)) (double (weight best))) c best))
                       (try-at 0)
                       (map try-at (range 1 10)))]
    [(assoc e :pending nil
              :task {:kind :wander :until (+ (long t) wander-timeout) :target target})
     nil]))

(defn- start-look [world eid e t]
  (let [[_ pid] (sense/nearest-player world (:pos e) 36.0)
        look (if pid
               {:target pid
                :until  (+ (long t) 40 (long (* 40.0 (rnd/rnd3 t eid (hash :lt)))))}
               {:yaw   (- (* 360.0 (rnd/rnd3 t eid (hash :y))) 180.0)
                :until (+ (long t) 20 (long (* 20.0 (rnd/rnd3 t eid (hash :lt)))))})]
    [(decide t eid (assoc e :pending nil :look look)) nil]))

(defn- start-pending [world eid e t]
  (case (:pending e)
    :eat (start-eat world eid e t)
    :wander (start-wander world eid e t)
    :look (start-look world eid e t)
    [(decide t eid e) nil]))

(defn- finish-bite [world e]
  (let [cell (get-in e [:task :cell])
        old (sense/block-at world cell)
        new (if (= tallgrass-id (bit-shift-right old 4)) 0 grass/dirt-state)]
    (when (or (= grass/grass-state old) (= tallgrass-id (bit-shift-right old 4)))
      (concat [[:set-block cell new]]
              (state/broadcast world (play/block-change cell new))
              (state/broadcast world (play/break-effect cell old))))))

(defn- run-eat [world eid e t]
  (let [remaining (- (long (get-in e [:task :until])) (long t))]
    (cond
      (<= remaining 0) [(decide t eid (assoc e :task nil)) nil]
      (= remaining 4)
      (let [deltas (finish-bite world e)
            e (if (and deltas (mobs/baby? e))
                (assoc e :baby-until (max (long t) (- (long (:baby-until e)) 1200)))
                e)]
        [e deltas])
      :else [e nil])))

(defn- run-wander [_ eid e t]
  (let [{:keys [until target path path-i path-goal]} (:task e)
        [tx tz] target
        done? (or (>= (long t) (long until))
                  (< (v/dist-sq (:pos e) (double tx) (double tz)) 0.36)
                  (and path-goal (nil? path))
                  (and path (>= (long (or path-i 0)) (count path))))]
    (if done?
      [(decide t eid (assoc e :task nil)) nil]
      [e nil])))

(def ^:private ^:const tempt-range-sq 100.0)
(def ^:private ^:const tempt-cooldown 100)

(defn- tempt-target [e tempters]
  (let [item (mobs/breeding-item (:type e))]
    (->> tempters
         (keep (fn [[pid it pos]]
                 (when (= it item)
                   (let [d2 (v/dist-sq (:pos e) pos)]
                     (when (< d2 tempt-range-sq) [d2 pid])))))
         (sort-by first)
         first
         second)))

(defn- tempted [_ e t tempters]
  (when (and (seq tempters)
             (>= (long t) (long (or (:tempt-cooldown-until e) 0))))
    (when-let [pid (tempt-target e tempters)]
      [(assoc e :task {:kind :tempt :player pid}) nil])))

(defn- run-tempt [_ _ e t tempters]
  (if-let [pid (tempt-target e tempters)]
    [(-> e
         (assoc-in [:task :player] pid)
         (assoc :look {:target pid :until (+ (long t) 2)}))
     nil]
    [(assoc e :task nil :tempt-cooldown-until (+ (long t) tempt-cooldown)) nil]))

(defn- start-follow [world eid e t]
  (when (and (mobs/baby? e)
             (not= :follow (get-in e [:task :kind]))
             (zero? (mod (+ (long t) (long eid)) 10)))
    (when-let [[d2 oid] (sense/nearest world (:pos e) 256.0
                                       (fn [oid o] (and (not= oid eid)
                                                        (= (:type e) (:type o))
                                                        (not (mobs/baby? o)))))]
      (when (>= (double d2) 9.0)
        [(assoc e :task {:kind :follow :parent oid}) nil]))))

(defn- run-follow [world _ e _]
  (let [o (get-in world [:entities (get-in e [:task :parent])])]
    (if (and (mobs/baby? e) o
             (<= 9.0 (v/dist-sq (:pos e) (:pos o)) 256.0))
      [e nil]
      [(assoc e :task nil) nil])))

(defn- spawn-baby [world eid pid e t]
  (let [cooled {:love-until 0 :breed-ready-at (+ (long t) breed-cooldown) :task nil}]
    (concat
      [[:spawn-entity (assoc (mobs/new-mob (:type e) (:pos e) (:color e) t)
                        :baby-until (+ (long t) baby-growth))]
       [:merge-entity eid cooled]
       [:merge-entity pid cooled]]
      (state/broadcast world (play/entity-status eid 18))
      (state/broadcast world (play/entity-status pid 18)))))

(defn- run-mate [world eid e t]
  (let [pid (get-in e [:task :partner])
        partner (get-in world [:entities pid])]
    (cond
      (not (and partner (mobs/in-love? partner t) (mobs/in-love? e t)))
      [(assoc e :task nil) nil]
      (and (< (v/dist-sq (:pos e) (:pos partner)) 9.0)
           (>= (- (long t) (long (get-in e [:task :since]))) mate-together))
      (if (< (long eid) (long pid))
        [(assoc e :task nil) (spawn-baby world eid pid e t)]
        [(assoc e :task nil) nil])
      :else [(assoc e :look {:target pid :until (+ (long t) 2)}) nil])))

(defn- start-mate [world eid e t]
  (if-let [[_ pid] (sense/nearest world (:pos e) 64.0
                                  (fn [oid o] (and (not= oid eid)
                                                   (= (:type e) (:type o))
                                                   (mobs/in-love? o t)
                                                   (not (mobs/baby? o)))))]
    [(assoc e :task {:kind :mate :partner pid :since t} :pending nil) nil]
    [e nil]))

(defn- panicking? [e t]
  (< (long t) (long (or (:panic-until e) 0))))

(defn- start-panic [world eid e t]
  (let [p (:pos e) x (v/x p) y (v/y p) z (v/z p)
        try-at (fn [i]
                 [(+ (double x) (- (* 10.0 (rnd/rnd4 t eid (hash :px) i)) 5.0))
                  (+ (double z) (- (* 10.0 (rnd/rnd4 t eid (hash :pz) i)) 5.0))])
        weight (fn [[cx cz]]
                 (if (= grass/grass-state
                        (sense/block-at world [(long (Math/floor (double cx)))
                                               (dec (long (Math/floor (double y))))
                                               (long (Math/floor (double cz)))]))
                   10.0 0.0))
        target (reduce (fn [best c] (if (> (double (weight c)) (double (weight best))) c best))
                       (try-at 0)
                       (map try-at (range 1 10)))]
    [(assoc e :pending nil
              :task {:kind :panic :until (+ (long t) wander-timeout) :target target})
     nil]))

(defn- run-panic [world eid e t]
  (let [{:keys [until target path path-i path-goal]} (:task e)
        [tx tz] target
        done? (or (not (panicking? e t))
                  (>= (long t) (long until))
                  (< (v/dist-sq (:pos e) (double tx) (double tz)) 0.36)
                  (and path-goal (nil? path))
                  (and path (>= (long (or path-i 0)) (count path))))]
    (if done?
      (if (panicking? e t)
        (start-panic world eid e t)
        [(decide t eid (assoc e :task nil)) nil])
      [e nil])))

(defn brain [world eid e t tempters]
  (let [kind (get-in e [:task :kind])]
    (cond
      (= :panic kind) (run-panic world eid e t)
      (panicking? e t) (start-panic world eid e t)
      (= :eat kind) (run-eat world eid e t)
      (= :mate kind) (run-mate world eid e t)
      (mobs/in-love? e t) (start-mate world eid e t)
      (= :tempt kind) (run-tempt world eid e t tempters)
      :else
      (or (tempted eid e t tempters)
          (start-follow world eid e t)
          (cond
            (= :follow kind) (run-follow world eid e t)
            (= :wander kind) (run-wander world eid e t)
            (>= (long t) (long (or (:wake-tick e) 0)))
            (if (:pending e)
              (start-pending world eid e t)
              [(decide t eid e) nil])
            :else [e nil])))))

(defn- feedable? [e t]
  (and (mobs/mob-type? (:type e))
       (not (mobs/baby? e))
       (not (mobs/in-love? e t))
       (<= (long (or (:breed-ready-at e) 0)) (long t))))

(defn feed-deltas [world events t]
  (mapcat (fn [[tag peid target]]
            (when (= :interact tag)
              (when-let [e (get-in world [:entities target])]
                (when (and (mobs/mob-type? (:type e))
                           (= (mobs/breeding-item (:type e))
                              (sense/held-of (get-in world [:entities peid]))))
                  (cond
                    (mobs/baby? e)
                    (let [remaining (max 0 (- (long (:baby-until e)) (long t)))]
                      [[:merge-entity target
                        {:baby-until (+ (long t) (long (* 0.9 remaining)))}]])
                    (feedable? e t)
                    (cons [:merge-entity target {:love-until (+ (long t) love-duration)}]
                          (state/broadcast world (play/entity-status target 18))))))))
          events))
