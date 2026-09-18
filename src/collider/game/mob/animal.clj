(ns collider.game.mob.animal
  "Farm animal goals and the selector that runs them by priority."
  (:require [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const stroll-interval 120)
(def ^:private ^:const stroll-idle 100)
(def ^:private ^:const stroll-timeout 200)
(def ^:private ^:const stroll-swim 0.001)
(def ^:private ^:const look-chance 0.02)
(def ^:private ^:const look-range-sq 36.0)
(def ^:private ^:const look-ticks 40)
(def ^:private ^:const look-around-ticks 20)
(def ^:private ^:const love-ticks 600)
(def ^:private ^:const mate-ticks 60)
(def ^:private ^:const breed-cooldown 6000)
(def ^:private ^:const baby-ticks 24000)
(def ^:private ^:const breed-range-sq 64.0)
(def ^:private ^:const breed-near-sq 9.0)
(def ^:private ^:const tempt-range-sq 100.0)
(def ^:private ^:const calm-ticks 100)
(def ^:private ^:const follow-near-sq 9.0)
(def ^:private ^:const follow-far-sq 256.0)
(def ^:private ^:const idle-reset-sq 1024.0)
(def ^:private ^:const ground-weight 10.0)
(def ^:private ^:const feeding-speedup 0.1)
(def ^:private ^:const ticks-per-second 20)
(def ^:private ^:const tries 10)

(defn rnd
  "Returns a number from 0 to 1 that tick t, the mob eid and key k decide."
  (^double [t eid k] (random/of-longs (long t) (long eid) (hash k)))
  (^double [t eid k i] (random/of-longs (long t) (long eid) (hash k) (long i))))

(defn one-in?
  "Returns true with a chance of one in n."
  [t eid k ^long n]
  (zero? (long (* n (rnd t eid k)))))

(defn- water? [world cell] (= :water (block/liquid-class (sense/block-at world cell))))
(defn- solid? [world cell] (block/solid? (sense/block-at world cell)))

(defn- random-offset [t eid k i h vert]
  (let [r (fn [j n] (- (long (* (inc (* 2 (long n))) (rnd t eid [k j] i))) (long n)))]
    [(r :x h) (r :y vert) (r :z h)]))

(defn- offset-cell [e [dx dy dz]]
  (let [p (:pos e)]
    [(long (Math/floor (+ (v/x p) (double dx))))
     (long (Math/floor (+ (v/y p) (double dy))))
     (long (Math/floor (+ (v/z p) (double dz))))]))

(defn- up-out-of-solid [world [x y z :as cell]]
  (if (solid? world cell)
    (loop [y (inc (long y))]
      (if (and (<= y chunk/max-y) (solid? world [x y z])) (recur (inc y)) [x y z]))
    cell))

(defn- stable? [world [x y z]]
  (and (chunk/in-range? y) (solid? world [x (dec (long y)) z])))

(defn- weight ^double [world e [x y z]]
  (if (= (block/state (get-in mobs/types [(:type e) :ground] :grass-block))
         (sense/block-at world [x (dec (long y)) z]))
    ground-weight
    0.0))

(defn- candidate [world e t eid k i h vert land?]
  (let [cell (offset-cell e (random-offset t eid k i h vert))]
    (when (stable? world cell)
      (if land?
        (let [c (up-out-of-solid world cell)] (when-not (water? world c) c))
        cell))))

(defn- best-cell [world e t eid k h vert land?]
  (loop [i 0 best nil best-w Double/NEGATIVE_INFINITY]
    (if (= i tries)
      best
      (let [cell (candidate world e t eid k i h vert land?)
            w (if cell (weight world e cell) Double/NEGATIVE_INFINITY)]
        (if (and cell (> w best-w))
          (recur (inc i) cell w)
          (recur (inc i) best best-w))))))

(defn- roam-task [kind t [x _ z]]
  {:kind kind :until (+ (long t) stroll-timeout) :target [(+ (long x) 0.5) (+ (long z) 0.5)]})

(defn- roam-done? [e t]
  (let [{:keys [until target path path-i path-goal]} (:task e)
        [tx tz] target]
    (or (>= (long t) (long until))
        (< (v/dist-sq (:pos e) (double tx) (double tz)) 0.36)
        (and path-goal (nil? path))
        (and path (>= (long (or path-i 0)) (count path))))))

(defn- roaming? [_ e t _] (not (roam-done? e t)))

(defn- water-near [world e]
  (let [[x y z :as here] (sense/feet-cell (:pos e))]
    (when-not (solid? world here)
      (->> (for [dx (range -5 6) dy (range -1 2) dz (range -5 6)
                 :let [c [(+ (long x) (long dx)) (+ (long y) (long dy)) (+ (long z) (long dz))]]
                 :when (water? world c)]
             c)
           (sort-by (fn [[cx _ cz]] (v/dist-sq (:pos e) (+ (long cx) 0.5) (+ (long cz) 0.5))))
           first))))

(defn- other [world e k] (get-in world [:entities (get-in e k)]))

(defn- glance [e oid t] (assoc e :look {:target oid :until (+ (long t) 2)}))

(defn- start-panic [world eid e t _]
  (when (mobs/panicking? e t)
    (when-let [cell (or (when (:burning? e) (water-near world e))
                        (best-cell world e t eid :panic 5 4 false))]
      [(assoc e :task (roam-task :panic t cell)) nil])))

(defn- partner-for [world eid e t]
  (second (sense/nearest world (:pos e) breed-range-sq
                         (fn [oid o] (and (not= oid eid)
                                          (= (:type e) (:type o))
                                          (mobs/in-love? o t)
                                          (not (mobs/panicking? o t))
                                          (< (v/dist3-sq (:pos e) (:pos o)) breed-range-sq))))))

(defn- start-mate [world eid e t _]
  (when (mobs/in-love? e t)
    (when-let [pid (partner-for world eid e t)]
      [(assoc e :task {:kind :mate :partner pid :since t}) nil])))

(defn- mating? [world e t _]
  (let [o (other world e [:task :partner])]
    (and o (mobs/in-love? o t) (not (mobs/panicking? o t))
         (< (- (long t) (long (get-in e [:task :since]))) (* 2 mate-ticks)))))

(defn- bred [spec eid pid e o t]
  (let [cooled {:love-until 0 :breed-ready-at (+ (long t) breed-cooldown)}]
    [(assoc e :task nil)
     [[:spawn-entity (assoc (mobs/new-mob (:type e) (:pos e) ((:child-color spec) t eid e o) t)
                       :baby-until (+ (long t) baby-ticks))]
      [:merge-entity eid cooled]
      [:merge-entity pid (assoc cooled :task nil)]
      (out/all (out/status eid :love))
      (out/all (out/status pid :love))]]))

(defn- mate-tick [spec world eid e t _]
  (let [pid (get-in e [:task :partner])
        o (get-in world [:entities pid])
        e (glance e pid t)]
    (if (and (>= (- (long t) (long (get-in e [:task :since]))) mate-ticks)
             (< (v/dist3-sq (:pos e) (:pos o)) breed-near-sq)
             (< (long eid) (long pid)))
      (bred spec eid pid e o t)
      [e nil])))

(defn- tempter [e tempters]
  (let [item (mobs/breeding-item (:type e))]
    (->> tempters
         (keep (fn [[pid items pos]]
                 (when (contains? items item)
                   (let [d2 (v/dist3-sq (:pos e) pos)]
                     (when (< d2 tempt-range-sq) [d2 pid])))))
         (sort-by first)
         first
         second)))

(defn- start-tempt [_ _ e t tempters]
  (when (>= (long t) (long (or (:tempt-cooldown-until e) 0)))
    (when-let [pid (tempter e tempters)]
      [(assoc e :task {:kind :tempt :player pid}) nil])))

(defn- tempt-tick [_ _ _ e t tempters]
  (let [pid (tempter e tempters)]
    [(glance (assoc-in e [:task :player] pid) pid t) nil]))

(defn- parent-for [world eid e]
  (let [p (:pos e)]
    (when-let [[d2 oid] (sense/nearest world p follow-far-sq
                                       (fn [oid o] (and (not= oid eid)
                                                        (= (:type e) (:type o))
                                                        (not (mobs/baby? o))
                                                        (<= (Math/abs (- (v/y (:pos o)) (v/y p))) 4.0)
                                                        (<= (Math/abs (- (v/x (:pos o)) (v/x p))) 8.0)
                                                        (<= (Math/abs (- (v/z (:pos o)) (v/z p))) 8.0))))]
      (when (>= (double d2) follow-near-sq) oid))))

(defn- start-follow [world eid e _ _]
  (when (mobs/baby? e)
    (when-let [oid (parent-for world eid e)]
      [(assoc e :follow oid) nil])))

(defn- following? [world e _ _]
  (let [o (other world e [:follow])]
    (and (mobs/baby? e) o
         (<= follow-near-sq (v/dist3-sq (:pos e) (:pos o)) follow-far-sq))))

(defn- stroll-cell [world eid e t]
  (cond
    (:wet? e) (or (best-cell world e t eid :stroll 15 7 true) (best-cell world e t eid :stroll 10 7 false))
    (>= (rnd t eid :swim) stroll-swim) (best-cell world e t eid :stroll 10 7 true)
    :else (best-cell world e t eid :stroll 10 7 false)))

(defn- start-wander [world eid e t _]
  (when (and (< (long (or (:no-action e) 0)) stroll-idle)
             (one-in? t eid :stroll (quot stroll-interval 2)))
    (when-let [cell (stroll-cell world eid e t)]
      [(assoc e :task (roam-task :wander t cell)) nil])))

(defn- look-goal [e t]
  (let [l (:look e)]
    (when (and (= :look-player (:kind l)) (> (long (:until l)) (long t))) l)))

(defn- start-look-player [world eid e t _]
  (when (< (rnd t eid :look) look-chance)
    (when-let [[_ pid] (sense/nearest-player world (:pos e) look-range-sq)]
      [(assoc e :look {:kind :look-player :target pid
                       :until (+ (long t) look-ticks (long (* look-ticks (rnd t eid :look-time))))})
       nil])))

(defn- looking? [world e _ _]
  (let [o (other world e [:look :target])]
    (and o (<= (v/dist3-sq (:pos e) (:pos o)) look-range-sq))))

(defn- start-look-around [_ eid e t _]
  (when (< (rnd t eid :around) look-chance)
    [(assoc e :task {:kind :look-around
                     :until (+ (long t) look-around-ticks (long (* look-around-ticks (rnd t eid :around-time))))}
              :look {:yaw (- (* 360.0 (rnd t eid :around-yaw)) 180.0) :until Long/MAX_VALUE})
     nil]))

(defn- looking-around? [_ e t _] (>= (long (get-in e [:task :until])) (long t)))

(def goals
  "The goals every farm animal has, highest priority first."
  [{:kind :panic :flags #{:move} :start start-panic :continue? roaming?}
   {:kind :mate :flags #{:move :look} :start start-mate :continue? mating? :tick mate-tick}
   {:kind      :tempt :flags #{:move :look} :start start-tempt :tick tempt-tick
    :continue? (fn [_ e _ tempters] (some? (tempter e tempters)))
    :stop      (fn [e t] (assoc e :task nil :tempt-cooldown-until (+ (long t) calm-ticks)))}
   {:kind     :follow :flags #{} :start start-follow :continue? following?
    :running? (fn [e _] (some? (:follow e)))
    :stop     (fn [e _] (assoc e :follow nil))}
   {:kind :wander :flags #{:move} :start start-wander :continue? roaming?}
   {:kind     :look-player :flags #{:look} :start start-look-player :continue? looking?
    :running? (fn [e t] (some? (look-goal e t)))
    :stop     (fn [e _] (assoc e :look nil))}
   {:kind :look-around :flags #{:move :look} :start start-look-around :continue? looking-around?
    :stop (fn [e _] (assoc e :task nil :look nil))}])

(defn- running? [g e t]
  (if-let [f (:running? g)] (f e t) (= (:kind g) (get-in e [:task :kind]))))

(defn- stopped [g e t]
  (if-let [f (:stop g)] (f e t) (assoc e :task nil)))

(defn- locks [spec e t]
  (into {} (for [g (:goals spec) :when (running? g e t) f (:flags g)] [f (:prio g)])))

(defn- free? [locked prio flags]
  (every? (fn [f] (let [p (locked f)] (or (nil? p) (< (long prio) (long p))))) flags))

(defn- displaced [spec e t flags]
  (reduce (fn [e g] (if (and (running? g e t) (some (:flags g) flags)) (stopped g e t) e))
          e
          (:goals spec)))

(defn- cleaned [spec world e t tempters]
  (reduce (fn [e g]
            (if (and (running? g e t) (not ((:continue? g) world e t tempters)))
              (stopped g e t)
              e))
          e
          (:goals spec)))

(defn- selected [spec world eid e t tempters]
  (reduce (fn [[e ds] {:keys [prio flags start] :as g}]
            (if (or (running? g e t) (not (free? (locks spec e t) prio flags)))
              [e ds]
              (if-let [[e2 ds2] (start world eid (displaced spec e t flags) t tempters)]
                [e2 (into ds ds2)]
                [e ds])))
          [e []]
          (:goals spec)))

(defn- ticked [spec world eid e t tempters]
  (let [kind (get-in e [:task :kind])]
    (if-let [f (some #(when (= kind (:kind %)) (:tick %)) (:goals spec))]
      (f spec world eid e t tempters)
      [e nil])))

(defn- idle-count [world e]
  (if (sense/nearest-player world (:pos e) idle-reset-sq) 0 (inc (long (or (:no-action e) 0)))))

(defn spec
  "Returns a breed's goal spec from its goals, highest priority first, and the
   function that picks a newborn's colour from both parents."
  ([goals] (spec goals (fn [_ _ a _] (:color a))))
  ([goals child-color]
   {:goals       (vec (map-indexed (fn [i g] (assoc g :prio i)) goals))
    :child-color child-color}))

(defn brain
  "Returns the mob and its deltas after one tick of its goals. Goals are chosen
   and ticked on every second tick only, which tick decides eid."
  [spec world eid e t tempters]
  (let [e (assoc e :no-action (idle-count world e))]
    (if (even? (+ (long t) (long eid)))
      (let [e (cleaned spec world e t tempters)
            [e ds] (selected spec world eid e t tempters)
            [e ds2] (ticked spec world eid e t tempters)]
        [e (concat ds ds2)])
      [e nil])))

(defn on-interact
  "Returns the deltas f gives for each interact event where the player and its
   target both exist. f takes the player eid, the player, the target eid and
   the target."
  [world events f]
  (mapcat (fn [[tag peid target]]
            (when (= :interact tag)
              (let [p (get-in world [:entities peid])
                    e (get-in world [:entities target])]
                (when (and p e) (f peid p target e)))))
          events))

(defn- feedable? [e t]
  (and (not (mobs/baby? e))
       (not (mobs/in-love? e t))
       (<= (long (or (:breed-ready-at e) 0)) (long t))))

(defn- fed-growth ^long [^long remaining]
  (let [seconds (long (* (double (quot remaining ticks-per-second)) feeding-speedup))]
    (- remaining (* seconds ticks-per-second))))

(defn- fed-deltas [t target e]
  (cond
    (mobs/baby? e)
    (let [remaining (max 0 (- (long (:baby-until e)) (long t)))]
      [[:merge-entity target {:baby-until (+ (long t) (fed-growth remaining))}]])
    (feedable? e t)
    [[:merge-entity target {:love-until (+ (long t) love-ticks)}]
     (out/all (out/status target :love))]))

(defn feed-deltas [world events t]
  (on-interact world events
               (fn [_ p target e]
                 (when (and (mobs/mob-type? (:type e))
                            (contains? (sense/hands-of p) (mobs/breeding-item (:type e))))
                   (fed-deltas t target e)))))
