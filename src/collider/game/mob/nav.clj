(ns collider.game.mob.nav
  "Ground navigation: the path a mob builds, kept in its :nav."
  (:require [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.space.path :as path]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-path-length 16.0)

(def ^:private ^:const recompute-gap 20)

(def ^:private ^:const stuck-interval 100)

(def ^:private ^:const stuck-factor 0.25)

(def ^:private ^:const max-vertical-to-waypoint 1.0)

(def ^:private ^:const max-surface-steps 16)

(def fresh
  "The navigation state of a mob that has never walked anywhere."
  {:path        nil :index 0 :target nil :reach 1 :speed 0.0 :tick 0
   :stuck-check 0 :stuck-pos [0.0 0.0 0.0]
   :timeout-node [0 0 0] :timeout-timer 0 :timeout-check 0
   :timeout-limit 0.0 :delayed? false :recompute 0})

(defn- nav-of [e] (or (:nav e) fresh))

(defn- half-of ^double [e] (double (nth (mobs/box-of e) 0)))

(defn- walker
  "Returns mob e as the path search wants to know it."
  [e]
  (let [[half height] (mobs/box-of e)]
    (assoc path/cow
      :width (* 2.0 (double half)) :height height :pos (:pos e)
      :on-ground? (boolean (:on-ground e))
      :in-water? (boolean (:wet? e)))))

(defn done?
  "Returns true when the mob has no path node left to walk to."
  [e]
  (let [nav (:nav e)]
    (or (nil? (:path nav))
        (>= (long (:index nav)) (count (:nodes (:path nav)))))))

(defn- can-update-path? [e]
  (boolean (or (:on-ground e) (control/in-liquid? e))))

(defn stop
  "Returns mob e with its path dropped."
  [e]
  (if (:path (:nav e)) (assoc-in e [:nav :path] nil) e))

(defn- air-at? [chunks ^long x ^long y ^long z]
  (block/air? (chunk/block-state chunks x y z)))

(defn- solid-at? [chunks ^long x ^long y ^long z]
  (block/solid? (chunk/block-state chunks x y z)))

(defn- loaded? [chunks ^long x ^long z]
  (let [cx (bit-shift-right x 4)
        cz (bit-shift-right z 4)]
    (not (identical? chunk/empty-chunk
                     (chunk/chunk-at chunks cx cz)))))

(defn- above-air ^long [lv ^long x ^long y ^long z]
  (let [chunks (:chunks lv) hi (chunk/level-max-y lv)]
    (loop [cy (inc y)]
      (if (and (<= cy hi) (air-at? chunks x cy z))
        (recur (inc cy))
        cy))))

(defn- column-y ^long [lv ^long x ^long y ^long z]
  (let [chunks (:chunks lv) lo (chunk/level-min-y lv)]
    (loop [cy (dec y)]
      (cond (< cy lo) (above-air lv x y z)
            (air-at? chunks x cy z) (recur (dec cy))
            :else (inc cy)))))

(defn- above-solid ^long [lv ^long x ^long y ^long z]
  (let [chunks (:chunks lv) hi (chunk/level-max-y lv)]
    (loop [cy (inc y)]
      (if (and (<= cy hi) (solid-at? chunks x cy z))
        (recur (inc cy))
        cy))))

(defn- surface-cell
  "Returns the goal cell lifted onto the surface above its column."
  [lv [x y z]]
  (let [chunks (:chunks lv) x (long x) z (long z)
        y (long (if (air-at? chunks x (long y) z)
                  (column-y lv x (long y) z)
                  y))]
    (if (solid-at? chunks x y z)
      [x (above-solid lv x y z) z]
      [x y z])))

(defn- search [world e cell ^long reach]
  (path/find-path world (walker e) #{cell} max-path-length reach
                  1.0))

(defn- searched [world e cell ^long reach]
  (let [p (search world e cell reach)
        nav {:target (:target p) :reach reach
             :timeout-node [0 0 0] :timeout-timer 0
             :timeout-limit 0.0}]
    [(update e :nav merge nav) p]))

(defn- keep-path? [e cell nav]
  (and (:path nav) (not (done? e)) (= cell (:target nav))))

(defn- pathed [world e cell ^long reach]
  (let [nav (:nav e)]
    (cond
      (< (v/y (:pos e)) (chunk/level-min-y world)) [e nil]
      (not (can-update-path? e)) [e nil]
      (keep-path? e cell nav) [e (:path nav)]
      :else (searched world e cell reach))))

(defn- create-path
  "Returns [mob path] for a walk to the cell, the path nil when none
  may be built now. A path under way to the same cell is kept."
  [world e cell ^long reach]
  (let [chunks (:chunks world)
        e (assoc e :nav (nav-of e))
        [gx _ gz] cell]
    (if (loaded? chunks gx gz)
      (pathed world e (surface-cell world cell) reach)
      [e nil])))

(defn- cauldron? [chunks n]
  (block/tagged? (chunk/block-state chunks (:x n) (:y n) (:z n))
                 "cauldrons"))

(defn- raised
  "Returns the nodes with the one at i raised a block, and with it
  the next one the mob would walk down to."
  [v ^long i]
  (let [n (nth v i) nx (get v (inc i))
        y (inc (long (:y n)))
        v (assoc v i (assoc n :y y))]
    (if (and nx (>= (long (:y n)) (long (:y nx))))
      (assoc v (inc i) (assoc nx :y y))
      v)))

(defn- trimmed
  "Returns the path with its nodes in cauldrons raised by one."
  [chunks p]
  (let [nodes (:nodes p)
        lift (fn [v i]
               (if (cauldron? chunks (nth v i)) (raised v i) v))]
    (assoc p :nodes (reduce lift nodes (range (count nodes))))))

(defn- water-at? [chunks ^long x ^long y ^long z]
  (= :water (block/block-of (chunk/block-state chunks x y z))))

(defn- surface-y ^double [world e]
  (let [pos (:pos e)
        chunks (:chunks world)
        x (long (Math/floor (v/x pos)))
        z (long (Math/floor (v/z pos)))
        y0 (long (Math/floor (v/y pos)))
        water? (fn [^long cy] (water-at? chunks x cy z))]
    (if-not (:wet? e)
      (Math/floor (+ (v/y pos) 0.5))
      (loop [cy y0 steps 0]
        (cond (not (water? cy)) cy
              (> (inc steps) max-surface-steps) y0
              :else (recur (inc cy) (inc steps)))))))

(defn- temp-mob-pos [world e]
  (let [pos (:pos e)] [(v/x pos) (surface-y world e) (v/z pos)]))

(defn- moved-to [world e p ^double speed]
  (let [nav (nav-of e)
        same? (= (:nodes p) (:nodes (:path nav)))
        e (assoc e :nav (if same? nav (assoc nav :path p :index 0)))]
    (if (done? e)
      e
      (let [trim (fn [q] (trimmed (:chunks world) q))
            e (update-in e [:nav :path] trim)]
        (update e :nav assoc :speed speed
                :stuck-check (:tick (:nav e))
                :stuck-pos (temp-mob-pos world e))))))

(defn- goal-cell [pos]
  (mapv (fn [c] (long (Math/floor (double c)))) pos))

(defn move-to
  "Returns mob e walking to the cell at speed.
  A goal it cannot reach leaves it without a path."
  [world e pos ^double speed]
  (let [[e p] (create-path world e (goal-cell pos) 1)]
    (if p
      (moved-to world e p speed)
      (stop (assoc e :nav (nav-of e))))))

(defn move-to-entity
  "Returns mob e walking to entity o at speed.
  A goal it cannot reach leaves the path it already walks."
  [world e o ^double speed]
  (let [[e p] (create-path world e (goal-cell (:pos o)) 1)]
    (if p (moved-to world e p speed) e)))

(defn- rebuilt [world e nav ^long t]
  (let [e (assoc-in (assoc e :nav nav) [:nav :path] nil)
        [e p] (create-path world e (:target nav) (:reach nav))]
    (update e :nav assoc :path p :index 0 :recompute t
            :delayed? false)))

(defn recompute-path
  "Returns mob e with its path to the same goal built again.
  Sooner than 20 ticks after the last one it only asks for a later
  recomputation."
  [world e]
  (let [nav (nav-of e) t (long (:tick world))]
    (cond
      (or (<= (- t (long (:recompute nav))) recompute-gap)
          (not (can-update-path? e)))
      (assoc e :nav (assoc nav :delayed? true))
      (nil? (:target nav)) (assoc e :nav nav)
      :else (rebuilt world e nav t))))

(defn- node-of [e ^long i] (nth (:nodes (:path (:nav e))) i))

(defn- current-node [e] (node-of e (long (:index (:nav e)))))

(defn- cell-of [n] [(:x n) (:y n) (:z n)])

(defn- bottom-centre [[x y z]]
  [(+ (double (long x)) 0.5) (double (long y))
   (+ (double (long z)) 0.5)])

(defn- entity-pos-at [e ^long i]
  (let [n (node-of e i)
        off (* 0.5 (long (+ (* 2.0 (half-of e)) 1.0)))]
    [(+ (double (long (:x n))) off) (double (long (:y n)))
     (+ (double (long (:z n))) off)]))

(defn cut-corner?
  "Tests whether a node of type t may be walked past on a corner."
  [t]
  (not (contains? #{:fire-in-neighbor :damaging-in-neighbor
                    :walkable-door} t)))

(defn- unit [p ^double l]
  (mapv (fn [c] (/ (double c) l)) p))

(defn- dot ^double [a b]
  (double (reduce + (map * a b))))

(defn- turned-back? [cur nxt mob-pos]
  (let [origin [0.0 0.0 0.0]
        to-cur (mapv - cur mob-pos)
        to-nxt (mapv - nxt mob-pos)
        cs (v/dist3-sq origin to-cur)
        ns (v/dist3-sq origin to-nxt)]
    (and (or (< ns cs) (< cs 0.5))
         (neg? (dot (unit to-nxt (Math/sqrt ns))
                    (unit to-cur (Math/sqrt cs)))))))

(defn- target-next?
  "Tests whether the mob has left the node it walks to behind."
  [e mob-pos]
  (let [nav (:nav e) i (long (:index nav))]
    (and (< (inc i) (count (:nodes (:path nav))))
         (let [cur (bottom-centre (cell-of (node-of e i)))
               nxt (bottom-centre (cell-of (node-of e (inc i))))]
           (and (< (v/dist3-sq mob-pos cur) 4.0)
                (turned-back? cur nxt mob-pos))))))

(defn- close-enough? [e]
  (let [n (current-node e) pos (:pos e)
        w (* 2.0 (half-of e))
        maxd (if (> w 0.75) (/ w 2.0) (- 0.75 (/ w 2.0)))]
    (and (< (Math/abs (- (v/x pos) (+ (double (long (:x n))) 0.5)))
            maxd)
         (< (Math/abs (- (v/z pos) (+ (double (long (:z n))) 0.5)))
            maxd)
         (< (Math/abs (- (v/y pos) (double (long (:y n)))))
            max-vertical-to-waypoint))))

(defn- stuck-check [e mob-pos]
  (let [nav (:nav e)
        gap (- (long (:tick nav)) (long (:stuck-check nav)))]
    (if (<= gap stuck-interval)
      e
      (let [s (double (:speed (:move e) 0.0))
            eff (if (>= s 1.0) s (* s s))
            thr (* eff (double stuck-interval) stuck-factor)
            d (v/dist3-sq mob-pos (:stuck-pos nav))
            moved? (>= d (* thr thr))
            nav (assoc nav :stuck-check (:tick nav)
                       :stuck-pos mob-pos
                       :path (when moved? (:path nav)))]
        (assoc e :nav nav)))))

(defn- timeout-of ^double [e mob-pos cell]
  (let [s (double (:speed (:move e) 0.0))]
    (if (pos? s)
      (* (/ (Math/sqrt (v/dist3-sq mob-pos (bottom-centre cell))) s)
         20.0)
      0.0)))

(defn- timed
  "Returns the navigation with the wait on its next node counted."
  [e nav mob-pos cell t]
  (if (= cell (:timeout-node nav))
    (update nav :timeout-timer +
            (- (long t) (long (:timeout-check nav))))
    (assoc nav :timeout-node cell
           :timeout-limit (timeout-of e mob-pos cell))))

(defn- timed-out? [nav]
  (let [lim (double (:timeout-limit nav))]
    (and (pos? lim)
         (> (double (:timeout-timer nav)) (* 3.0 lim)))))

(defn- node-timeout [world e mob-pos]
  (if (done? e)
    e
    (let [t (long (:tick world))
          cell (cell-of (current-node e))
          nav (assoc (timed e (:nav e) mob-pos cell t)
                     :timeout-check t)
          nav (if-not (timed-out? nav)
                nav
                (assoc nav :path nil :timeout-node [0 0 0]
                       :timeout-timer 0 :timeout-limit 0.0))]
      (assoc e :nav nav))))

(defn- advance? [e mob-pos]
  (or (close-enough? e)
      (and (cut-corner? (:type (current-node e)))
           (target-next? e mob-pos))))

(defn- follow-the-path [world e]
  (let [mob-pos (temp-mob-pos world e)
        e (cond-> e
                  (advance? e mob-pos)
                  (update-in [:nav :index] inc))]
    (node-timeout world (stuck-check e mob-pos) mob-pos)))

(defn- falling-past? [world e]
  (let [mob-pos (temp-mob-pos world e)
        pos (entity-pos-at e (long (:index (:nav e))))]
    (and (> (double (nth mob-pos 1)) (double (nth pos 1)))
         (not (:on-ground e))
         (= (long (Math/floor (double (nth mob-pos 0))))
            (long (Math/floor (double (nth pos 0)))))
         (= (long (Math/floor (double (nth mob-pos 2))))
            (long (Math/floor (double (nth pos 2))))))))

(defn- ground-y ^double [world [x y z]]
  (let [cx (long (Math/floor (double x)))
        cy (long (Math/floor (double y)))
        cz (long (Math/floor (double z)))]
    (if (air-at? (:chunks world) cx (dec cy) cz)
      (double y)
      (control/floor-level (:chunks world) cx cy cz))))

(defn- aimed [world e]
  (let [p (entity-pos-at e (long (:index (:nav e))))]
    (control/wanted e (nth p 0) (ground-y world p) (nth p 2)
                    (double (:speed (:nav e))))))

(defn- stepped [world e]
  (cond
    (can-update-path? e) (follow-the-path world e)
    (falling-past? world e) (update-in e [:nav :index] inc)
    :else e))

(defn tick
  "Runs one tick of the navigation of mob e.
  It walks the path on and tells the move control where to go."
  [world e]
  (let [e (cond-> e (:path (:nav e)) (update-in [:nav :tick] inc))
        e (if (:delayed? (:nav e)) (recompute-path world e) e)]
    (if (done? e)
      e
      (let [e (stepped world e)]
        (if (done? e) e (aimed world e))))))
