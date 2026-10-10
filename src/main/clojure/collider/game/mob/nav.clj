(ns collider.game.mob.nav
  "Ground navigation of mobs."
  (:require [collider.game.entity.gen :as gen]
            [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.steer :as steer]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
            [collider.world.space.path :as path]))

(set! *warn-on-reflection* true)

(def ^:private ^:const required-length 16.0)

(def ^:private ^:const recompute-gap 20)

(def fresh
  "The navigation of a mob that has never walked anywhere."
  {:path nil :index 0 :target nil :reach 1 :speed 0.0 :tick 0
   :stuck-check 0 :stuck-pos [0.0 0.0 0.0] :timeout-node [0 0 0]
   :timeout-timer 0 :timeout-check 0 :timeout-limit 0.0 :delayed? false
   :recompute 0 :stuck? false})

(defn- nav-of [e] (merge fresh (:nav e)))

(defn- half-of ^double [e] (double (nth (mobs/box-of e) 0)))

(defn- path-length
  "Returns the follow range of mob e with the bonus it drew at spawn,
  never under the 16 a path needs."
  ^double [e]
  (let [b (double (or (:follow-bonus e) 0.0))
        r (mobs/attribute (:type e) :follow-range)
        v (float (+ r (* r b)))]
    (double (Math/max v (float required-length)))))

(defn- walker [e]
  (let [[half height] (mobs/box-of e)
        len (float (path-length e))
        visits (long (Math/floor (* len (float 16.0))))]
    (assoc (mobs/walker (:type e))
      :max-visited visits :max-up-step (mobs/step-height (:type e))
      :width (* 2.0 (double half)) :height height :pos (:pos e)
      :on-ground? (boolean (:on-ground e)) :ctx (phys/context e)
      :in-water? (boolean (:wet? e)))))

(defn done?
  "Returns true when the mob has no path node left to walk to."
  [e]
  (control/walked? (:nav e)))

(defn- can-update-path? [e]
  (boolean (or (:on-ground e) (control/in-liquid? e))))

(defn stop
  "Returns mob e without its path. Its goal stays."
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
  [lv [x y z]]
  (let [chunks (:chunks lv) x (long x) z (long z)
        y (long (if (air-at? chunks x (long y) z)
                  (column-y lv x (long y) z)
                  y))]
    (if (solid-at? chunks x y z)
      [x (above-solid lv x y z) z]
      [x y z])))

(defn- search [world e cell reach len]
  (path/find-path world (walker e) #{cell} len reach 1.0))

(defn- searched [world e cell reach len]
  (let [p (search world e cell reach len)
        nav {:target (:target p) :reach reach
             :timeout-node [0 0 0] :timeout-timer 0
             :timeout-limit 0.0}
        nav (cond-> nav (:target p) (assoc :stuck? false))]
    [(update e :nav merge nav) p]))

(defn- keep-path? [e cell nav]
  (and (:path nav) (not (done? e)) (= cell (:target nav))))

(defn- pathed [world e cell reach len]
  (let [nav (:nav e)]
    (cond
      (< (v/y (:pos e)) (chunk/level-min-y world)) [e nil]
      (not (can-update-path? e)) [e nil]
      (keep-path? e cell nav) [e (:path nav)]
      :else (searched world e cell reach len))))

(defn create-path
  "Returns [e path] with the path mob e would walk to the cell, which
  it takes to be reached within reach, or a nil path. The mob keeps
  walking what it walked."
  [world e cell ^long reach]
  (let [chunks (:chunks world)
        e (assoc e :nav (nav-of e))
        [gx _ gz] cell]
    (if (loaded? chunks gx gz)
      (pathed world e (surface-cell world cell) reach (path-length e))
      [e nil])))

(defn short-path
  "Returns [e path] with the path mob e would walk to the cell as it
  is, no longer than len, or a nil path."
  [world e cell reach len]
  (pathed world (assoc e :nav (nav-of e)) cell reach (double len)))

(defn- cauldron? [chunks n]
  (block/tagged? (chunk/block-state chunks (:x n) (:y n) (:z n))
                 "cauldrons"))

(defn- raised [v ^long i]
  (let [n (nth v i) nx (get v (inc i))
        y (inc (long (:y n)))
        v (assoc v i (assoc n :y y))]
    (if (and nx (>= (long (:y n)) (long (:y nx))))
      (assoc v (inc i) (assoc nx :y y))
      v)))

(defn- trimmed [chunks p]
  (let [nodes (:nodes p)
        lift (fn [v i]
               (if (cauldron? chunks (nth v i)) (raised v i) v))]
    (assoc p :nodes (reduce lift nodes (range (count nodes))))))

(def ^:private ^:const max-surface-steps 16)

(defn- surface-y
  "Returns the height a mob at x y z walks its path from. A wet mob
  walks from the water surface above it."
  [chunks x y z wet?]
  (if-not wet?
    (Math/floor (+ y 0.5))
    (let [cx (long (Math/floor x)) cz (long (Math/floor z))
          y0 (long (Math/floor y))]
      (loop [cy y0 steps 0]
        (cond
          (not (identical? :water (block/block-of
                                    (chunk/block-state chunks cx cy cz))))
          (double cy)
          (> (inc steps) max-surface-steps) (double y0)
          :else (recur (inc cy) (inc steps)))))))

(defn- temp-mob-pos [world e]
  (let [pos (:pos e)]
    [(v/x pos)
     (surface-y (:chunks world) (v/x pos) (v/y pos) (v/z pos)
                (boolean (:wet? e)))
     (v/z pos)]))

(defn moved-to
  "Returns mob e walking path p at speed."
  [world e p ^double speed]
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

(defn path-to
  "Returns mob e walking to the cell at speed, which it takes to be
  reached within reach, or nil when no path leads there."
  [world e pos speed reach]
  (let [[e p] (create-path world e (goal-cell pos) (long reach))]
    (when p (moved-to world e p (double speed)))))

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
  Sooner than 20 ticks after the last one it only asks for a
  later recomputation."
  [world e]
  (let [nav (nav-of e) t (long (:tick world))]
    (cond
      (or (<= (- t (long (:recompute nav))) recompute-gap)
          (not (can-update-path? e)))
      (assoc e :nav (assoc nav :delayed? true))
      (nil? (:target nav)) (assoc e :nav nav)
      :else (rebuilt world e nav t))))

(defn cut-corner?
  "Returns true when a node of type t may be walked past on a corner."
  [t]
  (not (#{:fire-in-neighbor :damaging-in-neighbor :walkable-door} t)))

(def ^:private ^:const stuck-interval 100)

(def ^:private ^:const stuck-factor 0.25)

(defn- node [nav ^long i] (nth (:nodes (:path nav)) i))

(defn- offset ^double [^double half] (* 0.5 (long (+ (* 2.0 half) 1.0))))

(defn- close-enough?
  "Returns true when a mob width wide at x y z stands close enough to
  node n to take the next."
  [x y z n width]
  (let [maxd (if (> width 0.75) (/ width 2.0) (- 0.75 (/ width 2.0)))]
    (and (< (abs (- x (+ (long (:x n)) 0.5))) maxd)
         (< (abs (- z (+ (long (:z n)) 0.5))) maxd)
         (< (abs (- y (double (:y n)))) 1.0))))

(defn- turned-back?
  "Returns true when a mob at x y z near node c has the next node n
  behind it, so it may skip c."
  [x y z c n]
  (let [cx (- (+ (long (:x c)) 0.5) x) cy (- (double (:y c)) y)
        cz (- (+ (long (:z c)) 0.5) z)
        cs (+ (* cx cx) (* cy cy) (* cz cz))]
    (and (< cs 4.0)
         (let [nx (- (+ (long (:x n)) 0.5) x) ny (- (double (:y n)) y)
               nz (- (+ (long (:z n)) 0.5) z)
               ns (+ (* nx nx) (* ny ny) (* nz nz))
               cl (Math/sqrt cs) nl (Math/sqrt ns)]
           (and (or (< ns cs) (< cs 0.5))
                (< (+ (* (/ nx nl) (/ cx cl)) (* (/ ny nl) (/ cy cl))
                      (* (/ nz nl) (/ cz cl)))
                   0.0))))))

(defn- timeout
  "Returns the ticks a mob at x y z walking at speed is given to reach
  node n, 0 when it stands."
  [speed x y z n]
  (if-not (> speed 0.0)
    0.0
    (let [dx (- x (+ (long (:x n)) 0.5)) dy (- y (double (:y n)))
          dz (- z (+ (long (:z n)) 0.5))]
      (* (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) speed) 20.0))))

(defn- advanced? [nav x y z my half]
  (let [i (long (:index nav)) c (node nav i)
        nodes (:nodes (:path nav))]
    (or (close-enough? x y z c (* 2.0 half))
        (and (cut-corner? (:type c))
             (< (inc i) (count nodes))
             (turned-back? x my z c (nth nodes (inc i)))))))

(defn- dropped [nav] (assoc nav :path nil))

(defn- checked-stuck
  "Returns nav after the stuck check of a mob at x my z driving at
  drive, which runs once in a hundred ticks."
  [nav0 nav x my z drive]
  (if (> (- (long (:tick nav0)) (long (:stuck-check nav0)))
         stuck-interval)
    (let [eff (if (>= drive 1.0) drive (* drive drive))
          thr (* eff stuck-interval stuck-factor)
          [px py pz] (:stuck-pos nav0)
          dx (- x (double px)) dy (- my (double py)) dz (- z (double pz))
          stuck? (not (>= (+ (* dx dx) (* dy dy) (* dz dz)) (* thr thr)))
          nav (assoc nav :stuck-check (:tick nav0)
                     :stuck-pos [x my z] :stuck? stuck?)]
      (if stuck? (dropped nav) nav))
    nav))

(defn- timed
  "Returns nav after the timeout of the node it walks to. A mob that
  takes three times as long as it should drops its path."
  [nav drive x my z t]
  (let [n (node nav (:index nav))
        cell [(long (:x n)) (long (:y n)) (long (:z n))]
        nav (if (= cell (:timeout-node nav))
              (update nav :timeout-timer
                      #(+ (long %) (- t (long (:timeout-check nav)))))
              (assoc nav :timeout-node cell
                     :timeout-limit (timeout drive x my z n)))
        nav (assoc nav :timeout-check t)
        limit (double (:timeout-limit nav))]
    (if (and (> limit 0.0) (> (double (:timeout-timer nav)) (* 3.0 limit)))
      (assoc (dropped nav) :stuck? false :timeout-node [0 0 0]
             :timeout-timer 0 :timeout-limit 0.0)
      nav)))

(defn- followed [nav x y z my half drive t]
  (let [n (cond-> nav (advanced? nav x y z my half) (update :index inc))
        n (checked-stuck nav n x my z drive)]
    (if (control/walked? n) n (timed n drive x my z t))))

(defn- stepped [world e nav]
  (let [pos (:pos e) x (v/x pos) y (v/y pos) z (v/z pos)
        my (surface-y (:chunks world) x y z (boolean (:wet? e)))
        half (half-of e)]
    (if (can-update-path? e)
      (followed nav x y z my half (double (:speed (:move e) 0.0))
                (long (:tick world)))
      (let [n (node nav (:index nav)) off (offset half)]
        (if (and (> my (double (:y n))) (not (:on-ground e))
                 (== (Math/floor x) (Math/floor (+ (long (:x n)) off)))
                 (== (Math/floor z) (Math/floor (+ (long (:z n)) off))))
          (update nav :index inc)
          nav)))))

(defn- aimed [world e nav]
  (let [n (node nav (:index nav)) off (offset (half-of e))
        x (+ (long (:x n)) off) y (double (:y n)) z (+ (long (:z n)) off)
        cx (long (Math/floor x)) cy (long (Math/floor y))
        cz (long (Math/floor z))
        gy (if (zero? (chunk/block-state (:chunks world) cx (dec cy) cz))
             y
             (control/floor-level (:chunks world) cx cy cz))]
    (steer/wanted (:move e) x gy z (:speed nav))))

(defn- ticked [nav]
  (if (:path nav) (update nav :tick inc) nav))

(defn- walked-on [world e nav0 nav]
  (if (control/walked? nav)
    [(if (identical? nav nav0) e (assoc e :nav nav)) nil nil]
    (let [nav2 (stepped world e nav)]
      (cond (not (control/walked? nav2)) [e nav2 (aimed world e nav2)]
            (identical? nav0 nav2) [e nil nil]
            :else [(assoc e :nav nav2) nil nil]))))

(defn aim
  "Returns mob e after one tick of its navigation, with the path
  state and the move it aims at. Both are nil when it walks no path,
  and the caller sets them on the mob."
  [world e]
  (let [nav0 (:nav e)
        nav (ticked nav0)]
    (if (:delayed? nav)
      (let [e (recompute-path world (assoc e :nav nav))]
        (walked-on world e (:nav e) (:nav e)))
      (walked-on world e nav0 nav))))

(defn tick
  "Returns mob e after one tick of its navigation.
  The mob walks its path on and tells its move control where to go."
  [world e]
  (let [[e nav m] (aim world e)]
    (if nav (gen/with e {:nav nav :move m}) e)))
