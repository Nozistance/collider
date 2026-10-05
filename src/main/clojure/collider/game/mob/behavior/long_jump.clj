(ns collider.game.mob.behavior.long-jump
  "Long jumps to a block a mob cannot walk to, and the flight after."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.behavior.core :as c]
            [collider.game.mob.brain :as b]
            [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.randompos :as pos]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const gravity 0.08)

(def ^:private ^:const prepare-ticks 40)

(def ^:private angles [65 70 75 80])

(def ^:private ^:const path-length 8)

(def ^:private ^:const tiny (double (float 1.0E-5)))

(def ^:private ^:const spread (double (float 0.9)))

(def ^:private ^:const damping (double (float 0.95)))

(def ^:private ^:const slide (double (float 0.1)))

(defn- ceil ^long [^double a] (long (Math/ceil a)))

(defn- plus [p q] (mapv + p q))

(defn- minus [p q] (mapv - p q))

(defn- scaled [p ^double k] (mapv #(* (double %) k) p))

(defn- unit [p]
  (let [d (Math/sqrt (reduce + (map * p p)))]
    (if (< d tiny) [0.0 0.0 0.0] (mapv #(/ (double %) d) p))))

(defn- clear-at? [chunks e [half h] at]
  (let [[x y z] at half (double half)]
    (phys/box-free?
      chunks [(- x half) y (- z half) (+ x half) (+ y h) (+ z half)]
      (v/y (:pos e)) (phys/context e))))

(defn- clear-move?
  "Returns true when box b of mob e meets no block at the points
  from p to q, a little less apart than its least side."
  [chunks e b p q]
  (let [d (minus q p)
        m (Math/min (* 2.0 (double (b 0))) (double (b 1)))
        n (ceil (/ (Math/sqrt (reduce + (map * d d))) m))
        u (scaled (unit d) (* m spread))]
    (loop [i 0 at p]
      (or (>= i n)
          (let [at (if (== i (dec n)) q (plus at u))]
            (and (clear-at? chunks e b at) (recur (inc i) at)))))))

(defn- arc-at
  "Returns how far a leap at sine sa and cosine ca with speed squared
  vv takes a body that went ri across, toward x and z cosine cx and
  sine sx."
  [[sa ca vv cx sx] ^double ri]
  (let [[sa ca vv] (map double [sa ca vv])]
    [(* ri (double cx))
     (- (* (/ sa ca) ri)
        (/ (* (Math/pow ri 2.0) gravity)
           (* (* 2.0 vv) (Math/pow ca 2.0))))
     (* ri (double sx))]))

(defn- arc-clear?
  "Returns true when mob e at p meets no block in n steps of its
  leap k over r across."
  [chunks e p r n k]
  (let [b (entity/box (assoc e :pose :long-jumping))
        n (long n) dr (/ (double r) (double n))]
    (loop [i 0 ri 0.0 prev nil]
      (or (>= i (dec n))
          (let [ri (+ ri dr) at (plus p (arc-at k ri))]
            (and (or (nil? prev) (clear-move? chunks e b prev at))
                 (recur (inc i) ri at)))))))

(defn- aim
  "Returns the angle, the reach across squared and the rise from p
  to the edge of the block centre to that faces p."
  [p to]
  (let [d [(- (to 0) (p 0)) 0.0 (- (to 2) (p 2))]
        [dx y dz] (minus (minus to (scaled (unit d) 0.5)) p)]
    [(Math/atan2 dz dx) (+ (+ (* dx dx) 0.0) (* dz dz)) y]))

(defn- speed-squared ^double [rad ^double r2 ^double y]
  (let [ca (Math/cos rad)]
    (/ (* r2 gravity)
       (- (* (Math/sqrt r2) (Math/sin (num/fmul 2.0 rad)))
          (* (* 2.0 y) (Math/pow ca 2.0))))))

(defn jump-vector
  "Returns the speed mob e leaps with at angle a degrees to land on
  point to, or nil when it needs more than top or would hit a block
  on the way."
  [world e to a top]
  (let [p (vec (:pos e)) [xz r2 y] (aim p (vec to))
        rad (num/fdiv (num/fmul a (num/f32 Math/PI)) 180.0)
        vv (speed-squared rad r2 y) v0 (Math/sqrt vv)
        sa (Math/sin rad) ca (Math/cos rad) r (Math/sqrt r2)
        k [sa ca vv (Math/cos xz) (Math/sin xz)]
        n (* 2 (ceil (/ r (* v0 ca))))]
    (when-not (or (< vv 0.0) (> v0 (double top))
                  (not (arc-clear? (:chunks world) e p r n k)))
      (let [h (* v0 ca)]
        (scaled [(* h (k 3)) (* v0 sa) (* h (k 4))] damping)))))

(defn- other-column? [e [x _ z]]
  (let [[mx _ mz] (v/cell (:pos e))]
    (not (and (== (long x) (long mx)) (== (long z) (long mz))))))

(defn- landing? [world e [x y z :as cell]]
  (and (other-column? e cell)
       (block/solid-render? (sense/block-at world x (dec (long y)) z))
       (not (pos/has-malus? world (mobs/walker (:type e)) cell))))

(defn- spots
  "Returns the cells mob e may land on within w across and h up or
  down, in the order rolls pick them, the far ones first more often."
  [world e t eid i w h]
  (let [[x y z] (v/cell (:pos e))
        ds (for [dx (range (- w) (inc w)) dy (range (- h) (inc h))
                 dz (range (- w) (inc w))
                 :let [c [(+ x dx) (+ y dy) (+ z dz)]]
                 :when (landing? world e c)]
             [c (+ (* dx dx) (* dy dy) (* dz dz))])
        rank (fn [[c n]]
               (/ (Math/log (c/roll t eid i [:spot c])) (double n)))]
    (map first (sort-by rank > ds))))

(defn- leap [world e t eid i cell top]
  (let [pick #(random/below (c/roll t eid i [:angle cell %]) %)]
    (some #(jump-vector world e (v/centre cell) % top)
          (random/shuffled pick angles))))

(defn- tried [world [e] cell j]
  (let [e (b/remember e :look-target (c/at-block cell) b/forever)
        [e p] (nav/short-path world e cell 0 path-length)]
    (if (:reached? p) [e nil] (reduced [e j]))))

(defn- picked
  "Returns mob e with the first of cells it can leap to and not walk
  to as its look target, and the leap, or nil when there is none."
  [world eid e t i cells top]
  (reduce (fn [acc cell]
            (if-let [j (leap world (acc 0) t eid i cell top)]
              (tried world acc cell j)
              acc))
          [e nil] cells))

(defn- posed [eid e pose free?]
  [(assoc e :pose pose :discard-friction? free?)
   [[:merge-entity eid {:pose pose :discard-friction? free?}]]])

(defn- heard [eid e sound volume]
  (out/all (out/entity-sound (sound e) eid (:pos e) volume 1.0 nil)))

(defn- leapt [eid e jump sound]
  (let [n (Math/sqrt (reduce + (map * jump jump)))
        k (/ (+ n (mobs/jump-boost-power e)) n)
        [e ds] (posed eid e (:pose e) true)
        e (assoc e :yaw (control/body-yaw e)
                 :vel (v/v3 (scaled jump k)))]
    [(b/remember e :long-jump-mid-jump true b/forever)
     (conj ds (heard eid e sound 1.0))]))

(def ^:private cooldown :long-jump-cooldown-ticks)

(defn- half-wait ^long [t eid i span]
  (quot (c/sample t eid i :wait span) 2))

(defn- honey? [world e]
  (let [st (sense/block-at world (v/cell (:pos e)))]
    (= :honey-block (block/block-of st))))

(defn- may-start [span]
  (fn [w eid e t i]
    (if (and (:on-ground e) (not (:wet? e)) (not (:in-lava? e))
             (not (honey? w e)))
      [true e]
      (let [n (half-wait t eid i span)]
        [false (b/remember e cooldown n (+ (long t) n))]))))

(defn- given-up [e t eid i span]
  (-> (c/cool e cooldown (half-wait t eid i span) t)
      (b/erase :look-target)))

(defn- going [span]
  (fn [_ eid e t i]
    (let [{:keys [from jump left?]} (b/slot e i)]
      (cond (and (v/same? from (:pos e)) (not (:wet? e))
                 (or jump left?)) true
            (b/present? e :long-jump-mid-jump t) false
            :else [false (given-up e t eid i span)]))))

(defn- top-speed ^double [e mult]
  (num/f32 (* (mobs/attribute (:type e) :jump-strength)
              (num/f32 mult))))

(defn- chosen [world eid e t i [w h mult]]
  (let [cells (spots world e t eid i w h)
        [e j] (picked world eid e t i cells (top-speed e mult))]
    (b/with-slot e i
      (assoc (b/slot e i) :jump j :at t :left? (some? j)))))

(defn- jump-tick [reach sound]
  (fn [world eid e t i]
    (let [{:keys [jump at]} (b/slot e i)]
      (cond
        (nil? jump) (chosen world eid e t i reach)
        (>= (- (long t) (long at)) prepare-ticks)
        (leapt eid e jump sound)
        :else e))))

(defn long-jump-to-random-pos
  "Returns the behaviour that picks a block within w across and h up
  or down that a mob cannot walk to, looks at it and leaps there,
  no faster than mult times its jump strength, with the sound that
  sound gives. A failed try waits half of a wait from span."
  [span w h mult sound]
  {:id :long-jump-to-random-pos :duration [200 200]
   :needs {:look-target :registered cooldown :absent
           :long-jump-mid-jump :absent}
   :check (may-start span)
   :start (fn [_ _ e _ i]
            (b/with-slot e i {:from (:pos e) :left? true}))
   :continue? (going span)
   :tick (jump-tick [w h mult] sound)})

(defn- slid [e]
  (let [u (:vel e)]
    (assoc e :vel
           (v/v3 (* (v/x u) slide) (v/y u) (* (v/z u) slide)))))

(defn- landed [eid e t i span sound]
  (let [og? (:on-ground e)
        e (c/cool (b/erase (cond-> e og? slid) :long-jump-mid-jump)
                  cooldown (c/sample t eid i :wait span) t)
        [e ds] (posed eid e nil false)]
    [e (cond-> ds og? (conj (heard eid e sound 2.0)))]))

(defn long-jump-mid-jump
  "Returns the behaviour of a mob in the air after a long jump. It
  lands with the sound that sound gives and waits a time from span
  before the next."
  [span sound]
  {:id :long-jump-mid-jump :duration [100 100]
   :needs {:look-target :registered :long-jump-mid-jump :present}
   :continue? (fn [_ _ e _ _] (not (:on-ground e)))
   :start (fn [_ eid e _ _] (posed eid e :long-jumping true))
   :stop (fn [_ eid e t i] (landed eid e t i span sound))})
