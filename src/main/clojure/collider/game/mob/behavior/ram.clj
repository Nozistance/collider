(ns collider.game.mob.behavior.ram
  "Rams, the walk back from a target and the charge at it."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.behavior.core :as c]
            [collider.game.mob.brain :as b]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.randompos :as pos]
            [collider.game.mob.sense :as sense]
            [collider.game.mob.sensor :as sensor]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.feature.level :as feature]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tiny (double (float 1.0E-5)))

(def ^:private ^:table snapping
  (delay (set (data/tag-values "block" "snaps_goat_horn"))))

(defn- in-border? [o]
  (let [[h] (entity/box o) p (:pos o) b (double chunk/world-border)
        in? (fn [^double a] (and (>= a (- b)) (< a b)))
        h (double h) x (v/x p) z (v/z p)]
    (and (in? (- x h)) (in? (- z h))
         (in? (- (+ x h) tiny)) (in? (- (+ z h) tiny)))))

(defn- spared? [world o]
  (or (game-mode/invulnerable? o)
      (and (entity/player? o) (zero? (difficulty/id world)))))

(defn foe?
  "Returns true when goat e may ram entity o in world."
  [world e o]
  (and (entity/living? o) (game-mode/seen? o) (not= :goat (:type o))
       (or (get-in world [:rules :mob-griefing] true)
           (not= :armor-stand (:type o)))
       (in-border? o) (not (spared? world o))
       (animal/in-sight? world e o)))

(defn- walkable? [world e [x y z :as cell]]
  (and (block/solid-render? (sense/block-at world x (dec (long y)) z))
       (not (pos/has-malus? world (mobs/walker (:type e)) cell))))

(def ^:private sides [[0 -1] [1 0] [0 1] [-1 0]])

(defn- step [c [dx dz]]
  [(+ (long (c 0)) (long dx)) (c 1) (+ (long (c 2)) (long dz))])

(defn- furthest [world e at side far]
  (loop [i 0 c at]
    (let [n (step c side)]
      (if (and (< i (long far)) (walkable? world e n))
        (recur (inc i) n)
        c))))

(defn- reachable [world [e] cell]
  (let [[e p] (nav/create-path world e cell 0)]
    (if (:reached? p) (reduced [e cell]) [e nil])))

(defn- start-cell
  "Returns [e cell] with the cell from which mob e rams the target in
  cell to, at least lo and at most hi blocks away, or a nil cell."
  [world e to [lo hi]]
  (if (walkable? world e to)
    (let [me (v/cell (:pos e))
          cs (->> sides
                  (map #(furthest world e to % hi))
                  (filter #(>= (feature/manhattan % to) (long lo)))
                  (sort-by #(v/dist-sq me %)))]
      (reduce (partial reachable world) [e nil] cs))
    [e nil]))

(defn- chosen [world e i span oid o]
  (let [to (v/cell (:pos o))
        [e cell] (start-cell world e to span)]
    (b/with-slot e i
      {:candidate (when cell {:start cell :at to :oid oid})})))

(defn- lower [eid] (out/all (out/status eid :lower-head)))

(defn- raise [eid] (out/all (out/status eid :raise-head)))

(defn- heard [eid e sound pitch]
  (out/all (out/entity-sound sound eid (:pos e) 1.0 pitch nil)))

(defn- voice-pitch [e t eid i]
  (let [r #(num/f32 (c/roll t eid i %))
        d (num/f32 (- (r :pitch-a) (r :pitch-b)))]
    (num/f32 (+ (num/f32 (* d (num/f32 0.2)))
                (if (mobs/baby? e) 1.5 1.0)))))

(defn- toward ^double [a b]
  (let [s (Math/signum (double (- (long b) (long a))))]
    (+ (double b) 0.5 (* 0.5 s))))

(defn- edge [[sx _ sz] [x y z]]
  [(toward sx x) (double y) (toward sz z)])

(defn- aimed [eid e t i {:keys [start at]} sound]
  (let [e (-> (b/remember e :ram-target (edge start at) b/forever)
              (b/with-slot i {}))
        pitch (voice-pitch e t eid i)]
    [e [(lower eid) (heard eid e (sound e) pitch)]]))

(defn- waited [eid e t i cand ticks sound]
  (let [since (or (:since (b/slot e i)) t)
        e (b/with-slot e i (assoc (b/slot e i) :since since))]
    (if (>= (- (long t) (long since)) (long ticks))
      (aimed eid e t i cand sound)
      [e [(lower eid)]])))

(defn- prepared [world eid e t i cand [span speed ticks sound]]
  (let [{:keys [start at oid]} cand
        o (c/other world oid)
        wt (c/walk-target (c/at-block start) speed 0)
        lt (c/at-entity oid true)
        e (-> (b/remember e :walk-target wt b/forever)
              (b/remember :look-target lt b/forever))]
    (cond (not= at (v/cell (:pos o)))
          [(chosen world (nav/stop e) i span oid o) [(raise eid)]]
          (= start (v/cell (:pos e)))
          (waited eid e t i cand ticks sound)
          :else e)))

(defn- prepare-tick [opts]
  (fn [world eid e t i]
    (if-let [cand (:candidate (b/slot e i))]
      (prepared world eid e t i cand opts)
      e)))

(defn- prepare-start [foe? span]
  (fn [w eid e t i]
    (let [[oid e] (sensor/closest w eid e t #(foe? w e %2))]
      (if oid (chosen w e i span oid (c/other w oid)) e))))

(defn- candidate-alive? [w _ e _ i]
  (let [oid (:oid (:candidate (b/slot e i)))]
    (and (some? oid) (entity/alive? (c/other w oid)))))

(defn prepare-ram-nearest-target
  "Returns the behaviour that picks the nearest visible entity foe?
  accepts, walks at speed to a cell from lo to hi blocks off it in a
  line, waits there ticks and aims a ram, with the sound that sound
  gives. A try with no ram waits the cooldown on-fail gives."
  [on-fail [lo hi] speed foe? ticks sound]
  {:id :prepare-ram-nearest-target :duration [160 160]
   :needs {:look-target :registered :ram-cooldown-ticks :absent
           :nearest-visible-living-entities :present
           :ram-target :absent}
   :start (prepare-start foe? [lo hi])
   :continue? candidate-alive?
   :tick (prepare-tick [[lo hi] speed ticks sound])
   :stop (fn [_ eid e t _]
           (if (b/present? e :ram-target t)
             e
             [(c/cool e :ram-cooldown-ticks (on-fail e) t)
              [(raise eid)]]))})

(defn- level-of ^long [e k]
  (if-let [x (get (:effects e) k)] (inc (long (:amplifier x))) 0))

(defn- speed-factor ^double [e]
  (let [v (num/f32 (:speed (:move e) 0.0))
        s (num/f32 (* v (num/f32 1.65)))
        n (- (level-of e :speed) (level-of e :slowness))
        boost (num/f32 (* 0.25 n))]
    (num/f32 (+ (Math/max (num/f32 0.2) (Math/min 3.0 s)) boost))))

(defn- body-box [o]
  (let [[h ht] (entity/box o) p (:pos o) h (double h)]
    [(- (v/x p) h) (v/y p) (- (v/z p) h)
     (+ (v/x p) h) (+ (v/y p) (double ht)) (+ (v/z p) h)]))

(defn- rammed [world eid e foe?]
  (->> (sense/around world (:pos e) 3.0)
       (filter (fn [[oid o]]
                 (and (not= oid eid) (v/boxes-meet? (body-box e) (body-box o))
                      (foe? world e o))))
       (sort-by key)
       first))

(defn- finished [eid e t i between]
  (let [n (c/sample t eid i :ram (between e))]
    [(b/erase (c/cool e :ram-cooldown-ticks n t) :ram-target)
     [(raise eid)]]))

(defn- hit [world eid e t i [oid o] m]
  (let [{:keys [damage force impact]} m
        n (num/f32 (damage e))
        src {:type :mob-attack-no-aggro :cause eid :direct eid
             :from (:pos e) :attacker e}
        k (* (speed-factor e) (double (force e)))
        d (:dir (b/slot e i))
        [e ds] (finished eid e t i (:between m))]
    [e (concat (hurt/damage-deltas world oid o n src)
               [[:knockback oid k (v/x d) (v/z d)]]
               ds [(heard eid e (impact e) 1.0)])]))

(defn- snaps? [world e]
  (let [u (:vel e)
        d (v/normalized (v/v3 (v/x u) 0.0 (v/z u)) tiny)
        [x y z] (v/cell (v/add (:pos e) d))
        at #(contains? @snapping
                       (block/block-of (sense/block-at world x % z)))]
    (or (at y) (at (inc (long y))))))

(defn- snapped [world eid e t i {:keys [impact horn-break] :as m}]
  (let [[e' hs] (or ((:drop-horn m) world eid e t i) [e nil])
        [e' ds] (finished eid e' t i (:between m))]
    [e' (concat [(heard eid e (impact e) 1.0)] hs
                (when hs [(heard eid e (horn-break e) 1.0)]) ds)]))

(defn- lost? [world e t]
  (let [wt (b/recall e :walk-target t) r (b/recall e :ram-target t)]
    (or (nil? wt) (nil? r)
        (< (v/dist-sq (c/tracked-pos world (:to wt)) r) 0.0625))))

(defn- ram-tick [m]
  (fn [world eid e t i]
    (if-let [hit-one (rammed world eid e (:foe? m))]
      (hit world eid e t i hit-one m)
      (cond (snaps? world e) (snapped world eid e t i m)
            (lost? world e t) (finished eid e t i (:between m))
            :else e))))

(defn- ram-start [speed]
  (fn [_ _ e t i]
    (let [[x _ z] (v/cell (:pos e)) r (b/recall e :ram-target t)
          d (v/v3 (- (long x) (v/x r)) 0.0 (- (long z) (v/z r)))
          wt (c/walk-target (c/at-block (v/cell r)) speed 0)]
      (-> (b/with-slot e i {:dir (v/normalized d tiny)})
          (b/remember :walk-target wt b/forever)))))

(defn ram-target
  "Returns the behaviour that charges at speed to the ram target and
  hurts the first foe? it meets by damage, knocked back by force
  times its speed. A block that snaps horns stops it, and drop-horn
  may drop one. Either way it waits a time from between. The sounds
  come from impact and horn-break."
  [{:keys [speed] :as m}]
  {:id :ram-target :duration [200 200]
   :needs {:ram-cooldown-ticks :absent :ram-target :present}
   :start (ram-start speed)
   :continue? (fn [_ _ e t _] (b/present? e :ram-target t))
   :tick (ram-tick m)
   :tells? (fn [e t] (b/present? e :ram-target t))})
