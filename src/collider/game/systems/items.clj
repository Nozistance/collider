(ns collider.game.systems.items
  (:require [collider.rnd :as rnd]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]
            [collider.world.phys :as phys])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const despawn-age 6000)
(def ^:private ^:const throw-pickup-delay 40)
(defn- item-entities [world]
  (sort-by key (filter (fn [[_ e]] (= :item (:type e))) (:entities world))))

(defn- active-items [world]
  (let [active (state/active-chunks world)]
    (filterv (fn [[_ e]] (state/active-at? active (:pos e)))
             (item-entities world))))

(defn- same-stack? [a b]
  (= (:item a) (:item b)))

(defn- throw-velocity [world eid]
  (let [e (get-in world [:entities eid])
        yaw (Math/toRadians (double (or (:yaw e) 0.0)))
        pitch (Math/toRadians (double (or (:pitch e) 0.0)))
        t (:tick world)
        ang (* (rnd/rnd [t eid :a]) Math/PI 2.0)
        mag (* 0.02 (rnd/rnd [t eid :m]))]
    [(+ (* -0.3 (Math/sin yaw) (Math/cos pitch)) (* (Math/cos ang) mag))
     (+ (* -0.3 (Math/sin pitch)) 0.1
        (* 0.1 (- (rnd/rnd [t eid :y1]) (rnd/rnd [t eid :y2]))))
     (+ (* 0.3 (Math/cos yaw) (Math/cos pitch)) (* (Math/sin ang) mag))]))

(defn- around-velocity
  "A stack let go from the inventory screen: a random direction, up to half
   a block per tick, a small hop (vanilla createItemStackToDrop randomly)."
  [world eid salt]
  (let [t (:tick world)
        pow (* 0.5 (rnd/rnd [t eid salt :p]))
        dir (* Math/PI 2.0 (rnd/rnd [t eid salt :d]))]
    [(* -1.0 (Math/sin dir) pow) 0.2 (* (Math/cos dir) pow)]))

(defn dropped
  "Item entity of a stack the player lets go: thrown from the hand along the
   look, or around them from the inventory screen (randomly?)."
  ([world thrower stack] (dropped world thrower stack false 0))
  ([world thrower stack randomly? salt]
   (let [[px py pz] (get-in world [:entities thrower :pos])]
     {:type  :item
      :pos   [(double px) (+ (double py) 1.32) (double pz)]
      :vel   (if randomly? (around-velocity world thrower salt) (throw-velocity world thrower))
      :yaw   0.0 :pitch 0.0 :on-ground false
      :stack stack :age 0 :pickup-delay throw-pickup-delay})))

(defn- held-drop [world eid status]
  (let [e (get-in world [:entities eid])
        slot (+ 36 (long (or (:held-slot e) 0)))
        s (get-in e [:inventory slot])]
    (when s
      (let [total (long (:count s 1))
            n (if (= 4 (long status)) 1 total)
            left (when (< n total) (assoc s :count (- total n)))]
        {:thrower eid :stack (assoc s :count n) :take-from [slot left]}))))

(defn- drops [world events]
  (keep (fn [[tag eid a b]]
          (case tag
            :dig (when (and (#{3 4} (long a))
                            (get-in world [:entities eid]))
                   (held-drop world eid a))
            :creative-slot (when (and (neg? (long a)) b
                                      (get-in world [:entities eid]))
                             {:thrower eid :stack b})
            nil))
        events))

(defn- spawn-one [world ^long base ^long i {:keys [thrower stack take-from]}]
  (let [eid (+ base i)]
    (cons [:spawn-entity eid (dropped world thrower stack)]
          (when take-from
            [[:set-slot thrower (take-from 0) (take-from 1)]
             (out/to thrower (out/set-slot (take-from 0) (take-from 1)))]))))

(defn- spawn-deltas [world events]
  (let [base (long (:next-eid world 1000000))]
    (apply concat
           (map-indexed (fn [i d] (spawn-one world base i d))
                        (drops world events)))))

(def ^:private ^:const item-half 0.125)
(def ^:private ^:const item-height 0.25)
(defn- fluid-movement
  "Velocity of an item in a liquid (vanilla setFluidMovement): a slow drift
   up to 0.06 per tick, drag on the sides."
  [[vx vy vz] ^double drag]
  [(* (double vx) drag) (+ (double vy) (if (< (double vy) 0.06) 5.0E-4 0.0)) (* (double vz) drag)])

(defn- step-item
  "One tick of an item as vanilla ItemEntity.tick: the currents push it, in
   water or lava it drifts, else it falls; at rest on the ground it moves
   only every fourth tick; friction after the move; it is synced when its
   velocity jumps, it lands or takes off, or while it is in a liquid."
  [world eid e]
  (let [chunks (:chunks world) pos (:pos e)
        pushed (v/+ (:vel e) (liquid/entity-push chunks gen/flat-chunk pos item-half item-height (:vel e)))
        water (liquid/fluid-height chunks gen/flat-chunk pos item-half item-height :water)
        lava  (liquid/fluid-height chunks gen/flat-chunk pos item-half item-height :lava)
        in-fluid? (or (> water 0.1) (> lava 0.1))
        [vx vy vz] (cond
                     (> water 0.1) (fluid-movement pushed 0.99)
                     (> lava 0.1) (fluid-movement pushed 0.95)
                     :else [(v/x pushed) (- (v/y pushed) 0.04) (v/z pushed)])
        resting? (and (:on-ground e)
                      (<= (+ (* (double vx) (double vx)) (* (double vz) (double vz))) 1.0E-5)
                      (not= 0 (rem (+ (long (:tick world)) (long eid)) 4)))
        [pos' vel' on-ground]
        (if resting?
          [pos [vx vy vz] true]
          (let [^Move mv (phys/move chunks gen/flat-chunk pos [(double vx) (double vy) (double vz)] item-half item-height)
                on-ground (.on-ground mv)
                [mx my mz] (.vel mv)
                f (if on-ground 0.588 0.98)
                my (* (double my) 0.98)]
            [(.pos mv)
             [(* (double mx) f) (if (and on-ground (neg? my)) (* my -0.5) my) (* (double mz) f)]
             on-ground]))
        vel' (assoc vel' 1 (liquid/bubble-push chunks gen/flat-chunk pos' (double (vel' 1))))
        old (:vel e)
        jolt (let [dx (- (double (vel' 0)) (v/x old)) dy (- (double (vel' 1)) (v/y old)) dz (- (double (vel' 2)) (v/z old))]
               (+ (* dx dx) (* dy dy) (* dz dz)))
        age (inc (long (or (:age e) 0)))]
    (if (>= age despawn-age)
      [:remove-entity eid]
      [:merge-entity eid
       {:pos          pos'
        :vel          vel'
        :on-ground    on-ground
        :needs-sync?  (or in-fluid? (> jolt 0.01) (not= on-ground (boolean (:on-ground e))))
        :age          age
        :pickup-delay (max 0 (dec (long (or (:pickup-delay e) 0))))}])))

(defn- mergeable? [ea eb]
  (let [pa (:pos ea) ax (v/x pa) ay (v/y pa) az (v/z pa)
        pb (:pos eb) bx (v/x pb) by (v/y pb) bz (v/z pb)]
    (and (same-stack? (:stack ea) (:stack eb))
         (<= (+ (long (:count (:stack ea) 1)) (long (:count (:stack eb) 1))) 64)
         (< (Math/abs (- (double ax) (double bx))) 0.75)
         (< (Math/abs (- (double az) (double bz))) 0.75)
         (< (Math/abs (- (double ay) (double by))) 0.5))))

(defn- merge-partner [items ^long from a used]
  (first (for [j (range from (count items))
               :let [[eb b] (items j)]
               :when (and (not (used eb)) (mergeable? a b))]
           [eb b])))

(defn- absorb [ea a eb b]
  [[:merge-entity ea
    {:stack (update (:stack a) :count (fnil + 1) (long (:count (:stack b) 1)))
     :age   (min (long (or (:age a) 0)) (long (or (:age b) 0)))}]
   [:remove-entity eb]])

(defn- merge-deltas [items]
  (let [items (vec items)]
    (loop [i 0 used #{} out []]
      (if (>= i (count items))
        out
        (let [[ea a] (items i)]
          (if-let [[eb b] (when-not (used ea) (merge-partner items (inc i) a used))]
            (recur (inc i) (conj used ea eb) (into out (absorb ea a eb b)))
            (recur (inc i) used out)))))))

(def ^:private slot-order
  (vec (concat (range 36 45) (range 9 36))))

(defn- fill-existing [inv stack ^long n]
  (reduce (fn [[chs n] slot]
            (let [n (long n) cur (get inv slot)]
              (if (and (pos? n) cur (same-stack? cur stack) (< (long (:count cur 1)) 64))
                (let [take (min (- 64 (long (:count cur 1))) n)]
                  [(conj chs [slot (update cur :count (fnil + 1) take)]) (- n take)])
                [chs n])))
          [[] n]
          slot-order))

(defn- first-empty-slot [inv changes]
  (first (remove #(or (get inv %) (some (fn [[s _]] (= s %)) changes))
                 slot-order)))

(defn add-stack
  "Puts the stack into the inventory as a pickup does: [changes left-over],
   changes as [slot stack], left-over the part that did not fit."
  [inv stack]
  (let [[changes n] (fill-existing inv stack (long (:count stack 1)))
        n (long n)]
    (if-let [slot (when (pos? n) (first-empty-slot inv changes))]
      [(conj changes [slot (assoc stack :count n)]) nil]
      [changes (when (pos? n) (assoc stack :count n))])))

(defn- in-pickup-range? [pe ie]
  (let [pp (:pos pe) px (v/x pp) py (v/y pp) pz (v/z pp)
        pi (:pos ie) ix (v/x pi) iy (v/y pi) iz (v/z pi)]
    (and (< (Math/abs (- (double ix) (double px))) 1.425)
         (< (Math/abs (- (double iz) (double pz))) 1.425)
         (< -1.0 (- (double iy) (double py)) 2.3))))

(defn- collect-deltas [_world ieid _ie peid changes remaining players]
  (concat
    (for [[slot s] changes] [:set-slot peid slot s])
    (for [[slot s] changes] (out/to peid (out/set-slot slot s)))
    (if remaining
      [[:merge-entity ieid {:stack remaining}]]
      (concat
        (for [[oid o] players
              :when (contains? (:tracking o) ieid)]
          (out/to oid (out/collect ieid peid)))
        [[:remove-entity ieid]]))))

(defn- pickup-one [world players [peid pe] [out taken inv :as acc] [ieid ie]]
  (if (or (contains? taken ieid) (not (in-pickup-range? pe ie)))
    acc
    (let [[changes remaining] (add-stack inv (:stack ie))]
      (if (seq changes)
        [(into out (collect-deltas world ieid ie peid changes remaining players))
         (conj taken ieid)
         (into inv changes)]
        acc))))

(defn- player-pickups [world players ready [out taken] [_ pe :as entry]]
  (let [[out taken] (reduce (fn [acc item] (pickup-one world players entry acc item))
                            [out taken (:inventory pe)]
                            ready)]
    [out taken]))

(defn- pickup-deltas [world items]
  (let [players (state/player-entries world)
        ready   (filterv (fn [[_ ie]] (zero? (long (or (:pickup-delay ie) 0)))) items)]
    (first (reduce (fn [acc entry] (player-pickups world players ready acc entry))
                   [[] #{}]
                   players))))

(defn items [world events]
  (let [act (active-items world)]
    (-> [#(spawn-deltas world events)]
        (into (map (fn [[eid e]] #(vector (step-item world eid e)))) act)
        (conj #(merge-deltas act)
              #(pickup-deltas world act)))))
