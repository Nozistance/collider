(ns collider.game.systems.items
  "Dropped item motion, merging and pickup."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.game.entity :as entity]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys])
  (:import (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const despawn-age 6000)

(def ^:private ^:const below-world (- chunk/min-y 64.0))

(def ^:private ^:const throw-pickup-delay 40)

(def ^:private ^:const throw-power 0.3)

(def ^:private ^:const throw-spread 0.02)

(def ^:private ^:const throw-lift 0.1)

(def ^:private ^:const throw-jitter 0.1)

(def ^:private ^:const around-power 0.5)

(def ^:private ^:const around-lift 0.2)

(def ^:private ^:const hand-height 1.32)

(defn- item-entities [world]
  (let [item? (fn [[_ e]] (= :item (:type e)))]
    (sort-by key (filter item? (:entities world)))))

(defn- active-items [world]
  (let [active (state/active-chunks world)]
    (filterv (fn [[_ e]] (state/active-at? active (:pos e)))
             (item-entities world))))

(defn- same-stack? [a b]
  (and (= (:item a) (:item b)) (= (:components a) (:components b))))

(defn- throw-velocity [world eid]
  (let [e (get-in world [:entities eid])
        yaw (Math/toRadians (double (or (:yaw e) 0.0)))
        pitch (Math/toRadians (double (or (:pitch e) 0.0)))
        t (:tick world)
        ang (* (random/of-key t eid :a) Math/PI 2.0)
        mag (* throw-spread (random/of-key t eid :m))
        jitter (- (random/of-key t eid :y1)
                  (random/of-key t eid :y2))]
    [(+ (* (- throw-power) (Math/sin yaw) (Math/cos pitch))
        (* (Math/cos ang) mag))
     (+ (* (- throw-power) (Math/sin pitch)) throw-lift
        (* throw-jitter jitter))
     (+ (* throw-power (Math/cos yaw) (Math/cos pitch))
        (* (Math/sin ang) mag))]))

(defn- around-velocity [world eid salt]
  (let [t (:tick world)
        pow (* around-power (random/of-key t eid salt :p))
        dir (* Math/PI 2.0 (random/of-key t eid salt :d))]
    [(* -1.0 (Math/sin dir) pow) around-lift (* (Math/cos dir) pow)]))

(defn dropped
  "Returns the item entity a player throws out of hand."
  ([world thrower stack] (dropped world thrower stack false 0))
  ([world thrower stack randomly? salt]
   (let [[px py pz] (get-in world [:entities thrower :pos])
         at [(double px) (+ (double py) hand-height) (double pz)]
         vel (if randomly?
               (around-velocity world thrower salt)
               (throw-velocity world thrower))]
     (entity/item at vel stack throw-pickup-delay))))

(defn popped
  "Returns the item dropped by a broken block."
  [world pos stack salt]
  (let [t (:tick world)
        r (fn [k] (random/of-key t pos salt k))
        [x y z] pos]
    (entity/item [(+ (double x) 0.5 (- (* 0.5 (r :x)) 0.25))
                  (+ (double y) 0.5 (- (* 0.5 (r :y)) 0.25) -0.125)
                  (+ (double z) 0.5 (- (* 0.5 (r :z)) 0.25))]
                 (entity/pop-velocity [t pos salt])
                 stack)))

(defn split-drop
  "Returns the stack split into the piles a broken block drops."
  [world pos stack salt]
  (loop [n (long (:count stack 1)) i 0 acc []]
    (if-not (pos? n)
      acc
      (let [k [(:tick world) pos salt :split i]
            got (min n (+ 10 (long (* 21.0 (random/of-key k)))))]
        (recur (- n got) (inc i)
               (conj acc (assoc stack :count got)))))))

(defn- held-drop [world eid status]
  (let [e (get-in world [:entities eid])
        slot (+ 36 (long (or (:held-slot e) 0)))
        s (get-in e [:inventory slot])]
    (when s
      (let [total (long (:count s 1))
            n (if (= 4 (long status)) 1 total)
            left (when (< n total) (assoc s :count (- total n)))]
        {:thrower eid :stack (assoc s :count n)
         :take-from [slot left]}))))

(defn- alive? [world eid]
  (some? (get-in world [:entities eid])))

(defn- drop-of [world [tag eid a b]]
  (case tag
    :dig (when (and (#{3 4} (long a)) (alive? world eid))
           (held-drop world eid a))
    :creative-slot (when (and (neg? (long a)) b (alive? world eid))
                     {:thrower eid :stack b})
    nil))

(defn- drops [world events]
  (keep #(drop-of world %) events))

(defn- spawn-one [world {:keys [thrower stack take-from]}]
  (cons [:spawn-entity (dropped world thrower stack)]
        (when take-from
          [[:set-slot thrower (take-from 0) (take-from 1)]
           [:client-slots thrower {(take-from 0) (take-from 1)}
            (get-in world [:entities thrower :track :carried])]])))

(defn- spawn-deltas [world events]
  (mapcat #(spawn-one world %) (drops world events)))

(def ^:private ^:const item-half 0.125)

(def ^:private ^:const item-height 0.25)

(def ^:private ^:const air-drag 0.98)

(def ^:private ^:const ground-friction 0.588)

(def ^:private ^:const gravity 0.04)

(def ^:private ^:const water-drag 0.99)

(def ^:private ^:const lava-drag 0.95)

(def ^:private ^:const buoyancy 5.0E-4)

(def ^:private ^:const buoyancy-below 0.06)

(def ^:private ^:const fluid-depth 0.1)

(def ^:private ^:const bounce -0.5)

(def ^:private ^:const resting-speed-sq 1.0E-5)

(def ^:private ^:const resting-period 4)

(def ^:private ^:const merge-inflate 0.5)

(def ^:private ^:const pickup-inflate 1.0)

(def ^:private ^:const pickup-inflate-y 0.5)

(def ^:private ^:const player-height 1.8)

(def ^:private ^:const player-half 0.3)

(def ^:private ^:const pickup-reach
  (+ player-half item-half pickup-inflate))

(def ^:private ^:const pickup-bottom -1.0)

(defn- near? [^double a ^double b ^double d]
  (< (Math/abs (- a b)) d))

(defn- fluid-movement [[vx vy vz] ^double drag]
  (let [vy (double vy)
        lift (if (< vy buoyancy-below) buoyancy 0.0)]
    [(* (double vx) drag) (+ vy lift) (* (double vz) drag)]))

(defn- fluid-at [chunks pos kind]
  (liquid/fluid-height chunks pos item-half item-height kind))

(defn- item-drift [chunks pos vel]
  (let [push (liquid/entity-push chunks pos item-half item-height vel)
        pushed (v/+ vel push)
        water (fluid-at chunks pos :water)
        lava (fluid-at chunks pos :lava)]
    [(cond
       (> water fluid-depth) (fluid-movement pushed water-drag)
       (> lava fluid-depth) (fluid-movement pushed lava-drag)
       :else [(v/x pushed) (- (v/y pushed) gravity) (v/z pushed)])
     (or (> water fluid-depth) (> lava fluid-depth))]))

(defn- item-moved [chunks pos [vx vy vz]]
  (let [v [(double vx) (double vy) (double vz)]
        ^Move mv (phys/move chunks pos v item-half item-height)
        on-ground (phys/on-ground? mv)
        [mx my mz] (if on-ground
                     (motion/stepped-speed chunks (phys/pos mv)
                                           (phys/vel mv))
                     (phys/vel mv))
        f (if on-ground ground-friction air-drag)
        my (* (double my) air-drag)
        my (if (and on-ground (neg? my)) (* my bounce) my)]
    [(phys/pos mv)
     [(* (double mx) f) my (* (double mz) f)] on-ground]))

(defn- jolt-of ^double [vel' old]
  (let [dx (- (double (vel' 0)) (v/x old))
        dy (- (double (vel' 1)) (v/y old))
        dz (- (double (vel' 2)) (v/z old))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- resting? [e drift ^long age ^long eid]
  (let [vx (double (drift 0)) vz (double (drift 2))]
    (and (:on-ground e)
         (<= (+ (* vx vx) (* vz vz)) resting-speed-sq)
         (not= 0 (rem (+ age eid) resting-period)))))

(defn- settled [chunks e ^long eid ^long age]
  (let [pos (:pos e)
        [drift in-fluid?] (item-drift chunks pos (:vel e))
        stuck (:stuck e)
        rest? (resting? e drift age eid)
        push (if stuck (mapv * drift stuck) drift)
        moved (if rest?
                [pos drift true]
                (item-moved chunks pos push))
        [pos' v on-ground] moved
        v (if (and stuck (not rest?)) [0.0 0.0 0.0] v)
        vy (liquid/bubble-push chunks pos' (double (v 1)))
        st (motion/stuck-speed chunks pos' item-half item-height)]
    {:pos pos' :vel (assoc v 1 vy) :on-ground on-ground
     :in-fluid? in-fluid? :stuck (if rest? (or st stuck) st)}))

(defn- gone? [e ^long age pos]
  (or (>= age despawn-age)
      (< (v/y pos) below-world)
      (not (pos? (double (:health e 1.0))))))

(defn- needs-sync? [e s]
  (or (:in-fluid? s)
      (> (jolt-of (:vel s) (:vel e)) 0.01)
      (not= (:on-ground s) (boolean (:on-ground e)))))

(defn- step-item [world eid e]
  (let [age (inc (long (or (:age e) 0)))
        s (settled (:chunks world) e (long eid) age)
        stuck' (:stuck s)
        delay' (max 0 (dec (long (or (:pickup-delay e) 0))))]
    (if (gone? e age (:pos s))
      [:remove-entity eid]
      [:merge-entity eid
       (cond-> {:pos          (:pos s)
                :vel          (:vel s)
                :on-ground    (:on-ground s)
                :needs-sync?  (needs-sync? e s)
                :age          age
                :pickup-delay delay'}
               (or stuck' (:stuck e)) (assoc :stuck stuck'))])))

(defn- mergeable? [ea eb]
  (let [pa (:pos ea) pb (:pos eb)
        sa (:stack ea) sb (:stack eb)
        flat (+ item-half item-half merge-inflate)
        tall (+ item-height item-height)]
    (and (same-stack? sa sb)
         (<= (+ (long (:count sa 1)) (long (:count sb 1)))
             (data/max-stack (:item sb)))
         (near? (v/x pa) (v/x pb) flat)
         (near? (v/z pa) (v/z pb) flat)
         (near? (v/y pa) (v/y pb) tall))))

(defn- cell-key ^long [^long x ^long y ^long z]
  (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
          (bit-shift-left (bit-and z 0x3FFFFFF) 12)
          (bit-and y 0xFFF)))

(defn- cell-of ^long [pos]
  (cell-key (long (Math/floor (v/x pos)))
            (long (Math/floor (v/y pos)))
            (long (Math/floor (v/z pos)))))

(defn- merge-index [items]
  (persistent!
    (reduce (fn [m ^long i]
              (let [k (cell-of (:pos ((items i) 1)))]
                (assoc! m k (conj (get m k []) i))))
            (transient {})
            (range (count items)))))

(defn- neighbour-key ^long [x y z ^long c]
  (cell-key (+ (long x) (dec (quot c 9)))
            (+ (long y) (dec (rem (quot c 3) 3)))
            (+ (long z) (dec (rem c 3)))))

(defn- neighbour-idxs [index pos]
  (let [x (long (Math/floor (v/x pos)))
        y (long (Math/floor (v/y pos)))
        z (long (Math/floor (v/z pos)))
        at (fn [c] (get index (neighbour-key x y z c) []))]
    (sort (persistent!
            (reduce (fn [acc c] (reduce conj! acc (at c)))
                    (transient [])
                    (range 27))))))

(defn- merge-partner [items index from a used]
  (first (for [j (neighbour-idxs index (:pos a))
               :when (>= (long j) (long from))
               :let [[eb b] (items j)]
               :when (and (not (used eb)) (mergeable? a b))]
           [eb b])))

(defn- absorb [ea a eb b]
  (let [n (long (:count (:stack b) 1))
        stack (update (:stack a) :count (fnil + 1) n)
        age (min (long (or (:age a) 0)) (long (or (:age b) 0)))]
    [[:merge-entity ea {:stack stack :age age}]
     [:remove-entity eb]]))

(defn- merge-deltas [items]
  (let [items (vec items)
        index (merge-index items)]
    (loop [i 0 used #{} out []]
      (if (>= i (count items))
        out
        (let [[ea a] (items i)
              found (when-not (used ea)
                      (merge-partner items index (inc i) a used))]
          (if-let [[eb b] found]
            (recur (inc i) (conj used ea eb)
                   (into out (absorb ea a eb b)))
            (recur (inc i) used out)))))))

(def ^:private slot-order
  (vec (concat (range 36 45) (range 9 36))))

(defn- topping-up? [cur stack ^long cap ^long n]
  (and (pos? n) cur (same-stack? cur stack)
       (< (long (:count cur 1)) cap)))

(defn- topped-up [chs cur slot ^long take]
  (conj chs [slot (update cur :count (fnil + 1) take)]))

(defn- fill-existing [inv stack ^long n]
  (let [cap (long (data/max-stack (:item stack)))
        step (fn [[chs n] slot]
               (let [n (long n) cur (get inv slot)
                     have (long (:count cur 1))
                     take (min (- cap have) n)]
                 (if (topping-up? cur stack cap n)
                   [(topped-up chs cur slot take) (- n take)]
                   [chs n])))]
    (reduce step [[] n] slot-order)))

(defn- first-empty-slot [inv changes]
  (first (remove #(or (get inv %) (some (fn [[s _]] (= s %)) changes))
                 slot-order)))

(defn add-stack
  "Returns the slot changes that fit stack into inv, and the rest.
  The rest is nil when the whole stack found room."
  [inv stack]
  (let [[changes n] (fill-existing inv stack (long (:count stack 1)))
        n (long n)]
    (if-let [slot (when (pos? n) (first-empty-slot inv changes))]
      [(conj changes [slot (assoc stack :count n)]) nil]
      [changes (when (pos? n) (assoc stack :count n))])))

(defn- holds? [inv stack]
  (some #(and (= (:item %) (:item stack))
              (= (:components %) (:components stack)))
        (vals inv)))

(defn- shrunk [stack ^long n]
  (let [left (- (long (:count stack 1)) n)]
    (when (pos? left) (assoc stack :count left))))

(defn- kept [world eid e stack]
  (let [[changes left] (add-stack (:inventory e) stack)]
    (concat (for [[slot s] changes] [:set-slot eid slot s])
            (when left [[:spawn-entity (dropped world eid left)]]))))

(defn- emptied [world eid e hand made]
  (let [slot (state/hand-slot e hand)
        left (shrunk (state/hand-stack e hand) 1)]
    (if left
      (cons [:set-slot eid slot left] (kept world eid e made))
      [[:set-slot eid slot made]])))

(defn filled-result-deltas
  "Returns the deltas of the container in hand turning into stack.
  The last container of a stack becomes the filled item in the
  hand, the rest of it keeps its place. In creative the container
  stays and the result is added only when the player holds none
  already, unless always? asks for it anyway."
  ([world eid stack]
   (filled-result-deltas world eid stack false :main))
  ([world eid stack always?]
   (filled-result-deltas world eid stack always? :main))
  ([world eid stack always? hand]
   (let [e (get-in world [:entities eid])]
     (if (state/infinite-materials? e)
       (when (or always? (not (holds? (:inventory e) stack)))
         (kept world eid e stack))
       (emptied world eid e hand stack)))))

(defn consume-deltas
  "Returns the deltas of spending n of the item in hand.
  A player with infinite materials spends nothing."
  [eid e hand ^long n]
  (when-not (state/infinite-materials? e)
    [[:set-slot eid (state/hand-slot e hand)
      (shrunk (state/hand-stack e hand) n)]]))

(defn- remainder-deltas [world eid e hand stack left]
  (let [over (dec (long (:count stack 1)))
        made {:item (:item left) :count (long (:count left 1))}]
    (if (pos? over)
      (cons [:set-slot eid (state/hand-slot e hand)
             (assoc stack :count over)]
            (kept world eid e made))
      [[:set-slot eid (state/hand-slot e hand) made]])))

(defn use-item-deltas
  "Returns the deltas of a player using one item from hand.
  The remainder of an item that leaves one, an empty bucket for
  milk, takes the hand or falls into the inventory."
  [world eid e hand]
  (let [stack (state/hand-stack e hand)
        left (get-in (data/items) [(:item stack) :use-remainder])]
    (if (and left (not (state/infinite-materials? e)))
      (remainder-deltas world eid e hand stack left)
      (consume-deltas eid e hand 1))))

(defn- broken-deltas [eid e hand stack]
  (let [fx (out/status eid (if (= :off hand) :break-off :break-main))]
    [[:set-slot eid (state/hand-slot e hand) (shrunk stack 1)]
     (out/all fx) (out/to eid fx)]))

(defn hurt-item-deltas
  "Returns the deltas of wearing the item in hand by n points.
  An item worn past its last point breaks and leaves the hand;
  a player with infinite materials wears nothing out."
  [eid e hand ^long n]
  (let [stack (state/hand-stack e hand)
        most (long (get-in stack [:components :max-damage] 0))
        worn (+ n (long (get-in stack [:components :damage] 0)))]
    (when (and (pos? most) (not (state/infinite-materials? e)))
      (if (>= worn most)
        (broken-deltas eid e hand stack)
        [[:set-slot eid (state/hand-slot e hand)
          (assoc-in stack [:components :damage] worn)]]))))

(defn- in-pickup-range? [pe ie]
  (let [pp (:pos pe) pi (:pos ie)
        dy (- (double (v/y pi)) (double (v/y pp)))]
    (and (near? (v/x pi) (v/x pp) pickup-reach)
         (near? (v/z pi) (v/z pp) pickup-reach)
         (< pickup-bottom dy (+ player-height pickup-inflate-y)))))

(defn- collect-deltas [ieid peid changes remaining]
  (concat
    (for [[slot s] changes] [:set-slot peid slot s])
    (if remaining
      [[:merge-entity ieid {:stack remaining}]]
      [(out/all (out/collect ieid peid))
       [:remove-entity ieid]])))

(defn- pickup-one [[peid pe] [out taken inv :as acc] [ieid ie]]
  (if (or (contains? taken ieid) (not (in-pickup-range? pe ie)))
    acc
    (let [[changes remaining] (add-stack inv (:stack ie))]
      (if (seq changes)
        [(into out (collect-deltas ieid peid changes remaining))
         (conj taken ieid)
         (into inv changes)]
        acc))))

(defn- player-pickups [ready [out taken] [_ pe :as entry]]
  (let [take (fn [acc item] (pickup-one entry acc item))
        [out taken] (reduce take [out taken (:inventory pe)] ready)]
    [out taken]))

(defn- pickup-deltas [world items]
  (let [ready? (fn [[_ ie]] (zero? (long (or (:pickup-delay ie) 0))))
        ready (filterv ready? items)]
    (first (reduce (fn [acc entry] (player-pickups ready acc entry))
                   [[] #{}]
                   (state/player-entries world)))))

(defn items
  "Returns the tick steps of every dropped item in an active chunk."
  [world d]
  (let [events (:input d)
        act (active-items world)]
    (-> [#(spawn-deltas world events)]
        (into (map (fn [[eid e]] #(vector (step-item world eid e))))
              act)
        (conj #(merge-deltas act)))))

(defn pickups
  "Returns the deltas of players taking up nearby items."
  [world _d]
  [#(pickup-deltas world (active-items world))])
