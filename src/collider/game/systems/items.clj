(ns collider.game.systems.items
  "Dropped item motion, merging and pickup."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.game.game-mode :as game-mode]
            [collider.game.areas :as areas]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys])
  (:import (collider.world Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const despawn-age 6000)

(def ^:private ^:const throw-pickup-delay 40)

(def ^:private ^:const throw-power 0.3)

(def ^:private ^:const throw-spread 0.02)

(def ^:private ^:const throw-lift 0.1)

(def ^:private ^:const throw-jitter 0.1)

(def ^:private ^:const around-power 0.5)

(def ^:private ^:const around-lift 0.2)

(def ^:private ^:const hand-drop (double (float 0.3)))

(defn- same-stack? [a b]
  (and (= (:item a) (:item b)) (= (:components a) (:components b))))

(defn- throw-velocity [world eid salt]
  (let [e (get-in world [:entities eid])
        yaw (Math/toRadians (double (or (:yaw e) 0.0)))
        pitch (Math/toRadians (double (or (:pitch e) 0.0)))
        t (:tick world)
        ang (* (random/of-key t eid salt :a) Math/PI 2.0)
        mag (* throw-spread (random/of-key t eid salt :m))
        jitter (- (random/of-key t eid salt :y1)
                  (random/of-key t eid salt :y2))]
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
  "Returns the item entity a player throws out of hand.
  With randomly? the item flies in a random direction."
  ([world thrower stack] (dropped world thrower stack false 0))
  ([world thrower stack randomly? salt]
   (let [e (get-in world [:entities thrower])
         [px py pz] (:pos e)
         y (- (+ (double py) (entity/eye-height e)) hand-drop)
         at [(double px) y (double pz)]
         vel (if randomly?
               (around-velocity world thrower salt)
               (throw-velocity world thrower salt))]
     (entity/item at vel stack throw-pickup-delay))))

(defn thrown-deltas
  "Returns the deltas of player eid throwing the stacks.
  The stats of each throw come with them."
  [world eid stacks]
  (mapcat (fn [i s]
            [[:spawn-entity (dropped world eid s false i)]
             [:award eid (keyword "dropped" (name (:item s)))
              (long (:count s 1))]
             [:award eid :custom/drop 1]])
          (range) stacks))

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

(def ^:private ^:const scatter-spread 0.11485000171139836)

(defn- scatter-spot [pos roll]
  (let [f #(Math/floor (double (nth pos %)))]
    [(+ (f 0) (* (double (roll :x)) 0.75) 0.125)
     (+ (f 1) (* (double (roll :y)) 0.75))
     (+ (f 2) (* (double (roll :z)) 0.75) 0.125)]))

(defn- scatter-axis ^double [roll i a ^double mode]
  (let [r #(double (roll [:vel i %]))]
    (+ mode (* scatter-spread (- (r a) (r (inc (long a))))))))

(defn- scatter-speed [roll i]
  [(scatter-axis roll i 0 0.0) (scatter-axis roll i 2 0.2)
   (scatter-axis roll i 4 0.0)])

(defn- pile ^long [roll i ^long left]
  (min left (+ 10 (long (* 21.0 (double (roll [:split i])))))))

(defn scattered
  "Returns the items that stack at pos scatters into.
  Piles of 10 to 30 leave one spot inside the cell, each thrown its
  own way, none held back from pickup. roll gives a number in [0, 1)
  for each key."
  [pos stack roll]
  (let [at (scatter-spot pos roll)]
    (loop [n (long (:count stack 1)) i 0 acc []]
      (if-not (pos? n)
        acc
        (let [got (pile roll i n)
              s (assoc stack :count got)
              it (entity/item at (scatter-speed roll i) s 0)]
          (recur (- n got) (inc i) (conj acc it)))))))

(defn- held-drop [world eid status]
  (let [e (get-in world [:entities eid])
        slot (player/hand-slot e :main)
        s (get-in e [:inventory slot])]
    (when s
      (let [total (long (:count s 1))
            n (if (= 4 (long status)) 1 total)
            left (when (< n total) (assoc s :count (- total n)))]
        {:stack (assoc s :count n) :take-from [slot left]}))))

(defn- creative-drop [s]
  (when (and s (<= 1 (long (:count s 1)) (stack/max-size s)))
    {:stack s}))

(defn- drop-of
  [world [tag eid a b]]
  (when-let [e (get-in world [:entities eid])]
    (case tag
      :dig (when (and (#{3 4} (long a))
                      (not (game-mode/spectator? e)))
             (held-drop world eid a))
      :creative-slot
      (when (and (neg? (long a)) (game-mode/creative? e))
        (creative-drop b))
      nil)))

(defn- taken [world eid [slot left]]
  [[:set-slot eid slot left]
   [:client-slots eid {slot left}
    (get-in world [:entities eid :track :carried])]])

(defn event-deltas
  "Returns the deltas of one drop event of its player."
  [world [_ eid :as ev]]
  (when-let [{:keys [stack take-from]} (drop-of world ev)]
    (vec (concat (thrown-deltas world eid [stack])
                 (when take-from (taken world eid take-from))))))

(defn- item-half ^double [] (size/half :item))

(defn- item-height ^double [] (size/height :item))

(def ^:private ^:const air-drag (double (float 0.98)))

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

(def ^:private ^:const no-pickup-delay 32767)

(def ^:private ^:const moved-rate 2)

(def ^:private ^:const resting-rate 40)

(def ^:private ^:const pickup-inflate 1.0)

(def ^:private ^:const pickup-inflate-y 0.5)

(defn- near? [^double a ^double b ^double d]
  (< (Math/abs (- a b)) d))

(defn- fluid-movement [[vx vy vz] ^double drag]
  (let [vy (double vy)
        lift (if (< vy buoyancy-below) buoyancy 0.0)]
    [(* (double vx) drag) (+ vy lift) (* (double vz) drag)]))

(def ^:private dry {:water 0.0 :lava 0.0 :push [0.0 0.0 0.0]})

(defn- fluid-at [chunks dim pos vel]
  (if (phys/dry? chunks pos (item-half) (item-height))
    dry
    (liquid/fluid-info chunks pos (item-half) (item-height) vel dim)))

(defn- item-drift [chunks dim pos vel]
  (let [{:keys [water lava push]} (fluid-at chunks dim pos vel)
        pushed (v/+ vel push)
        water (double water)
        lava (double lava)]
    [(cond
       (> water fluid-depth) (fluid-movement pushed water-drag)
       (> lava fluid-depth) (fluid-movement pushed lava-drag)
       :else [(v/x pushed) (- (v/y pushed) gravity) (v/z pushed)])
     (or (> water fluid-depth) (> lava fluid-depth))]))

(defn- supported [chunks e ^Move mv]
  (if-not (phys/on-ground? mv)
    [nil false]
    (let [p (phys/pos mv)
          sb (phys/supporting-block chunks p (item-half))
          o (:pos e)]
      (if (or sb (:no-blocks? e))
        [sb (nil? sb)]
        (let [s (phys/supporting-block
                  chunks (v/v3 (v/x o) (v/y p) (v/z o)) (item-half))]
          [s (nil? s)])))))

(defn- ground-friction ^double [chunks pos sup]
  (let [f (motion/friction (motion/below-state chunks pos sup))]
    (double (float (* air-drag f)))))

(defn- moved-speed [chunks ^Move mv sup on-ground]
  (let [p (phys/pos mv)
        sf (motion/block-speed-factor chunks p sup)
        w (phys/vel mv)
        vel [(* (v/x w) sf) (v/y w) (* (v/z w) sf)]]
    (if on-ground (motion/stepped-speed chunks p vel) vel)))

(defn- dragged [chunks pos sup on-ground [mx my mz]]
  (let [f (if on-ground (ground-friction chunks pos sup) air-drag)
        my (* (double my) air-drag)
        my (if (and on-ground (neg? my)) (* my bounce) my)]
    [(* (double mx) f) my (* (double mz) f)]))

(defn- item-moved [chunks e vel]
  (let [vel (mapv double vel)
        ^Move mv (phys/move chunks (:pos e) vel (item-half)
                            (item-height))
        og (phys/on-ground? mv)
        [sup nb?] (supported chunks e mv)
        p (phys/pos mv)]
    [p (dragged chunks p sup og (moved-speed chunks mv sup og))
     og sup nb?]))

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

(defn- moved-or-resting [chunks e drift rest? push]
  (if rest?
    [(:pos e) drift true (:support e) (:no-blocks? e)]
    (item-moved chunks e push)))

(defn- settled [chunks dim e eid age]
  (let [[drift in-fluid?] (item-drift chunks dim (:pos e) (:vel e))
        stuck (:stuck e)
        rest? (resting? e drift age eid)
        push (if stuck (mapv * drift stuck) drift)
        [pos' v on-ground sup nb?]
        (moved-or-resting chunks e drift rest? push)
        v (if (and stuck (not rest?)) [0.0 0.0 0.0] v)
        vy (liquid/bubble-push chunks pos' (double (v 1)))
        st (motion/stuck-speed chunks pos' (item-half) (item-height))]
    {:pos pos' :vel (assoc v 1 vy) :on-ground on-ground
     :support sup :no-blocks? nb? :in-fluid? in-fluid?
     :stuck (if rest? (or st stuck) st)}))

(defn- gone? [world e ^long age]
  (or (>= age despawn-age)
      (< (v/y (:pos e)) (chunk/void-y world))
      (not (pos? (double (:health e 1.0))))))

(defn- needs-sync? [e s]
  (or (:in-fluid? s)
      (> (jolt-of (:vel s) (:vel e)) 0.01)
      (not= (:on-ground s) (boolean (:on-ground e)))))

(defn- delay-left ^long [e]
  (let [d (long (or (:pickup-delay e) 0))]
    (if (= no-pickup-delay d) d (max 0 (dec d)))))

(defn- stepped [e s age]
  (cond-> {:pos (:pos s) :vel (:vel s) :on-ground (:on-ground s)
           :support (:support s) :no-blocks? (:no-blocks? s)
           :needs-sync? (needs-sync? e s)
           :age age
           :pickup-delay (delay-left e)}
    (or (:stuck s) (:stuck e)) (assoc :stuck (:stuck s))))

(defn- step-item [world eid e]
  (let [age (inc (long (or (:age e) 0)))
        s (settled (:chunks world) (:dim world) e (long eid) age)]
    (if (gone? world e age)
      [:remove-entity eid]
      [:merge-entity eid (stepped e s age)])))

(defn- mergeable? [ea eb]
  (let [pa (:pos ea) pb (:pos eb)
        sa (:stack ea) sb (:stack eb)
        flat (+ (item-half) (item-half) merge-inflate)
        tall (item-height)]
    (and (same-stack? sa sb)
         (<= (+ (long (:count sa 1)) (long (:count sb 1)))
             (data/max-stack (:item sb)))
         (near? (v/x pa) (v/x pb) flat)
         (near? (v/z pa) (v/z pb) flat)
         (near? (v/y pa) (v/y pb) tall))))

(defn- fl ^long [^double a] (long (Math/floor a)))

(defn- cell-key ^long [^long x ^long y ^long z]
  (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
          (bit-shift-left (bit-and z 0x3FFFFFF) 12)
          (bit-and y 0xFFF)))

(defn- cell-of ^long [pos]
  (cell-key (fl (v/x pos)) (fl (v/y pos)) (fl (v/z pos))))

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
  (let [x (fl (v/x pos))
        y (fl (v/y pos))
        z (fl (v/z pos))
        at (fn [c] (get index (neighbour-key x y z c) []))]
    (sort (persistent!
            (reduce (fn [acc c] (reduce conj! acc (at c)))
                    (transient [])
                    (range 27))))))

(defn- cell-moved [index ^long i from to]
  (let [a (cell-of from) b (cell-of to)]
    (if (== a b)
      index
      (-> index
          (assoc! a (filterv #(not= i (long %)) (get index a)))
          (assoc! b (conj (get index b []) i))))))

(defn- crossed? [from to]
  (or (not (== (fl (v/x from)) (fl (v/x to))))
      (not (== (fl (v/y from)) (fl (v/y to))))
      (not (== (fl (v/z from)) (fl (v/z to))))))

(defn- merge-rate ^long [from to]
  (if (crossed? from to) moved-rate resting-rate))

(defn- merge-ready?
  [[_ e]]
  (when e
    (let [s (:stack e)]
      (and (not= no-pickup-delay (long (or (:pickup-delay e) 0)))
           (< (long (or (:age e) 0)) despawn-age)
           (< (long (:count s 1))
              (long (data/max-stack (:item s))))))))

(defn- merge-due?
  [[_ e from]]
  (zero? (rem (long (or (:age e) 0))
              (merge-rate from (:pos e)))))

(defn- of-long ^long [e k] (long (or (get e k) 0)))

(defn- count-of ^long [e] (long (:count (:stack e) 1)))

(defn- absorbed
  "Returns item a after it took the stack of item b, as
  ItemEntity.merge: the longer pickup delay and the younger age."
  [a b]
  (let [n (+ (count-of a) (count-of b))
        d (max (of-long a :pickup-delay) (of-long b :pickup-delay))]
    (assoc a :stack (assoc (:stack a) :count n)
             :age (min (of-long a :age) (of-long b :age))
             :pickup-delay d)))

(defn- absorb-deltas [ea a eb]
  [[:merge-entity ea (select-keys a [:stack :age :pickup-delay])]
   [:remove-entity eb]])

(defn- took
  "Returns [es out] after item i of es took item j of es, which is
  gone now. When the turn of i comes after the turn of j, i steps
  from what it holds then."
  [es ^booleans gone ^booleans fresh out [i j]]
  (let [[ea a] (nth es i) [eb b] (nth es j) a (absorbed a b)
        i (long i) j (long j)]
    (aset gone j true)
    (when (> i j) (aset fresh i true))
    [(assoc es i [ea a]) (reduce conj! out (absorb-deltas ea a eb))]))

(defn- tried
  "Returns [es out done?] after item i of es tried to merge with item
  j, as ItemEntity.tryToMerge: the smaller stack goes into the other.
  It is done when i went into j."
  [es gone fresh out i j]
  (let [a (nth (nth es i) 1) b (nth (nth es j) 1)
        ok? (and (merge-ready? [nil b]) (mergeable? a b))]
    (cond
      (not ok?) [es out false]
      (< (count-of b) (count-of a))
      (conj (took es gone fresh out [i j]) false)
      :else (conj (took es gone fresh out [j i]) true))))

(defn- merged
  "Returns [es out] after item i of es merged with the items near it
  in their order, as ItemEntity.mergeWithNeighbours. The items before
  it moved this tick, the items after it not yet."
  [index es gone fresh out i]
  (loop [js (neighbour-idxs index (:pos (nth (nth es i) 1)))
         es es out out]
    (let [j (first js)]
      (cond (nil? j) [es out]
            (or (== (long j) (long i)) (aget ^booleans gone (long j)))
            (recur (rest js) es out)
            :else (let [[es out done?] (tried es gone fresh out i j)]
                    (if done? [es out] (recur (rest js) es out)))))))

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

(defn kept
  "Returns the deltas that put stack into the inventory of player
  eid, and throw out what does not fit."
  [world eid e stack]
  (let [[changes left] (add-stack (:inventory e) stack)]
    (concat (for [[slot s] changes] [:set-slot eid slot s])
            (when left [[:spawn-entity (dropped world eid left)]]))))

(defn- emptied [world eid e hand made]
  (let [slot (player/hand-slot e hand)
        left (shrunk (player/hand-stack e hand) 1)]
    (if left
      (cons [:set-slot eid slot left] (kept world eid e made))
      [[:set-slot eid slot made]])))

(defn- creative-filled [world eid e stack always?]
  (when (or always? (not (holds? (:inventory e) stack)))
    (kept world eid e stack)))

(defn filled-result-deltas
  "Returns the deltas of the container in hand turning into stack.
  The last container becomes the filled item in the hand. From a
  larger stack the filled item goes to the inventory. In creative the
  container stays, and the filled item goes to the inventory only when
  the player holds none or always? is true."
  ([world eid stack]
   (filled-result-deltas world eid stack false :main))
  ([world eid stack always?]
   (filled-result-deltas world eid stack always? :main))
  ([world eid stack always? hand]
   (let [e (get-in world [:entities eid])]
     (if (player/infinite-materials? e)
       (creative-filled world eid e stack always?)
       (emptied world eid e hand stack)))))

(defn consume-deltas
  "Returns the deltas of spending n of the item in hand.
  A player with infinite materials spends nothing."
  [eid e hand ^long n]
  (when-not (player/infinite-materials? e)
    [[:set-slot eid (player/hand-slot e hand)
      (shrunk (player/hand-stack e hand) n)]]))

(defn- remainder-deltas [world eid e hand stack left]
  (let [over (dec (long (:count stack 1)))
        made {:item (:item left) :count (long (:count left 1))}]
    (if (pos? over)
      (cons [:set-slot eid (player/hand-slot e hand)
             (assoc stack :count over)]
            (kept world eid e made))
      [[:set-slot eid (player/hand-slot e hand) made]])))

(defn use-item-deltas
  "Returns the deltas of a player using one item from hand.
  Some items leave a remainder, such as an empty bucket after milk.
  The remainder takes the hand or goes to the inventory."
  [world eid e hand]
  (let [stack (player/hand-stack e hand)
        left (get-in (data/items) [(:item stack) :use-remainder])]
    (if (and left (not (player/infinite-materials? e)))
      (remainder-deltas world eid e hand stack left)
      (consume-deltas eid e hand 1))))

(defn- broken-deltas [eid e hand stack]
  (let [fx (out/status eid (if (= :off hand) :break-off :break-main))]
    [[:set-slot eid (player/hand-slot e hand) (shrunk stack 1)]
     (out/all fx) (out/to eid fx)]))

(defn hurt-item-deltas
  "Returns the deltas of wearing the item in hand by n points.
  An item worn past its last point breaks and leaves the hand. A
  player with infinite materials wears nothing out."
  [eid e hand ^long n]
  (let [stack (player/hand-stack e hand)
        worn (+ n (stack/damage stack))]
    (when (and (stack/damageable? stack)
               (not (player/infinite-materials? e)))
      (if (>= worn (stack/max-damage stack))
        (broken-deltas eid e hand stack)
        [[:set-slot eid (player/hand-slot e hand)
          (stack/with-damage stack worn)]]))))

(defn- in-pickup-range? [pe ie]
  (let [pp (:pos pe) pi (:pos ie)
        [half h] (entity/box pe)
        reach (+ (double half) (item-half) pickup-inflate)
        dy (- (double (v/y pi)) (double (v/y pp)))]
    (and (near? (v/x pi) (v/x pp) reach)
         (near? (v/z pi) (v/z pp) reach)
         (< (- (+ (item-height) pickup-inflate-y)) dy
            (+ (double h) pickup-inflate-y)))))

(defn- collect-deltas [ieid peid changes remaining]
  (concat
    (for [[slot s] changes] [:set-slot peid slot s])
    (if remaining
      [[:merge-entity ieid {:stack remaining}]]
      [(out/all (out/collect ieid peid))
       [:remove-entity ieid]])))

(defn- pickup-one [[peid pe] [out inv :as acc] [ieid ie]]
  (if (in-pickup-range? pe ie)
    (let [[changes remaining] (add-stack inv (:stack ie))]
      (if (seq changes)
        [(into out (collect-deltas ieid peid changes remaining))
         (into inv changes)]
        acc))
    acc))

(defn- ready? [[_ ie]]
  (zero? (long (or (:pickup-delay ie) 0))))

(defn- takes? [pe]
  (and (pos? (double (:health pe 20.0)))
       (not (game-mode/spectator? pe))))

(defn player-pickups
  "Returns the deltas of player p, an entry, taking up the items it
  touches, one after another."
  [world p]
  (when (takes? (val p))
    (let [items (filterv ready? (areas/active-of-types world [:item]))
          take (fn [acc item] (pickup-one p acc item))]
      (when (seq items)
        (first (reduce take [[] (:inventory (val p))] items))))))

(defn- stepped-item [world [eid e]]
  (let [d (step-item world eid e)]
    (if (= :remove-entity (nth d 0))
      [eid nil (:pos e) d]
      [eid (entity/merged e (nth d 2)) (:pos e) d])))

(defn- turn-of [world steps ^booleans fresh es i]
  (if (aget fresh (long i))
    (stepped-item world (nth es i))
    (nth steps i)))

(defn- due? [e from]
  (and (merge-ready? [nil e]) (merge-due? [nil e from])))

(defn- item-turn
  "Returns [es index out] after the turn of item i of es: its step,
  then its merges. An item gone before its turn does not step."
  [world steps [gone fresh] [es index out] i]
  (let [[eid e from d] (turn-of world steps fresh es i)
        out (conj! out d)]
    (if (nil? e)
      (do (aset ^booleans gone (long i) true) [es index out])
      (let [es (assoc es i [eid e])
            index (cell-moved index i from (:pos e))]
        (if (due? e from)
          (let [[es out] (merged index es gone fresh out i)]
            [es index out])
          [es index out])))))

(defn- walked
  "Returns the deltas of items, each in its turn."
  [world items steps]
  (let [n (count items) flags [(boolean-array n) (boolean-array n)]
        ^booleans gone (nth flags 0)]
    (loop [i 0 acc [items (transient (merge-index items))
                    (transient [])]]
      (cond (= i n) (persistent! (nth acc 2))
            (aget gone i) (recur (inc i) acc)
            :else
            (recur (inc i) (item-turn world steps flags acc i))))))

(defn turns
  "Returns the deltas of every dropped item in an active chunk, each
  in its turn, as ItemEntity.tick."
  [world]
  (let [items (areas/active-of-types world [:item])
        steps (deltas/pmapcat #(vector (stepped-item world %)) items)]
    (when (pos? (count items))
      (walked world items steps))))

