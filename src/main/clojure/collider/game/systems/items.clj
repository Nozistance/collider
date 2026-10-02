(ns collider.game.systems.items
  "Dropped item motion, merging and pickup."
  (:require [collider.data :as data]
            [collider.game.inventory :as inventory]
            [collider.game.item :as item]
            [collider.random :as random]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.entity.shove :as shove]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.areas :as areas]
            [collider.game.player :as player]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.motion :as motion]
            [collider.world.phys :as phys]
            [collider.game.turn.overlay :as overlay])
  (:import (collider.world Move)))

(set! *warn-on-reflection* true)

(def ^:private ^:const despawn-age 6000)

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
    (vec (concat (item/thrown-deltas world eid [stack])
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
        pushed (v/add vel push)
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

(defn- passed
  "Returns the step of item e that passes through blocks by drift,
  as Entity.move with noPhysics: it keeps its ground flag and
  support, and no block acts on it."
  [chunks e drift rest?]
  (let [{:keys [pos on-ground support]} e
        og (boolean on-ground)
        p (if rest? pos (v/add pos drift))
        vel (if rest? drift (dragged chunks p support og drift))]
    {:pos p :vel vel :on-ground og :support support
     :no-blocks? (:no-blocks? e) :stuck (:stuck e)}))

(def ^:private ^:const wall-inset 1.0E-7)

(defn- in-wall? [chunks e]
  (not (phys/clear? chunks (:pos e) (item-half) (item-height)
                    wall-inset)))

(defn- felt
  "Returns the step of item e that blocks act on, as Entity.move."
  [chunks e drift rest?]
  (let [stuck (:stuck e)
        push (if stuck (mapv * drift stuck) drift)
        [pos' v on-ground sup nb?]
        (moved-or-resting chunks e drift rest? push)
        v (if (and stuck (not rest?)) [0.0 0.0 0.0] v)
        vy (liquid/bubble-push chunks pos' (double (v 1)))
        st (motion/stuck-speed chunks pos' (item-half) (item-height))]
    {:pos pos' :vel (assoc v 1 vy) :on-ground on-ground
     :support sup :no-blocks? nb?
     :stuck (if rest? (or st stuck) st)}))

(defn- settled [chunks dim e eid age t]
  (let [[drift in-fluid?] (item-drift chunks dim (:pos e) (:vel e))
        ghost? (in-wall? chunks e)
        drift (if ghost?
                (shove/shoved chunks (:pos e) (item-height) drift
                              (random/of-key t eid :shove))
                drift)
        rest? (resting? e drift age eid)
        s (if ghost?
            (passed chunks e drift rest?)
            (felt chunks e drift rest?))]
    (assoc s :in-fluid? in-fluid?)))

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
        s (settled (:chunks world) (:dim world) e (long eid) age
                   (:tick world))]
    (if (gone? world e age)
      [:remove-entity eid]
      [:merge-entity eid (stepped e s age)])))

(defn- mergeable? [ea eb]
  (let [pa (:pos ea) pb (:pos eb)
        sa (:stack ea) sb (:stack eb)
        flat (+ (item-half) (item-half) merge-inflate)
        tall (item-height)]
    (and (inventory/same-stack? sa sb)
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
    (let [[changes remaining] (inventory/add-stack inv (:stack ie))]
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
  (let [d (step-item (overlay/seen world eid) eid e)]
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
