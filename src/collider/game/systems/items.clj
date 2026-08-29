(ns collider.game.systems.items
  (:require [collider.rnd :as rnd]
            [collider.game.state :as state]
            [collider.proto.packets.play :as play]
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
  (and (= (:item a) (:item b)) (= (:damage a 0) (:damage b 0))))

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

(defn- item-entity [world thrower stack]
  (let [[px py pz] (get-in world [:entities thrower :pos])]
    {:type  :item
     :pos   [(double px) (+ (double py) 1.32) (double pz)]
     :vel   (throw-velocity world thrower)
     :yaw   0.0 :pitch 0.0 :on-ground false
     :stack stack :age 0 :pickup-delay throw-pickup-delay}))

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
    (cons [:spawn-entity eid (item-entity world thrower stack)]
          (when take-from
            [[:set-slot thrower (take-from 0) (take-from 1)]
             [:send thrower (play/set-slot (take-from 0) (take-from 1))]]))))

(defn- spawn-deltas [world events]
  (let [base (long (:next-eid world 1000000))]
    (apply concat
           (map-indexed (fn [i d] (spawn-one world base i d))
                        (drops world events)))))

(def ^:private ^:const item-half 0.125)
(def ^:private ^:const item-height 0.25)
(defn- liquid-push [world pos]
  (liquid/entity-push (:chunks world) gen/flat-chunk pos item-half item-height))

(defn- step-item [world eid e]
  (let [[vx vy vz] (v/+ (:vel e) (liquid-push world (:pos e)))
        ^Move mv (phys/move (:chunks world) gen/flat-chunk (:pos e)
                            [(double vx) (- (double vy) 0.04) (double vz)]
                            item-half item-height)
        pos (.pos mv) vel (.vel mv) on-ground (.on-ground mv)
        [mx my mz] vel
        f (if on-ground 0.588 0.98)
        age (inc (long (or (:age e) 0)))]
    (if (>= age despawn-age)
      [:remove-entity eid]
      [:merge-entity eid
       {:pos          pos
        :vel          (v/+ [(* (double mx) f) (* (double my) 0.98) (* (double mz) f)]
                           (liquid-push world pos))
        :on-ground    on-ground
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

(defn- add-stack [inv stack]
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
    (for [[slot s] changes] [:send peid (play/set-slot slot s)])
    (if remaining
      [[:merge-entity ieid {:stack remaining}]]
      (concat
        (for [[oid o] players
              :when (contains? (:tracking o) ieid)]
          [:send oid (play/collect-item ieid peid)])
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
