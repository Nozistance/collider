(ns collider.world.blocks.chorus
  "Chorus plants and flowers with their support, connections
  and growth."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- plant? [^long st] (= :chorus-plant (block/type-of st)))

(defn- flower? [^long st] (= :chorus-flower (block/type-of st)))

(defn- roots? [^long st] (block/tagged? st "supports_chorus_plant"))

(defn- held-by [chunks p squeezed? d]
  (let [q (dir/toward p d)]
    (when (plant? (chunk/at chunks q))
      (if squeezed?
        :blocked
        (let [under (chunk/at chunks (dir/down q))]
          (when (or (plant? under) (roots? under)) :held))))))

(defn- plant-supported? [chunks p]
  (let [below (chunk/at chunks (dir/down p))
        above (chunk/at chunks (dir/up p))
        squeezed? (and (pos? above) (pos? below))
        branch (some #(held-by chunks p squeezed? %) dir/horizontal)]
    (case branch
      :blocked false
      :held true
      (or (plant? below) (roots? below)))))

(defn- one-plant-beside? [chunks p]
  (loop [ds dir/horizontal one? false]
    (if-let [d (first ds)]
      (let [n (chunk/at chunks (dir/toward p d))]
        (cond
          (plant? n) (when-not one? (recur (next ds) true))
          (pos? n) false
          :else (recur (next ds) one?)))
      one?)))

(defn- flower-supported? [chunks p]
  (let [below (chunk/at chunks (dir/down p))]
    (cond
      (plant? below) true
      (block/tagged? below "supports_chorus_flower") true
      (zero? below) (one-plant-beside? chunks p))))

(defn supported?
  [chunks p ^long st]
  (boolean (if (flower? st)
             (flower-supported? chunks p)
             (plant-supported? chunks p))))

(defn- connects? [^long n d]
  (or (plant? n) (flower? n) (and (= :down d) (roots? n))))

(defn connected
  "Returns the chorus plant st at p with its sides joined to the
  plants around it. seen holds states that override the chunks."
  (^long [chunks p ^long st] (connected chunks p st {}))
  (^long [chunks p ^long st seen]
   (let [at #(long (get seen % (chunk/at chunks %)))
         side #(block/flag (connects? (at (dir/toward p %)) %))]
     (->> dir/six
          (reduce #(assoc %1 %2 (side %2)) (block/props-of st))
          (block/state (block/block-of st))))))

(defn- empty-at? [chunks seen q]
  (zero? (long (get seen q (chunk/at chunks q)))))

(defn- neighbours-empty? [chunks seen p ignore]
  (every? #(or (= % ignore) (empty-at? chunks seen (dir/toward p %)))
          dir/horizontal))

(defn- pillar [chunks p]
  (loop [h 1 i 0]
    (if (= i 4)
      [h false]
      (let [n (chunk/at chunks (dir/toward p :down (inc h)))]
        (if (plant? n) (recur (inc h) (inc i)) [h (roots? n)])))))

(defn- pillar-growth [chunks p pick]
  (let [[h on-roots?] (pillar chunks p)
        bound (if on-roots? 5 4)]
    [(or (< h 2) (<= h (long (pick :height bound)))) on-roots?]))

(defn- up-growth [chunks p pick]
  (let [below (chunk/at chunks (dir/down p))]
    (cond
      (block/tagged? below "supports_chorus_flower") [true false]
      (plant? below) (pillar-growth chunks p pick)
      (zero? below) [true false]
      :else [false false])))

(def ^:private order [:north :east :south :west])

(def ^:private grew :sound-chorus-grow)

(def ^:private died :sound-chorus-death)

(defn- flower-of ^long [^long age]
  (block/state :chorus-flower {:age (keyword (str age))}))

(defn- dead [p]
  [[p (flower-of 5) [[:event died]]]])

(defn- branch-to? [chunks seen p d]
  (let [q (dir/toward p d)]
    (and (empty-at? chunks seen q)
         (empty-at? chunks seen (dir/down q))
         (neighbours-empty? chunks seen q (dir/opposite d)))))

(defn- branches [chunks p age on-roots? pick]
  (let [n (+ (long (pick :tries 4)) (if on-roots? 1 0))
        flower (flower-of (inc (long age)))]
    (loop [i 0 seen {} acc []]
      (if (= i n)
        [acc seen]
        (let [d (order (long (pick [:dir i] 4)))
              q (dir/toward p d)]
          (if (branch-to? chunks seen p d)
            (recur (inc i) (assoc seen q flower)
                   (conj acc [q flower [[:event grew]]]))
            (recur (inc i) seen acc)))))))

(defn- up-free? [chunks above]
  (and (neighbours-empty? chunks {} above nil)
       (zero? (chunk/at chunks (dir/up above)))))

(defn- grown-up [chunks p above age]
  [[p (connected chunks p (block/state :chorus-plant))]
   [above (flower-of age) [[:event grew]]]])

(defn- branched [chunks p age on-roots? pick]
  (let [[made seen] (branches chunks p age on-roots? pick)
        plant (block/state :chorus-plant)]
    (if (seq made)
      (conj made [p (connected chunks p plant seen)])
      (dead p))))

(defn flower-tick
  "Returns the changes a chorus flower at p makes as it grows.
  Returns nil when it stays. pick takes a salt and a bound n
  and returns a number below n."
  [chunks p ^long st pick]
  (let [above (dir/up p) age (block/prop-long st :age)]
    (when (and (zero? (chunk/at chunks above))
               (chunk/in-range? (long (above 1))) (< age 5))
      (let [[up? on-roots?] (up-growth chunks p pick)]
        (cond
          (and up? (up-free? chunks above))
          (grown-up chunks p above age)
          (< age 4) (branched chunks p age on-roots? pick)
          :else (dead p))))))
