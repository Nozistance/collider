(ns collider.world.feature.tree
  "Trees of roots, trunk, foliage and decorations, with the distances
  of their leaves and the shapes around them."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.feature.decorate :as decorate]
            [collider.world.feature.foliage :as foliage]
            [collider.world.feature.level :as lv]
            [collider.world.feature.trunk :as trunk])
  (:import (clojure.lang IDeref)
           (java.util HashSet)))

(set! *warn-on-reflection* true)

(defn- cell-hash ^long [^long x ^long y ^long z]
  (let [yz (unchecked-add y (unchecked-multiply z 31))]
    (unchecked-int (unchecked-add (unchecked-multiply yz 31) x))))

(defn- cell [[x y z]]
  (let [v [(long x) (long y) (long z)]
        h (int (cell-hash x y z))]
    (reify
      IDeref
      (deref [_] v)
      Object
      (hashCode [_] h)
      (equals [this o]
        (and (identical? (class this) (class o)) (= v (deref o)))))))

(defn- cells ^HashSet [ps]
  (let [c (HashSet.)]
    (doseq [p ps] (.add c (cell p)))
    c))

(defn- order [^HashSet c] (mapv deref c))

(defn- add! [^HashSet c p] (.add c (cell p)))

(defn- poll! [^HashSet c]
  (let [it (.iterator c) p (.next it)]
    (.remove it)
    @p))

(defn- none? [^HashSet c] (.isEmpty c))

(defn- size-at ^long [ms ^long h ^long y]
  (case (:type ms)
    :two-layers-feature-size
    (if (< y (long (:limit ms 1)))
      (:lower-size ms 0)
      (:upper-size ms 1))
    :three-layers-feature-size
    (cond (< y (long (:limit ms 1))) (:lower-size ms 0)
          (>= y (- h (long (:upper-limit ms 1)))) (:upper-size ms 1)
          :else (:middle-size ms 1))))

(defn- blocked? [l cfg p]
  (or (not (trunk/free? l (:trunk-placer cfg) p))
      (and (not (:ignore-vines cfg))
           (= :vine (block/block-of (lv/at l p))))))

(defn- layer-blocked? [l cfg o r y]
  (some #(blocked? l cfg %)
        (for [x (range (- r) (inc r)) z (range (- r) (inc r))]
          (lv/off o x y z))))

(defn- free-height ^long [l cfg ^long h o]
  (or (first (for [y (range (+ h 2))
                   :let [r (size-at (:minimum-size cfg) h y)]
                   :when (layer-blocked? l cfg o r y)]
               (- y 2)))
      h))

(defn- fits? [cfg ^long clipped ^long h]
  (let [m (:min-clipped-height (:minimum-size cfg))]
    (or (>= clipped h) (and m (>= clipped (long m))))))

(defn- grown [cfg fh radius [l atts]]
  (reduce #(foliage/place %1 cfg %2 fh radius) l atts))

(defn- in-height? [o t ^long h]
  (let [lo (min (long (o 1)) (long (t 1)))
        hi (+ (max (long (o 1)) (long (t 1))) h 1)]
    (and (>= lo (inc chunk/min-y)) (<= hi (inc chunk/max-y)))))

(defn- rooted [l cfg h t o fh radius]
  (let [rp (:root-placer cfg)
        clipped (free-height l cfg h t)]
    (when (fits? cfg clipped h)
      (when-let [l (if rp (decorate/roots l rp o t) l)]
        (grown cfg fh radius (trunk/place l cfg clipped t))))))

(defn- body [l cfg o]
  (let [h (trunk/height l (:trunk-placer cfg))
        fh (foliage/height l cfg h)
        radius (foliage/radius l cfg)
        rp (:root-placer cfg)
        t (if rp (decorate/trunk-origin l rp o) o)]
    (when (in-height? o t h)
      (rooted l cfg h t o fh radius))))

(defn- by-y [ps] (vec (sort-by #(% 1) (order (cells ps)))))

(defn- context [l]
  {:logs (by-y (:trunks l)) :leaves (by-y (:foliage l))
   :roots (by-y (:roots l))})

(defn- bounds [ps]
  [(reduce #(mapv min %1 %2) ps) (reduce #(mapv max %1 %2) ps)])

(defn- inside? [[lo hi] p]
  (every? true? (map #(<= (long %1) (long %2) (long %3)) lo p hi)))

(defn- distance-of [st]
  (cond (block/tagged? st "prevents_nearby_leaf_decay") 0
        (contains? (block/props-of st) :distance)
        (block/prop-long st :distance)))

(defn- spread-to [box ^objects checks p [l shape ^long d] side]
  (let [q (lv/at-dir p side)
        n (and (inside? box q) (not (shape q))
               (distance-of (lv/at l q)))
        n (when n (min (long n) (inc d)))]
    (if (and n (< (long n) 7))
      (do (add! (aget checks n) q) [l shape (min d (long n))])
      [l shape d])))

(defn- spread [l box checks shape p d]
  (reduce #(spread-to box checks p %1 %2) [l shape d] dir/six))

(defn- visit [l box checks shape p d]
  (if-not (inside? box p)
    [l shape d]
    (let [l (if (zero? d)
              l
              (lv/put l p (block/with (lv/at l p) :distance d)))]
      (spread l box checks (conj shape p) p d))))

(defn- leaves [l box]
  (let [checks (object-array (repeatedly 7 #(cells [])))
        shape (into #{} (filter #(inside? box %))
                    (concat (:decorations l) (:roots l)))]
    (doseq [p (order (cells (:trunks l)))] (add! (aget checks 0) p))
    (loop [l l shape shape d 0]
      (cond
        (>= d 7) [l shape]
        (none? (aget checks d)) (recur l shape (inc d))
        :else (let [p (poll! (aget checks d))
                    [l shape d] (visit l box checks shape p d)]
                (recur l shape (long d)))))))

(defn- line-faces [shape ps lo-side hi-side]
  (let [fs (mapv #(contains? shape %) ps)
        n (count ps)]
    (for [i (range (inc n))
          :let [full? (and (< i n) (fs i))
                last? (and (pos? i) (fs (dec i)))]
          :when (not= full? last?)]
      (if full?
        [:edge (ps i) lo-side]
        [:edge (ps (dec i)) hi-side]))))

(defn- faces [[[x0 y0 z0] [x1 y1 z1]] shape]
  (let [xs (range x0 (inc (long x1))) ys (range y0 (inc (long y1)))
        zs (range z0 (inc (long z1)))
        line #(line-faces shape (mapv %1 %2) %3 %4)]
    (concat
      (for [x xs y ys f (line #(vector x y %) zs :north :south)] f)
      (for [z zs x xs f (line #(vector x % z) ys :down :up)] f)
      (for [y ys z zs f (line #(vector % y z) xs :west :east)] f))))

(defn- finished [l cfg]
  (let [l (decorate/decorate l (:decorators cfg) (context l))
        parts ((juxt :roots :trunks :foliage :decorations) l)
        box (bounds (apply concat parts))
        [l shape] (leaves l box)]
    (update l :changes into (faces box shape))))

(defn place
  "Returns l with the tree of config cfg grown at origin o, with
  :placed? false when it does not grow."
  [l cfg o]
  (let [l' (body l cfg o)]
    (if (and l' (or (seq (:trunks l')) (seq (:foliage l'))))
      (assoc (finished l' cfg) :placed? true)
      (assoc (or l' l) :placed? false))))
