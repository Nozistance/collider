(ns longmap.core-test
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [longmap.core :as i])
  (:import (longmap LongMap LongSet)))

(def cases (Long/getLong "longmap.cases" 2000))

(defn- chunk-id [[x z]]
  (bit-or (bit-shift-left x 32) (bit-and z 0xffffffff)))

(def gen-chunk
  (let [c (gen/choose -40 40)]
    (gen/fmap chunk-id (gen/tuple c c))))

(def gen-key
  (gen/frequency
   [[4 (gen/fmap #(+ 1000000 %) (gen/choose 0 2000))]
    [3 gen-chunk]
    [2 (gen/choose 0 300)]
    [2 (gen/choose -1000 0)]
    [1 gen/large-integer]
    [1 (gen/elements [Long/MIN_VALUE (inc Long/MIN_VALUE) -1 0
                      (dec Long/MAX_VALUE) Long/MAX_VALUE])]]))

(defn- gen-op [ops]
  (gen/hash-map :op (gen/elements ops) :i gen/nat :j gen/nat
                :k gen-key :k2 gen-key :n gen/nat
                :ks (gen/vector gen-key 0 12)
                :v (gen/elements [0 1 2 3 :a :b])))

(defn- pick [pool n]
  (nth pool (mod n (count pool))))

(defn- present [m n k]
  (if (seq m) (nth (seq m) (mod n (count m))) k))

(defn- left [a _] a)

(defn- cat-vec
  ([] [])
  ([a b] (into a b)))

(defn- env [pool {:keys [i j k k2 n] :as op}]
  (let [[a ma] (pick pool i) [b mb] (pick pool j)]
    (assoc op :a a :ma ma :b b :mb mb :lo (min k k2) :hi (max k k2)
           :gone (take n (if (map? ma) (keys ma) ma)))))

(defn- churn [t adds gone]
  (persistent! (reduce disj! (reduce conj! t adds) gone)))

(defn- churn-map [t adds gone]
  (persistent! (reduce dissoc! (reduce conj! t adds) gone)))

(defn- between [m lo hi]
  (subseq m >= lo <= hi))

(def set-ops
  {:conj (fn [{:keys [a ma k]}] [(conj a k) (conj ma k)])
   :disj (fn [{:keys [a ma n k]}]
           (let [d (present ma n k)] [(disj a d) (disj ma d)]))
   :into (fn [{:keys [a ma ks]}] [(into a ks) (into ma ks)])
   :transient (fn [{:keys [a ma ks gone]}]
                [(churn (transient a) ks gone)
                 (reduce disj (into ma ks) gone)])
   :union (fn [{:keys [a ma b mb]}] [(i/union a b) (into ma mb)])
   :inter (fn [{:keys [a ma b mb]}]
            [(i/intersection a b) (set/intersection ma mb)])
   :diff (fn [{:keys [a ma b mb]}]
           [(i/difference a b) (set/difference ma mb)])
   :minus (fn [{:keys [a ma gone]}]
            [(i/difference a (i/int-set gone)) (reduce disj ma gone)])
   :range (fn [{:keys [a ma lo hi]}]
            [(i/range a lo hi) (apply sorted-set (between ma lo hi))])
   :sorted (fn [{:keys [ks]}]
             [(i/from-sorted (long-array (sort ks)))
              (apply sorted-set ks)])})

(defn- pairs [{:keys [ks v]}]
  (map vector ks (cycle [v 1 :a])))

(def map-ops
  {:assoc (fn [{:keys [a ma k v]}] [(assoc a k v) (assoc ma k v)])
   :dissoc (fn [{:keys [a ma n k]}]
             (let [d (present (keys ma) n k)]
               [(dissoc a d) (dissoc ma d)]))
   :into (fn [{:keys [a ma] :as op}]
           [(into a (pairs op)) (into ma (pairs op))])
   :transient (fn [{:keys [a ma gone] :as op}]
                [(churn-map (transient a) (pairs op) gone)
                 (reduce dissoc (into ma (pairs op)) gone)])
   :merge (fn [{:keys [a ma b mb]}] [(i/merge a b) (merge ma mb)])
   :merge-with (fn [{:keys [a ma b mb]}]
                 [(i/merge-with left a b) (merge-with left ma mb)])
   :range (fn [{:keys [a ma lo hi]}]
            [(i/range a lo hi)
             (into (sorted-map) (between ma lo hi))])
   :sorted (fn [op]
             (let [m (into (sorted-map) (pairs op))
                   ks (long-array (keys m))]
               [(i/from-sorted ks (object-array (vals m))) m]))})

(defn- ordered? [x m]
  (and (= x m) (= m x) (= (seq x) (seq m)) (= (rseq x) (rseq m))
       (= (count x) (count m)) (= (reduce conj [] x) (vec m))))

(defn- same-set? [^LongSet x m probes]
  (.check x)
  (and (ordered? x m) (= (into #{} x) x)
       (= (hash x) (hash m) (hash (into #{} x)))
       (every? #(= (contains? x %) (contains? m %)) probes)
       (= (reduce (fn [acc ^long k] (conj acc k)) [] x) (vec m))))

(defn- set-laws? [x m probes]
  (let [out (i/int-set (remove #(contains? m %) probes))
        mid (when (seq m) (nth (seq m) (quot (count m) 2)))
        part (if mid (i/range x (first m) mid) x)]
    (and (identical? x (i/union x x))
         (identical? x (i/intersection x x))
         (identical? x (i/union x part))
         (identical? x (i/difference x out))
         (identical? x (into x (take 3 m)))
         (empty? (i/difference x x))
         (= [] (i/diff x x conj []))
         (= x (i/from-sorted (i/keys-array x))))))

(defn- set-diff? [x m y my]
  (= (i/diff x y (fn [acc k added] (conj acc [k added])) [])
     (let [only (into (set/difference m my) (set/difference my m))]
       (for [k (sort only)] [k (contains? my k)]))))

(defn- entry-vec [acc ^long k v]
  (conj acc [k v]))

(defn- same-map? [^LongMap x m probes]
  (.check x)
  (and (ordered? x m) (= (into {} x) x)
       (= (hash x) (hash m) (hash (into {} x)))
       (every? #(= (get x %) (get m %)) probes)
       (= (keys x) (keys m)) (= (vals x) (vals m))
       (= (reduce-kv entry-vec [] x) (vec m))))

(defn- map-laws? [^LongMap x m]
  (let [[k v] (first m)
        part (if k (i/range x k (key (last m))) x)]
    (and (identical? x (i/merge x x))
         (identical? x (i/merge x part))
         (or (nil? k) (identical? x (assoc x k v)))
         (= [] (i/diff x x conj []))
         (= x (i/from-sorted (i/keys-array x) (.vals x)))
         (= (keys m) (seq (.keySet x))))))

(defn- changed [m my]
  (for [k (sort (into (set (keys m)) (keys my)))
        :let [a (get m k) b (get my k)]
        :when (not= a b)]
    [k a b]))

(defn- map-diff? [x m y my]
  (= (i/diff x y (fn [acc k a b] (conj acc [k a b])) [])
     (changed m my)))

(defn- folds? [x m f]
  (every? #(= (vec m) (r/fold % cat-vec f x)) [1 6]))

(defn- set-ok? [pool [x m] {:keys [j ks k]}]
  (let [[y my] (pick pool j) probes (concat ks [k] (take 8 m))]
    (and (same-set? x m probes) (set-laws? x m probes)
         (set-diff? x m y my) (folds? x m conj))))

(defn- map-ok? [pool [x m] {:keys [j ks k]}]
  (let [[y my] (pick pool j)
        probes (concat ks [k] (take 8 (keys m)))]
    (and (same-map? x m probes) (map-laws? x m)
         (map-diff? x m y my) (folds? x m entry-vec))))

(defn- survives? [start ops ok? steps]
  (loop [pool [start] steps steps]
    (if-let [op (first steps)]
      (let [r ((ops (:op op)) (env pool op))]
        (when (ok? pool r op) (recur (conj pool r) (next steps))))
      true)))

(defn- check [start ops ok?]
  (let [steps (gen/vector (gen-op (vec (keys ops))) 1 200)
        p (prop/for-all [s steps] (survives? start ops ok? s))
        r (tc/quick-check cases p)]
    (is (:pass? r) (pr-str (select-keys r [:seed :shrunk])))))

(deftest set-fuzz
  (check [(i/int-set) (sorted-set)] set-ops set-ok?))

(deftest map-fuzz
  (check [(i/int-map) (sorted-map)] map-ops map-ok?))

(deftest chained-difference
  (is (= #{} (-> (i/int-set [10 1000297])
                 (i/difference (i/int-set [10]))
                 (i/difference (i/int-set [1000297]))))))

(deftest nil-values
  (is (thrown? IllegalArgumentException (assoc (i/int-map) 1 nil)))
  (let [nils (constantly nil) m (i/int-map 1 1)]
    (is (thrown? IllegalArgumentException (i/merge-with nils m m)))))

(deftest merge-with-self
  (let [m (i/int-map 1 1 2 2)]
    (is (= {1 2 2 4} (i/merge-with + m m)))))

(deftest key-types
  (is (= {1 :a} (assoc (i/int-map) (int 1) :a)))
  (is (thrown? IllegalArgumentException (assoc (i/int-map) :k 1)))
  (is (thrown? IllegalArgumentException (conj (i/int-set) 1.5)))
  (is (nil? (get (i/int-map 1 :a) :k)))
  (is (not (contains? (i/int-set [1]) "1"))))

(deftest primitives
  (let [m (i/lput (i/int-map) 5 :x) s (i/ladd (i/int-set) -5)]
    (is (= :x (i/lget m 5)))
    (is (= :nf (i/lget m 6 :nf)))
    (is (and (i/lhas? m 5) (i/lhas? s -5) (not (i/lhas? s 5))))
    (is (= [5 5 -5 -5]
           [(i/lfirst m) (i/llast m) (i/lfirst s) (i/llast s)]))
    (is (empty? (i/ldrop m 5)))
    (is (empty? (i/lremove s -5)))))

(deftest transients
  (let [t (transient (i/int-map))]
    (persistent! t)
    (is (thrown? IllegalAccessError (assoc! t 1 1))))
  (let [a (i/int-set (range 100))
        b (persistent! (conj! (transient a) 1000))]
    (is (= (set (range 100)) a))
    (is (= (conj (set (range 100)) 1000) b))))

(deftest meta-and-reduced
  (is (= {:a 1} (meta (with-meta (i/int-map 1 1) {:a 1}))))
  (is (= 3 (reduce #(if (= 3 %2) (reduced %2) %2) 0
                   (i/int-set (range 10))))))
