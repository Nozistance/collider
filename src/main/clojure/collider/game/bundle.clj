(ns collider.game.bundle
  "Bundles with the stacks they hold, their weight and the item picked
  to come out first."
  (:require [collider.data :as data]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def ^:private nested-weight 1/16)

(defn fits-inside?
  "Returns true when item may go inside a container item."
  [item]
  (not= :shulker-box (:type (get (data/blocks) item))))

(defn- holds? [stack]
  (some? (stack/component stack :bundle-contents)))

(defn bundle-item?
  "Returns true when stack is a bundle by its item."
  [stack]
  (some? (get-in (data/items)
                 [(:item stack) :components :bundle-contents])))

(defn bundle?
  "Returns true when stack is a bundle that holds bundle contents."
  [stack]
  (and (bundle-item? stack) (holds? stack)))

(defn contents
  "Returns the stacks inside bundle, first out first."
  [bundle]
  (mapv stack/of-template (stack/component bundle :bundle-contents)))

(declare weight)

(defn- load-of [stacks]
  (transduce (map #(* (weight %) (stack/size %))) + 0 stacks))

(defn weight
  "Returns the share of a bundle one item of stack takes."
  [stack]
  (cond
    (holds? stack) (+ (load-of (contents stack)) nested-weight)
    (seq (stack/component stack :bees)) 1
    :else (/ 1 (stack/max-size stack))))

(defn- room ^long [stacks w]
  (max 0 (long (quot (- 1 (load-of stacks)) w))))

(defn room-for
  "Returns how many items of stack still fit into bundle."
  ^long [bundle stack]
  (room (contents bundle) (weight stack)))

(defn- stackable? [s]
  (and (> (stack/max-size s) 1)
       (not (and (stack/damageable? s) (pos? (stack/damage s))))))

(defn- index-of [stacks s]
  (when (stackable? s)
    (let [at (fn [i x] (when (stack/same-kind? x s) i))]
      (first (keep-indexed at stacks)))))

(defn- with-contents [bundle stacks]
  (stack/put bundle :bundle-contents (mapv stack/template stacks)))

(defn- selected ^long [bundle] (long (:bundle-selected bundle -1)))

(defn- select-in [bundle ^long n ^long i]
  (if (and (not= i (selected bundle)) (< -1 i n))
    (assoc bundle :bundle-selected i)
    (dissoc bundle :bundle-selected)))

(defn- without [v ^long i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn- merged [stacks s ^long n]
  (if-let [i (index-of stacks s)]
    (let [x (nth stacks i)]
      (into [(assoc x :count (+ (stack/size x) n))]
            (without stacks i)))
    (into [(assoc s :count n)] stacks)))

(defn insert
  "Returns bundle with as much of stack as fits put in, and how many
  items went in."
  [bundle stack]
  (let [stacks (contents bundle)
        n (if (and stack (fits-inside? (:item stack)))
            (min (stack/size stack) (room stacks (weight stack)))
            0)]
    (if (pos? n)
      [(with-contents bundle (merged stacks stack n)) n]
      [bundle 0])))

(defn remove-one
  "Returns bundle without the stack that comes out next, and that
  stack. The stack is nil when the bundle is empty."
  [bundle]
  (let [stacks (contents bundle)
        i (selected bundle)
        i (if (< -1 i (count stacks)) i 0)]
    (if (seq stacks)
      [(-> (with-contents bundle (without stacks i))
           (dissoc :bundle-selected))
       (nth stacks i)]
      [bundle nil])))

(defn select
  "Returns bundle with the stack at index i picked to come out next,
  or with none picked when i is out of range or already picked."
  [bundle i]
  (if (bundle? bundle)
    (select-in bundle (count (contents bundle)) (long i))
    bundle))
