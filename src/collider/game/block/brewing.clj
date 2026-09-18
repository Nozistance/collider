(ns collider.game.block.brewing
  "The brewing stand: its mixes, its fuel and its brewing timer."
  (:require [collider.data :as data]
            [collider.game.craft :as craft]))

(set! *warn-on-reflection* true)

(def ^:const brew-time 400)
(def ^:const fuel-uses 20)

(def bottles
  #{:potion :splash-potion :lingering-potion :glass-bottle})

(def ^:private ^:table index
  (delay (let [b (data/brewing)]
           (assoc b :containers (set (:containers b))
                    :fuel (set (:fuel b))
                    :ingredients (into #{} (map :ingredient)
                                       (concat (:container-mixes b)
                                               (:potion-mixes b)))))))

(defn fuel? [stack]
  (contains? (:fuel @index) (:item stack)))

(defn ingredient?
  "Returns true for an item that some mix brews with."
  [stack]
  (contains? (:ingredients @index) (:item stack)))

(defn- potion-of [stack]
  (get-in stack [:components :potion-contents :potion]))

(defn- matching [mixes from ingredient]
  (first (filter #(and (= from (:from %))
                       (= (:item ingredient) (:ingredient %)))
                 mixes)))

(defn- container-mix [source ingredient]
  (matching (:container-mixes @index) (:item source) ingredient))

(defn- potion-mix [source ingredient]
  (when-let [p (potion-of source)]
    (matching (:potion-mixes @index) p ingredient)))

(defn has-mix?
  "Returns true when brewing ingredient over source changes it."
  [source ingredient]
  (and (contains? (:containers @index) (:item source))
       (boolean (or (container-mix source ingredient)
                    (potion-mix source ingredient)))))

(defn- of-potion [item potion]
  {:item       item :count 1
   :components {:potion-contents {:potion         potion
                                  :custom-color   nil
                                  :custom-effects []
                                  :custom-name    nil}}})

(defn mix
  "Returns what brewing ingredient over source leaves in its slot."
  [ingredient source]
  (if-let [p (potion-of source)]
    (if-let [m (container-mix source ingredient)]
      (of-potion (:to m) p)
      (if-let [m (potion-mix source ingredient)]
        (of-potion (:item source) (:to m))
        source))
    source))

(defn- brewable? [items]
  (let [ing (nth items 3)]
    (and ing (ingredient? ing)
         (boolean (some #(and % (has-mix? % ing)) (take 3 items))))))

(defn- shrink [items ^long i]
  (let [s (nth items i) n (dec (long (:count s 1)))]
    (assoc items i (when (pos? n) (assoc s :count n)))))

(defn- refuel [e]
  (let [f (nth (:items e) 4)]
    (if (and (<= (long (:fuel e 0)) 0) f (fuel? f))
      (assoc e :fuel fuel-uses :items (shrink (:items e) 4))
      e)))

(defn- spent
  "Returns the ingredient slot and what its remainder spills. The
   remainder takes the slot only once the stack is used up."
  [ing]
  (let [n (dec (long (:count ing 1)))
        left (craft/remainder ing)]
    (if (pos? n)
      [(assoc ing :count n) left]
      [left nil])))

(defn- brewed [items]
  (let [ing (nth items 3)
        mixed (reduce #(assoc %1 %2 (mix ing (nth %1 %2)))
                      items (range 3))
        [rest' spill] (spent ing)]
    [(assoc mixed 3 rest') spill]))

(defn- finish [e]
  (let [[items spill] (brewed (:items e))]
    [(assoc e :items items :brew 0) true spill]))

(defn- running [e ^long left]
  (let [ing (nth (:items e) 3)
        ok? (brewable? (:items e))]
    (cond
      (and (zero? left) ok?) (finish e)
      (or (not ok?) (not= (:item ing) (:ingredient e)))
      [(assoc e :brew 0) false nil]
      :else [(assoc e :brew left) false nil])))

(defn- start [e]
  (if (and (brewable? (:items e)) (pos? (long (:fuel e 0))))
    [(assoc e :fuel (dec (long (:fuel e 0)))
              :brew brew-time
              :ingredient (:item (nth (:items e) 3)))
     false nil]
    [e false nil]))

(defn tick
  "Runs one server tick, returning the stand, whether it finished a
   brew and what its ingredient remainder spilled."
  [e]
  (let [e (refuel e)
        left (dec (long (:brew e 0)))]
    (if (>= left 0) (running e left) (start e))))
