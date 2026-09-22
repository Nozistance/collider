(ns collider.game.block.grindstone
  "What a grindstone strips off items and the experience it frees."
  (:require [collider.data :as data]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def ^:private ^:table curses
  (delay (set (data/tag-values "enchantment" "curse"))))

(defn curse? [name] (contains? @curses name))

(defn accepts?
  "Returns true when a slot of a grindstone takes stack."
  [stack]
  (or (stack/damageable? stack) (stack/any-enchantments? stack)))

(defn- grown-cost ^long [^long n]
  (reduce (fn [^long c _] (stack/increased-repair-cost c))
          0 (range n)))

(defn- stripped
  "Returns item without any enchantment that is not a curse."
  [item]
  (let [all (stack/enchantments item)
        kept (into {} (filter (comp curse? key)) all)
        held? (stack/has? item (stack/enchant-key item))
        item (cond-> item held? (stack/set-enchantments kept))
        book? (and (= :enchanted-book (:item item)) (empty? kept))]
    (-> (cond-> item book? (stack/transmute :book))
        (stack/put :repair-cost (grown-cost (count kept))))))

(defn- upgraded [have [name level]]
  (let [at (long (get have name 0))]
    (if (and (curse? name) (pos? at))
      have
      (assoc have name (min 255 (max at (long level)))))))

(defn- merge-enchants [target source]
  (if (stack/has? target (stack/enchant-key target))
    (stack/set-enchantments
      target (reduce upgraded (stack/enchantments target)
                     (stack/enchantments source)))
    target))

(defn- pair-count [input additional]
  (if (stack/damageable? input)
    1
    (when (and (>= (stack/max-size input) 2)
               (= input additional))
      2)))

(defn- worn [item ^long durability ^long left]
  (-> (stack/put item :max-damage durability)
      (stack/with-damage (max (- durability left) 0))))

(defn- merged [input additional]
  (when (= (:item input) (:item additional))
    (let [durability (max (stack/max-damage input)
                       (stack/max-damage additional))
          left (+ (- (stack/max-damage input) (stack/damage input))
                  (- (stack/max-damage additional)
                     (stack/damage additional))
                  (quot (* durability 5) 100))]
      (when-let [n (pair-count input additional)]
        (-> (assoc input :count n)
            (cond-> (stack/damageable? input)
              (worn durability left))
            (merge-enchants additional)
            (stripped))))))

(defn result
  "Returns what a grindstone offers for the two inputs."
  [input additional]
  (cond
    (and (nil? input) (nil? additional)) nil
    (or (> (stack/size input) 1) (> (stack/size additional) 1)) nil
    (and input additional) (merged input additional)
    :else (let [item (or input additional)]
            (when (stack/any-enchantments? item) (stripped item)))))

(defn- min-cost ^long [name ^long level]
  (let [c (:min-cost (data/enchantment name))]
    (+ (long (:base c 0)) (* (long (:per-level c 0)) (dec level)))))

(defn- worth ^long [^long sum name level]
  (if (curse? name) sum (+ sum (min-cost name (long level)))))

(defn- item-experience ^long [stack]
  (reduce-kv worth 0 (stack/enchantments stack)))

(defn experience
  "Returns what the two inputs are worth before the roll."
  [input additional]
  (+ (item-experience input) (item-experience additional)))

(defn reward
  "Returns the experience taking the result frees, given a roll.
  Nothing hands it out: the game has no experience orbs yet."
  [input additional ^double roll]
  (let [amount (experience input additional)]
    (if (pos? amount)
      (let [half (long (Math/ceil (/ (double amount) 2.0)))]
        (+ half (long (* roll (double half)))))
      0)))
