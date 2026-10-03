(ns collider.game.stack
  "Stack components read and written over the item defaults."
  (:require [clojure.string :as str]
            [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn- proto [item k] (get-in (data/items) [item :components k]))

(defn component
  "Returns the value of component k on stack, or nil."
  [stack k]
  (let [cs (:components stack)]
    (cond
      (contains? cs k) (get cs k)
      (contains? (:removed stack) k) nil
      :else (proto (:item stack) k))))

(defn has?
  "Returns true when stack carries component k."
  [stack k]
  (some? (component stack k)))

(defn- tidy [stack]
  (cond-> stack
    (empty? (:components stack)) (dissoc :components)
    (empty? (:removed stack)) (dissoc :removed)))

(defn put
  "Returns stack with component k set to v, or dropped when v is nil."
  [stack k v]
  (let [s (update stack :components dissoc k)]
    (tidy (cond
            (= v (proto (:item stack) k)) (update s :removed disj k)
            (nil? v) (update s :removed (fnil conj #{}) k)
            :else (-> (update s :components assoc k v)
                      (update :removed disj k))))))

(defn size
  "Returns how many items stack holds, zero for no stack."
  ^long [stack]
  (if stack (long (:count stack 1)) 0))

(defn max-size
  "Returns how many items fit in one stack like stack."
  ^long [stack]
  (long (or (component stack :max-stack-size) 64)))

(defn damage
  "Returns the wear of stack."
  ^long [stack]
  (long (or (component stack :damage) 0)))

(defn max-damage
  "Returns the wear at which stack breaks, zero when it never wears."
  ^long [stack]
  (long (or (component stack :max-damage) 0)))

(defn damageable?
  "Returns true when the item wears out and is not unbreakable."
  [stack]
  (and (some? stack) (has? stack :max-damage)
       (not (has? stack :unbreakable)) (has? stack :damage)))

(defn with-damage
  "Returns stack worn to v, kept within its bounds."
  [stack ^long v]
  (put stack :damage (min (max v 0) (max-damage stack))))

(defn repair-cost
  "Returns what one more repair of stack adds to the cost."
  ^long [stack]
  (long (or (component stack :repair-cost) 0)))

(defn increased-repair-cost
  "Returns the repair cost after one more repair from base."
  ^long [^long base]
  (min (+ (* base 2) 1) Integer/MAX_VALUE))

(defn enchant-key
  "Returns the component that holds the enchantments of stack."
  [stack]
  (if (= :enchanted-book (:item stack))
    :stored-enchantments
    :enchantments))

(defn enchantments
  "Returns the enchantments a station works with, by name."
  [stack]
  (or (component stack (enchant-key stack)) {}))

(defn set-enchantments
  "Returns stack with the enchantments m."
  [stack m]
  (put stack (enchant-key stack) m))

(defn any-enchantments?
  "Returns true when stack carries or stores an enchantment."
  [stack]
  (boolean (or (seq (component stack :enchantments))
               (seq (component stack :stored-enchantments)))))

(defn custom-name
  "Returns the name given to stack, or nil."
  [stack]
  (component stack :custom-name))

(defn- book-title [stack]
  (let [t (:raw (:title (component stack :written-book-content)))]
    (when-not (str/blank? t) t)))

(defn hover-name
  "Returns the name shown on stack. A given name comes first, then
  the title of a book, then the name of the item."
  [stack]
  (or (custom-name stack) (book-title stack)
      (data/item-name (:item stack))))

(defn transmute
  "Returns stack turned into item, keeping the components it carries."
  [stack item]
  (assoc stack :item item))

(defn template
  "Returns the item, count and component patch of stack."
  [stack]
  (when stack
    (cond-> {:item (:item stack) :count (long (:count stack 1))}
      (or (:components stack) (:removed stack))
      (assoc :patch (select-keys stack [:components :removed])))))

(defn of-template
  "Returns the stack template t describes."
  [t]
  (when t
    (merge {:item (:item t) :count (long (:count t 1))} (:patch t))))

(defn same-kind?
  "Returns true when stacks a and b differ in count at most."
  [a b]
  (boolean (and a b (= (dissoc a :count) (dissoc b :count)))))

(defn shrunk
  "Returns stack with k fewer items, or nil when none stay."
  [stack ^long k]
  (when (< k (size stack)) (assoc stack :count (- (size stack) k))))

(defn span
  "Returns the stacks of v at slots from to to, last first when
  reverse? is true."
  [v ^long from ^long to reverse?]
  (map v (if reverse?
           (range (dec to) (dec from) -1)
           (range from to))))

(defn in-range?
  "Returns true when v is within the bounds min and max inclusive.
  A bound that is absent does not limit."
  [{:keys [min max]} v]
  (and (or (nil? min) (<= min v)) (or (nil? max) (<= v max))))

(def water-bottle
  "A bottle of water."
  {:item       :potion :count 1
   :components {:potion-contents
                {:potion :water :custom-color nil
                 :custom-effects [] :custom-name nil}}})

(defn water-bottle?
  "Returns true when stack is a bottle of water."
  [stack]
  (let [path [:components :potion-contents :potion]]
    (and (= :potion (:item stack)) (= :water (get-in stack path)))))
