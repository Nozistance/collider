(ns collider.game.stack
  "Stack components read and written over the item defaults."
  (:require [collider.data :as data]))

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

(defn has? [stack k] (some? (component stack k)))

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

(defn size ^long [stack] (if stack (long (:count stack 1)) 0))

(defn max-size ^long [stack]
  (long (or (component stack :max-stack-size) 64)))

(defn damage ^long [stack] (long (or (component stack :damage) 0)))

(defn max-damage ^long [stack]
  (long (or (component stack :max-damage) 0)))

(defn damageable?
  "Returns true when the item wears out and is not unbreakable."
  [stack]
  (and (some? stack) (has? stack :max-damage)
       (not (has? stack :unbreakable)) (has? stack :damage)))

(defn with-damage [stack ^long v]
  (put stack :damage (min (max v 0) (max-damage stack))))

(defn repair-cost ^long [stack]
  (long (or (component stack :repair-cost) 0)))

(defn increased-repair-cost ^long [^long base]
  (min (+ (* base 2) 1) Integer/MAX_VALUE))

(defn enchant-key [stack]
  (if (= :enchanted-book (:item stack))
    :stored-enchantments
    :enchantments))

(defn enchantments
  "Returns the enchantments a station works with, by name."
  [stack]
  (or (component stack (enchant-key stack)) {}))

(defn set-enchantments [stack m]
  (put stack (enchant-key stack) m))

(defn any-enchantments? [stack]
  (boolean (or (seq (or (component stack :enchantments) {}))
               (seq (or (component stack :stored-enchantments) {})))))

(defn custom-name [stack] (component stack :custom-name))

(defn hover-name
  "Returns the name shown on stack, custom or the item's own."
  [stack]
  (or (custom-name stack) (data/item-name (:item stack))))

(defn transmute
  "Returns stack turned into item, keeping the components it carries."
  [stack item]
  (assoc stack :item item))
