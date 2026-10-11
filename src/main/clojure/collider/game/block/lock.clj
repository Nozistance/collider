(ns collider.game.block.lock
  "The locks of containers. A lock is an item predicate as a tag
  holds it, which the main hand must pass."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.components :as cs]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(defn- id-of [x] (if (keyword? x) (subs (str x) 1) (str x)))

(defn- tag-items [^String s]
  (data/tag-values "item" (str/replace (subs s 1) #"^minecraft:" "")))

(defn- items-of [v]
  (into #{}
        (mapcat #(let [s (id-of %)]
                   (if (str/starts-with? s "#") (tag-items s) [(data/kebab s)])))
        (if (sequential? v) v [v])))

(defn- counted? [c ^long n]
  (if (number? c)
    (= (long c) n)
    (<= (long (:min c Long/MIN_VALUE)) n (long (:max c Long/MAX_VALUE)))))

(defn- holds? [s [k v]]
  (let [ck (data/kebab (id-of k))
        [op x] (cs/decode ck v)]
    (and s (= :ok op) (= x (stack/component s ck)))))

(defn unlocks?
  "Returns true when stack s, nil for an empty hand, opens lock. No
  lock, or an empty one, opens with anything."
  [lock s]
  (and (or (nil? (:items lock))
           (boolean (and s (contains? (items-of (:items lock)) (:item s)))))
       (or (nil? (:count lock))
           (counted? (:count lock) (if s (stack/size s) 0)))
       (every? #(holds? s %) (:components lock))
       (empty? (:predicates lock))))
