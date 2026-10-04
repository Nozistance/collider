(ns collider.game.entity.gen
  "Code that the macros of entities expand to."
  (:require [collider.game.entity.records]
            [collider.vec :as v])
  (:import (clojure.lang PersistentArrayMap)
           (collider.game.entity.records Mob)))

(set! *warn-on-reflection* true)

(def ^:private mob-fields (Mob/getBasis))

(defn- field [e f] (list '. e (symbol (str "-" f))))

(defmacro with-extmap [e x]
  (let [o (with-meta (gensym "m") {:tag `Mob})]
    `(let [~o ~e]
       (new Mob ~@(map #(field o %) mob-fields) (meta ~o) ~x))))

(defmacro extmap [e]
  (field (with-meta e {:tag `Mob}) "__extmap"))

(defn- same? [o vs]
  `(and ~@(map (fn [[k s]] `(v/same? ~s ~(field o (name k)))) vs)))

(defn- rebuilt [o at]
  `(new Mob ~@(map at mob-fields) (meta ~o) ~(field o "__extmap")))

(defmacro with
  "Returns entity e with the keys of kvs set to their values.
  A mob that already holds them all is returned as it is."
  [e kvs]
  (assert (every? (set (map keyword mob-fields)) (keys kvs)))
  (let [x (gensym "e")
        o (with-meta (gensym "m") {:tag `Mob})
        vs (into {} (map (fn [[k _]] [k (gensym (name k))])) kvs)
        at (fn [f] (get vs (keyword f) (field o f)))]
    `(let [~x ~e ~@(mapcat (fn [[k v]] [(vs k) v]) kvs)]
       (if (instance? Mob ~x)
         (let [~o ~x] (if ~(same? o vs) ~o ~(rebuilt o at)))
         (assoc ~x ~@(into [] cat vs))))))

(defn- fields-of [cls]
  (eval (list (symbol (str cls) "getBasis"))))

(defn- filled [e fields]
  (let [put (fn [i f] `(aset ~i ~(field e f)))]
    `(doto (object-array ~(count fields))
       ~@(map-indexed put fields))))

(defn- put-field [a x k v fields]
  (let [put (fn [i f] [(keyword f) `(do (aset ~a ~i ~v) ~x)])]
    `(case ~k
       ~@(into [] (comp (map-indexed put) cat) fields)
       (assoc ~x ~k ~v))))

(defmacro merger
  "Returns the function that merges a map into a record of class cls."
  [cls]
  (let [fields (fields-of cls)
        e (with-meta (gensym "e") {:tag cls})
        a (with-meta (gensym "a") {:tag 'objects})
        [m x k v] (map gensym ["m" "x" "k" "v"])
        got (fn [i] `(aget ~a ~i))
        f `(fn [~x ~k ~v] ~(put-field a x k v fields))]
    `(fn [~e ~m]
       (let [~a ~(filled e fields)
             ext# (reduce-kv ~f ~(field e "__extmap") ~m)]
         (new ~cls ~@(map got (range (count fields))) (meta ~e)
              ext#)))))

(defn- diff-step [o n a c c' k]
  (let [g (symbol (str ".-" (name k)))]
    [c' `(if (v/same? (~g ~n) (~g ~o))
           ~c
           (do (aset ~a ~c ~k)
               (aset ~a (unchecked-inc ~c) (~g ~n))
               (unchecked-add ~c 2)))]))

(defn- diff-size [o n k]
  (let [g (symbol (str ".-" (name k)))]
    `(if (v/same? (~g ~n) (~g ~o)) 0 2)))

(defmacro diff-fields
  "Returns the fields ks whose values mob new changed from mob old."
  [old new & ks]
  (let [o (with-meta (gensym "o") {:tag `Mob})
        n (with-meta (gensym "n") {:tag `Mob})
        a (with-meta (gensym "a") {:tag 'objects})
        cs (vec (repeatedly (inc (count ks)) #(gensym "c")))
        step (fn [i k] (diff-step o n a (cs i) (cs (inc i)) k))
        add (fn [acc k] `(unchecked-add ~acc ~(diff-size o n k)))
        size (reduce add 0 ks)]
    `(let [~o ~old ~n ~new ~a (object-array ~size)
           ~(cs 0) 0 ~@(mapcat step (range) ks)]
       (PersistentArrayMap. ~a))))
