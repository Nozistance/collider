(ns collider.tables.reflect
  "Calls into the classes of the vanilla server."
  (:require [collider.tables.value :refer [kw]])
  (:import (clojure.lang Reflector)
           (java.lang.reflect Field)))

(set! *warn-on-reflection* true)

(def ^:dynamic ^ClassLoader *loader*)

(defn any-class
  "Returns the class of the server with the full name n."
  ^Class [n]
  (Class/forName n true *loader*))

(defn cls
  "Returns the class n of the net.minecraft package."
  ^Class [n]
  (any-class (str "net.minecraft." n)))

(defn call [obj m & args]
  (Reflector/invokeInstanceMethod obj m (object-array args)))

(defn call-static [c m & args]
  (^[Class String Object/1] Reflector/invokeStaticMethod
    (cls c) m (object-array args)))

(defn class-field [^Class c f]
  (^[Class String] Reflector/getStaticField c f))

(defn static-field [c f]
  (class-field (cls c) f))

(defn- field-name [^Field f] (Field/.getName f))

(defn- declared-fields [^Class c]
  (->> (iterate Class/.getSuperclass c)
       (take-while some?)
       (mapcat #(sort-by field-name (Class/.getDeclaredFields %)))))

(defn hidden-field
  "Returns the field of obj of class c or its parents that has the
  name or the type want, private or not."
  [^Class c obj want]
  (let [match? (if (string? want)
                 #(= want (Field/.getName %))
                 #(= want (Field/.getType %)))]
    (when-let [^Field f (first (filter match? (declared-fields c)))]
      (Field/.get (doto f (Field/.setAccessible true)) obj))))

(defn key-of [reg x] (kw (str (call reg "getKey" x))))

(defn elements [reg] (iterator-seq (Iterable/.iterator reg)))

(defn registry [name]
  (static-field "core.registries.BuiltInRegistries" name))
