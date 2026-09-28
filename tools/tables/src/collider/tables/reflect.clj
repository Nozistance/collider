(ns collider.tables.reflect
  "Calls into the classes of the vanilla server."
  (:import (clojure.lang Reflector)))

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
