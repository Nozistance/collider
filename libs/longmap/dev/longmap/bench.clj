(ns longmap.bench
  "Micro-bench of longmap against int-map and sorted-map."
  (:require [clojure.data.int-map :as im]
            [longmap.core :as lm])
  (:import (com.sun.management ThreadMXBean)
           (java.lang.management ManagementFactory)))

(def ^:private ^ThreadMXBean threads
  (ManagementFactory/getThreadMXBean))

(defn- allocated ^long []
  (.getCurrentThreadAllocatedBytes threads))

(defn- calls
  "Returns how many times `f` ran in `budget` nanoseconds."
  ^long [f ^long budget]
  (let [t0 (System/nanoTime)]
    (loop [c 1]
      (f)
      (if (< (- (System/nanoTime) t0) budget) (recur (inc c)) c))))

(defn- measure
  "Returns ns and bytes per key of `f`, `n` keys per call."
  [f n]
  (dotimes [_ 2000] (f))
  (let [t0 (System/nanoTime) a0 (allocated)
        per (* (calls f 400000000) (double n))]
    [(/ (- (System/nanoTime) t0) per) (/ (- (allocated) a0) per)]))

(defn- chunk-id ^long [^long x ^long z]
  (bit-or (bit-shift-left x 32) (bit-and z 0xffffffff)))

(def shapes
  {:eids (vec (range 1000000 1000600))
   :chunks (vec (for [x (range -32 33) z (range -32 33)]
                  (chunk-id x z)))
   :ticks (vec (range 0 4000 7))})

(def kinds
  {:longmap [lm/int-map lm/int-set lm/union]
   :int-map [im/int-map im/int-set im/union]
   :sorted [sorted-map sorted-set into]})

(defn- built [[mk sk] ks]
  {:m (reduce #(assoc %1 %2 %2) (mk) ks) :s (into (sk) ks)})

(defn- sum-vals ^long [^long a _ v]
  (+ a (long v)))

(defn- ops [[mk _ union :as kind] ks]
  (let [{:keys [m s]} (built kind ks) half (take-nth 2 ks)
        other (into (:s (built kind half)) (map #(+ 3 %)) half)]
    {:get #(reduce (fn [_ k] (get m k)) nil ks)
     :contains #(reduce (fn [_ k] (contains? s k)) nil ks)
     :assoc #(reduce (fn [a k] (assoc a k k)) (mk) ks)
     :union #(union s other)
     :reduce-kv #(reduce-kv sum-vals 0 m)}))

(defn- row [shape kind op]
  (let [ks (shapes shape) f (op (ops (kinds kind) ks))
        [ns b] (measure f (count ks))]
    (format "| %-7s | %-9s | %-8s | %7.1f | %7.1f |"
            (name shape) (name op) (name kind) ns b)))

(defn -main [& _]
  (println "| keys    | op        | impl     | ns/key  | B/key   |")
  (println "|---|---|---|---|---|")
  (doseq [shape [:eids :chunks :ticks]
          op [:get :contains :assoc :union :reduce-kv]
          kind [:longmap :int-map :sorted]]
    (println (row shape kind op)))
  (shutdown-agents))
