(ns collider.game.command.args
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.reader :as r]))

(set! *warn-on-reflection* true)

(defn- between? [c lo hi] (<= (int lo) (int c) (int hi)))

(defn- path-char? [c]
  (or (between? c \a \z) (between? c \0 \9)
      (= c \_) (= c \-) (= c \.) (= c \/)))

(defn- ns-char? [c] (and (not= c \/) (path-char? c)))

(defn- id-char? [c] (or (= c \:) (path-char? c)))

(defn- identifier [raw]
  (let [i (str/index-of raw ":")
        nm (if (and i (pos? i)) (subs raw 0 i) "minecraft")
        path (if i (subs raw (inc i)) raw)]
    (when (and (not= nm "..") (every? ns-char? nm)
               (every? path-char? path))
      (str nm ":" path))))

(defn- read-raw [[s n]]
  (let [len (count s)]
    (loop [i n]
      (if (and (< i len) (id-char? (nth s i)))
        (recur (inc i))
        [(subs s n i) [s i]]))))

(defn read-id [rd]
  (let [[raw end] (read-raw rd)]
    (if-let [id (identifier raw)]
      [id end]
      (r/error-at rd "argument.id.invalid"))))

(defn id-arg [] {:id "minecraft:resource_location" :parse read-id})

(defn dimension-arg [] {:id "minecraft:dimension" :parse read-id})

(defn dimension [id levels]
  (or (some #(when (= id (data/wire %)) %) levels)
      (r/error "argument.dimension.invalid" id)))

(defn- entries [registry]
  (or (keys (get (data/registries) registry))
      (get (data/datapack) registry)))

(defn- wire-index [registry]
  (into {} (map (fn [k] [(data/wire k) k])) (entries registry)))

(def ^:private not-found "argument.resource.not_found")

(defn- registry-id [registry] (str "minecraft:" registry))

(defn- not-found-at [end id registry]
  (r/error-at end not-found id (registry-id registry)))

(defn- find-resource [index registry rd]
  (let [res (read-id rd)
        k (when-not (r/error? res) (@index (first res)))]
    (cond (r/error? res) res
          k (assoc res 0 k)
          :else (not-found-at (second res) (first res) registry))))

(defn resource-arg [registry]
  (let [index (delay (wire-index registry))]
    {:id "minecraft:resource" :registry registry
     :parse (partial find-resource index registry)}))

(def ^:private units {"d" 24000 "s" 20 "t" 1 "" 1})

(def ^:private time-unit "argument.time.invalid_unit")

(def ^:private time-low "argument.time.tick_count_too_low")

(defn- ticks [v factor]
  (Integer/valueOf
   (Math/round (unchecked-float (* (double v) (double factor))))))

(defn- read-time [lo rd]
  (let [res (r/read-float rd)]
    (if (r/error? res)
      res
      (let [[unit end] (r/read-unquoted (second res))
            factor (units unit)
            t (when factor (ticks (first res) factor))]
        (cond (nil? factor) (r/error-at end time-unit)
              (< t lo) (r/error-at end time-low lo t)
              :else [t end])))))

(defn time-arg
  ([] (time-arg 0))
  ([lo] {:id "minecraft:time" :min lo :parse (partial read-time lo)}))
