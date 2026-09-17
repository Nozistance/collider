(ns collider.game.mob.mobs
  "Mob kinds and the start state of a new mob."
  (:require [collider.data :as data]
            [collider.random :as random]))

(set! *warn-on-reflection* true)

(def ^:private ^:const black 15)
(def ^:private ^:const gray 7)
(def ^:private ^:const light-gray 8)
(def ^:private ^:const brown 12)
(def ^:private ^:const pink 6)
(def ^:private ^:const white 0)
(def ^:private temperate-colors [[5 black] [5 gray] [5 light-gray] [3 brown]])
(def ^:private ^:const temperate-total 100.0)
(def ^:private ^:const common-total 500.0)

(defn- weighted [^long r entries]
  (loop [lo 0 [[w c] & more] entries]
    (when w
      (if (< r (+ lo (long w))) c (recur (+ lo (long w)) more)))))

(defn- common-color [ks]
  (if (zero? (long (* common-total (random/of-key (conj ks :pink))))) pink white))

(defn- sheep-color [ks]
  (or (weighted (long (* temperate-total (random/of-key ks))) temperate-colors)
      (common-color ks)))

(defn- sounds [type]
  (into {} (for [k [:say :step :hurt :death]] [k (keyword (name type) (name k))])))

(def types
  {:sheep (merge (sounds :sheep)
                 {:half          0.45 :height 1.3 :speed 0.23
                  :max-health    8.0
                  :breeding-item :wheat
                  :speeds        {:panic 1.25 :tempt 1.1 :follow 1.1}
                  :spawn-color   sheep-color})
   :cow   (merge (sounds :cow)
                 {:half          0.45 :height 1.4 :speed 0.2
                  :max-health    10.0
                  :breeding-item :wheat
                  :speeds        {:panic 2.0 :tempt 1.25 :follow 1.25}})
   :mooshroom (merge (sounds :cow)
                     {:half          0.45 :height 1.4 :speed 0.2
                      :max-health    10.0
                      :breeding-item :wheat
                      :ground        :mycelium
                      :speeds        {:panic 2.0 :tempt 1.25 :follow 1.25}})})

(defn egg-type [item]
  (let [t (get-in (data/items) [item :spawns])]
    (when (contains? types t) t)))
(defn max-health [type] (get-in types [type :max-health]))
(defn mob-type? [type] (contains? types type))
(defn breeding-item [type] (get-in types [type :breeding-item]))
(defn say-sound [type] (get-in types [type :say]))
(defn step-sound [type] (get-in types [type :step]))
(defn hurt-sound [type] (get-in types [type :hurt]))
(defn death-sound [type] (get-in types [type :death]))
(def ^:private sheep-meta
  (into {} (for [color (range 16) baby [false true] burning [false true] sheared [false true]]
             [[color baby burning sheared]
              (cond-> {:color color :baby? baby :burning? burning} sheared (assoc :sheared? true))])))

(defn burning? [e] (boolean (:burning? e)))
(defn metadata [e]
  (case (:type e)
    :sheep (sheep-meta [(long (or (:color e) 0))
                        (some? (:baby-until e))
                        (burning? e)
                        (boolean (:sheared? e))])
    :cow {:baby? (some? (:baby-until e)) :burning? (burning? e)}
    :mooshroom {:baby? (some? (:baby-until e)) :burning? (burning? e) :variant (long (or (:color e) 0))}))

(defn new-mob [type pos color tick]
  {:type        type
   :pos         pos
   :vel         [0.0 0.0 0.0]
   :yaw         0.0 :pitch 0.0 :on-ground false
   :color       color
   :task        nil
   :health      (max-health type)
   :health-sent (max-health type)})

(defn egg-mob [type pos ks tick]
  (let [color-fn (get-in types [type :spawn-color] (constantly 0))
        yaw (- (* 360.0 (random/of-key (conj ks :yaw))) 180.0)]
    (assoc (new-mob type pos (color-fn ks) tick)
      :yaw yaw :head-yaw yaw)))

(defn exp-delay ^long [mean ^long t ^long eid kind]
  (max 1 (long (* (double mean) (- (Math/log (max 1.0E-9 (random/of-longs t eid (hash kind)))))))))

(defn in-love? [e t] (> (long (or (:love-until e) 0)) (long t)))
(defn baby? [e] (some? (:baby-until e)))
