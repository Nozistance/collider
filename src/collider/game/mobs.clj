(ns collider.game.mobs
  (:require [collider.rnd :as rnd]))

(set! *warn-on-reflection* true)

(defn- sheep-color [ks]
  (let [r (long (* 100.0 (rnd/rnd ks)))]
    (cond
      (< r 5)  15
      (< r 10) 7
      (< r 15) 8
      (< r 18) 12
      (zero? (long (* 500.0 (rnd/rnd (conj ks :pink))))) 6
      :else    0)))

(def types
  {:sheep {:half 0.45 :height 1.3 :speed 0.23
           :max-health 8.0
           :breeding-item :wheat
           :action-means {:eat 1000 :wander 120 :look 50}
           :say :sheep/say
           :step :sheep/step
           :spawn-color sheep-color}})

(defn egg-type [item]
  (when (keyword? item)
    (when-let [[_ m] (re-matches #"(.*)-spawn-egg" (name item))]
      (let [t (keyword m)]
        (when (contains? types t) t)))))
(defn max-health [type] (get-in types [type :max-health]))
(defn mob-type? [type] (contains? types type))
(defn breeding-item [type] (get-in types [type :breeding-item]))
(defn action-means [type] (get-in types [type :action-means]))
(defn say-sound [type] (get-in types [type :say]))
(defn step-sound [type] (get-in types [type :step]))
(def ^:private sheep-meta
  (into {} (for [color (range 16) baby [false true] burning [false true]]
             [[color baby burning] {:color color :baby? baby :burning? burning}])))

(defn burning? [e] (boolean (:burning? e)))
(defn metadata [e]
  (case (:type e)
    :sheep (sheep-meta [(long (or (:color e) 0))
                        (some? (:baby-until e))
                        (burning? e)])))

(defn new-mob [type pos color tick]
  {:type type
   :pos pos
   :vel [0.0 0.0 0.0]
   :yaw 0.0 :pitch 0.0 :on-ground false
   :color color
   :task nil
   :pending nil
   :wake-tick tick
   :health (max-health type)
   :health-sent (max-health type)})

(defn egg-mob [type pos ks tick]
  (let [color-fn (get-in types [type :spawn-color] (constantly 0))
        yaw (- (* 360.0 (rnd/rnd (conj ks :yaw))) 180.0)]
    (assoc (new-mob type pos (color-fn ks) tick)
           :yaw yaw :head-yaw yaw)))

(defn exp-delay ^long [mean ^long t ^long eid kind]
  (max 1 (long (* (double mean) (- (Math/log (max 1.0E-9 (rnd/rnd3 t eid (hash kind)))))))))

(defn in-love? [e t] (> (long (or (:love-until e) 0)) (long t)))
(defn baby? [e] (some? (:baby-until e)))
