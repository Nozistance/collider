(ns collider.world.blocks.plate
  "Pressure plates and the count of the bodies on them."
  (:require [collider.data :as data]
            [collider.math :as math]
            [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private touch [1 0 1 15 4 15])

(defn- touch-box [p]
  (mapv (fn [c b] (+ (long c) (/ (double b) 16.0))) (into p p) touch))

(defn plate?
  [^long st]
  (contains? #{:pressure-plate :weighted-pressure-plate}
             (block/type-of st)))

(defn signal-of
  "Returns the signal plate st gives."
  ^long [^long st]
  (if (= :weighted-pressure-plate (block/type-of st))
    (block/prop-long st :power)
    (if (= :true (:powered (block/props-of st))) 15 0)))

(defn- with-signal ^long [^long st ^long n]
  (if (= :weighted-pressure-plate (block/type-of st))
    (block/with-long st :power n)
    (block/with st :powered (block/flag (pos? n)))))

(defn told
  "Returns the ops that tell the cells around plate st at p and around
  the block under it."
  [p ^long st]
  [[:notify p nil st] [:notify (dir/toward p :down) nil st]])

(defn- fact [block k]
  (or (get (data/info block) k)
      (throw (ex-info "a plate the tables do not know"
                      {:block block :key k}))))

(defn- weighed
  "Returns the signal of a weighted plate that most bodies weigh down
  fully, with n bodies on it."
  ^long [^long most ^long n]
  (if (pos? n)
    (let [m (double most)
          part (math/fdiv (min (double n) m) m)]
      (long (Math/ceil (math/fmul part 15.0))))
    0))

(defn- counted
  "Returns the signal plate st at p gives for the bodies on it."
  ^long [p ^long st bodies]
  (let [block (block/block-of st)]
    (if (= :weighted-pressure-plate (block/type-of st))
      (weighed (fact block :max-weight) (long (bodies (touch-box p) false)))
      (let [living? (= :mobs (fact block :sensitivity))]
        (if (pos? (long (bodies (touch-box p) living?))) 15 0)))))

(defn- clicks [block ^long old ^long s]
  (cond
    (and (pos? old) (zero? s))
    [[:sound (data/open-sound block false) 1.0 1.0]
     [:game-event :block-deactivate]]
    (and (zero? old) (pos? s))
    [[:sound (data/open-sound block true) 1.0 1.0]
     [:game-event :block-activate]]))

(defn checked
  "Returns the ops of plate st at p after it counts the bodies on it at
  tick t. Bodies takes a box and whether only players and mobs count."
  [p ^long st bodies t]
  (let [old (signal-of st)
        block (block/block-of st)
        s (counted p st bodies)
        st' (with-signal st s)
        at (+ (long t) (data/pressed-ticks block))]
    (cond-> []
      (not= old s)
      (into (cons [p st' (clicks block old s) update/clients]
                  (told p st')))
      (pos? s) (conj [:tick p at 0 block]))))

(defn- due [chunks p ctx]
  (let [st (chunk/at chunks p)]
    (when (and (plate? st) (pos? (signal-of st)))
      (checked p st (:bodies ctx) (:tick ctx)))))

(def rule
  "The block rule of pressure plates. They are held as attached blocks
  are and count the bodies on them again on their tick."
  (assoc support/rule
         :name :plate
         :match? (fn [_chunks st _p] (plate? st))
         :due due))
