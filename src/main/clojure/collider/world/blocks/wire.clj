(ns collider.world.blocks.wire
  "The shape of redstone wire, the sides it runs to and where it
  climbs a block."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def ^:private horizontal [:north :east :south :west])

(def ^:private shape-order [:west :east :north :south :down :up])

(defn wire?
  [^long st]
  (= :redstone-wire (block/type-of st)))

(defn side-of
  "Returns :up, :side or :none, how wire st runs toward d."
  [^long st d]
  (get (block/props-of st) d))

(defn- connected? [s] (not= :none s))

(defn- dot? [^long st]
  (not-any? #(connected? (side-of st %)) horizontal))

(defn- cross? [^long st]
  (every? #(connected? (side-of st %)) horizontal))

(defn- with-power ^long [^long st ^long power]
  (block/with-long st :power power))

(defn- power-of ^long [^long st] (block/prop-long st :power))

(def ^:private ^:table plain
  (delay (block/state :redstone-wire)))

(def ^:private ^:table cross
  (delay (block/state :redstone-wire {:north :side :east :side
                                      :south :side :west :side})))

(defn- stands-on? [^long st]
  (or (block/face-sturdy? st :up) (= :hopper (block/block-of st))))

(defn- connects-to? [^long st d]
  (case (block/type-of st)
    :redstone-wire true
    :repeater (let [f (block/facing-of st)]
                (or (= f d) (= (dir/opposite f) d)))
    :observer (= d (block/facing-of st))
    (and (block/signal-source? st) (some? d))))

(defn- connecting-side
  [chunks p d up?]
  (let [at #(chunk/at chunks %)
        q (dir/toward p d)
        n (at q)]
    (or (when (and up?
                   (or (contains? block/trapdoor-types (block/type-of n))
                       (stands-on? n))
                   (connects-to? (at (dir/toward q :up)) nil))
          (if (block/face-sturdy? n (dir/opposite d)) :up :side))
        (if (and (not (connects-to? n d))
                 (or (block/conductor? n)
                     (not (connects-to? (at (dir/toward q :down)) nil))))
          :none
          :side))))

(defn- up-open? [chunks p]
  (not (block/conductor? (chunk/at chunks (dir/toward p :up)))))

(defn- missing ^long [chunks p ^long st]
  (let [up? (up-open? chunks p)]
    (reduce (fn [^long st d]
              (if (connected? (side-of st d))
                st
                (block/with st d (connecting-side chunks p d up?))))
            st horizontal)))

(defn connection-state
  "Returns the wire st at p with its sides worked out from its
  neighbours. A dot stays a dot while nothing joins it, and a wire
  joined on one axis only runs straight along it."
  ^long [chunks p ^long st]
  (let [st' (missing chunks p (with-power @plain (power-of st)))]
    (if (and (dot? st) (dot? st'))
      st'
      (let [on? #(connected? (side-of st' %))
            ns-empty? (not (or (on? :north) (on? :south)))
            ew-empty? (not (or (on? :east) (on? :west)))]
        (cond-> st'
          (and (not (on? :west)) ns-empty?) (block/with :west :side)
          (and (not (on? :east)) ns-empty?) (block/with :east :side)
          (and (not (on? :north)) ew-empty?) (block/with :north :side)
          (and (not (on? :south)) ew-empty?) (block/with :south :side))))))

(defn placed
  "Returns the wire a player places at p."
  ^long [chunks p]
  (connection-state chunks p @cross))

(defn- shaped-toward ^long [chunks p ^long st d]
  (case d
    :down (if (stands-on? (chunk/at chunks (dir/toward p :down))) st 0)
    :up (connection-state chunks p st)
    (let [s (connecting-side chunks p d (up-open? chunks p))]
      (if (and (= (connected? s) (connected? (side-of st d)))
               (not (cross? st)))
        (block/with st d s)
        (connection-state chunks p (-> (with-power @cross (power-of st))
                                       (block/with d s)))))))

(defn shaped
  "Returns the wire st at p after a change on side d, or after a look
  at every side when d is nil. Air when it cannot stay."
  ^long [chunks p ^long st d]
  (if d
    (shaped-toward chunks p st d)
    (reduce (fn [^long st d]
              (if (wire? st) (shaped-toward chunks p st d) (reduced st)))
            st shape-order)))

(defn toggled
  "Returns the wire st at p a hand turns from a cross to a dot or back,
  nil when it is neither or nothing changes."
  [chunks p ^long st]
  (when (or (cross? st) (dot? st))
    (let [base (if (cross? st) @plain @cross)
          st' (connection-state chunks p (with-power base (power-of st)))]
      (when (not= st' st) st'))))

(defn told-on-turn
  "Returns the ops that tell the conductors beside wire p whose side
  turned on or off, going from st to st'."
  [chunks p ^long st ^long st']
  (vec (for [d horizontal
             :let [q (dir/toward p d)]
             :when (and (not= (connected? (side-of st d))
                              (connected? (side-of st' d)))
                        (block/conductor? (chunk/at chunks q)))]
         [:notify q (dir/opposite d) st'])))
