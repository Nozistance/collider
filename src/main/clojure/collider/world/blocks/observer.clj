(ns collider.world.blocks.observer
  "The observer, which pulses from its back when the cell it faces
  changes its shape."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:const pulse-ticks
  "The ticks an observer waits to turn on and stays on."
  2)

(defn observer?
  "Returns true when st is an observer."
  [^long st]
  (= :observer (block/type-of st)))

(defn powered?
  "Returns true when observer st is on."
  [^long st]
  (= :true (:powered (block/props-of st))))

(defn front
  "Returns the ops of observer st at p that tell the cell behind it,
  and the cells around that cell, of a change."
  [p ^long st]
  (let [f (block/facing-of st)
        q (dir/toward p (dir/opposite f))]
    [[:changed q st f] [:notify q f st]]))

(defn- scheduled? [{:keys [scheduled?]} p]
  (if scheduled?
    (scheduled? p :observer)
    (throw (ex-info "no scheduled ticks to read" {:pos p}))))

(defn- pulse [p tick]
  [:tick p (+ (long tick) pulse-ticks) 0 :observer])

(defn- started [p ^long st {:keys [side tick] :as ctx}]
  (when (and (= side (block/facing-of st)) (not (powered? st))
             (not (scheduled? ctx p)))
    [(pulse p tick)]))

(defn- placed [p ^long st {:keys [old] :as ctx}]
  (when (and (not (observer? (long old))) (powered? st)
             (not (scheduled? ctx p)))
    (let [off (block/with st :powered :false)]
      (into [[p off nil (bit-or update/known-shape update/clients)]]
            (front p off)))))

(defn- due [chunks p {:keys [tick]}]
  (let [st (chunk/at chunks p)
        on (powered? st)]
    (-> [[p (block/with st :powered (block/flag (not on))) nil
          update/clients]]
        (cond-> (not on) (conj (pulse p tick)))
        (into (front p st)))))

(def rule
  "The block rule of the observer. Its tick reads only its own cell."
  {:name    :observer
   :match?  (fn [_chunks st _p] (observer? st))
   :pass    :shape
   :reach   0
   :wake    (constantly :neighbor)
   :reshape (fn [chunks p ctx]
              (let [st (chunk/at chunks p)]
                (case (:pass ctx)
                  :shape (started p st ctx)
                  :place (placed p st ctx))))
   :due     due})
