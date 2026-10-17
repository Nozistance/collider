(ns collider.world.blocks.note
  "Note blocks that a neighbour's signal plays."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- note? [^long st] (= :note (block/type-of st)))

(defn plays?
  "Returns true when the note block st at p sounds, which it does
  under air or when its instrument plays under anything."
  [chunks p ^long st]
  (or (:above? (data/instrument (:instrument (block/props-of st))))
      (block/air-type? (chunk/at chunks (dir/toward p :up)))))

(defn- powered? [^long st] (= :true (:powered (block/props-of st))))

(defn- changes [chunks p _ctx]
  (let [st (chunk/at chunks p)
        on (signal/has-neighbor-signal? chunks p)]
    (when (not= on (powered? st))
      (let [st' (block/with st :powered (block/flag on))]
        [[p st' (when (and on (plays? chunks p st)) [[:note st]])]]))))

(def rule
  "The block rule of note blocks."
  {:name    :note
   :match?  (fn [_chunks st _p] (note? st))
   :pass    :neighbor
   :place?  false
   :wake    (fn [chunks _dim _tick p _old _side]
              (when (not= (signal/has-neighbor-signal? chunks p)
                          (powered? (chunk/at chunks p)))
                :neighbor))
   :reshape changes})
