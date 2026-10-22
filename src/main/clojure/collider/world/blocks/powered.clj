(ns collider.world.blocks.powered
  "Doors, trapdoors, fence gates and heads that a neighbour's signal
  opens or powers."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private head-types
  #{:skull :wall-skull :wither-skull :wither-wall-skull :player-head
    :player-wall-head})

(defn- on? [^long st k] (= :true (k (block/props-of st))))

(defn- other-half [p ^long st]
  (dir/toward p (if (= :lower (:half (block/props-of st))) :up :down)))

(defn door-signal?
  "Returns true when a neighbour of the door st at p, or of its other
  half, powers it."
  [chunks p ^long st]
  (or (signal/has-neighbor-signal? chunks p)
      (signal/has-neighbor-signal? chunks (other-half p st))))

(defn- signal? [chunks p ^long st]
  (if (contains? block/door-types (block/type-of st))
    (door-signal? chunks p st)
    (signal/has-neighbor-signal? chunks p)))

(defn- opened-fx [^long st open? tick p]
  (let [kind (data/open-sound (block/block-of st) open?)
        pitch (random/hinge-pitch [tick p :door])]
    [[:sound kind 1.0 pitch]
     [:game-event (if open? :block-open :block-close)]]))

(defn- turned
  "Returns the state st takes at signal on, and whether it
  opens or shuts."
  [^long st on]
  (let [t (block/type-of st)
        st' (block/with st :powered (block/flag on))]
    (if (contains? head-types t)
      st'
      (block/with st' :open (block/flag on)))))

(defn- changes [chunks p {:keys [tick]}]
  (let [st (chunk/at chunks p)
        on (signal? chunks p st)]
    (when (not= on (on? st :powered))
      (let [st' (turned st on)
            fx (cond-> []
                 (and (not (contains? head-types (block/type-of st)))
                      (not= on (on? st :open)))
                 (into (opened-fx st on tick p))
                 (on? st :waterlogged) (conj :fluid-tick))]
        [[p st' fx update/clients]]))))

(defn- wake [chunks _dim _tick p old _side]
  (let [st (chunk/at chunks p)]
    (when (and (not (and (contains? block/door-types (block/type-of st))
                         (= (block/block-of old) (block/block-of st))))
               (not= (signal? chunks p st) (on? st :powered)))
      :neighbor)))

(def rule
  "The block rule of blocks a signal opens or powers."
  {:name    :powered
   :match?  (fn [_chunks st _p]
              (let [t (block/type-of st)]
                (or (contains? block/door-types t)
                    (contains? block/trapdoor-types t)
                    (= :fence-gate t)
                    (contains? head-types t))))
   :pass    :neighbor
   :place?  false
   :wake    wake
   :reshape changes})

(defn placed
  "Returns st powered, and opened, as a player places it at pos."
  ^long [chunks pos ^long st]
  (turned st (signal? chunks pos st)))
