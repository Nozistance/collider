(ns collider.net.render.audience
  "Who sees an effect of a tick."
  (:require [collider.data.long-map :as lm]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- players [world]
  (vec (sort (vals (:players world)))))

(def ^:private ^:const explosion-range-sq 4096.0)

(defn- in-earshot? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (v/dist-sq p center) explosion-range-sq)))

(def ^:private ^:const level-range-sq (* 64.0 64.0))

(defn- in-level-range? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (v/dist-sq p center) level-range-sq)))

(defn- sound-range-sq [volume]
  (let [v (double volume)
        r (if (> v 1.0) (* 16.0 v) 16.0)]
    (* r r)))

(defn- in-sound-range? [world center volume eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (v/dist-sq p center) (sound-range-sq volume))))

(def ^:private ^:const particle-range-sq (* 32.0 32.0))

(defn- block-center [pos]
  [(+ (Math/floor (v/x pos)) 0.5)
   (+ (Math/floor (v/y pos)) 0.5)
   (+ (Math/floor (v/z pos)) 0.5)])

(defn- in-particle-range? [world center eid]
  (when-let [p (get-in world [:entities eid :pos])]
    (< (v/dist-sq (block-center p) center) particle-range-sq)))

(defn- chunk-of-msg [m]
  (case (:msg m)
    :blocks-changed (:cp m)
    :block-entity (chunk/block-chunk (:pos m))))

(defn- tracking-chunk? [world cp eid]
  (contains? (get-in world [:entities eid :sent-chunks]) cp))

(defn- ranged-recipients [world ps m]
  (case (:msg m)
    (:blocks-changed :block-entity)
    (let [cp (chunk-of-msg m)]
      (filterv #(tracking-chunk? world cp %) ps))
    (:level-event :break-effect :fizz :bonemeal :extinguish
     :block-event)
    (filterv #(in-level-range? world (:pos m) %) ps)
    :sound
    (filterv #(in-sound-range? world (:pos m) (:volume m) %) ps)
    (:particles :trail)
    (filterv #(in-particle-range? world (:pos m) %) ps)
    :explosion
    (filterv #(in-earshot? world (:center m) %) ps)
    ps))

(def ^:private home (first schema/dims))

(def ^:private everyone
  #{:time :rain-started :rain-stopped :player-chat :system-chat
    :tab-add :tab-remove :tab-latency :tab-header :default-spawn
    :tab-game-mode
    :game-rules :reloaded :view-distance :simulation-distance
    :rule-flag})

(defn sight-of
  "Returns the players of world by level, with the levels they see."
  [world]
  (let [ps (players world)
        dim-of #(or (level/dim-of world %) home)
        of (into {} (map (fn [p] [p (dim-of p)])) ps)]
    {:ps     ps
     :of     of
     :by-dim (group-by of ps)
     :levels (:levels (level/synced world))}))

(defn level-of
  "Returns level dim of sight."
  [sight dim]
  (get (:levels sight) (or dim home)))

(defn own-level
  "Returns the level player eid of sight stands in."
  [sight eid]
  (level-of sight (get (:of sight) eid)))

(defn- forgotten [deltas pid]
  (for [[tag _ _ gone] (get deltas pid)
        :when (= :tracking tag)
        eid gone]
    eid))

(defn- add-viewer [a eid pid]
  (assoc! a eid (conj (get a eid []) pid)))

(defn viewer-index
  "Returns the players that see each entity, the ones that forget it
  in the entity deltas included."
  [sight deltas]
  (persistent!
    (reduce (fn [acc pid]
              (let [lv (own-level sight pid)
                    seen (get-in lv [:entities pid :tracking])
                    all (concat seen (forgotten deltas pid))]
                (reduce (fn [a eid] (add-viewer a eid pid))
                        acc all)))
            (transient (lm/long-map))
            (:ps sight))))

(def ^:private entity-msgs
  #{:move :move-look :look :sync-pos :head-look :velocity :meta
    :equipment :animation :status :collect :attributes
    :damage-event})

(defn- level-audience [sight dim m]
  (ranged-recipients (level-of sight dim)
                     (get (:by-dim sight) dim []) m))

(defn audience
  "Returns the players that effect m reaches by its message."
  [sight m]
  (let [dim (:dim m)]
    (cond (everyone (:msg m)) (:ps sight)
          (some? dim) (level-audience sight dim m)
          :else (into [] (mapcat #(level-audience sight % m))
                      schema/dims))))

(defn recipients
  "Returns the players that get effect m."
  [sight viewers m]
  (cond
    (:to m) [(:to m)]
    (entity-msgs (:msg m))
    (get @viewers (long (or (:via m) (:eid m))) [])
    :else
    (let [base (audience sight m)]
      (if (:except m) (remove #{(:except m)} base) base))))
