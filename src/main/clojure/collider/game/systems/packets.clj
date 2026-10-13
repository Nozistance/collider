(ns collider.game.systems.packets
  "The packet events of the tick, one after another in the order they
  came."
  (:require [collider.game.apply :as apply]
            [collider.game.camera :as camera]
            [collider.game.deltas :as deltas]
            [collider.game.input :as input]
            [collider.game.level :as level]
            [collider.game.mob.interact :as interact]
            [collider.game.player :as player]
            [collider.game.systems.attacks :as attacks]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.hanging :as hanging]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.players :as players]
            [collider.game.systems.spawning :as spawning]
            [collider.game.turn.landing :as landing]))

(set! *warn-on-reflection* true)

(defn- own [f]
  (fn [w ev] (when (number? (nth ev 1 nil)) (f w ev))))

(defn- spectated [w [_ eid tid]] (camera/spectate-deltas w eid tid))

(def ^:private player-input
  {:move player/move
   :client-tick-end player/client-tick-end
   :abilities player/abilities
   :teleport-ack player/teleport-ack
   :chunk-batch-ack player/chunk-batch-ack
   :entity-action player/entity-action
   :input (fn [w [_ eid flags]] (level/update-entity w eid merge flags))
   :client-settings player/client-settings
   :place player/place
   :use-item player/use-item
   :release-use player/release-use})

(defn- changed [eid e e']
  (when-not (identical? e e')
    (when-let [gone (seq (remove #(contains? e' %) (keys e)))]
      (throw (ex-info "a player event took keys away" {:keys gone})))
    [[:merge-entity eid
      (into {} (remove (fn [[k v]] (identical? v (get e k)))) e')]]))

(defn- played [w [tag eid :as ev]]
  (let [w' ((player-input tag) w ev)
        e' (get-in w' [:entities eid])
        heard (input/played w w' ev)]
    (concat (when e' (changed eid (get-in w [:entities eid]) e'))
            (when (seq heard) [[:heard heard]])
            (when-let [m (:moves heard)] (landing/moved-deltas w' m)))))

(defn- respawned [w [_ eid :as ev]]
  (let [w' (player/respawn w ev)
        req (get-in w' [:spawning eid])]
    (when-not (identical? w w')
      (concat [[:spawn-progress eid req]]
              (changed eid (get-in w [:entities eid])
                       (get-in w' [:entities eid]))
              (spawning/arrival w' eid req)))))

(def ^:private handlers
  [[(set (keys player-input)) played]
   [#{:respawn} respawned]
   [#{:spectate} spectated]
   [#{:dig :place :use-item :sign-update} blocks/edit-deltas]
   [#{:client-settings} (own player/settings-deltas)]
   [#{:click :menu-click :pick :bundle-select} (own inventory/event-deltas)]
   [#{:dig :creative-slot} (own items/event-deltas)]
   [#{:swing} (own players/swing-deltas)]
   [#{:menu-click :menu-close :menu-button :rename-item :bundle-select}
    containers/event-deltas]
   [chat/tags chat/input-deltas]
   [#{:interact :attack} hanging/input-deltas]
   [#{:attack :swing} attacks/input-deltas]
   [#{:interact} interact/input-deltas]])

(defn- handled [w ev]
  (let [tag (nth ev 0)]
    (loop [w w hs (filter #(contains? (first %) tag) handlers) acc []]
      (if-let [[_ f] (first hs)]
        (let [ds (vec (f w ev))
              more (next hs)]
          (recur (if (and more (seq ds)) (first (apply/applied w ds)) w)
                 more (into acc ds)))
        acc))))

(defn- arrived [ds]
  (sort-by #(:at (meta %))
           (mapcat #(deltas/input-of (val %)) ds)))

(defn packets
  "Returns the deltas of the packet events of every level in the
  order they came, each seeing the world after the ones before it.
  Takes the world and the deltas of each level."
  {:wake :always :once true :whole true}
  [world ds]
  (deltas/of-vec (apply/fold-world world (arrived ds) handled)))
