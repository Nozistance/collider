(ns collider.game.systems.packets
  "The packet events of the tick, one after another in the order they
  came."
  (:require [collider.game.apply :as apply]
            [collider.game.camera :as camera]
            [collider.game.deltas :as deltas]
            [collider.game.mob.interact :as interact]
            [collider.game.player :as player]
            [collider.game.systems.attacks :as attacks]
            [collider.game.systems.blocks :as blocks]
            [collider.game.systems.chat :as chat]
            [collider.game.systems.containers :as containers]
            [collider.game.systems.hanging :as hanging]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.players :as players]))

(set! *warn-on-reflection* true)

(defn- own [f]
  (fn [w [_ ev]] (when (number? (nth ev 1 nil)) (f w ev))))

(defn- spectated [w [_ [_ eid tid]]] (camera/spectate-deltas w eid tid))

(def ^:private handlers
  [[#{:spectate} spectated]
   [#{:dig :place :use-item :sign-update} blocks/edit-deltas]
   [#{:client-settings} (own player/settings-deltas)]
   [#{:click :menu-click :pick :bundle-select} (own inventory/event-deltas)]
   [#{:dig :creative-slot} (own items/event-deltas)]
   [#{:swing} (own players/swing-deltas)]
   [#{:menu-click :menu-close :menu-button :rename-item :bundle-select}
    (fn [w [_ ev]] (containers/event-deltas w ev))]
   [chat/tags (fn [w [_ ev]] (chat/input-deltas w ev))]
   [#{:interact :attack} hanging/input-deltas]
   [#{:attack :swing} attacks/input-deltas]
   [#{:interact} interact/input-deltas]])

(def ^:private tags
  (into #{:held-item :edit-book} (mapcat first) handlers))

(defn- handled [w [_ ev :as x]]
  (let [tag (nth ev 0)]
    (into [] (mapcat (fn [[ts f]] (when (contains? ts tag) (f w x))))
          handlers)))

(defn packets
  "Returns the deltas of the packet events of the tick in the order
  they came, each seeing the world after the ones before it."
  {:wake {:events tags}}
  [world d]
  (deltas/of-vec (apply/fold-input world (:input d) handled)))
