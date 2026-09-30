(ns collider.game.systems.packets
  "Packet events that touch only their own player."
  (:require [collider.game.deltas :as deltas]
            [collider.game.apply :as apply]
            [collider.game.player :as player]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.players :as players]))

(set! *warn-on-reflection* true)

(def own-systems
  "The systems whose packet events touch the acting player only."
  [#'player/slot-deltas
   #'inventory/event-deltas
   #'items/event-deltas
   #'players/swing-deltas])

(def ^:private own-tags
  #{:click :menu-click :pick :dig :creative-slot :swing :held-item
    :edit-book :bundle-select})

(defn- own? [ev]
  (and (contains? own-tags (nth ev 0))
       (number? (nth ev 1 nil))))

(defn- grouped [events]
  (let [eid-of (fn [ev] (long (nth ev 1)))]
    (sort-by key (group-by eid-of (filter own? events)))))

(defn- one-deltas [world ev]
  (try (into [] (mapcat (fn [f] (f world ev))) own-systems)
       (catch Throwable t
         (apply/dropped! #'one-deltas ev t)
         (vec (player/slot-part world ev)))))

(defn- fold-deltas [world events]
  (loop [w world evs (seq events) acc []]
    (if-not evs
      acc
      (let [ds (one-deltas w (first evs))
            more (next evs)]
        (recur (if (and more (seq ds)) (apply/entities w ds) w)
               more (into acc ds))))))

(defn by-player
  "Returns the deltas of the events of each player in their order."
  {:wake {:events own-tags}}
  [world d]
  (deltas/fold (fn [[_ events]] (fold-deltas world events))
               (vec (grouped (:input d)))))
