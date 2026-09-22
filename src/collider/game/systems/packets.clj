(ns collider.game.systems.packets
  "The packet events a player alone can reach, one job per player."
  (:require [collider.game.state :as state]
            [collider.game.systems.inventory :as inventory]
            [collider.game.systems.items :as items]
            [collider.game.systems.players :as players]))

(set! *warn-on-reflection* true)

(def own-systems
  "The systems whose packet events touch the acting player only."
  [#'inventory/event-deltas
   #'items/event-deltas
   #'players/swing-deltas])

(def ^:private own-tags
  #{:click :menu-click :pick :dig :creative-slot :swing})

(defn- own? [ev]
  (and (contains? own-tags (nth ev 0))
       (number? (nth ev 1 nil))))

(defn- grouped [events]
  (let [eid-of (fn [ev] (long (nth ev 1)))]
    (sort-by key (group-by eid-of (filter own? events)))))

(defn- one-deltas [world ev]
  (into [] (mapcat (fn [f] (f world ev))) own-systems))

(defn- fold-deltas [world events]
  (loop [w world evs (seq events) acc []]
    (if-not evs
      acc
      (let [ds (one-deltas w (first evs))
            more (next evs)]
        (recur (if (and more (seq ds)) (state/apply-entities w ds) w)
               more (into acc ds))))))

(defn by-player
  "Returns one job per player, his events in the order they came."
  [world d]
  (mapv (fn [[_ events]] #(fold-deltas world events))
        (grouped (:input d))))
