(ns collider.game.commands
  "The commands players run, by name."
  (:require [collider.game.commands.data :as data]
            [collider.game.commands.entity :as entity]
            [collider.game.commands.fill :as fill]
            [collider.game.commands.player :as player]
            [collider.game.commands.reply :as reply]
            [collider.game.commands.say :as say]
            [collider.game.commands.teleport :as teleport]
            [collider.game.commands.world :as world]))

(set! *warn-on-reflection* true)

(def handlers
  "Every command by name, each as (f world eid args)."
  (merge fill/handlers teleport/handlers entity/handlers data/handlers
         player/handlers world/handlers say/handlers))

(defn deltas
  "Returns the deltas of player eid running the parsed command."
  [world eid [tag op & args]]
  (if-let [f (if (identical? :plugin tag) op (handlers op))]
    (f world eid args)
    (reply/tell eid (str "unknown world command: " op))))

(defn author
  "Returns the author of the deltas of command event ev."
  [[_ eid]]
  (cond-> {:with :command} (integer? eid) (assoc :by eid)))
