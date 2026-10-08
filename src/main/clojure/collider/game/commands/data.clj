(ns collider.game.commands.data
  "The data command."
  (:require [collider.game.command.nbt-text :as nbt-text]
            [collider.game.command.selector :as sel]
            [collider.game.commands.reply
             :refer [answer entity-name fail say]]
            [collider.game.entity.save-data :as save-data]))

(set! *warn-on-reflection* true)

(defn- get-entity-deltas [world eid [s]]
  (if-let [[_ _ e] (first (sel/selected world eid s))]
    (answer (say eid "commands.data.entity.query" (entity-name e)
                 (nbt-text/pretty (save-data/saved e (:tick world)))))
    (fail eid "argument.entity.notfound.entity")))

(def handlers
  {:data-get-entity get-entity-deltas})
