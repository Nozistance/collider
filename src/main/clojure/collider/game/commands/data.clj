(ns collider.game.commands.data
  "The data command."
  (:require [collider.game.command.nbt-path :as nbt-path]
            [collider.game.command.nbt-text :as nbt-text]
            [collider.game.command.reader :as r]
            [collider.game.command.targets :as targets]
            [collider.game.commands.reply
             :refer [answer entity-name fail say]]
            [collider.game.entity.save-data :as save-data])
  (:import (java.util Locale)))

(set! *warn-on-reflection* true)

(defn- floor
  "Returns d rounded down to an int. Past either end of the int range
  it is the largest int, as the game gives it."
  [^double d]
  (let [i (unchecked-int d)] (if (< d i) (unchecked-dec-int i) i)))

(defn- query [eid e tag]
  (answer (say eid "commands.data.entity.query" (entity-name e)
               (nbt-text/pretty tag))))

(defn- scaled [eid e path scale tag]
  (if (number? tag)
    (answer (say eid "commands.data.entity.get" (:text path) (entity-name e)
                 (String/format Locale/ROOT "%.2f" (object-array [scale]))
                 (floor (* (double tag) (double scale)))))
    (fail eid "commands.data.get.invalid" (:text path))))

(defn- at-path [eid e path scale tag]
  (let [found (nbt-path/tags path tag)]
    (cond (r/error? found) (apply fail eid (:key found) (:args found))
          (next found) (fail eid "commands.data.get.multiple")
          scale (scaled eid e path scale (first found))
          :else (query eid e (first found)))))

(defn- get-entity-deltas [world eid [s path scale]]
  (if-let [[_ _ e] (first (targets/selected world eid s))]
    (let [tag (save-data/saved e (:tick world))]
      (if path (at-path eid e path scale tag) (query eid e tag)))
    (fail eid "argument.entity.notfound.entity")))

(def handlers
  {:data-get-entity get-entity-deltas})
