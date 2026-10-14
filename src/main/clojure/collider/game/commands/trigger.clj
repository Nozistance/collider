(ns collider.game.commands.trigger
  "The trigger command, which a player of any level runs to change its
  own score on a trigger objective once the objective lets it."
  (:require [collider.game.commands.holders :as holders]
            [collider.game.commands.reply :refer [fail say]]
            [collider.game.scoreboard :as scoreboard]))

(set! *warn-on-reflection* true)

(defn- trigger-key [k] (str "commands.trigger." k))

(defn- next-value [kind old v]
  (case kind
    nil (unchecked-int (inc (long old)))
    :add (unchecked-int (+ (long old) (long v)))
    :set v))

(defn- said [eid kind o v]
  (if kind
    (say eid (trigger-key (str (name kind) ".success"))
         (scoreboard/shown o) v)
    (say eid (trigger-key "simple.success") (scoreboard/shown o))))

(defn- scored [sb kind o h old v]
  (let [v' (next-value kind (:value old) v)]
    (scoreboard/triggered sb o (:name h) (:display h) v')))

(defn- triggered [world eid kind o h v]
  (let [sb (:scoreboard world)
        old (scoreboard/score sb (:name o) (:name h))]
    (cond
      (not= "trigger" (:criteria o))
      (fail eid (trigger-key "failed.invalid"))
      (:locked old true) (fail eid (trigger-key "failed.unprimed"))
      :else (concat (scored sb kind o h old v) (said eid kind o v)))))

(defn- trigger-deltas [world eid [kind nm v]]
  (if-let [o (scoreboard/named (:scoreboard world) nm)]
    (let [h (:ok (holders/holder world eid nil))]
      (triggered world eid kind o h v))
    (fail eid "arguments.objective.notFound" nm)))

(def handlers {:trigger trigger-deltas})
