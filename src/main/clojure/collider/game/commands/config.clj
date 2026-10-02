(ns collider.game.commands.config
  "Config reloads and the game rules players ask for and set."
  (:require [collider.game.commands.reply :as reply]
            [collider.game.commands.world :as world-commands]
            [collider.game.gamerules :as rules]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn- set-rule-delta [world eid [k v]]
  (when-let [r (rules/rule-of k)]
    (world-commands/rule-deltas world eid r v)))

(defn- rules-event-deltas [world [tag eid entries]]
  (case tag
    :rules-request [(out/to eid (out/game-rules (:rules world)))]
    :set-rules (mapcat #(set-rule-delta world eid %) entries)
    nil))

(defn- distance-fx [old new k fx]
  (let [n (get new k)]
    (when (not= (get old k) n) [(out/everyone (fx n))])))

(defn- config-loaded [world m]
  (let [old (:config world)]
    (concat [[:set-config (merge old m)]
             (out/everyone (out/reloaded))]
            (distance-fx old m :view-distance out/view-distance)
            (distance-fx old m :simulation-distance
              out/simulation-distance))))

(defn- commit-synced
  [world [_ commit]]
  [[:set-config (assoc (:config world) :commit commit)]])

(defn- config-event-deltas [world [tag eid m :as ev]]
  (case tag
    :config-loaded (config-loaded world m)
    :config-failed (reply/fail eid "commands.reload.failure")
    :commit-synced (commit-synced world ev)
    nil))

(defn event-deltas
  "Returns the deltas of a config reload or game rule event ev."
  [world ev]
  (concat (rules-event-deltas world ev)
          (config-event-deltas world ev)))
