(ns collider.game.commands.team
  "The team command, which makes, removes and lists teams."
  (:require [collider.game.commands.reply :refer [answer fail name-list say]]
            [collider.game.out :as out]
            [collider.game.team :as team]))

(set! *warn-on-reflection* true)

(defn- known [world eid nm f]
  (if-let [t (team/named (:teams world) nm)]
    (f t)
    (fail eid "team.notFound" nm)))

(defn- told [eid msg]
  (answer [(out/to eid (out/system-chat msg))]))

(defn- add-deltas [world eid [nm display]]
  (if (team/named (:teams world) nm)
    (fail eid "commands.team.add.duplicate")
    (let [shown (or display nm) t (team/made nm shown)]
      (concat [[:team-add nm shown]
               (out/all (out/team 0 (team/made nm nm)))
               (out/all (out/team 2 t))]
              (say eid "commands.team.add.success" (team/shown t))))))

(defn- remove-deltas [world eid [nm]]
  (known world eid nm
         (fn [t]
           (concat [[:team-remove nm] (out/all (out/team 1 t))]
                   (say eid "commands.team.remove.success"
                        (team/shown t))))))

(defn- list-deltas [world eid _]
  (let [ts (:teams world)]
    (told eid (if (empty? ts)
                {:translate "commands.team.list.teams.empty"}
                {:translate "commands.team.list.teams.success"
                 :with [(count ts) (name-list (map team/shown ts))]}))))

(defn- member [nm] {:text nm :color "green"})

(defn- members-deltas [world eid [nm]]
  (known world eid nm
         (fn [t]
           (let [ps (:players t)]
             (told eid
                   (if (empty? ps)
                     {:translate "commands.team.list.members.empty"
                      :with [(team/shown t)]}
                     {:translate "commands.team.list.members.success"
                      :with [(team/shown t) (count ps)
                             (name-list (map member (sort ps)))]}))))))

(def handlers
  {:team-add add-deltas
   :team-remove remove-deltas
   :team-list list-deltas
   :team-members members-deltas})
