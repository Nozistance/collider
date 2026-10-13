(ns collider.game.commands.team
  "The team command, which makes, removes and lists teams."
  (:require [collider.game.command.targets :as targets]
            [collider.game.commands.reply :refer [answer fail name-list say]]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.team :as team])
  (:import (java.util UUID)))

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

(defn- scoreboard-name [[id _ e]]
  (if (= :player (:type e)) (:name e) (str (entity/uuid-of id e))))

(defn- team-name [teams nm e]
  (let [base (entity/display-name e)
        t (team/of-member teams nm)]
    (if-not t
      base
      (merge (cond-> {:text "" :extra [(:prefix t)
                                       (if (= :player (:type e))
                                         (:name e)
                                         (dissoc base :hover :insertion))
                                       (:suffix t)]}
               (:color t) (assoc :color (team/color-name t)))
             (select-keys base [:hover :insertion :click])))))

(defn- of-entity [world x]
  (let [nm (scoreboard-name x)
        shown (team-name (:teams world) nm (nth x 2))]
    {:name nm :shown (if (map? shown)
                       (assoc shown :hover {:action :show-text :value nm})
                       shown)}))

(defn- uuid-of [^String s]
  (try (UUID/fromString s) (catch IllegalArgumentException _ nil)))

(defn- named [world eid ^String s]
  (let [only {:name s :shown s}]
    (cond
      (.startsWith s "#") [only]
      (uuid-of s)
      (if-let [x (targets/by-uuid world false (uuid-of s))]
        [(of-entity world x)] [only])
      :else (if-let [x (first (targets/selected world eid {:name s}))]
              [(of-entity world x)] [only]))))

(defn- holders
  "Returns the members that arg names, or the error deltas."
  [world eid arg]
  (let [hs (cond
             (nil? arg)
             (map #(of-entity world %)
                  (targets/selected world eid {:self? true :entities? true}))
             (:selector arg)
             (map #(of-entity world %)
                  (targets/selected world eid (:selector arg)))
             (= "*" (:name arg)) :none
             :else (named world eid (:name arg)))]
    (cond (= :none hs) {:error (fail eid "argument.scoreHolder.empty")}
          (empty? hs) {:error (fail eid "argument.entity.notfound.entity")}
          :else {:ok (vec hs)})))

(defn- moved [teams t nms]
  (loop [[nm & more] nms ts teams fx []]
    (if-not nm
      fx
      (let [old (team/of-member ts nm)]
        (recur more (team/joined ts (:name t) [nm])
               (cond-> fx
                 old (conj (out/all (out/team 4 old [nm])))
                 :always (conj (out/all (out/team 3 t [nm])))))))))

(defn- reported [eid hs key-base & with]
  (if (= 1 (count hs))
    (apply say eid (str key-base ".single") (:shown (first hs)) with)
    (apply say eid (str key-base ".multiple") (count hs) with)))

(defn- join-deltas [world eid [nm arg]]
  (known world eid nm
         (fn [t]
           (let [{:keys [ok error]} (holders world eid arg)]
             (or error
                 (let [nms (mapv :name ok)]
                   (concat [[:team-join nm nms]]
                           (moved (:teams world) t nms)
                           (reported eid ok "commands.team.join.success"
                                     (team/shown t)))))))))

(defn- left-fx [teams nms]
  (loop [[nm & more] nms ts teams fx []]
    (if-not nm
      fx
      (let [old (team/of-member ts nm)]
        (recur more (team/left ts [nm])
               (cond-> fx old (conj (out/all (out/team 4 old [nm])))))))))

(defn- leave-deltas [world eid [arg]]
  (let [{:keys [ok error]} (holders world eid arg)]
    (or error
        (let [nms (mapv :name ok)]
          (concat [[:team-leave nms]]
                  (left-fx (:teams world) nms)
                  (reported eid ok "commands.team.leave.success"))))))

(defn- empty-deltas [world eid [nm]]
  (known world eid nm
         (fn [t]
           (let [ps (:players t)]
             (if (empty? ps)
               (fail eid "commands.team.empty.unchanged")
               (concat [[:team-leave ps]]
                       (map #(out/all (out/team 4 t [%])) ps)
                       (say eid "commands.team.empty.success" (count ps)
                            (team/shown t))))))))

(def handlers
  {:team-add add-deltas
   :team-remove remove-deltas
   :team-list list-deltas
   :team-members members-deltas
   :team-join join-deltas
   :team-leave leave-deltas
   :team-empty empty-deltas})
