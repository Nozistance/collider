(ns collider.game.commands.team
  "The team command, which makes, removes and lists teams."
  (:require [clojure.string :as str]
            [collider.game.commands.holders :as holders]
            [collider.game.commands.reply
             :refer [answer fail name-list say]]
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
               (out/all (out/team :add (team/made nm nm)))
               (out/all (out/team :change t))]
              (say eid "commands.team.add.success" (team/shown t))))))

(defn- remove-deltas [world eid [nm]]
  (known world eid nm
         (fn [t]
           (concat [[:team-remove nm] (out/all (out/team :remove t))]
                   (say eid "commands.team.remove.success"
                        (team/shown t))))))

(defn- list-deltas [world eid _]
  (let [ts (:teams world)
        names (name-list (map team/shown ts))]
    (told eid (if (empty? ts)
                {:translate "commands.team.list.teams.empty"}
                {:translate "commands.team.list.teams.success"
                 :with [(count ts) names]}))))

(defn- member [nm] {:text nm :color "green"})

(defn- members-msg [t]
  (let [ps (:players t)]
    (if (empty? ps)
      {:translate "commands.team.list.members.empty"
       :with [(team/shown t)]}
      {:translate "commands.team.list.members.success"
       :with [(team/shown t) (count ps)
              (name-list (map member (sort ps)))]})))

(defn- members-deltas [world eid [nm]]
  (known world eid nm #(told eid (members-msg %))))

(defn- told-left [fx ts nm]
  (let [old (team/of-member ts nm)]
    (cond-> fx old (conj (out/all (out/team :leave old [nm]))))))

(defn- moved [teams t nms]
  (let [step (fn [[ts fx] nm]
               [(team/joined ts (:name t) [nm])
                (conj (told-left fx ts nm)
                      (out/all (out/team :join t [nm])))])]
    (second (reduce step [teams []] nms))))

(defn- reported [eid hs key-base & with]
  (if (= 1 (count hs))
    (apply say eid (str key-base ".single") (:shown (first hs)) with)
    (apply say eid (str key-base ".multiple") (count hs) with)))

(defn- join-deltas [world eid [nm arg]]
  (known world eid nm
         (fn [t]
           (let [{:keys [ok error]}
                 (holders/holders world eid arg
                                  (holders/tracked world))]
             (or error
                 (let [nms (mapv :name ok)]
                   (concat [[:team-join nm nms]]
                           (moved (:teams world) t nms)
                           (reported eid ok
                                     "commands.team.join.success"
                                     (team/shown t)))))))))

(defn- left-fx [teams nms]
  (let [step (fn [[ts fx] nm]
               [(team/left ts [nm]) (told-left fx ts nm)])]
    (second (reduce step [teams []] nms))))

(defn- leave-deltas [world eid [arg]]
  (let [{:keys [ok error]} (holders/holders world eid arg
                                  (holders/tracked world))]
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
                       (map #(out/all (out/team :leave t [%])) ps)
                       (say eid "commands.team.empty.success"
                            (count ps) (team/shown t))))))))

(defn- changed [t eid k v ok-key & with]
  (let [t' (assoc t k v)
        shown (map #(% t') with)]
    (concat [[:team-set (:name t) {k v}]
             (out/all (out/team :change t'))]
            (apply say eid ok-key (team/shown t') shown))))

(defn- setter
  "Returns the handler that sets field k of a team to (f value), or
  fails with unchanged-key when it already is that."
  [k f unchanged-key ok-key & with]
  (fn [world eid [nm v]]
    (known world eid nm
           (fn [t]
             (let [v (f v)]
               (if (= v (get t k))
                 (fail eid unchanged-key)
                 (apply changed t eid k v ok-key with)))))))

(defn- switch [k option]
  (fn [world eid [nm v]]
    (known world eid nm
           (fn [t]
             (let [base (str "commands.team.option." option)]
               (if (= v (get t k))
                 (fail eid (str base (if v ".alreadyEnabled"
                                        ".alreadyDisabled")))
                 (changed t eid k v
                          (str base
                               (if v ".enabled" ".disabled")))))))))

(def ^:private rule-names
  {"always" :always "never" :never
   "hideForOtherTeams" :hide-for-other-teams
   "hideForOwnTeam" :hide-for-own-team
   "pushOtherTeams" :push-other-teams "pushOwnTeam" :push-own-team})

(def ^:private rule-texts
  (zipmap (vals rule-names) (keys rule-names)))

(defn- shown-rule [kind k]
  (fn [t] {:translate (str "team." kind "." (rule-texts (get t k)))}))

(defn- color-of [v] (keyword (str/replace v "_" "-")))

(defn- color-reset [world eid [nm]]
  (known world eid nm
         (fn [t]
           (if (nil? (:color t))
             (fail eid "commands.team.option.color.unchanged")
             (changed t eid :color nil
                      "commands.team.option.color.clear.success")))))

(defn- affix [k key]
  (fn [world eid [nm v]]
    (known world eid nm
           (fn [t]
             (concat [[:team-set nm {k v}]
                      (out/all (out/team :change (assoc t k v)))]
                     (told eid {:translate key :with [v]}))))))

(def handlers
  {:team-add add-deltas
   :team-remove remove-deltas
   :team-list list-deltas
   :team-members members-deltas
   :team-join join-deltas
   :team-leave leave-deltas
   :team-empty empty-deltas
   :team-display (setter :display identity
                         "commands.team.option.name.unchanged"
                         "commands.team.option.name.success")
   :team-color (setter :color color-of
                       "commands.team.option.color.unchanged"
                       "commands.team.option.color.success"
                       team/color-name)
   :team-color-reset color-reset
   :team-friendly-fire (switch :friendly-fire "friendlyfire")
   :team-see-invisible (switch :see-invisible "seeFriendlyInvisibles")
   :team-nametag
   (setter :nametag rule-names
           "commands.team.option.nametagVisibility.unchanged"
           "commands.team.option.nametagVisibility.success"
           (shown-rule "visibility" :nametag))
   :team-death-message
   (setter :death-message rule-names
           "commands.team.option.deathMessageVisibility.unchanged"
           "commands.team.option.deathMessageVisibility.success"
           (shown-rule "visibility" :death-message))
   :team-collision
   (setter :collision rule-names
           "commands.team.option.collisionRule.unchanged"
           "commands.team.option.collisionRule.success"
           (shown-rule "collision" :collision))
   :team-prefix (affix :prefix "commands.team.option.prefix.success")
   :team-suffix
   (affix :suffix "commands.team.option.suffix.success")})
