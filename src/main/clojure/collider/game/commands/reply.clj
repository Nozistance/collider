(ns collider.game.commands.reply
  "The replies of commands and the names they show."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.markup :as markup]
            [collider.game.out :as out]))

(set! *warn-on-reflection* true)

(defn- joined [runs]
  (case (count runs)
    0 ""
    1 (first runs)
    {:text "" :extra runs}))

(defn tell
  "Returns the deltas that show the lines to one player."
  [eid & lines]
  (mapv #(out/to eid (out/system-chat (joined (markup/parse-runs %))))
        (mapcat #(str/split-lines (str %)) lines)))

(defn- reports [ds admins?]
  (mapv (fn [d]
          (if (= :fx (nth d 0))
            [:fx (assoc (nth d 1) :feedback true :admins admins?)]
            d))
        ds))

(defn success
  "Returns deltas ds marked as feedback that admins also see."
  [ds]
  (reports ds true))

(defn answer
  "Returns deltas ds marked as feedback for the sender alone."
  [ds]
  (reports ds false))

(defn say*
  "Returns the success line of key with the arguments in with."
  [eid key with]
  (let [msg {:translate key :with (vec with)}]
    (success [(out/to eid (out/system-chat msg))])))

(defn say
  "Returns the success line of key with the arguments to eid."
  [eid key & with]
  (say* eid key with))

(defn failure
  "Returns the red chat line of text."
  [text]
  (out/system-chat {:text "" :color "red" :extra [text]}))

(defn fail
  "Returns the failure line of key with the arguments to eid."
  [eid key & with]
  [(out/to eid (failure {:translate key :with (vec with)}))])

(defn entity-name
  "Returns the name of entity e as chat shows it."
  [e]
  (let [k (str "entity.minecraft." (data/snake (:type e)))
        nm (if (= :player (:type e)) (:name e) {:translate k})
        hover {:action :show-entity :id (:type e) :uuid (:uuid e)
               :name nm}]
    (cond
      (nil? (:uuid e)) nm
      (= :player (:type e))
      {:text nm :insertion nm :hover hover
       :click {:action :suggest-command
               :command (str "/tell " nm " ")}}
      :else (assoc nm :hover hover :insertion (str (:uuid e))))))

(def ^:private separator {:text ", " :color "gray"})

(defn name-list
  "Returns the names as one line, split by commas."
  [names]
  (case (count names)
    0 ""
    1 (first names)
    {:text "" :extra (vec (interpose separator names))}))

(defn dimension-id
  [dim]
  (str "minecraft:" (data/snake dim)))
