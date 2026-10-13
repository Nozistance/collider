(ns collider.game.team
  "Teams of the scoreboard, in the order they were made."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn made
  "Returns a new team of name, shown as display."
  [name display]
  {:name name :display display :prefix "" :suffix "" :color nil
   :friendly-fire true :see-invisible true :nametag :always
   :death-message :always :collision :always :players []})

(defn named
  "Returns the team of teams called name, or nil."
  [teams name]
  (some #(when (= name (:name %)) %) teams))

(defn added
  "Returns teams with a new team of name, shown as display."
  [teams name display]
  (conj (vec teams) (made name display)))

(defn removed
  "Returns teams without the team called name."
  [teams name]
  (into [] (remove #(= name (:name %))) teams))

(defn- styled [display name]
  (let [style {:insertion name :hover {:action :show-text :value name}}]
    (merge (if (map? display) display {:text display}) style)))

(defn shown
  "Returns the name of team t as chat shows it, in square brackets
  and its colour."
  [t]
  (cond-> {:translate "chat.square_brackets"
           :with [(styled (:display t) (:name t))]}
    (:color t) (assoc :color (data/snake (:color t)))))
