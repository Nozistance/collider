(ns collider.game.team
  "Teams of the scoreboard, in the order they were made."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def color-names
  "The colours a team takes, by the names commands read."
  ["black" "dark_blue" "dark_green" "dark_aqua" "dark_red" "dark_purple"
   "gold" "gray" "dark_gray" "blue" "green" "aqua" "red" "light_purple"
   "yellow" "white"])

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

(defn of-member
  "Returns the team of teams that member nm is in, or nil."
  [teams nm]
  (some #(when (some #{nm} (:players %)) %) teams))

(defn- without [t nm]
  (update t :players #(into [] (remove #{nm}) %)))

(defn left
  "Returns teams with members nms out of the teams they were in."
  [teams nms]
  (mapv #(reduce without % nms) teams))

(defn joined
  "Returns teams with members nms moved into the team called name."
  [teams name nms]
  (mapv #(if (= name (:name %))
           (update % :players into nms)
           %)
        (left teams nms)))

(defn changed
  "Returns teams with the fields of m set on the team called name."
  [teams name m]
  (mapv #(if (= name (:name %)) (merge % m) %) teams))

(defn- styled [display name]
  (let [style {:insertion name :hover {:action :show-text :value name}}]
    (merge (if (map? display) display {:text display}) style)))

(defn color-name
  "Returns the colour of team t as text styles name it."
  [t]
  (data/snake (:color t)))

(defn shown
  "Returns the name of team t as chat shows it, in square brackets
  and its colour."
  [t]
  (cond-> {:translate "chat.square_brackets"
           :with [(styled (:display t) (:name t))]}
    (:color t) (assoc :color (color-name t))))
