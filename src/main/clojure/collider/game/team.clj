(ns collider.game.team
  "Teams of the scoreboard, in the order they were made."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]))

(set! *warn-on-reflection* true)

(def color-names
  "The colours a team takes, by the names commands read."
  ["black" "dark_blue" "dark_green" "dark_aqua" "dark_red"
   "dark_purple" "gold" "gray" "dark_gray" "blue" "green" "aqua" "red"
   "light_purple" "yellow" "white"])

(def ^:private color-values
  (zipmap color-names
          [0 170 43520 43690 11141120 11141290 16755200 11184810
           5592405 5592575 5635925 5636095 16733525 16733695 16777045
           16777215]))

(def ^:private ^:const black-on-bar -13619152)

(defn waypoint-color
  "Returns the colour the locator bar shows a member of team t in, or
  nil when t has none. Black shows as dark grey."
  [t]
  (when-let [c (:color t)]
    (if (= :black c)
      black-on-bar
      (color-values (data/snake c)))))

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

(defn of-entity
  "Returns the team of teams that entity eid is in, or nil."
  [teams eid e]
  (of-member teams (entity/scoreboard-name eid e)))

(defn allied?
  "Returns true when entity a of teams is in a team and entity b is in
  the same one."
  [teams aid a bid b]
  (let [t (of-entity teams aid a)]
    (boolean (and t (= (:name t) (:name (of-entity teams bid b)))))))

(defn spares?
  "Returns true when player victim of teams takes no harm from player
  attacker, both in one team that keeps friendly fire off."
  [teams victim attacker]
  (and (allied? teams nil victim nil attacker)
       (not (:friendly-fire (of-entity teams nil victim)))))

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
  (let [hover {:action :show-text :value name}
        style {:insertion name :hover hover}]
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
