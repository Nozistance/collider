(ns collider.game.scoreboard
  "Objectives of the scoreboard, in the order they were made, and the
  display slots that show them."
  (:require [collider.game.out :as out]
            [collider.game.team :as team]))

(set! *warn-on-reflection* true)

(def slot-names
  "The display slots by id."
  (into ["list" "sidebar" "below_name"]
        (map #(str "sidebar.team." %) team/color-names)))

(defn slot-id
  "Returns the id of the display slot called nm, or nil."
  [nm]
  (let [i (.indexOf ^java.util.List slot-names nm)]
    (when-not (neg? i) i)))

(def ^:private kept
  "The criteria the game sets for each player, with how they show."
  {"health" :hearts "food" :integer "air" :integer "armor" :integer
   "xp" :integer "level" :integer})

(def ^:private written
  (into #{"dummy" "trigger" "deathCount" "playerKillCount"
          "totalKillCount"}
        (mapcat #(vector (str "teamkill." %) (str "killedByTeam." %)))
        team/color-names))

(defn criterion?
  "Returns true when nm names a criterion."
  [nm]
  (or (contains? written nm) (contains? kept nm)))

(defn made
  "Returns a new objective of name and criterion, shown as display."
  [name criterion display]
  {:name name :criteria criterion :display display
   :render (get kept criterion :integer) :auto-update false
   :format nil})

(defn named
  "Returns the objective of sb called nm, or nil."
  [sb nm]
  (some #(when (= nm (:name %)) %) (:objectives sb)))

(defn shown
  "Returns the name of objective o as chat shows it, in square
  brackets, its own name on hover."
  [o]
  (let [d (:display o)]
    {:translate "chat.square_brackets"
     :with [(assoc (if (map? d) d {:text d})
                   :hover {:action :show-text :value (:name o)})]}))

(defn slots-of
  "Returns the ids of the slots of sb that show the objective called
  nm, in order."
  [sb nm]
  (filter #(= nm (get (:slots sb) %)) (range (count slot-names))))

(defn- tracking [sb nm method]
  (into [(out/objective method (named sb nm))]
        (map #(out/display-slot % nm))
        (slots-of sb nm)))

(defn shows
  "Returns sb with slot showing the objective called nm, none when nm
  is nil."
  [sb slot nm]
  (if nm
    (assoc-in sb [:slots slot] nm)
    (update sb :slots dissoc slot)))

(defn- old-fx [sb' slot old nm]
  (if (seq (slots-of sb' old))
    [(out/display-slot slot nm)]
    (tracking sb' old 1)))

(defn display-fx
  "Returns the effects of slot of sb showing the objective called nm
  instead, none when nm is nil."
  [sb slot nm]
  (let [old (get (:slots sb) slot) sb' (shows sb slot nm)]
    (concat (when (and old (not= old nm)) (old-fx sb' slot old nm))
            (when nm
              (if (seq (slots-of sb nm))
                [(out/display-slot slot nm)]
                (tracking sb' nm 0))))))

(defn removal-fx
  "Returns the effects of the objective called nm going from sb."
  [sb nm]
  (let [step (fn [[fx s] i]
               [(into fx (display-fx s i nil)) (shows s i nil)])]
    (first (reduce step [[] sb] (slots-of sb nm)))))

(defn removed
  "Returns sb without the objective called nm."
  [sb nm]
  (-> (reduce #(shows %1 %2 nil) sb (slots-of sb nm))
      (update :objectives #(filterv (fn [o] (not= nm (:name o))) %))))

(defn added
  "Returns sb with objective o last."
  [sb o]
  (update sb :objectives (fnil conj []) o))

(defn joining
  "Returns the effects that show sb to a player who joins. Each
  objective in a slot shows once, in the order of the slots."
  [sb]
  (->> (range (count slot-names))
       (keep #(get (:slots sb) %))
       distinct
       (mapcat #(tracking sb % 0))))
