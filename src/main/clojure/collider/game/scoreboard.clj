(ns collider.game.scoreboard
  "Objectives of the scoreboard, in the order they were made, and the
  display slots that show them."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.out :as out]
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

(def ^:private stat-registries
  {"mined" "block" "crafted" "item" "used" "item" "broken" "item"
   "picked_up" "item" "dropped" "item" "killed" "entity_type"
   "killed_by" "entity_type" "custom" "custom_stat"})

(defn- by-dots
  "Returns the namespace and path of s, a resource location with a dot
  for its colon."
  [^String s]
  (let [i (.indexOf s ".")]
    (cond (neg? i) ["minecraft" s]
          (zero? i) ["minecraft" (subs s 1)]
          :else [(subs s 0 i) (subs s (inc i))])))

(defn- entry [registry s]
  (let [[ns path] (by-dots s)
        k (keyword (str/replace path \_ \-))]
    (when (and (= "minecraft" ns) (not (str/includes? path "-"))
               (contains? (get (data/registries) registry) k))
      path)))

(defn- stat [^String nm]
  (when-let [i (str/index-of nm ":")]
    (let [t (entry "stat_type" (subs nm 0 i))
          v (when t (entry (stat-registries t) (subs nm (inc i))))]
      (when v (str "minecraft." t ":minecraft." v)))))

(defn criterion
  "Returns the name of the criterion nm names, or nil. A statistic
  goes by its full name."
  [nm]
  (if (or (contains? written nm) (contains? kept nm)) nm (stat nm)))

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

(defn changed
  "Returns sb with the fields of m set on the objective called nm."
  [sb nm m]
  (update sb :objectives
          (fn [os] (mapv #(if (= nm (:name %)) (merge % m) %) os))))

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
