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

(def operations
  "The operators of scoreboard players operation."
  #{"=" "+=" "-=" "*=" "/=" "%=" "<" ">" "><"})

(def ^:private slot-ids (zipmap slot-names (range)))

(defn slot-id
  "Returns the id of the display slot called nm, or nil."
  [nm]
  (slot-ids nm))

(def ^:private kept
  "The criteria the game sets for each player, with how they show."
  {"health" :hearts "food" :integer "air" :integer "armor" :integer
   "xp" :integer "level" :integer})

(def ^:private writable
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
  (if (or (contains? writable nm) (contains? kept nm)) nm (stat nm)))

(defn read-only?
  "Returns true when the game alone sets the scores of objective o."
  [o]
  (contains? kept (:criteria o)))

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

(defn tracked?
  "Returns true when a slot of sb shows the objective called nm."
  [sb nm]
  (boolean (seq (slots-of sb nm))))

(defn- score-fx [sb nm]
  (map (fn [[h s]] (out/score h nm s)) (get-in sb [:scores nm])))

(defn- tracking [sb nm method]
  (concat [(out/objective method (named sb nm))]
          (map #(out/display-slot % nm) (slots-of sb nm))
          (when (= :add method) (score-fx sb nm))))

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
    (tracking sb' old :remove)))

(defn display-fx
  "Returns the effects of slot of sb showing the objective called nm
  instead, none when nm is nil."
  [sb slot nm]
  (let [old (get (:slots sb) slot) sb' (shows sb slot nm)]
    (concat (when (and old (not= old nm)) (old-fx sb' slot old nm))
            (when nm
              (if (seq (slots-of sb nm))
                [(out/display-slot slot nm)]
                (tracking sb' nm :add))))))

(defn removal-fx
  "Returns the effects of the objective called nm going from sb."
  [sb nm]
  (let [step (fn [[fx s] i]
               [(into fx (display-fx s i nil)) (shows s i nil)])]
    (first (reduce step [[] sb] (slots-of sb nm)))))

(defn removed
  "Returns sb without the objective called nm. The holders of its
  scores stay."
  [sb nm]
  (-> (reduce #(shows %1 %2 nil) sb (slots-of sb nm))
      (update :objectives #(filterv (fn [o] (not= nm (:name o))) %))
      (update :scores dissoc nm)))

(defn score
  "Returns the score of holder h on the objective called nm, or nil."
  [sb nm h]
  (get-in sb [:scores nm h]))

(defn holders
  "Returns the names that hold scores of sb, or held them since."
  [sb]
  (:holders sb #{}))

(defn scored
  "Returns sb with score s of holder h on the objective called nm."
  [sb nm h s]
  (-> (assoc-in sb [:scores nm h] s)
      (update :holders (fnil conj #{}) h)))

(defn- of-holder [sb h]
  (keep (fn [[nm ss]] (when (contains? ss h) nm)) (:scores sb)))

(defn unscored
  "Returns sb without the score of holder h on the objective called
  nm."
  [sb nm h]
  (update-in sb [:scores nm] dissoc h))

(defn forgotten
  "Returns sb without holder h and its scores."
  [sb h]
  (-> (update sb :scores update-vals #(dissoc % h))
      (update :holders (fnil disj #{}) h)))

(defn stored
  "Returns sb as a save keeps it, without the holders of no score."
  [sb _]
  (cond-> sb
    (contains? sb :holders)
    (assoc :holders (set (mapcat keys (vals (:scores sb)))))))

(defn- new-score [] {:value 0 :locked true})

(defn- put [sb nm h s]
  (cons [:score-set nm h s]
        (when (tracked? sb nm) [(out/all (out/score h nm s))])))

(defn created
  "Returns the deltas of holder h getting a new score on the objective
  called nm, which no player sees yet, none when it has one."
  [sb nm h]
  (when-not (score sb nm h) [[:score-set nm h (new-score)]]))

(defn applied
  "Returns sb after the scores that deltas ds set."
  [sb ds]
  (reduce (fn [sb [k nm h s]]
            (if (= :score-set k) (scored sb nm h s) sb))
          sb ds))

(defn- shown-by [o display old]
  (when (and (:auto-update o) display (not= display (:display old)))
    display))

(defn written
  "Returns the deltas of the score of holder h on objective o set to
  v. An objective that updates the display takes display, the name
  h shows by, when there is one. A fresh score, new and not yet
  sent, is sent whatever it holds."
  ([sb o h display v]
   (written sb o h display v (nil? (score sb (:name o) h))))
  ([sb o h display v fresh?]
   (let [nm (:name o) old (or (score sb nm h) (new-score))
         shown (shown-by o display old)
         s (cond-> (assoc old :value v) shown (assoc :display shown))]
     (when (or fresh? shown (not= v (:value old)))
       (put sb nm h s)))))

(defn unlocked
  "Returns the deltas of the score of holder h on the objective called
  nm unlocked, none when it is not locked."
  [sb nm h]
  (let [old (score sb nm h)]
    (when (:locked old true)
      (let [s (assoc (or old (new-score)) :locked false)]
        (if old [[:score-set nm h s]] (put sb nm h s))))))

(defn triggered
  "Returns the deltas of the score of holder h on objective o locked
  and set to v, as written takes display."
  [sb o h display v]
  (let [nm (:name o) s (assoc (score sb nm h) :locked true)]
    (cons [:score-set nm h s]
          (written (scored sb nm h s) o h display v false))))

(defn renamed
  "Returns the deltas of the score of holder h on the objective called
  nm shown by name d, none when it already is."
  [sb nm h d]
  (let [old (score sb nm h)]
    (when (or (nil? old) (not= d (:display old)))
      (put sb nm h (assoc (or old (new-score)) :display d)))))

(defn reformatted
  "Returns the deltas of the score of holder h on the objective called
  nm shown in number format f."
  [sb nm h f]
  (put sb nm h (assoc (or (score sb nm h) (new-score)) :format f)))

(defn reset-all
  "Returns the deltas of holder h of sb losing all its scores."
  [sb h]
  (when (contains? (holders sb) h)
    [[:holder-reset h] (out/all (out/score-reset h nil))]))

(defn reset-one
  "Returns the deltas of holder h of sb losing its score on the
  objective called nm."
  [sb nm h]
  (let [had? (some? (score sb nm h))
        left (remove #{nm} (of-holder sb h))]
    (cond (and (contains? (holders sb) h) (empty? left))
          (reset-all sb h)
          (not had?) nil
          :else (cons [:score-reset nm h]
                      (when (tracked? sb nm)
                        [(out/all (out/score-reset h nm))])))))

(defn changed
  "Returns sb with the fields of m set on the objective called nm."
  [sb nm m]
  (update sb :objectives
          (fn [os] (mapv #(if (= nm (:name %)) (merge % m) %) os))))

(defn added
  "Returns sb with objective o last."
  [sb o]
  (update sb :objectives (fnil conj []) o))

(defn- triggers [sb]
  (filter #(= "trigger" (:criteria %)) (:objectives sb)))

(defn triggerable
  "Returns the names of the trigger objectives of sb that holder h may
  trigger now."
  [sb h]
  (for [o (triggers sb)
        :let [s (score sb (:name o) h)]
        :when (and s (not (:locked s)))]
    (:name o)))

(defn enableable
  "Returns the names of the trigger objectives of sb that one of the
  holders hs has no open score on."
  [sb hs]
  (for [o (triggers sb)
        :when (some #(:locked (score sb (:name o) %) true) hs)]
    (:name o)))

(defn joining
  "Returns the effects that show sb to a player who joins. Each
  objective in a slot shows once, in the order of the slots."
  [sb]
  (->> (range (count slot-names))
       (keep #(get (:slots sb) %))
       distinct
       (mapcat #(tracking sb % :add))))
