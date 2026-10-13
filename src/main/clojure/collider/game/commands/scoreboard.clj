(ns collider.game.commands.scoreboard
  "The scoreboard command, which keeps objectives and shows them in
  display slots."
  (:require [collider.game.commands.holders :as holders]
            [collider.game.commands.reply :as reply :refer [fail say]]
            [collider.game.out :as out]
            [collider.game.scoreboard :as scoreboard]))

(set! *warn-on-reflection* true)

(defn- key-of [k] (str "commands.scoreboard.objectives." k))

(defn- known [world eid nm f]
  (if-let [o (scoreboard/named (:scoreboard world) nm)]
    (f o)
    (fail eid "arguments.objective.notFound" nm)))

(defn- add-deltas [world eid [nm criterion display]]
  (if (scoreboard/named (:scoreboard world) nm)
    (fail eid (key-of "add.duplicate"))
    (let [o (scoreboard/made nm criterion (or display nm))]
      (cons [:objective-add o]
            (say eid (key-of "add.success") (scoreboard/shown o))))))

(defn- removal [world eid nm o]
  (concat [[:objective-remove nm]]
          (map out/all (scoreboard/removal-fx (:scoreboard world) nm))
          (say eid (key-of "remove.success") (scoreboard/shown o))))

(defn- remove-deltas [world eid [nm]]
  (known world eid nm #(removal world eid nm %)))

(defn- listed [os]
  (if (empty? os)
    {:translate (key-of "list.empty")}
    {:translate (key-of "list.success")
     :with [(count os) (reply/name-list (map scoreboard/shown os))]}))

(defn- list-deltas [world eid _]
  (let [os (:objectives (:scoreboard world))]
    (reply/answer [(out/to eid (out/system-chat (listed os)))])))

(defn- shown-deltas [world slot nm]
  (let [id (scoreboard/slot-id slot)]
    (cons [:display-slot id nm]
          (map out/all
               (scoreboard/display-fx (:scoreboard world) id nm)))))

(defn- showing [world slot]
  (get-in world [:scoreboard :slots (scoreboard/slot-id slot)]))

(defn- cleared [world eid slot]
  (if (showing world slot)
    (concat (shown-deltas world slot nil)
            (say eid (key-of "display.cleared") slot))
    (fail eid (key-of "display.alreadyEmpty"))))

(defn- set-slot [world eid slot nm o]
  (if (= nm (showing world slot))
    (fail eid (key-of "display.alreadySet"))
    (concat (shown-deltas world slot nm)
            (say eid (key-of "display.set") slot (:display o)))))

(defn- display-deltas [world eid [slot nm]]
  (if (nil? nm)
    (cleared world eid slot)
    (known world eid nm #(set-slot world eid slot nm %))))

(defn- modified
  "Returns the deltas of fields m set on objective o, and the line of
  key with the arguments that fs take of the changed objective."
  [world eid o m key & fs]
  (let [o' (merge o m) nm (:name o)]
    (concat [[:objective-set nm m]]
            (when (scoreboard/tracked? (:scoreboard world) nm)
              [(out/all (out/objective 2 o'))])
            (apply say eid (key-of key) (map #(% o') fs)))))

(defn- set-field [world eid o k v key fs]
  (when-not (= v (get o k))
    (apply modified world eid o {k v} key fs)))

(defn- unless-same
  "Returns the handler that sets field k of an objective to (f v), or
  does nothing when it already is that."
  [k f key-fn & fs]
  (fn [world eid [nm v]]
    (let [v (f v)]
      (known world eid nm
             #(set-field world eid % k v (key-fn v) fs)))))

(defn- auto-key [on?]
  (str "modify.displayAutoUpdate." (if on? "enable" "disable")))

(defn- fixed [v] {:type :fixed :value v})

(defn- styled [v] {:type :styled :value v})

(defn- format-set [world eid o fmt]
  (modified world eid o {:format fmt}
            (str "modify.objectiveFormat." (if fmt "set" "clear"))
            :name))

(defn- formatted [f]
  (fn [world eid [nm v]]
    (known world eid nm #(format-set world eid % (f v)))))

(def ^:private named
  (unless-same :display identity (constantly "modify.displayname")
               :name scoreboard/shown))

(def ^:private rendered
  (unless-same :render keyword (constantly "modify.rendertype")
               scoreboard/shown))

(def ^:private updating
  (unless-same :auto-update identity auto-key :name scoreboard/shown))

(defn- player-key [k] (str "commands.scoreboard.players." k))

(defn- told [eid key & with]
  (out/to eid (out/system-chat {:translate (player-key key)
                                :with (vec with)})))

(defn- named-holders [world eid arg]
  (holders/holders world eid arg (holders/tracked world)))

(defn- writable [world eid nm f]
  (known world eid nm
         #(if (scoreboard/read-only? %)
            (fail eid "arguments.objective.readonly" nm)
            (f %))))

(defn- after [sb nm f v h]
  (let [old (:value (scoreboard/score sb nm (:name h)) 0)]
    (unchecked-int (f old v))))

(defn- scoring
  "Returns the handler that sets each score of the holders to (f old
  v), and tells it by line."
  [f line]
  (fn [world eid [arg nm v]]
    (let [{:keys [ok error]} (named-holders world eid arg)
          sb (:scoreboard world)
          write (fn [o]
                  (let [vs (mapv #(after sb nm f v %) ok)
                        total (unchecked-int (reduce + vs))
                        put #(scoreboard/written sb o (:name %1)
                                                 (:display %1) %2)]
                    (concat (mapcat put ok vs)
                            (line eid o ok v total))))]
      (or error (writable world eid nm write)))))

(defn- set-line [eid o hs v _]
  (if (next hs)
    (say eid (player-key "set.success.multiple") (scoreboard/shown o)
         (count hs) v)
    (say eid (player-key "set.success.single") (scoreboard/shown o)
         (:shown (first hs)) v)))

(defn- change-line [verb]
  (fn [eid o hs v total]
    (if (next hs)
      (say eid (player-key (str verb ".success.multiple")) v
           (scoreboard/shown o) (count hs))
      (say eid (player-key (str verb ".success.single")) v
           (scoreboard/shown o) (:shown (first hs)) total))))

(defn- got [world eid h o]
  (if-let [s (-> (:scoreboard world)
                 (scoreboard/score (:name o) (:name h)))]
    (reply/answer [(told eid "get.success" (:shown h) (:value s)
                         (scoreboard/shown o))])
    (fail eid (player-key "get.null") (:name o) (:shown h))))

(defn- get-deltas [world eid [arg nm]]
  (let [{:keys [ok error]} (holders/holder world eid arg)]
    (or error (known world eid nm #(got world eid ok %)))))

(defn- holders-deltas [world eid _]
  (let [hs (sort (scoreboard/holders (:scoreboard world)))]
    (reply/answer
      [(if (empty? hs)
         (told eid "list.empty")
         (told eid "list.success" (count hs)
               (reply/name-list hs)))])))

(defn- entries [world eid h]
  (for [o (:objectives (:scoreboard world))
        :let [s (scoreboard/score (:scoreboard world) (:name o) h)]
        :when s]
    (told eid "list.entity.entry" (scoreboard/shown o) (:value s))))

(defn- listed-scores [world eid h]
  (let [es (entries world eid (:name h))]
    (reply/answer
      (if (empty? es)
        [(told eid "list.entity.empty" (:shown h))]
        (cons (told eid "list.entity.success" (:shown h) (count es))
              es)))))

(defn- holder-scores [world eid [arg]]
  (let [{:keys [ok error]} (holders/holder world eid arg)]
    (or error (listed-scores world eid ok))))

(defn- reset-line [eid hs k & with]
  (let [one? (= 1 (count hs))
        k (player-key (str k (if one? ".single" ".multiple")))]
    (apply say eid k
           (concat with [(if one? (:shown (first hs)) (count hs))]))))

(defn- resets-deltas [world eid [arg]]
  (let [{:keys [ok error]} (named-holders world eid arg)
        sb (:scoreboard world)]
    (or error
        (concat (mapcat #(scoreboard/reset-all sb (:name %)) ok)
                (reset-line eid ok "reset.all")))))

(defn- reset-deltas [world eid [arg nm]]
  (let [{:keys [ok error]} (named-holders world eid arg)
        sb (:scoreboard world)
        reset #(scoreboard/reset-one sb nm (:name %))]
    (or error
        (known world eid nm
               (fn [o]
                 (concat (mapcat reset ok)
                         (reset-line eid ok "reset.specific"
                                     (scoreboard/shown o))))))))

(def handlers
  {:score-set (scoring (fn [_ v] v) set-line)
   :score-add (scoring + (change-line "add"))
   :score-remove (scoring - (change-line "remove"))
   :score-get get-deltas
   :holders-list holders-deltas
   :holder-scores holder-scores
   :scores-reset resets-deltas
   :score-reset reset-deltas
   :objective-name named
   :objective-render rendered
   :objective-auto-update updating
   :objective-format (formatted (constantly nil))
   :objective-format-blank (formatted (constantly {:type :blank}))
   :objective-format-fixed (formatted fixed)
   :objective-format-styled (formatted styled)
   :objective-add add-deltas
   :objective-remove remove-deltas
   :objectives-list list-deltas
   :objective-display display-deltas})
