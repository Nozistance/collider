(ns collider.game.systems.chat
  "Chat lines, commands and tab completion."
  (:require [clojure.string :as str]
            [collider.game.command.forms :as forms]
            [collider.game.command.tree :as cmd]
            [collider.game.commands :as commands]
            [collider.game.commands.config :as config]
            [collider.game.commands.player :as player-commands]
            [collider.game.commands.reply :as reply]
            [collider.game.commands.teleport :as teleport]
            [collider.game.delta :as delta]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- sourced [world r origin]
  (let [pos (if (contains? r :origin) (:origin r) origin)]
    (assoc world :source
           {:dim (:dim r (:dim world)) :pos pos
            :relative (:relative r #{})})))

(defn- origin-of [world eid]
  (when-let [p (get-in world [:entities eid :pos])]
    [(v/x p) (v/y p) (v/z p)]))

(def ^:private here
  {:translate "command.context.here" :color "red" :italic true})

(defn- context [^String s at]
  (let [c (min (long at) (count s))]
    {:text "" :color "gray"
     :click {:action :suggest-command :command (str "/" s)}
     :extra (cond-> (if (> c 10) ["..."] [])
              :always (conj (subs s (max 0 (- c 10)) c))
              (< c (count s))
              (conj {:text (subs s c) :color "red"
                     :underlined true})
              :always (conj here))}))

(defn- parse-failed [eid text r]
  (let [at (:cursor r)
        failed #(out/to eid (reply/failure %))]
    (cond-> [(failed (:failure r))]
      at (conj (failed (context (subs text 1) at))))))

(defn- feedback? [world ds]
  (reduce (fn [on d]
            (if (= [:set-rule :send-command-feedback] (take 2 d))
              (nth d 2)
              on))
          (get-in world [:rules :send-command-feedback] true) ds))

(defn- admin-report [world eid m]
  (when-let [e (get-in world [:entities eid])]
    (let [who (reply/entity-name world [eid nil e])]
      (out/except eid (out/system-chat
                        {:translate "chat.type.admin"
                         :with [who (:text m)]
                         :color "gray" :italic true})))))

(defn- report-deltas [world eid on? d]
  (let [m (when (= :fx (nth d 0)) (nth d 1))]
    (cond (not (:feedback m)) [d]
          (not on?) nil
          :else (cond-> [[:fx (dissoc m :feedback :admins)]]
                  (:admins m) (conj (admin-report world eid m))))))

(defn- reported [world eid ds]
  (let [on? (feedback? world ds)]
    (into [] (comp (mapcat #(report-deltas world eid on? %))
                   (remove nil?))
          ds)))

(defn- gamemaster? [world eid]
  (when-let [e (get-in world [:entities eid])]
    (<= (long forms/gamemaster) (player/permission-level e))))

(defn- permission [world eid]
  (player/permission-level (get-in world [:entities eid])))

(defn- parsed [world eid text origin]
  (cmd/parse text origin (:dim world :overworld)
             (permission world eid) [0.0 0.0] (forms/extra-of world)))

(defn- command-deltas [world eid text]
  (let [origin (origin-of world eid)
        r (parsed world eid text origin)]
    (cond
      (:failure r) (parse-failed eid text r)
      (:error r) (reply/tell eid (:error r))
      :else (let [w (sourced world r origin)
                  ds (commands/deltas w eid (:delta r))]
              (reported w eid ds)))))

(defn- public-deltas [world eid text]
  (when-let [e (get-in world [:entities eid])]
    (let [who (reply/entity-name world [eid nil e])]
      [(out/all (out/player-chat
                  {:translate "chat.type.text" :with [who text]}))])))

(def ^:private chat-off
  (assoc (out/system-chat {:translate "chat.disabled.options" :color "red"})
         :always? true))

(defn- muted? [world eid]
  (= :hidden (get-in world [:entities eid :chat-visibility])))

(defn- said-deltas [world eid raw]
  (let [text (str/trim (str raw))]
    (cond
      (str/blank? text) nil
      (str/starts-with? text "/") (command-deltas world eid text)
      (muted? world eid) [(out/to eid chat-off)]
      :else (public-deltas world eid text))))

(defn- tab-deltas [world eid text id]
  (let [lv (permission world eid)
        w (sourced world {} (origin-of world eid))
        {:keys [start texts tips]} (cmd/suggestions w text lv eid)
        [start len] (if (seq texts)
                      [start (- (count text) (long start))]
                      [0 0])]
    [(out/to eid (out/suggestions (or id 0) start len texts tips))]))

(defn- mode-changed [world eid mode]
  (when (gamemaster? world eid)
    (let [x [eid (:dim world) (get-in world [:entities eid])]]
      (reported world eid
                (player-commands/mode-set world eid mode x)))))

(defn- event-deltas [world [tag eid text _ id]]
  (case tag
    :chat (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text id)
    :change-game-mode (mode-changed world eid text)
    :teleport-to-entity (teleport/spectator-teleport world eid text)
    nil))

(defn input-deltas
  "Returns the deltas of one chat, command or config event."
  [world ev]
  (let [ds (concat (event-deltas world ev)
                   (config/event-deltas world ev))]
    (delta/authored (vec ds) (commands/author ev))))

(def tags
  #{:chat :tab-complete :change-game-mode :teleport-to-entity
    :rules-request :set-rules :config-loaded :config-failed
    :commit-synced :hooks-set :config-shown :config-written
    :config-refused})
