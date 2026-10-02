(ns collider.game.commands.say
  "The commands that talk: say, me, msg, tellraw, list, help and
  version."
  (:require [collider.data :as data]
            [collider.game.command.selector :as sel]
            [collider.game.command.tree :as cmd]
            [collider.game.commands.reply
             :refer [answer entity-name fail name-list say]]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.game.player :as player])
  (:import (java.util Date)))

(set! *warn-on-reflection* true)

(defn- names-of [world eid s]
  (name-list (map #(entity-name (nth % 2))
                  (sel/selected world eid s))))

(defn- message-step [world eid ^String text]
  (fn [[acc at] [a b s]]
    [(cond-> acc
       (< (long at) (long a)) (conj (subs text at a))
       :always (conj (names-of world eid s)))
     b]))

(defn- message-content
  "Returns the text of a message argument with its selectors named."
  [world eid {:keys [^String text parts]}]
  (if (empty? parts)
    text
    (let [s0 (first (first parts))
          step (message-step world eid text)
          [extra end] (reduce step [[] s0] parts)
          tail? (< (long end) (count text))]
      {:text (subs text 0 s0)
       :extra (cond-> extra tail? (conj (subs text end)))})))

(defn- decorated
  "Returns the chat line of chat type id with params by name."
  [id params]
  (let [pack (get (data/pack "chat_type") (str "minecraft:" id))
        {:strs [translation_key parameters style]} (get pack "chat")]
    (merge {:translate translation_key :with (mapv params parameters)}
           (update-keys style data/kebab))))

(defn- sender-name [world eid]
  (entity-name (get-in world [:entities eid])))

(defn- broadcast [id]
  (fn [world eid [m]]
    (let [content (message-content world eid m)
          who (sender-name world eid)
          line (decorated id {"sender" who "content" content})]
      [(out/everyone (out/player-chat line))])))

(defn- whispered [who content [id _ e]]
  (let [to {"target" (entity-name e) "content" content}
        from {"sender" (:name who) "content" content}
        line #(out/player-chat (decorated %1 %2))]
    [(out/to (:eid who) (line "msg_command_outgoing" to))
     (out/to id (line "msg_command_incoming" from))]))

(defn- msg-deltas [world eid [s m]]
  (let [xs (sel/player-selected world eid s)
        who {:eid eid :name (sender-name world eid)}]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapcat #(whispered who (message-content world eid m) %) xs))))

(defn- tellraw-deltas [world eid [s text]]
  (let [xs (sel/player-selected world eid s)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapv (fn [[id]] (out/to id (out/system-chat text))) xs))))

(defn- name-and-id [[id _ e]]
  {:translate "commands.list.nameAndId"
   :with [(:name e) (str (entity/uuid-of id e))]})

(defn- list-deltas [f]
  (fn [world eid _]
    (let [xs (sel/player-entries world)
          most (get-in world [:config :max-players] 20)]
      (answer (say eid "commands.list.players" (count xs) most
                   (name-list (map f xs)))))))

(defn- help-deltas [world eid [text]]
  (let [lv (player/permission-level (get-in world [:entities eid]))
        extra (cmd/extra-of world)
        lines (cmd/help-lines lv text (sel/source-pos world) extra)]
    (if (nil? lines)
      (fail eid "commands.help.failed")
      (answer (mapv #(out/to eid (out/system-chat %)) lines)))))

(defn- version-lines
  "Returns the lines of /version after its header."
  [{:keys [id data series protocol build-time resource-pack data-pack
           stable] :as v}]
  (let [t (fn [k with]
            {:translate (str "commands.version." k) :with with})
        stability (if stable "yes" "no")]
    [(t "id" [id]) (t "name" [(:name v)]) (t "data" [data])
     (t "series" [series])
     (t "protocol" [protocol (str "0x" (Long/toHexString protocol))])
     (t "build_time" [(str (Date. (long build-time)))])
     (t "pack.resource" [resource-pack]) (t "pack.data" [data-pack])
     {:translate (str "commands.version.stable." stability)}]))

(defn- version-deltas [_world eid _]
  (mapv #(out/to eid (out/system-chat %))
        (cons {:translate "commands.version.header"}
              (version-lines (data/version)))))

(def handlers
  "The commands that talk by name."
  {:say (broadcast "say_command") :me (broadcast "emote_command")
   :msg msg-deltas :tellraw tellraw-deltas :help help-deltas
   :list (list-deltas #(entity-name (nth % 2)))
   :list-uuids (list-deltas name-and-id)
   :version version-deltas})
