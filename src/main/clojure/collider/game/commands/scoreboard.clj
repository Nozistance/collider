(ns collider.game.commands.scoreboard
  "The scoreboard command, which keeps objectives and shows them in
  display slots."
  (:require [collider.game.commands.reply :as reply :refer [fail say]]
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

(def handlers
  {:objective-add add-deltas
   :objective-remove remove-deltas
   :objectives-list list-deltas
   :objective-display display-deltas})
