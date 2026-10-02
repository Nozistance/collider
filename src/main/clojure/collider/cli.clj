(ns collider.cli
  "Messages to the person who runs the server."
  (:require [clojure.string :as str]
            [collider.log :as log]
            [collider.proto.codec :as c]))

(set! *warn-on-reflection* true)

(def ^:private doing
  {:load "Loading tables"})

(defmulti render!
  "Shows a server start event to the person."
  :event)

(defmethod render! :default [_] nil)

(defmethod render! :begin [{:keys [step]}]
  (log/info (str (doing step) "...")))

(defn- step-done [{:keys [step took]}]
  (case step
    :load (str "Loaded tables " (log/seconds took))))

(defmethod render! :end [m]
  (log/info (step-done m)))

(defmethod render! :host [{:keys [java cores heap config] :as m}]
  (log/info "Starting Collider version" c/game-version)
  (log/info "Java" java (str "(" cores " cores, " heap " heap)"))
  (log/info (if (:config-written? m) "Writing default" "Loading")
            config)
  (log/info (str "Preparing level \"" (:world m) "\""))
  (when-let [n (:chunks m)]
    (log/info "Loading" n "chunks," (:entities m) "entities...")))

(defmethod render! :ready [{:keys [port took]}]
  (log/info "Starting Collider server on" (str "*:" port))
  (log/info "Done" (str (log/seconds took) "!")))

(defn- plugin-name [{:keys [id version]}]
  (str (name id) " " version))

(defmethod render! :plugins [{:keys [loaded]}]
  (when (seq loaded)
    (log/info "Loaded" (count loaded)
              (str (if (next loaded) "plugins" "plugin") ":")
              (str/join ", " (map plugin-name loaded)))))

(def ^:private usage
  ["Usage: collider [run] [{edn settings}...]"
   "       collider <command> [args...]"
   "Commands:"
   "  run      start the server; each edn map overrides config.edn"
   "  help     show this"
   "  version  show the version"
   "  gen      generate a world (not built yet)"
   "  import   import a world from Anvil (not built yet)"
   "  export   export a world to Anvil (not built yet)"])

(defn- plugin-line [[c id]]
  (format "  %-8s from plugin %s" c (name id)))

(defmethod render! :help [{:keys [commands]}]
  (run! log/plain usage)
  (when (seq commands)
    (log/plain "Plugin commands:")
    (run! (comp log/plain plugin-line) (sort commands))))

(defmethod render! :version [{:keys [commit]}]
  (log/plain (str "Collider for Minecraft " c/game-version
                  (when commit (str ", commit " commit)))))

(def ^:private stars (apply str (repeat 43 "*")))

(defn- capital [s]
  (if (seq s) (str (str/upper-case (subs s 0 1)) (subs s 1)) ""))

(defn error-lines
  "Returns the lines of an error: what happened, why, what to do and
  a note for those who look closer."
  [{:keys [what why command note]}]
  (let [why (if (string? why) [why] (vec why))
        one? (= 1 (count why))]
    (cond-> [(cond-> (capital (str what)) one? (str ": " (peek why)))]
      (not one?) (into why)
      command (conj command)
      note (conj (str "Note for advanced users: " note)))))

(defmethod render! :error [m]
  (let [ls (error-lines m)]
    (if (< 3 (count ls))
      (do (log/error stars)
          (run! log/plain ls)
          (log/plain stars))
      (run! log/error ls))))
