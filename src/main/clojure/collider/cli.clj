(ns collider.cli
  "The command line of the server and its messages to the person who
  runs it."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.config :as config]
            [collider.core :as core]
            [collider.data :as data]
            [collider.log :as log]
            [collider.plugin :as plugin])
  (:import (clojure.lang ExceptionInfo))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private doing
  {:load "Loading tables"})

(defmulti render!
  "Shows an event to the person who runs the server."
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
  (log/info "Starting Collider version" data/game)
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
  (log/plain (str "Collider for Minecraft " data/game
                  (when commit (str ", commit " commit)))))

(def ^:private stars (apply str (repeat 43 "*")))

(defn- capital [s]
  (if (seq s) (str (str/upper-case (subs s 0 1)) (subs s 1)) ""))

(defn error-lines
  "Returns the lines that tell the person about an error. A single
  reason goes on the first line, after what failed."
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

(defn- serve-or-exit! [opts]
  (try (core/start (assoc opts :report render!))
       (catch ExceptionInfo e
         (render! (assoc (ex-data e) :event :error))
         (System/exit 1))))

(defonce ^{:doc "The server -main started, for the REPL."} running
  (atom nil))

(defn- serve! [args]
  (log/to-file! "logs")
  (when-not (data/dir)
    (render! (assoc (ex-data (data/no-tables)) :event :error))
    (System/exit 1))
  (let [written? (config/write-default!)
        opts (apply merge {} (map edn/read-string args))
        server (serve-or-exit! (assoc opts :config-written? written?))
        ^Thread accept (:accept server)]
    (reset! running server)
    (.join accept)))

(defn- failed! [e]
  (render! (assoc (ex-data e) :event :error)))

(defn- not-built [c]
  (ex-info (str c " is not built yet")
           {:what (str "collider " c " is not built yet")
            :why  ["Worlds come from the server itself for now."]}))

(defn- unknown [c]
  (ex-info (str "unknown command " c)
           {:what (str "unknown command " c)
            :why  ["See the commands with: collider help"]
            :exit 2}))

(defn- plugins-dir [opts]
  (:plugins-dir opts plugin/default-dir))

(defn- cli-commands [dir]
  (when (.isDirectory (io/file dir))
    (into {} (for [{m :manifest} (plugin/scan dir), c (keys (:cli m))]
               [c (:id m)]))))

(defn- help [opts]
  (render! {:event :help :commands (cli-commands (plugins-dir opts))})
  0)

(defn- plugin-command [c args opts]
  (when-not (contains? (cli-commands (plugins-dir opts)) c)
    (throw (unknown c)))
  (core/run-cli c args opts))

(defn- version []
  (render! {:event :version :commit (core/build-commit)})
  0)

(defn command
  "Runs the command c with args and returns its exit code.
  The code is 0 on success, 1 on failure and 2 for an unknown command.
  A plugin command runs with the plugins in :plugins-dir of opts,
  without the network and the tick."
  [c args opts]
  (try
    (case c
      ("help" "-h" "--help") (help opts)
      ("version" "--version") (version)
      ("gen" "import" "export") (throw (not-built c))
      (plugin-command c args opts))
    (catch ExceptionInfo e
      (failed! e)
      (:exit (ex-data e) 1))))

(defn- edn-arg? [s] (str/starts-with? (str/triml s) "{"))

(defn -main
  "Starts the server, or runs another command of the command line.
  With no command or run, each argument is an edn map that overrides
  the config."
  [& args]
  (let [[c & more] args]
    (cond (or (nil? c) (edn-arg? c)) (serve! args)
          (= "run" c) (serve! more)
          :else (System/exit (command c more {})))))
