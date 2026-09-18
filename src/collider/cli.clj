(ns collider.cli
  "Talking to the person who runs the server."
  (:require [clojure.string :as str]
            [collider.log :as log]
            [collider.proto.codec :as c]))

(set! *warn-on-reflection* true)

(def ^:private doing
  {:jar     "Fetching server.jar"
   :reports "Generating reports"
   :tables  "Generating tables"
   :load    "Loading tables"})

(defmulti render!
  "Shows an event of the first start or of the server start to
  the person."
  :event)

(defmethod render! :default [_] nil)

(defmethod render! :intro [{:keys [version stale?]}]
  (when stale? (log/info "Game data in data is incomplete or from another version, regenerating"))
  (log/info "Preparing game data for version" version))

(defmethod render! :begin [{:keys [step]}]
  (log/info (str (doing step) "...")))

(defn- jar-done [{:keys [source bytes path took]}]
  (case source
    :mojang (str "Downloaded server.jar, " (log/human-bytes bytes) " " (log/seconds took))
    :cached "Using cached server.jar"
    :local (str "Using server.jar from " path)))

(defn- step-done [{:keys [step files count dir took] :as m}]
  (case step
    :jar (jar-done m)
    :reports (str "Generated " files " reports " (log/seconds took))
    :tables (str "Generated " count " tables into " dir " " (log/seconds took))
    :load (str "Loaded tables " (log/seconds took))))

(defmethod render! :end [m]
  (log/info (step-done m)))

(defmethod render! :host [{:keys [java cores heap config config-written? world chunks entities]}]
  (log/info "Starting Collider version" c/game-version)
  (log/info "Java" java (str "(" cores " cores, " heap " heap)"))
  (log/info (if config-written? "Writing default" "Loading") config)
  (log/info (str "Preparing level \"" world "\""))
  (when chunks (log/info "Loading" chunks "chunks," entities "entities...")))

(defmethod render! :ready [{:keys [port took]}]
  (log/info "Starting Collider server on" (str "*:" port))
  (log/info "Done" (str (log/seconds took) "!")))

(defmethod render! :error [{:keys [what why command]}]
  (log/error (str "**** " (str/upper-case what) "!"))
  (doseq [l (if (string? why) [why] why)] (log/error l))
  (when command (log/error command)))
