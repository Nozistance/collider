(ns collider.cli
  "Talking to the person who runs the server."
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

(defmethod render! :error [{:keys [what why command]}]
  (log/error (str "**** " (str/upper-case what) "!"))
  (doseq [l (if (string? why) [why] why)] (log/error l))
  (when command (log/error command)))
