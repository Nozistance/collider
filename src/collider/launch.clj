(ns collider.launch
  "The check for the game data before the server starts."
  (:require [collider.cli :as cli]
            [collider.data :as data]
            [collider.log :as log])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main
  "Starts the server when a full set of tables is ready.
  Stops with exit code 1 when none is."
  [& args]
  (log/to-file! "logs")
  (when-not (data/dir)
    (cli/render! (assoc (ex-data (data/no-tables)) :event :error))
    (System/exit 1))
  (apply (requiring-resolve 'collider.core/-main) args))
