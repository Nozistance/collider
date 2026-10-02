(ns longmap.runner
  (:require [clojure.test :as t]
            [longmap.core-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'longmap.core-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
