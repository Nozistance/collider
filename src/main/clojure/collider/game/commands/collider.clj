(ns collider.game.commands.collider
  "The collider command, the tools of this server."
  (:require [collider.game.out :as out])
  (:import (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private ^:const ticks-per-second 20)

(defn- told [eid text]
  (out/to eid (out/system-chat {:text text})))

(defn- dump-deltas [world eid [seconds]]
  (let [s (long (or seconds 5))
        file (str "dumps/deltas-" (:tick world) ".edn")]
    [(out/all (out/debug-dump (* ticks-per-second s) file))
     (told eid (str "Dumping the last " s " s of deltas to " file))]))

(defn- fmt [pattern x]
  (String/format Locale/ROOT pattern (to-array [(double (or x 0.0))])))

(defn- tps-deltas [world eid _]
  (if-let [{:keys [tps mspt p50-ms p99-ms deltas-per-tick]} (:perf world)]
    (mapv #(told eid %)
          [(str "TPS " (fmt "%.1f" tps))
           (str "MSPT " (fmt "%.2f" mspt) " ms")
           (str "p50 " (fmt "%.2f" p50-ms) " ms, p99 "
                (fmt "%.2f" p99-ms) " ms")
           (str "deltas a tick " (fmt "%.1f" deltas-per-tick))])
    [(told eid "No timing yet")]))

(def handlers
  {:collider-dump dump-deltas
   :collider-tps tps-deltas})
