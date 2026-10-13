(ns collider.game.commands.holders
  "The holders of scores and team places that commands name."
  (:require [collider.game.command.targets :as targets]
            [collider.game.commands.reply :refer [fail]]
            [collider.game.entity :as entity]
            [collider.game.scoreboard :as scoreboard]
            [collider.game.team :as team])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- of-entity [world [id _ e]]
  (let [nm (entity/scoreboard-name id e)
        t (team/of-entity (:teams world) id e)
        d (entity/display-name id e t)]
    {:name nm :display d
     :shown (assoc d :hover {:action :show-text :value nm})}))

(defn name-only
  "Returns the holder that is a name alone."
  [s]
  {:name s :shown s})

(defn- uuid-of [^String s]
  (try (UUID/fromString s) (catch IllegalArgumentException _ nil)))

(defn- named [world eid ^String s]
  (let [only (name-only s)]
    (cond
      (.startsWith s "#") [only]
      (uuid-of s)
      (if-let [x (targets/by-uuid world false (uuid-of s))]
        [(of-entity world x)] [only])
      :else (if-let [x (first (targets/selected world eid {:name s}))]
              [(of-entity world x)] [only]))))

(defn- selected [world eid sel]
  (map #(of-entity world %) (targets/selected world eid sel)))

(def ^:private self {:self? true :entities? true})

(defn- failed [eid k] {:error (fail eid k)})

(defn tracked
  "Returns the holders of the scores of world, which * names."
  [world]
  (->> (scoreboard/holders (:scoreboard world))
       sort
       (map name-only)))

(defn holders
  "Returns the holders that arg names, or the error deltas. The
  wildcard names the holders in wildcard, none when it is empty."
  [world eid arg wildcard]
  (let [hs (cond
             (nil? arg) (selected world eid self)
             (:selector arg) (selected world eid (:selector arg))
             (= "*" (:name arg)) (or (seq wildcard) :none)
             :else (named world eid (:name arg)))]
    (cond (= :none hs) (failed eid "argument.scoreHolder.empty")
          (empty? hs) (failed eid "argument.entity.notfound.entity")
          :else {:ok (vec hs)})))

(defn holder
  "Returns the first holder that arg names, or the error deltas."
  [world eid arg]
  (let [{:keys [ok] :as r} (holders world eid arg nil)]
    (if (:error r) r {:ok (first ok)})))
