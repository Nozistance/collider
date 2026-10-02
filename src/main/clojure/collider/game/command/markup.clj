(ns collider.game.command.markup
  "Chat markup: bold, italic, underlined, struck and code runs."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private markers
  [["***" {:bold true :italic true}]
   ["**" {:bold true}]
   ["__" {:underlined true}]
   ["~~" {:strikethrough true}]
   ["*" {:italic true}]
   ["_" {:italic true}]
   ["`" {:color "gray"}]])

(declare parse-runs)

(defn- starts-at? [s m ^long i]
  (let [end (+ i (count m))]
    (and (<= end (count s)) (= m (subs s i end)))))

(defn- marker-at [s ^long i]
  (some (fn [[m st]] (when (starts-at? s m i) [m st])) markers))

(defn- marked [s m st styles start close]
  (let [inner (subs s start close)]
    (if (= m "`")
      [(merge styles st {:text inner})]
      (parse-runs inner (merge styles st)))))

(defn- span [s styles plain ^long i]
  (when-let [[m st] (marker-at s i)]
    (let [start (+ i (count m))
          close (str/index-of s m start)]
      (if (and close (> (long close) start))
        [(+ (long close) (count m))
         (marked s m st styles start close) plain]
        [start nil (str plain m)]))))

(defn- flushed [out styles plain]
  (if (pos? (count plain))
    (conj out (assoc styles :text plain))
    out))

(defn- escape-at? [s ^long i]
  (and (= \\ (nth s i))
       (< (inc i) (count s))
       (or (marker-at s (inc i))
           (= \\ (nth s (inc i))))))

(defn- step [s styles [^long i plain out]]
  (if (escape-at? s i)
    [(+ 2 i) (str plain (nth s (inc i))) out]
    (if-let [[end runs plain'] (span s styles plain i)]
      (if runs
        [end "" (into (flushed out styles plain) runs)]
        [end plain' out])
      [(inc i) (str plain (nth s i)) out])))

(defn parse-runs
  "Returns the styled runs of a line with chat markup."
  ([s] (parse-runs s {}))
  ([s styles]
   (loop [[i plain out :as st] [0 "" []]]
     (if (>= (long i) (count s))
       (flushed out styles plain)
       (recur (step s styles st))))))
