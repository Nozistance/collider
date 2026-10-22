(ns collider.game.command.reader
  "Command text read at a cursor."
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (java.util.regex Matcher)))

(set! *warn-on-reflection* true)

(defn reader
  ([s] [s 0])
  ([s n] [s n]))

(defn can-read?
  ([rd] (can-read? rd 1))
  ([[s n] k] (<= (+ n k) (count s))))

(defn remaining [[s n]] (subs s n))

(defn- skip-while [pred [s n]]
  (let [len (count s)]
    (loop [i n]
      (if (and (< i len) (pred (nth s i)))
        (recur (inc i))
        [s i]))))

(defn- whitespace? [c] (Character/isWhitespace (char c)))

(defn skip-whitespace [rd] (skip-while whitespace? rd))

(defn at?
  [[s n :as rd] c]
  (and (can-read? rd) (= c (nth s n))))

(defn skip
  "Returns rd with the cursor one character on."
  [[s n]]
  [s (inc n)])

(defn error [k & args]
  {:key k :args (vec args) :cursor -1 :input nil})

(defn error-at [[s n] k & args]
  {:key k :args (vec args) :cursor n :input s})

(defn error? [x]
  (and (map? x) (contains? x :key) (contains? x :cursor)))

(def ^:private format-spec #"%(?:(\d+)\$)?([A-Za-z%]|$)")

(defn- plain? [^String s] (neg? (.indexOf s "%")))

(defn- arg-text [x] (if (nil? x) "null" (str x)))

(defn- filled
  "Returns template t with args in its places, or nil when they
  do not fit."
  [^String t args]
  (let [^Matcher m (re-matcher format-spec t)]
    (loop [out "" at 0 n 0]
      (if (.find m (int at))
        (let [pre (subs t at (.start m))
              pos (.group m 1)
              i (if pos (dec (parse-long pos)) n)]
          (when (plain? pre)
            (cond
              (= "%%" (.group m)) (recur (str out pre "%") (.end m) n)
              (and (= "s" (.group m 2)) (< -1 i (count args)))
              (recur (str out pre (arg-text (nth args i))) (.end m)
                     (if pos n (inc n))))))
        (let [tail (subs t at)]
          (when (plain? tail) (str out tail)))))))

(defn message
  "Returns the text of error e and the input up to its cursor, as its
  exception tells them."
  [{:keys [key args cursor input]}]
  (let [t (get (data/lang) key key)
        text (or (filled t args) t)
        c (min (count input) (long cursor))]
    (if (or (nil? input) (neg? (long cursor)))
      text
      (str text " at position " cursor ": " (when (> c 10) "...")
           (subs input (max 0 (- c 10)) c) "<--[HERE]"))))

(defn- between? [c lo hi] (<= (int lo) (int c) (int hi)))

(defn- number-char? [c]
  (or (between? c \0 \9) (= c \.) (= c \-)))

(defn- unquoted-char? [c]
  (or (between? c \0 \9) (between? c \A \Z) (between? c \a \z)
      (= c \_) (= c \-) (= c \.) (= c \+)))

(defn- quote? [c] (or (= c \") (= c \')))

(defn- read-number [kind parse [s n :as rd]]
  (let [[_ e :as end] (skip-while number-char? rd)
        text (subs s n e)]
    (if (= "" text)
      (error-at end (str "parsing." kind ".expected"))
      (try [(parse text) end]
           (catch NumberFormatException _
             (error-at rd (str "parsing." kind ".invalid") text))))))

(defn read-int [rd]
  (read-number "int" #(Integer/valueOf ^String %) rd))

(defn read-float [rd]
  (read-number "float" #(Float/valueOf ^String %) rd))

(defn read-double [rd]
  (read-number "double" #(Double/valueOf ^String %) rd))

(defn read-unquoted [[s n :as rd]]
  (let [[_ e :as end] (skip-while unquoted-char? rd)]
    [(subs s n e) end]))

(defn read-until [[s n] term]
  (let [len (count s)]
    (loop [i n acc [] esc false]
      (if (>= i len)
        (error-at [s i] "parsing.quote.expected.end")
        (let [c (nth s i) j (inc i)]
          (cond esc (if (or (= c term) (= c \\))
                      (recur j (conj acc c) false)
                      (error-at [s i] "parsing.quote.escape" (str c)))
                (= c \\) (recur j acc true)
                (= c term) [(str/join acc) [s j]]
                :else (recur j (conj acc c) false)))))))

(defn read-string [[s n :as rd]]
  (cond (not (can-read? rd)) ["" rd]
        (quote? (nth s n)) (read-until [s (inc n)] (nth s n))
        :else (read-unquoted rd)))

(defn read-boolean [rd]
  (let [r (read-string rd)]
    (if (error? r)
      r
      (let [[v end] r]
        (case v
          "" (error-at end "parsing.bool.expected")
          "true" [true end]
          "false" [false end]
          (error-at rd "parsing.bool.invalid" v))))))

(defn expect [[s n :as rd] ch]
  (if (and (can-read? rd) (= ch (nth s n)))
    [s (inc n)]
    (error-at rd "parsing.expected" (str ch))))

(defn- ranged [id kind read lo hi]
  {:id id :min lo :max hi
   :parse (fn [rd]
            (let [r (read rd) v (when-not (error? r) (first r))]
              (cond (error? r) r
                    (< v lo) (error-at rd (str kind ".low") lo v)
                    (> v hi) (error-at rd (str kind ".big") hi v)
                    :else r)))})

(defn int-arg
  ([] (int-arg Integer/MIN_VALUE))
  ([lo] (int-arg lo Integer/MAX_VALUE))
  ([lo hi]
   (ranged "brigadier:integer" "argument.integer" read-int
           (Integer/valueOf (int lo)) (Integer/valueOf (int hi)))))

(defn float-arg
  ([] (float-arg (- Float/MAX_VALUE)))
  ([lo] (float-arg lo Float/MAX_VALUE))
  ([lo hi]
   (ranged "brigadier:float" "argument.float" read-float
           (Float/valueOf (float lo)) (Float/valueOf (float hi)))))

(defn read-greedy
  "Reads the rest of the text."
  [[s _ :as rd]]
  [(remaining rd) [s (count s)]])
