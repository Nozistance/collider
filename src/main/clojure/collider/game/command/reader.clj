(ns collider.game.command.reader
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn reader
  ([s] [s 0])
  ([s n] [s n]))

(defn can-read?
  ([rd] (can-read? rd 1))
  ([[s n] k] (<= (+ n k) (count s))))

(defn remaining [[s n]] (subs s n))

(defn consumed [[s n]] (subs s 0 n))

(defn- skip-while [pred [s n]]
  (let [len (count s)]
    (loop [i n]
      (if (and (< i len) (pred (nth s i)))
        (recur (inc i))
        [s i]))))

(defn- whitespace? [c] (Character/isWhitespace (char c)))

(defn skip-whitespace [rd] (skip-while whitespace? rd))

(defn error [k & args]
  {:key k :args (vec args) :cursor -1 :input nil})

(defn error-at [[s n] k & args]
  {:key k :args (vec args) :cursor n :input s})

(defn error? [x]
  (and (map? x) (contains? x :key) (contains? x :cursor)))

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

(defn read-long [rd]
  (read-number "long" #(Long/valueOf ^String %) rd))

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

(defn read-quoted [[s n :as rd]]
  (cond (not (can-read? rd)) ["" rd]
        (quote? (nth s n)) (read-until [s (inc n)] (nth s n))
        :else (error-at rd "parsing.quote.expected.start")))

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

(defn context [{:keys [cursor input]}]
  (when (and input (>= cursor 0))
    (let [c (min cursor (count input))]
      (str (when (> c 10) "...")
           (subs input (max 0 (- c 10)) c)
           "<--[HERE]"))))

(defn message [{:keys [key args]}]
  (cond-> {:translate key} (seq args) (assoc :with args)))

(def ^:private here
  {:translate "command.context.here" :color "red" :italic true})

(defn- line [input c command]
  {:text "" :color "gray"
   :click {:action :suggest-command :command (str "/" command)}
   :extra (cond-> (if (> c 10) ["..."] [])
            (pos? c) (conj (subs input (max 0 (- c 10)) c))
            (< c (count input))
            (conj {:text (subs input c) :color "red"
                   :underlined true})
            :always (conj here))})

(defn chat
  ([err] (chat err (:input err)))
  ([{:keys [cursor input] :as err} command]
   (if (and input (>= cursor 0))
     [(message err) (line input (min cursor (count input)) command)]
     [(message err)])))

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

(defn long-arg
  ([] (long-arg Long/MIN_VALUE))
  ([lo] (long-arg lo Long/MAX_VALUE))
  ([lo hi]
   (ranged "brigadier:long" "argument.long" read-long
           (Long/valueOf (long lo)) (Long/valueOf (long hi)))))

(defn float-arg
  ([] (float-arg (- Float/MAX_VALUE)))
  ([lo] (float-arg lo Float/MAX_VALUE))
  ([lo hi]
   (ranged "brigadier:float" "argument.float" read-float
           (Float/valueOf (float lo)) (Float/valueOf (float hi)))))

(defn double-arg
  ([] (double-arg (- Double/MAX_VALUE)))
  ([lo] (double-arg lo Double/MAX_VALUE))
  ([lo hi]
   (ranged "brigadier:double" "argument.double" read-double
           (Double/valueOf (double lo))
           (Double/valueOf (double hi)))))

(defn bool-arg [] {:id "brigadier:bool" :parse read-boolean})

(defn- read-greedy [[s _ :as rd]] [(remaining rd) [s (count s)]])

(def ^:private string-reads
  {:word read-unquoted :phrase read-string :greedy read-greedy})

(defn string-arg [mode]
  {:id "brigadier:string" :mode mode :parse (string-reads mode)})

(defn escape-if-required [s]
  (if (every? unquoted-char? s)
    s
    (let [e (str/escape s {\\ "\\\\" \" "\\\""})]
      (str "\"" e "\""))))
