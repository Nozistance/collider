(ns collider.game.systems.chat
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [collider.game.commands :as cmd]
            [collider.game.state :as state]
            [collider.proto.packets.play :as play]))

(set! *warn-on-reflection* true)

(def ^:private markers
  [["***" {:bold true :italic true}]
   ["**" {:bold true}]
   ["__" {:underlined true}]
   ["~~" {:strikethrough true}]
   ["*"  {:italic true}]
   ["_"  {:italic true}]
   ["`"  {:color "gray"}]])

(defn- marker-at [^String s ^long i]
  (some (fn [[^String m st]] (when (.startsWith s m i) [m st])) markers))

(defn parse-runs
  ([s] (parse-runs s {}))
  ([^String s styles]
   (loop [i 0, plain (StringBuilder.), out []]
     (let [flush! (fn [o] (if (pos? (.length plain)) (conj o (assoc styles :text (str plain))) o))]
       (if (>= i (.length s))
         (flush! out)
         (let [c (.charAt s i)]
           (cond
             (and (= c \\) (< (inc i) (.length s))
                  (or (marker-at s (inc i)) (= \\ (.charAt s (inc i)))))
             (do (.append plain (.charAt s (inc i)))
                 (recur (+ i 2) plain out))
             :else
             (if-let [[^String m st] (marker-at s i)]
               (let [start (+ i (.length m))
                     close (.indexOf s m start)]
                 (if (> close start)
                   (let [inner (.substring s start close)
                         runs  (if (= m "`")
                                 [(merge styles st {:text inner})]
                                 (parse-runs inner (merge styles st)))]
                     (recur (+ close (.length m)) (StringBuilder.) (into (flush! out) runs)))
                   (do (.append plain m)
                       (recur (long start) plain out))))
               (do (.append plain c)
                   (recur (inc i) plain out))))))))))

(defn message-json [name text]
  (let [runs (parse-runs text)]
    (json/write-str
     {:translate "chat.type.text"
      :with [{:text name}
             (case (count runs)
               0 {:text ""}
               1 (first runs)
               {:text "" :extra runs})]})))

(defn system-json [text]
  (let [runs (parse-runs text)]
    (json/write-str
     (case (count runs)
       0 {:text ""}
       1 (first runs)
       {:text "" :extra runs}))))

(defn- chat-packet [json] {:packet/key ::play/chat :json json :position 0})
(defn tell [eid & lines]
  (mapv (fn [line] [:send eid (chat-packet (system-json line))])
        (mapcat #(str/split-lines (str %)) lines)))

(defn- fill-deltas [world eid [ax ay az bx by bz block]]
  (let [[x1 x2] (sort [(long ax) (long bx)])
        [y1 y2] (sort [(long ay) (long by)])
        [z1 z2] (sort [(long az) (long bz)])
        n (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1)))
        st      (bit-shift-left (long block) 4)
        changes (vec (for [x (range x1 (inc x2))
                           y (range y1 (inc y2))
                           z (range z1 (inc z2))]
                       [[x y z] st]))]
    (concat
     [[:set-blocks changes]]
     (tell eid (format "filled **%d** blocks" n)))))

(defn- world-command-deltas [world eid [_ op & args]]
  (case op
    :fill (fill-deltas world eid args)
    :time-set (let [t (long (first args))]
                (cons [:set-time t]
                      (tell eid (format "set the time to **%d**" t))))
    :time-add (let [t (+ (long (:time-of-day world 0)) (long (first args)))]
                (cons [:set-time t]
                      (tell eid (format "added **%d** to the time" (first args)))))
    :time-query (tell eid (case (first args)
                            "daytime"  (format "the time is **%d**" (long (:time-of-day world 0)))
                            "gametime" (format "the game time is **%d**" (long (:tick world 0)))))
    (tell eid (str "unknown world command: " op))))

(defn- command-deltas [world eid text]
  (let [origin (when-let [[x y z] (get-in world [:entities eid :pos])]
                 [(long (Math/floor (double x)))
                  (long (Math/floor (double y)))
                  (long (Math/floor (double z)))])
        {:keys [delta error]} (cmd/parse text origin)]
    (if error
      (tell eid error)
      (world-command-deltas world eid delta))))

(defn- public-deltas [world eid text]
  (when-let [e (get-in world [:entities eid])]
    (state/broadcast world (chat-packet (message-json (:name e) text)))))

(defn- said-deltas [world eid raw]
  (let [text (str/trim (str raw))]
    (cond
      (str/blank? text)           nil
      (str/starts-with? text "/") (command-deltas world eid text)
      :else                       (public-deltas world eid text))))

(defn- tab-deltas [world eid text target]
  [[:send eid (play/tab-complete (cmd/suggest world text target))]])

(defn- event-deltas [world [tag eid text target]]
  (case tag
    :chat         (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text target)
    nil))

(defn- chat-deltas [world events]
  (into [] (mapcat #(event-deltas world %)) events))

(defn chat [world events]
  [#(chat-deltas world events)])
