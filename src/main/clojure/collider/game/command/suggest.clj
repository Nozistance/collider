(ns collider.game.command.suggest
  "Completions of half-typed command arguments."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.clock :as clock]
            [collider.game.command.args.block :as blocks]
            [collider.game.command.forms :as forms]
            [collider.game.command.reader :as r]
            [collider.game.command.selector :as sel]
            [collider.game.mode :as game-mode]
            [collider.game.schema :as schema]))

(set! *warn-on-reflection* true)

(defn- prefixed [prefix xs]
  (let [p (str/lower-case prefix)]
    (vec (filter #(str/starts-with? (str/lower-case %) p) xs))))

(defn- starting-with
  "Returns the xs that start with prefix, without prefix itself."
  [prefix xs]
  (filterv #(not= prefix %) (prefixed prefix xs)))

(defn suggest-player
  "Returns the names of the players in world that start with prefix."
  [world prefix]
  (prefixed prefix (sort (keys (:players world)))))

(defn sugg
  "Returns the completions texts that replace text from index start."
  [start texts]
  {:start start :texts texts})

(defn- unit-suggestions [w start]
  (let [res (r/read-float (r/reader w))]
    (when-not (r/error? res)
      (let [n (long (second (second res)))]
        (sugg (+ (long start) n)
              (starting-with (subs w n) ["d" "s" "t"]))))))

(defn- clock-in [opts cx]
  (if (:clock-arg opts)
    (some #(when (= "clock" (:name (:node %))) (:value %))
          (:nodes (:ctx cx)))
    (clock/default-of (:dim cx))))

(defn- marker-ids [opts cx]
  (let [k (clock-in opts cx)]
    (sort (for [[id m] (when k (clock/markers k)) :when (:show? m)]
            id))))

(defn- timeline-ids [opts cx]
  (let [k (clock-in opts cx)]
    (sort (for [[t {c :clock}] (clock/timelines)
                :when (and k (= k c))]
            t))))

(defn- sound-events [] (get (data/registries) "sound_event"))

(defn- item-names []
  (->> (keys (get (data/registries) "item"))
       (map data/snake)
       sort
       vec))

(defn- effect-names []
  (->> (keys (data/mob-effects)) (map data/wire) sort vec))

(defn- valid? [reader s] (not (r/error? (reader (r/reader s)))))

(defn- coord-texts [reader ^String rem]
  (let [c (if (str/starts-with? rem "^") "^" "~")
        fs (if (= "" rem) [] (str/split rem #" "))
        n (count fs)
        heads (reductions #(str %1 " " %2) (concat fs (repeat 3 c)))
        full (nth heads 2)]
    (when (and (< n 3) (valid? reader full))
      (drop n (take 3 heads)))))

(defn- coord-suggestions [reader text start]
  (->> (coord-texts reader (subs text start))
       (sel/suggest-strings text start)))

(defn- block-suggestions [kind reader ^String w start]
  (let [res (reader (r/reader w))
        whole? (and (not (r/error? res))
                    (= (count w) (second (second res))))]
    (cond
      whole? (when (and (not (str/includes? w "["))
                        (blocks/varying? (first res)))
               (sugg (+ (long start) (count w)) ["["]))
      (str/includes? w "[") nil
      :else (let [ids (blocks/ids (= :block-predicate kind))]
              (sel/suggest-ids w 0 ids "")))))

(defn- shifted [s start] (when s (update s :start + (long start))))

(defn- ids-of [w ids] (sel/suggest-ids w 0 ids ""))

(def ^:private mode-names
  (delay (mapv name (sort-by game-mode/id (keys game-mode/ids)))))

(defn- id-suggestions [kind opts w start cx]
  (case kind
    :marker (sugg start (:texts (ids-of w (marker-ids opts cx))))
    :timeline (sugg start (:texts (ids-of w (timeline-ids opts cx))))
    :sound (let [ids (sort (map data/wire (keys (sound-events))))]
             (sugg start (:texts (ids-of w ids))))
    :clock (let [ids (map data/wire (clock/names))]
             (sugg start (:texts (ids-of w ids))))
    :mob-effect (sugg start (:texts (ids-of w (effect-names))))
    :entity-type (sugg start (:texts (ids-of w (sel/entity-types))))
    nil))

(defn- arg-suggestions [[_ [kind opts]] reader w start cx]
  (case kind
    (:ticks :duration) (unit-suggestions w start)
    (:block :block-predicate)
    (shifted (block-suggestions kind reader w 0) start)
    :item (sugg start (starting-with w (item-names)))
    :game-mode (sugg start (starting-with w @mode-names))
    :anchor (sugg start (starting-with w ["eyes" "feet"]))
    (:bool :rule-value)
    (sugg start (if (= :int (:type opts))
                  []
                  (starting-with w ["false" "true"])))
    (id-suggestions kind opts w start cx)))

(defn- target-suggestions [text start cx]
  (let [p (sel/parse [text start] (forms/selectors? cx))]
    (sel/suggestions p text (:players cx))))

(defn arg-suggest
  "Returns the completer of argument a that reader reads."
  [[_ [kind] :as a] reader]
  (case kind
    (:coord :dcoord)
    (fn [text start _ _] (coord-suggestions reader text start))
    :targets (fn [text start _ cx] (target-suggestions text start cx))
    :dimension
    (fn [text start _ _]
      (let [ids (sort (map data/wire schema/dims))]
        (sel/suggest-ids text start ids "")))
    (fn [^String text start ctx cx]
      (let [w (subs text start)]
        (arg-suggestions a reader w start (assoc cx :ctx ctx))))))
