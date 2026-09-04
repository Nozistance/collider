(ns collider.game.systems.chat
  (:require [clojure.string :as str]
            [collider.game.commands :as cmd]
            [collider.game.out :as out]
            [collider.game.mobs :as mobs]
            [collider.game.rules :as rules]
            [collider.game.systems.items :as items]
            [collider.vec :as v]
            [collider.world.block :as block]))

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

(defn tell [eid & lines]
  (mapv (fn [line] (out/to eid (out/system-chat (parse-runs line))))
        (mapcat #(str/split-lines (str %)) lines)))

(defn- fill-deltas [eid [ax ay az bx by bz block]]
  (let [[x1 x2] (sort [(long ax) (long bx)])
        [y1 y2] (sort [(long ay) (long by)])
        [z1 z2] (sort [(long az) (long bz)])
        n (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1)))
        st      (block/state block)
        changes (vec (for [x (range x1 (inc x2))
                           y (range y1 (inc y2))
                           z (range z1 (inc z2))]
                       [[x y z] st]))]
    (concat
     [[:set-blocks changes]]
     (tell eid (format "filled **%d** blocks" n)))))

(defn- say [eid key & with]
  [(out/to eid (out/system-chat [{:translate key :with (vec with)}]))])

(defn- rule-deltas
  "Sets the rule from its text, or reads it when text is nil (vanilla
   commands.gamerule.set / .query); the rules screen of everyone is refreshed."
  [world eid rule text]
  (let [id (subs (rules/wire-name rule) 10)]
    (if (nil? text)
      (say eid "commands.gamerule.query" id (rules/serialize rule (get-in world [:rules rule])))
      (if-some [v (rules/parse rule text)]
        (concat [[:set-rule rule v]
                 (out/all (out/game-rules (assoc (:rules world) rule v)))]
                (say eid "commands.gamerule.set" id (rules/serialize rule v)))
        (tell eid (str id ": give " (if (= :bool (:type (rules/table rule))) "true or false" "a whole number in range") ", not \"" text "\""))))))

(defn- rules-event-deltas
  "The rules screen: a request answers with all values, a change from the
   screen sets them one by one."
  [world [tag eid entries]]
  (case tag
    :rules-request [(out/to eid (out/game-rules (:rules world)))]
    :set-rules (mapcat (fn [[k v]] (when-let [r (rules/rule-of k)] (rule-deltas world eid r v))) entries)
    nil))

(defn- entity-name
  "Component naming an entity: the player's name or the mob's translation."
  [e]
  (if (= :player (:type e)) (:name e) {:translate (str "entity.minecraft." (str/replace (name (:type e)) "-" "_"))}))

(defn- targets
  "Entity ids a selector names, as seen from eid."
  [world eid {:keys [self all nearest entities type name]}]
  (let [players (vals (:players world))
        typed (fn [ids] (if type (filter #(= type (get-in world [:entities % :type])) ids) ids))]
    (cond
      self [eid]
      all (typed players)
      nearest (let [p (get-in world [:entities eid :pos])]
                (when-let [near (first (sort-by #(v/dist-sq p (get-in world [:entities % :pos])) (typed players)))]
                  [near]))
      entities (typed (keys (:entities world)))
      name (when-let [id (get-in world [:players name])] [id]))))

(defn- tp-deltas [world eid [x y z]]
  (let [e (get-in world [:entities eid]) pos [x y z]]
    [[:merge-entity eid {:pos (v/v3 pos) :tp-target pos}]
     (out/to eid (out/teleport pos (:yaw e 0.0) (:pitch e 0.0)))
     (out/to eid (out/system-chat [{:translate "commands.teleport.success.location.single"
                                    :with [(:name e) (format "%.2f" (double x)) (format "%.2f" (double y)) (format "%.2f" (double z))]}]))]))

(defn- give-deltas [world eid [sel item n]]
  (let [ids (targets world eid sel)]
    (if (empty? ids)
      (say eid "argument.entity.notfound.player")
      (concat
       (mapcat (fn [id]
                 (let [e (get-in world [:entities id])
                       [changes left] (items/add-stack (or (:inventory e) {}) {:item item :count n})]
                   (concat
                    (mapcat (fn [[slot stack]] [[:set-slot id slot stack] (out/to id (out/set-slot slot stack))]) changes)
                    (when left [[:spawn-entity (items/dropped world id left true 0)]])
                    (say eid "commands.give.success.single" n {:translate (str "item.minecraft." (str/replace (name item) "-" "_"))} (:name e)))))
               ids)))))

(defn- kill-deltas [world eid [sel]]
  (let [ids (targets world eid sel)]
    (cond
      (empty? ids) (say eid "argument.entity.notfound.entity")
      :else (concat
             (mapcat (fn [id]
                       (if (= :player (get-in world [:entities id :type]))
                         [[:merge-entity id {:health 0.0}]]
                         [[:remove-entity id]]))
                     ids)
             (if (= 1 (count ids))
               (say eid "commands.kill.success.single" (entity-name (get-in world [:entities (first ids)])))
               (say eid "commands.kill.success.multiple" (count ids)))))))

(defn- summon-deltas [world eid [type x y z]]
  (let [p (get-in world [:entities eid :pos])
        at [(double (or x (v/x p))) (double (or y (v/y p))) (double (or z (v/z p)))]
        t (:tick world)]
    [[:spawn-entity (mobs/egg-mob type at [t eid :summon] t)]
     (out/to eid (out/system-chat [{:translate "commands.summon.success" :with [{:translate (str "entity.minecraft." (name type))}]}]))]))

(defn- setblock-deltas [eid [x y z block]]
  [[:set-blocks [[[x y z] (block/state block)]]]
   (out/to eid (out/system-chat [{:translate "commands.setblock.success" :with [(str x) (str y) (str z)]}]))])

(defn- world-command-deltas [world eid [_ op & args]]
  (case op
    :gamerule (rule-deltas world eid (first args) (second args))
    :tp (tp-deltas world eid args)
    :give (give-deltas world eid args)
    :kill (kill-deltas world eid args)
    :summon (summon-deltas world eid args)
    :setblock (setblock-deltas eid args)
    :fill (fill-deltas eid args)
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
  (let [origin (when-let [p (get-in world [:entities eid :pos])]
                 [(v/x p) (v/y p) (v/z p)])
        {:keys [delta error]} (cmd/parse text origin)]
    (if error
      (tell eid error)
      (world-command-deltas world eid delta))))

(defn- public-deltas [world eid text]
  (when-let [e (get-in world [:entities eid])]
    [(out/all (out/player-chat (:name e) (parse-runs text)))]))

(defn- said-deltas [world eid raw]
  (let [text (str/trim (str raw))]
    (cond
      (str/blank? text)           nil
      (str/starts-with? text "/") (command-deltas world eid text)
      :else                       (public-deltas world eid text))))

(defn- tab-deltas [world eid text target id]
  (let [start (inc (.lastIndexOf ^String text " "))]
    [(out/to eid (out/suggestions (or id 0) start (- (count text) start) (cmd/suggest world text target)))]))

(defn- event-deltas [world [tag eid text target id]]
  (case tag
    :chat         (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text target id)
    nil))

(defn- chat-deltas [world events]
  (into [] (mapcat #(concat (event-deltas world %) (rules-event-deltas world %))) events))

(defn chat [world events]
  [#(chat-deltas world events)])
