(ns collider.game.systems.chat
  "Chat lines, commands and tab completion."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.tree :as cmd]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather]))

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

(defn- marker-at [^String s ^long i]
  (some (fn [[^String m st]]
          (when (.startsWith s m i) [m st]))
        markers))

(defn- marked [^String s m st styles start close]
  (let [inner (.substring s start close)]
    (if (= m "`")
      [(merge styles st {:text inner})]
      (parse-runs inner (merge styles st)))))

(defn- span
  "Returns the end index and runs of the span opening at i.
  A marker that never closes stands for itself."
  [^String s styles ^StringBuilder plain ^long i]
  (when-let [[^String m st] (marker-at s i)]
    (let [start (+ i (.length m))
          close (^[String int] String/.indexOf s m start)]
      (if (> close start)
        [(+ close (.length m)) (marked s m st styles start close)]
        (do (.append plain m) [start nil])))))

(defn- flushed [out styles ^StringBuilder plain]
  (if (pos? (.length plain))
    (conj out (assoc styles :text (str plain)))
    out))

(defn- escape-at? [^String s ^long i]
  (and (= \\ (.charAt s i)) (< (inc i) (.length s))
       (or (marker-at s (inc i))
           (= \\ (.charAt s (inc i))))))

(defn- step
  "Returns the parse state after the markup at the index it holds."
  [^String s styles [i ^StringBuilder plain out]]
  (if (escape-at? s i)
    (do (.append plain (.charAt s (inc i)))
        [(+ 2 (long i)) plain out])
    (if-let [[end runs] (span s styles plain i)]
      (if runs
        [end (StringBuilder.) (into (flushed out styles plain) runs)]
        [end plain out])
      (do (.append plain (.charAt s i))
          [(inc (long i)) plain out]))))

(defn parse-runs
  "Returns the styled runs of a line with chat markup."
  ([s] (parse-runs s {}))
  ([^String s styles]
   (loop [st [0 (StringBuilder.) []]]
     (if (>= (long (first st)) (.length s))
       (flushed (peek st) styles (second st))
       (recur (step s styles st))))))

(defn tell
  "Returns the deltas that show the lines to one player."
  [eid & lines]
  (mapv (fn [line] (out/to eid (out/system-chat (parse-runs line))))
        (mapcat #(str/split-lines (str %)) lines)))

(defn- say [eid key & with]
  (let [msg {:translate key :with (vec with)}]
    [(out/to eid (out/system-chat [msg]))]))

(defn- unloaded?
  "Tells whether any position belongs to no chunk the world holds.
  A chunk a player keeps counts even before its data arrives."
  [world positions]
  (let [held (chunks/needed-ids world)]
    (some #(let [id (chunk/block-chunk %)]
             (not (or (contains? (:chunks world) id)
                      (contains? held id))))
          positions)))

(defn- box [[ax ay az bx by bz]]
  (mapv (fn [a b] (sort [(long a) (long b)])) [ax ay az] [bx by bz]))

(defn- fill-changes [chunks [[x1 x2] [y1 y2] [z1 z2]] st]
  (vec (for [x (range x1 (inc x2))
             y (range y1 (inc y2))
             z (range z1 (inc z2))
             :when (not= st (chunk/chunks-get-block chunks [x y z]))]
         [[x y z] st])))

(defn- box-ids [[[x1 x2] _ [z1 z2]]]
  (for [cx (range (bit-shift-right (long x1) 4)
                  (inc (bit-shift-right (long x2) 4)))
        cz (range (bit-shift-right (long z1) 4)
                  (inc (bit-shift-right (long z2) 4)))]
    (chunk/pos->id cx cz)))

(defn- fetched
  "Returns [chunks deltas] with every absent chunk of the box read."
  [world bounds]
  (let [in? #(contains? (:chunks world) %)
        read (fn [id] [id (chunks/read-absent world id)])
        loaded (into {} (comp (remove in?) (map read))
                     (box-ids bounds))]
    [(reduce-kv #(assoc %1 %2 (:chunk %3)) (:chunks world) loaded)
     (chunks/read-absent-deltas loaded)]))

(defn- fill-result [eid changes]
  (if (empty? changes)
    (say eid "commands.fill.failed")
    (cons [:set-blocks changes]
          (say eid "commands.fill.success" (str (count changes))))))

(defn- filled [world eid bounds block]
  (let [[chunks adds] (fetched world bounds)
        changes (fill-changes chunks bounds (block/state block))]
    (concat adds (fill-result eid changes))))

(defn- flat-in? [^long x ^long z]
  (and (<= -30000000 x) (< x 30000000)
       (<= -30000000 z) (< z 30000000)))

(defn- in-world?
  "Level.isInWorldBounds: inside the height of the level and
  the flat bounds of every level."
  [world [x y z]]
  (and (chunk/in-level? world (long y))
       (flat-in? (long x) (long z))))

(defn- pos-error
  "Returns the key of the reason pos is no command target, or nil.
  BlockPosArgument.getLoadedBlockPos."
  [world pos]
  (cond
    (unloaded? world [pos]) "argument.pos.unloaded"
    (not (in-world? world pos)) "argument.pos.outofworld"))

(defn- spawnable?
  "Level.isInSpawnableBounds for the block that holds pos."
  [pos]
  (let [[x y z] (mapv #(long (Math/floor (double %))) pos)]
    (and (<= -20000000 (long y)) (< (long y) 20000000)
         (flat-in? x z))))

(defn- fill-deltas [world eid [ax ay az bx by bz block]]
  (if-let [k (some #(pos-error world %) [[ax ay az] [bx by bz]])]
    (say eid k)
    (filled world eid (box [ax ay az bx by bz]) block)))

(defn- rule-hint [rule]
  (if (= :bool (:type (rules/table rule)))
    "true or false"
    "a whole number in range"))

(defn- rule-error [eid id rule text]
  (tell eid (str id ": give " (rule-hint rule)
                 ", not \"" text "\"")))

(defn- rule-set [world eid id rule v]
  (concat [[:set-rule rule v]
           (out/all (out/game-rules (assoc (:rules world) rule v)))]
          (say eid "commands.gamerule.set" id
               (rules/serialize rule v))))

(defn- rule-deltas [world eid rule text]
  (let [id (subs (rules/wire-name rule) 10)]
    (if (nil? text)
      (say eid "commands.gamerule.query" id
           (rules/serialize rule (get-in world [:rules rule])))
      (if-some [v (rules/parse rule text)]
        (rule-set world eid id rule v)
        (rule-error eid id rule text)))))

(defn- set-rule-delta [world eid [k v]]
  (when-let [r (rules/rule-of k)]
    (rule-deltas world eid r v)))

(defn- rules-event-deltas [world [tag eid entries]]
  (case tag
    :rules-request [(out/to eid (out/game-rules (:rules world)))]
    :set-rules (mapcat #(set-rule-delta world eid %) entries)
    nil))

(defn- entity-name [e]
  (if (= :player (:type e))
    (:name e)
    {:translate (str "entity.minecraft." (data/snake (:type e)))}))

(defn- item-name [item]
  {:translate (str "item.minecraft." (data/snake item))})

(defn- typed-ids [world type not-type? ids]
  (if type
    (filter #(= (boolean not-type?)
                (not= type (get-in world [:entities % :type])))
            ids)
    ids))

(defn- nearest-id [world eid ids]
  (let [p (get-in world [:entities eid :pos])
        at (fn [id] (get-in world [:entities id :pos]))
        d (fn [id] [(v/dist-sq p (at id)) id])]
    (when-let [near (first (sort-by d ids))]
      [near])))

(defn- targets [world eid sel]
  (let [{:keys [self all nearest entities type not-type? name]} sel
        players (map key (state/player-entries world))
        typed #(typed-ids world type not-type? %)]
    (cond
      self [eid]
      all (typed players)
      nearest (nearest-id world eid (typed players))
      entities (typed (keys (:entities world)))
      name (when-let [id (get-in world [:players name])] [id]))))

(defn- teleported [world eid pos]
  (let [e (get-in world [:entities eid])
        at (mapv #(format "%.2f" (double %)) pos)
        msg {:translate "commands.teleport.success.location.single"
             :with (into [(:name e)] at)}]
    [[:teleport eid pos]
     (out/to eid (out/teleport pos (:yaw e 0.0) (:pitch e 0.0)))
     (out/to eid (out/system-chat [msg]))]))

(defn- tp-deltas [world eid [x y z]]
  (if (spawnable? [x y z])
    (teleported world eid [x y z])
    (say eid "commands.teleport.invalidPosition")))

(defn- given [world eid id item n]
  (let [e (get-in world [:entities id])
        inv (or (:inventory e) {})
        [changes left] (items/add-stack inv {:item item :count n})]
    (concat
      (map (fn [[slot stack]] [:set-slot id slot stack]) changes)
      (when left
        [[:spawn-entity (items/dropped world id left true 0)]])
      (say eid "commands.give.success.single"
           n (item-name item) (:name e)))))

(defn- give-deltas [world eid [sel item n]]
  (let [ids (targets world eid sel)]
    (if (empty? ids)
      (say eid "argument.entity.notfound.player")
      (mapcat #(given world eid % item n) ids))))

(defn- killed [world id]
  (if (= :player (get-in world [:entities id :type]))
    [[:merge-entity id {:health 0.0}]]
    [[:remove-entity id]]))

(defn- kill-report [world eid ids]
  (if (= 1 (count ids))
    (say eid "commands.kill.success.single"
         (entity-name (get-in world [:entities (first ids)])))
    (say eid "commands.kill.success.multiple" (count ids))))

(defn- kill-deltas [world eid [sel]]
  (let [ids (targets world eid sel)]
    (if (empty? ids)
      (say eid "argument.entity.notfound.entity")
      (concat (mapcat #(killed world %) ids)
              (kill-report world eid ids)))))

(defn- summoned [world eid type at]
  (let [t (:tick world)
        kind {:translate (str "entity.minecraft." (name type))}
        msg {:translate "commands.summon.success" :with [kind]}]
    [[:spawn-entity (mobs/egg-mob type at [t eid :summon] t)]
     (out/to eid (out/system-chat [msg]))]))

(defn- summon-deltas [world eid [type x y z]]
  (let [p (get-in world [:entities eid :pos])
        at [(double (or x (v/x p)))
            (double (or y (v/y p)))
            (double (or z (v/z p)))]]
    (if (spawnable? at)
      (summoned world eid type at)
      (say eid "commands.summon.invalidPosition"))))

(defn- setblock-deltas [world eid [x y z block]]
  (let [pos [x y z]
        st (block/state block)
        k (pos-error world pos)]
    (cond
      k (say eid k)
      (= st (chunk/chunks-get-block (:chunks world) pos))
      (say eid "commands.setblock.failed")
      :else (cons [:set-blocks [[pos st]]]
                  (say eid "commands.setblock.success"
                       (str x) (str y) (str z))))))

(defn- block-under [world eid [x y z]]
  (let [p (get-in world [:entities eid :pos])]
    [(long (or x (Math/floor (v/x p))))
     (long (or y (Math/floor (v/y p))))
     (long (or z (Math/floor (v/z p))))]))

(defn- spawn-pos-deltas
  "Returns the deltas of f unless the given position is out of
  bounds. BlockPosArgument.getSpawnablePos."
  [f world eid args]
  (if (or (some nil? args) (spawnable? args))
    (f world eid args)
    (say eid "argument.pos.outofbounds")))

(defn- world-spawn-deltas [world eid args]
  (let [at (block-under world eid args)
        with (conj (mapv str at) "0.0" "0.0" "minecraft:overworld")
        msg {:translate "commands.setworldspawn.success"
             :with with}]
    [[:set-world-spawn at]
     (out/all (out/default-spawn at))
     (out/to eid (out/system-chat [msg]))]))

(defn- spawnpoint-set-deltas [world eid args]
  (let [at (block-under world eid args)
        e (get-in world [:entities eid])
        with (conj (mapv str at) "0.0" "0.0" "minecraft:overworld"
                   (entity-name e))
        spawn {:pos at :yaw 0.0 :pitch 0.0}
        msg {:translate "commands.spawnpoint.success.single"
             :with with}]
    [[:merge-entity eid {:forced-spawn spawn}]
     (out/to eid (out/system-chat [msg]))]))

(defn- duration-of ^long [world ^long given bounds salt]
  (if (pos? given)
    given
    (weather/sample (random/of-key (:tick world) salt) bounds)))

(defn- clear-parameters [world ^long given]
  (let [d (duration-of world given weather/rain-delay
                       :weather-clear)]
    (weather/parameters d 0 false false)))

(defn- rain-parameters [world ^long given thunder?]
  (let [bounds (if thunder?
                 weather/thunder-duration
                 weather/rain-duration)
        salt (if thunder? :weather-thunder :weather-rain)
        d (duration-of world given bounds salt)]
    (weather/parameters 0 d true thunder?)))

(defn- weather-parameters [world kind given]
  (case kind
    :clear (clear-parameters world given)
    :rain (rain-parameters world given false)
    :thunder (rain-parameters world given true)))

(defn- weather-deltas [world eid kind given]
  (let [given (long (or given 0))
        m (weather-parameters world kind given)
        msg {:translate (str "commands.weather.set." (name kind))}]
    [[:set-weather m]
     (out/to eid (out/system-chat [msg]))]))

(defn- time-query [world what]
  (case what
    "daytime" (format "the time is **%d**"
                      (long (:time-of-day world 0)))
    "gametime" (format "the game time is **%d**"
                       (long (:tick world 0)))))

(defn- time-set-deltas [eid ^long t]
  (cons [:set-time t]
        (tell eid (format "set the time to **%d**" t))))

(defn- time-add-deltas [world eid amount]
  (let [t (+ (long (:time-of-day world 0)) (long amount))
        line (format "added **%d** to the time" amount)]
    (cons [:set-time t] (tell eid line))))

(defn- time-deltas [world eid op args]
  (case op
    :time-set (time-set-deltas eid (long (first args)))
    :time-add (time-add-deltas world eid (first args))
    :time-query (tell eid (time-query world (first args)))))

(defn- weather-command-deltas [world eid op args]
  (let [given (first args)]
    (case op
      :weather-clear (weather-deltas world eid :clear given)
      :weather-rain (weather-deltas world eid :rain given)
      :weather-thunder (weather-deltas world eid :thunder given))))

(def ^:private commands
  {:tp tp-deltas :give give-deltas :kill kill-deltas
   :summon summon-deltas :setblock setblock-deltas
   :setworldspawn (partial spawn-pos-deltas world-spawn-deltas)
   :spawnpoint (partial spawn-pos-deltas spawnpoint-set-deltas)
   :fill fill-deltas})

(defn- world-command-deltas [world eid [_ op & args]]
  (if-let [f (commands op)]
    (f world eid args)
    (case op
      :gamerule (rule-deltas world eid (first args) (second args))
      (:weather-clear :weather-rain :weather-thunder)
      (weather-command-deltas world eid op args)
      (:time-set :time-add :time-query)
      (time-deltas world eid op args)
      (tell eid (str "unknown world command: " op)))))

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
      (str/blank? text) nil
      (str/starts-with? text "/") (command-deltas world eid text)
      :else (public-deltas world eid text))))

(defn- tab-deltas [world eid text target id]
  (let [start (inc (.lastIndexOf ^String text " "))
        len (- (count text) start)
        sug (cmd/suggest world text target)]
    [(out/to eid (out/suggestions (or id 0) start len sug))]))

(defn- event-deltas [world [tag eid text target id]]
  (case tag
    :chat (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text target id)
    nil))

(defn- one-deltas [world ev]
  (vec (concat (event-deltas world ev)
               (rules-event-deltas world ev))))

(defn- chat-deltas [world events]
  (state/fold-events world events one-deltas))

(defn chat
  "Turns the chat lines and commands of this tick into deltas."
  [world d]
  (let [events (:input d)]
    [#(chat-deltas world events)]))
