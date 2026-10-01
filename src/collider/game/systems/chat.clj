(ns collider.game.systems.chat
  "Chat lines, commands and tab completion."
  (:require [clojure.string :as str]
            [collider.config :as config]
            [collider.data :as data]
            [collider.game.camera :as camera]
            [collider.game.clock :as clock]
            [collider.game.deltas :as deltas]
            [collider.game.effect :as effect]
            [collider.game.experience :as xp]
            [collider.game.command.args :as cmd-args]
            [collider.game.command.item-args :as item-args]
            [collider.game.command.block-args :as block-args]
            [collider.game.command.reader :as cmd-reader]
            [collider.game.command.tree :as cmd]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.gamerules :as rules]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.out :as out]
            [collider.game.schema :as schema]
            [collider.game.stack :as stack]
            [collider.game.apply :as apply]
            [collider.game.level :as level]
            [collider.game.player :as player]
            [collider.game.systems.blocks.clone :as clone]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.effects :as effects]
            [collider.game.systems.items :as items]
            [collider.game.systems.sleep :as sleep]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather])
  (:import (collider.game.mob Steer)
           (java.util Date Locale)))

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
          (when (String/.startsWith s m i) [m st]))
        markers))

(defn- marked [^String s m st styles start close]
  (let [inner (String/.substring s start close)]
    (if (= m "`")
      [(merge styles st {:text inner})]
      (parse-runs inner (merge styles st)))))

(defn- span [^String s styles ^StringBuilder plain ^long i]
  (when-let [[^String m st] (marker-at s i)]
    (let [n (String/.length m)
          start (+ i n)
          close (^[String int] String/.indexOf s m start)]
      (if (> close start)
        [(+ close n) (marked s m st styles start close)]
        (do (^[String] StringBuilder/.append plain m)
            [start nil])))))

(defn- flushed [out styles ^StringBuilder plain]
  (if (pos? (StringBuilder/.length plain))
    (conj out (assoc styles :text (str plain)))
    out))

(defn- escape-at? [^String s ^long i]
  (and (= \\ (String/.charAt s i))
       (< (inc i) (String/.length s))
       (or (marker-at s (inc i))
           (= \\ (String/.charAt s (inc i))))))

(defn- step
  [^String s styles [i ^StringBuilder plain out]]
  (if (escape-at? s i)
    (do (^[char] StringBuilder/.append plain
                 (String/.charAt s (inc i)))
        [(+ 2 (long i)) plain out])
    (if-let [[end runs] (span s styles plain i)]
      (if runs
        [end (StringBuilder.) (into (flushed out styles plain) runs)]
        [end plain out])
      (do (^[char] StringBuilder/.append plain (String/.charAt s i))
          [(inc (long i)) plain out]))))

(defn parse-runs
  "Returns the styled runs of a line with chat markup."
  ([s] (parse-runs s {}))
  ([^String s styles]
   (loop [st [0 (StringBuilder.) []]]
     (if (>= (long (first st)) (String/.length s))
       (flushed (peek st) styles (second st))
       (recur (step s styles st))))))

(defn- joined [runs]
  (case (count runs)
    0 ""
    1 (first runs)
    {:text "" :extra runs}))

(defn tell
  "Returns the deltas that show the lines to one player."
  [eid & lines]
  (mapv #(out/to eid (out/system-chat (joined (parse-runs %))))
        (mapcat #(str/split-lines (str %)) lines)))

(defn- reports [ds admins?]
  (mapv (fn [d]
          (if (= :fx (nth d 0))
            [:fx (assoc (nth d 1) :feedback true :admins admins?)]
            d))
        ds))

(defn- success [ds] (reports ds true))

(defn- answer [ds] (reports ds false))

(defn- say* [eid key with]
  (let [msg {:translate key :with (vec with)}]
    (success [(out/to eid (out/system-chat msg))])))

(defn- say [eid key & with] (say* eid key with))

(defn- failure [text]
  (out/system-chat {:text "" :color "red" :extra [text]}))

(defn- fail [eid key & with]
  [(out/to eid (failure {:translate key :with (vec with)}))])

(defn- unloaded? [world positions]
  (let [held (chunks/needed-ids world)]
    (some #(let [id (chunk/block-chunk %)]
             (not (or (contains? (:chunks world) id)
                      (contains? held id))))
          positions)))

(defn- box [[ax ay az bx by bz]]
  (mapv (fn [a b] (sort [(long a) (long b)])) [ax ay az] [bx by bz]))

(defn- shell? [[[x1 x2] [y1 y2] [z1 z2]] [x y z]]
  (or (= x x1) (= x x2) (= y y1) (= y y2) (= z z1) (= z z2)))

(defn- fill-changes
  [[[x1 x2] [y1 y2] [z1 z2] :as bounds] st mode]
  (into [] (keep (fn [p]
                   (cond (shell? bounds p) [p st]
                         (= "outline" mode) nil
                         (= "hollow" mode) [p 0]
                         :else [p st])))
        (for [z (range z1 (inc z2))
              y (range y1 (inc y2))
              x (range x1 (inc x2))]
          [x y z])))

(defn- box-ids [[[x1 x2] _ [z1 z2]]]
  (for [cx (range (bit-shift-right (long x1) 4)
                  (inc (bit-shift-right (long x2) 4)))
        cz (range (bit-shift-right (long z1) 4)
                  (inc (bit-shift-right (long z2) 4)))]
    (chunk/pos->id cx cz)))

(defn- fetched [world bounds]
  (let [in? #(contains? (:chunks world) %)
        read (fn [id] [id (chunks/read-absent world id)])
        loaded (into {} (comp (remove in?) (map read))
                     (box-ids bounds))]
    [(reduce-kv #(assoc %1 %2 (:chunk %3)) (:chunks world) loaded)
     (chunks/read-absent-deltas loaded)]))

(defn- source-dim [world]
  (get-in world [:source :dim] (:dim world)))

(defn- source-pos [world]
  (get-in world [:source :pos]))

(defn- level-view [world dim]
  (let [server (:server world)]
    (if (or (= dim (:dim world)) (nil? server))
      world
      (assoc (level/level server dim) :server server))))

(defn- in-level [world dim ds]
  (cond
    (empty? ds) nil
    (= dim (:dim world)) (vec ds)
    :else [[:level-deltas dim (vec ds)]]))

(defn- source-level [world] (level-view world (source-dim world)))

(defn- mode-opts [mode test]
  (cond-> {}
    test (assoc :test test)
    (= "keep" mode) (assoc :test block/air-type?)
    (= "destroy" mode) (assoc :destroy? true)
    (= "strict" mode) (assoc :strict? true)))

(defn- edited [world changes opts]
  (let [dim (source-dim world)
        lv (level-view world dim)
        [chunks adds] (fetched lv (box (into (ffirst changes)
                                             (first (peek changes)))))
        [ds n placed] (edit/command-deltas (assoc lv :chunks chunks)
                                           changes opts)]
    [(in-level world dim adds) (in-level world dim ds) n placed]))

(defn- area ^long [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (inc (- (long x2) (long x1))) (inc (- (long y2) (long y1)))
     (inc (- (long z2) (long z1)))))

(defn- rule-value [world k]
  (get-in world [:rules k] (rules/defaults k)))

(defn- too-big
  "The error of a command that would change more cells than the
  rule max_block_modifications allows, FillCommand and CloneCommands."
  [world eid k bounds]
  (let [limit (long (rule-value world :max-block-modifications))
        n (area bounds)]
    (when (> n limit) (fail eid k limit n))))

(defn- filled [world eid bounds block mode test]
  (let [changes (fill-changes bounds (:state block) mode)
        [adds ds n] (edited world changes (mode-opts mode test))]
    (if (zero? (long n))
      (concat adds (fail eid "commands.fill.failed"))
      (concat adds ds (say eid "commands.fill.success" n)))))

(defn- flat-in? [^long x ^long z]
  (and (<= -30000000 x) (< x 30000000)
       (<= -30000000 z) (< z 30000000)))

(defn- in-world?
  [world [x y z]]
  (and (chunk/in-level? world (long y))
       (flat-in? (long x) (long z))))

(defn- pos-error [world pos]
  (cond
    (unloaded? world [pos]) "argument.pos.unloaded"
    (not (in-world? world pos)) "argument.pos.outofworld"))

(defn- spawnable? [pos]
  (let [[x y z] (mapv #(long (Math/floor (double %))) pos)]
    (and (<= -20000000 (long y)) (< (long y) 20000000)
         (flat-in? x z))))

(defn- fill-by [world eid corners block mode test]
  (let [bounds (box corners)]
    (if-let [k (some #(pos-error (source-level world) %)
                     [(subvec corners 0 3) (subvec corners 3 6)])]
      (fail eid k)
      (or (too-big world eid "commands.fill.toobig" bounds)
          (filled world eid bounds block mode test)))))

(defn- fill-deltas [world eid [ax ay az bx by bz block mode]]
  (fill-by world eid [ax ay az bx by bz] block mode nil))

(defn- fill-where-deltas
  [world eid [ax ay az bx by bz block _ filter mode]]
  (fill-by world eid [ax ay az bx by bz] block mode
           #(block-args/matches? filter % nil)))

(defn- clone-args [[{:keys [from? to?]} & xs]]
  (let [[sd xs] (if from? [(first xs) (rest xs)] [nil xs])
        [x1 y1 z1 x2 y2 z2 & xs] xs
        [td xs] (if to? [(first xs) (rest xs)] [nil xs])
        [x y z f m] xs]
    {:from sd :to td :begin [x1 y1 z1] :end [x2 y2 z2]
     :dest [x y z] :filter f :mode m}))

(defn- dim-of [world id]
  (if id (cmd-args/dimension id schema/dims) (source-dim world)))

(defn- dim-error [d]
  (when (cmd-reader/error? d) (into [(:key d)] (:args d))))

(defn- corner-error [world dim pos]
  (when-let [k (pos-error (level-view world dim) pos)] [k]))

(defn- clone-error
  "The first error CloneCommands meets reading its arguments: the
  source level, its corners, the target level, the destination."
  [world {:keys [begin end dest]} fd td]
  (or (dim-error fd)
      (corner-error world fd begin)
      (corner-error world fd end)
      (dim-error td)
      (corner-error world td dest)))

(defn- moved-by [[lo hi] o]
  [(+ (long lo) (long o)) (+ (long hi) (long o))])

(defn- clone-boxes [{:keys [begin end dest]}]
  (let [b (box (into begin end))
        off (mapv (fn [[lo] d] (- (long d) (long lo))) b dest)]
    [b off (mapv moved-by b off)]))

(defn- meet? [[lo hi] [lo' hi']]
  (and (<= (long lo') (long hi)) (<= (long lo) (long hi'))))

(defn- overlap? [b d] (every? true? (map meet? b d)))

(defn- chunk-corners [[x0 _ z0] [x1 _ z1]]
  (let [c #(bit-shift-right (long %) 4)]
    (for [cx (range (c x0) (inc (c x1)))
          cz (range (c z0) (inc (c z1)))]
      [(* 16 cx) 0 (* 16 cz)])))

(defn- chunks-at?
  "LevelReader.hasChunksAt between corners a and b as given."
  [lv [_ y0 _ :as a] [_ y1 _ :as b]]
  (and (>= (long y1) (chunk/level-min-y lv))
       (<= (long y0) (chunk/level-max-y lv))
       (not (unloaded? lv (chunk-corners a b)))))

(defn- opened-level [world dim boxes]
  (reduce (fn [[lv adds] bounds]
            (let [[chunks more] (fetched lv bounds)]
              [(assoc lv :chunks chunks) (into adds more)]))
          [(level-view world dim) []] boxes))

(defn- clone-test [{:keys [filter]} filtered?]
  (cond filtered? (fn [st _] (block-args/matches? filter st nil))
        (= "masked" filter) (fn [st _] (not (block/air-type? st)))))

(defn- copied-tail [p]
  (cond-> (into [] (mapcat (fn [[q e]] (edit/be-changed q e)))
                (:entities p))
    (seq (:ticks p)) (conj [:schedule-copied (:ticks p)])))

(defn- clone-run
  "The deltas of a clone planned as p and the cells it set."
  [world [fd from fadds] [td to tadds] p]
  (if (= fd td)
    (let [ops (into (vec (:clear p)) (:place p))
          [ds n] (edit/ops-deltas to ops)]
      [(in-level world td (concat tadds ds (copied-tail p))) n])
    (let [[cds] (when (seq (:clear p))
                  (edit/ops-deltas from (:clear p)))
          [ds n] (edit/ops-deltas to (:place p))]
      [(concat (in-level world fd (concat fadds cds))
               (in-level world td (concat tadds ds (copied-tail p))))
       n])))

(defn- cloned [world eid a opts fd td [b off d]]
  (let [[from fadds] (opened-level world fd (if (= fd td) [b d] [b]))
        [to tadds] (if (= fd td) [from fadds]
                       (opened-level world td [d]))
        p (clone/plan from to b off (clone-test a (:filtered? opts))
                      (:mode a) (:strict? opts))
        [ds n] (clone-run world [fd from fadds] [td to tadds] p)]
    (if (zero? (long n))
      (concat ds (fail eid "commands.clone.failed"))
      (concat ds (say eid "commands.clone.success" n)))))

(defn- clone-loaded? [world a fd td [_ _ d]]
  (and (chunks-at? (level-view world fd) (:begin a) (:end a))
       (chunks-at? (level-view world td) (:dest a) (mapv peek d))))

(defn- clone-deltas [world eid [opts :as xs]]
  (let [a (clone-args xs)
        fd (dim-of world (:from a))
        td (dim-of world (:to a))
        [b _ d :as boxes] (clone-boxes a)]
    (if-let [[k & with] (clone-error world a fd td)]
      (apply fail eid k with)
      (or (when (and (not (#{"force" "move"} (:mode a))) (= fd td)
                     (overlap? b d))
            (fail eid "commands.clone.overlap"))
          (too-big world eid "commands.clone.toobig" b)
          (when-not (clone-loaded? world a fd td boxes)
            (fail eid "argument.pos.unloaded"))
          (cloned world eid a opts fd td boxes)))))

(defn- rule-hint [rule]
  (if (= :bool (:type (rules/table rule)))
    "true or false"
    "a whole number in range"))

(defn- rule-error [eid id rule text]
  (tell eid (str id ": give " (rule-hint rule)
                 ", not \"" text "\"")))

(defn- clocks-resent [world rule]
  (when (= :advance-time rule)
    [(out/all (out/time (long (:tick world))
                        (clock/full-sync world)))]))

(defn- rule-shown [world rule v]
  (case rule
    (:immediate-respawn :limited-crafting)
    [(out/all (out/rule-flag rule v))]
    :reduced-debug-info
    (for [p (vals (:players world))]
      (out/to p (out/status p (if v :reduced-debug :full-debug))))
    nil))

(defn- rule-set [world eid id rule v]
  (let [w (assoc-in world [:rules rule] v)]
    (concat [[:set-rule rule v]
             (out/all (out/game-rules (:rules w)))]
            (rule-shown w rule v)
            (clocks-resent w rule)
            (say eid "commands.gamerule.set" id
                 (rules/serialize rule v)))))

(defn- rule-query [world eid id rule]
  (let [v (rules/serialize rule (get-in world [:rules rule]))
        msg {:translate "commands.gamerule.query" :with [id v]}]
    (answer [(out/to eid (out/system-chat msg))])))

(defn- rule-deltas [world eid rule text]
  (let [id (subs (rules/wire-name rule) 10)]
    (if (nil? text)
      (rule-query world eid id rule)
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
  (let [k (str "entity.minecraft." (data/snake (:type e)))
        nm (if (= :player (:type e)) (:name e) {:translate k})
        hover {:action :show-entity :id (:type e) :uuid (:uuid e)
               :name nm}]
    (cond
      (nil? (:uuid e)) nm
      (= :player (:type e))
      {:text nm :insertion nm :hover hover
       :click {:action :suggest-command
               :command (str "/tell " nm " ")}}
      :else (assoc nm :hover hover :insertion (str (:uuid e))))))

(def ^:private rarity-colors
  {:common "white" :uncommon "yellow" :rare "aqua"
   :epic "light_purple"})

(defn- item-name [item]
  {:translate "chat.square_brackets"
   :with [{:text "" :extra [(data/item-title item)]}]
   :color (rarity-colors (data/rarity item))
   :hover {:action :show-item :id item}})

(defn- levels [world]
  (if (:server world)
    (map (fn [d] [d (level-view world d)]) schema/dims)
    [[(:dim world) world]]))

(defn- player-entries [world]
  (sort-by first
           (for [[dim lv] (levels world)
                 [id e] (level/player-entries lv)]
             [id dim e])))

(defn- entity-entries [world]
  (for [[dim lv] (levels world)
        [id e] (sort-by key (:entities lv))]
    [id dim e]))

(defn- alive? [e]
  (not (and (:health e) (<= (double (:health e)) 0.0))))

(defn- xyz [p] [(v/x p) (v/y p) (v/z p)])

(def ^:private still (v/v3 [0.0 0.0 0.0]))

(defn- dist-sq ^double [from e]
  (let [p (:pos e)
        d #(- (double %1) (double (nth from %2)))
        dx (d (v/x p) 0) dy (d (v/y p) 1) dz (d (v/z p) 2)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- in-distance? [[lo hi] from e]
  (let [d (dist-sq from e) sq #(* (double %) (double %))]
    (and (or (nil? lo) (>= d (double (sq lo))))
         (or (nil? hi) (<= d (double (sq hi)))))))

(defn- in-bounds? [[lo hi] v]
  (and (or (nil? lo) (>= (double v) (double lo)))
       (or (nil? hi) (<= (double v) (double hi)))))

(declare wrapped)

(defn- turned? [[lo hi] rot]
  (let [a (wrapped (or lo 0.0)) b (wrapped (or hi 359.0))
        r (wrapped rot)]
    (if (> a b) (or (>= r a) (<= r b)) (and (>= r a) (<= r b)))))

(defn- type-tagged? [id t]
  (let [tag (get (data/tags) "entity_type")]
    (some #{t} (get tag (str/replace id #"^minecraft:" "")))))

(defn- tagged? [tag e]
  (if (= "" tag) (empty? (:tags e)) (contains? (:tags e) tag)))

(defn- pred? [[k a inv] e]
  (let [f #(not= (boolean inv) (boolean %))]
    (case k
      :alive (alive? e)
      :name (f (= a (when (= :player (:type e)) (:name e))))
      :gamemode (and (= :player (:type e)) (f (= a (:game-mode e))))
      :team (f (= a ""))
      :type (f (= a (:type e)))
      :type-tag (f (type-tagged? a (:type e)))
      :tag (f (tagged? a e))
      false)))

(declare xp-of)

(defn- leveled? [world sel e]
  (or (nil? (:level sel))
      (and (= :player (:type e))
           (in-bounds? (:level sel) (:level (xp-of world e))))))

(defn- entity-box [e]
  (let [[hw h] (entity/box e) [x y z] (xyz (:pos e))
        hw (double hw)]
    [[(- x hw) (+ x hw)] [y (+ y (double h))] [(- z hw) (+ z hw)]]))

(defn- overlaps? [a b]
  (every? (fn [[[a0 a1] [b0 b1]]]
            (and (< (double a0) (double b1))
                 (> (double a1) (double b0))))
          (map vector a b)))

(defn- sel-pos [world sel]
  (let [p (source-pos world) o (:pos sel)]
    (mapv #(double (get o % (nth p %))) [0 1 2])))

(defn- sel-box [sel pos]
  (let [d (:delta sel) hi (second (:range sel))]
    (cond
      (seq d) (mapv (fn [i]
                      (let [v (double (get d i 0.0))
                            c (double (pos i))]
                        [(+ c (min v 0.0)) (+ c (max v 0.0) 1.0)]))
                    [0 1 2])
      hi (mapv #(vector (- (double %) (double hi))
                        (+ (double %) (double hi) 1.0))
               pos))))

(defn- selects? [world sel pos box [_ _ e]]
  (and (every? #(pred? % e) (:preds sel))
       (or (nil? (:type sel)) (= (:type sel) (:type e)))
       (or (nil? (:rot-x sel)) (turned? (:rot-x sel) (:pitch e 0.0)))
       (or (nil? (:rot-y sel)) (turned? (:rot-y sel) (:yaw e 0.0)))
       (leveled? world sel e)
       (or (nil? box) (overlaps? box (entity-box e)))
       (or (nil? (:range sel)) (in-distance? (:range sel) pos e))))

(defn- ordered [world sel pos xs]
  (case (:order sel)
    :nearest (sort-by #(dist-sq pos (nth % 2)) xs)
    :furthest (sort-by #(- (dist-sq pos (nth % 2))) xs)
    :random (sort-by #(random/of-key [(:tick world) :selector
                                      (first %)])
                     xs)
    xs))

(defn- by-uuid [world players? u]
  (some (fn [[id _ e :as x]]
          (when (= u (entity/uuid-of id e)) x))
        (if players? (player-entries world) (entity-entries world))))

(defn- by-name [world nm]
  (some #(when (= nm (:name (nth % 2))) %) (player-entries world)))

(defn- self-entry [world eid sel ok?]
  (let [x [eid (:dim world) (get-in world [:entities eid])]]
    (when (and (peek x) (ok? x)
               (or (:entities? sel) (= :player (:type (peek x)))))
      [x])))

(defn- candidates [world sel]
  (let [xs (if (:entities? sel)
             (entity-entries world)
             (player-entries world))
        dim (source-dim world)]
    (if (:world? sel) (filter #(= dim (second %)) xs) xs)))

(defn- selected
  "EntitySelector.findEntities, and findPlayers when sel includes no
  other entities."
  [world eid sel]
  (let [pos (sel-pos world sel) box (sel-box sel pos)
        ok? #(selects? world sel pos box %)]
    (cond
      (:name sel) (keep identity [(by-name world (:name sel))])
      (:uuid sel) (keep identity [(by-uuid world false (:uuid sel))])
      (:self? sel) (self-entry world eid sel ok?)
      :else (->> (filter ok? (candidates world sel))
                 (ordered world sel pos)
                 (take (:limit sel))))))

(defn- player-selected [world eid sel]
  (if (:uuid sel)
    (keep identity [(by-uuid world true (:uuid sel))])
    (selected world eid sel)))

(declare rel-bits)

(defn- crossed [lv eid dim pos [yaw pitch] rel]
  (let [e (get-in lv [:entities eid])
        known (sort-by chunk/id->pos (seq (:sent-chunks e)))
        seen (sort (seq (:tracking e)))
        sent (if (set? rel)
               [0.0 0.0 (rel-bits rel false)]
               [yaw pitch 0])]
    [[:change-dimension eid dim pos yaw pitch]
     (out/to eid (out/change-dimension dim pos sent known seen))]))

(defn- rel-bits [rel same?]
  (reduce (fn [^long b ^long a]
            (cond-> (bit-or b (bit-shift-left 1 (+ 5 a)))
              same? (bit-or (bit-shift-left 1 a))))
          (cond-> 0 (:y-rot rel) (bit-or 8) (:x-rot rel) (bit-or 16))
          (filter int? rel)))

(defn- sent-angle [rel? v old]
  (if rel? (wrapped (- (double v) (double old))) v))

(defn- sent-turn [e [yaw pitch] rel]
  [(sent-angle (:y-rot rel) yaw (:yaw e 0.0))
   (sent-angle (:x-rot rel) pitch (:pitch e 0.0))])

(defn- known-vel [e]
  (if (= :player (:type e)) (:client-vel e) (:vel e)))

(defn- kept-vel [e rel grounded?]
  (let [old (or (known-vel e) still)
        at #(if (contains? rel %) (double (nth (xyz old) %)) 0.0)]
    (v/v3 [(at 0) (if grounded? 0.0 (at 1)) (at 2)])))

(defn- packet-pos [e pos rel]
  (let [p (:pos e)]
    (mapv (fn [a] (if (contains? rel a)
                    (- (double (nth pos a)) (double (nth (xyz p) a)))
                    (double (nth pos a))))
          [0 1 2])))

(defn- player-teleport [id e pos turn rel]
  (let [[yaw pitch] turn]
    (if (= :entity rel)
      [(out/to id (out/teleport pos yaw pitch))]
      (let [bits (rel-bits rel true)
            at (packet-pos e pos rel)
            [y p] (sent-turn e turn rel)]
        [(out/to id (out/teleport at y p bits))]))))

(defn- stood [ds e]
  (let [of (fn [tag] (some #(when (= tag (nth % 0)) (nth % 2)) ds))]
    (assoc e :pos (v/v3 (of :teleport)) :sleeping nil
           :yaw (:yaw (of :merge-entity)) :pitch 0.0)))

(defn- woken [lv id e]
  (if (:sleeping e)
    (let [ds (vec (sleep/wake-deltas lv id))
          left (dec (count (sleep/sleepers lv)))]
      [(conj (pop ds) (sleep/announcement lv left) (peek ds))
       (stood ds e)])
    [nil e]))

(defn- shifted [pos rel e0 e]
  (let [from (xyz (:pos e0)) to (xyz (:pos e))]
    (mapv (fn [a]
            (if (contains? rel a)
              (+ (- (double (nth pos a)) (double (nth from a)))
                 (double (nth to a)))
              (nth pos a)))
          [0 1 2])))

(defn- player-placed [id e pos [yaw pitch :as turn] rel]
  (into [[:teleport id pos]
         [:merge-entity id
          {:yaw yaw :pitch pitch :head-yaw yaw :on-ground true
           :vel (kept-vel e (if (set? rel) rel #{}) true)}]]
        (player-teleport id e pos turn rel)))

(defn- player-moved [world id from e0 to pos turn rel]
  (let [lv (level-view world from)
        [wake e] (woken lv id e0)
        own? (set? rel)
        turn (if (:own-turn rel) [(:yaw e 0.0) (:pitch e 0.0)] turn)]
    (in-level
      world from
      (concat
        wake
        (if (= from to)
          (let [pos (if own? (shifted pos rel e0 e) pos)]
            (player-placed id e pos turn rel))
          (crossed lv id to pos turn rel))))))

(defn- placed-props [e pos yaw pitch rel]
  (cond-> {:pos (v/v3 pos) :yaw yaw :pitch pitch :head-yaw yaw
           :vel (kept-vel e rel true) :on-ground true}
    (:nav e) (assoc :nav (:nav (nav/stop e)))))

(defn- recreated [world id e pos yaw pitch rel]
  (let [t (:tick world)]
    (when-let [m (entity/loaded (entity/saved e t) t)]
      (assoc m :pos (v/v3 pos) :yaw yaw :pitch pitch :head-yaw yaw
             :vel (kept-vel e rel false)
             :uuid (entity/uuid-of id e)))))

(defn- arrival-chunk [world to pos]
  (let [lv (level-view world to)
        cid (chunk/block-chunk (mapv #(long (Math/floor %)) pos))]
    (when-not (or (contains? (:chunks lv) cid)
                  (contains? (:loading lv) cid))
      (chunks/read-absent-deltas
       {cid (chunks/read-absent lv cid)}))))

(defn- entity-moved [world id from e to pos [yaw pitch] rel]
  (let [rel (if (set? rel) rel #{})]
    (if (= from to)
      (let [m (placed-props e pos yaw pitch rel)]
        (in-level world from [[:merge-entity id m]]))
      (concat
        (in-level world from [[:remove-entity id]])
        (when-let [m (recreated world id e pos yaw pitch rel)]
          (let [ds (arrival-chunk world to pos)]
            (in-level world to (concat ds [[:spawn-entity m]]))))))))

(defn- moved
  [world [id from e] to pos [yaw pitch] rel]
  (let [turn [(double yaw) (double pitch)]]
    (if (= :player (:type e))
      (player-moved world id from e to pos turn rel)
      (entity-moved world id from e to pos turn rel))))

(defn- coord [x]
  (String/format Locale/ROOT "%f" (object-array [(double x)])))

(defn- pos-report [eid placed pos]
  (let [at (mapv coord pos)]
    (if (= 1 (count placed))
      (say eid "commands.teleport.success.location.single"
           (entity-name (nth (first placed) 2)) (at 0) (at 1) (at 2))
      (say eid "commands.teleport.success.location.multiple"
           (count placed) (at 0) (at 1) (at 2)))))

(defn- own-turn [[_ _ e]] [(:yaw e 0.0) (:pitch e 0.0)])

(def ^:private own-rotation #{:y-rot :x-rot :own-turn})

(defn- tp-moves [world placed pos turn-of look]
  (let [to (source-dim world)
        rel (get-in world [:source :relative] #{})]
    (mapcat (fn [x]
              (let [[t r] (turn-of x)]
                (concat (moved world x to pos t (into rel r))
                        (when look (look x pos)))))
            placed)))

(defn- tp-at
  ([world eid placed pos] (tp-at world eid placed pos nil nil))
  ([world eid placed pos turn-of look]
   (cond
     (empty? placed) (fail eid "argument.entity.notfound.entity")
     (not (spawnable? pos))
     (fail eid "commands.teleport.invalidPosition")
     :else
     (concat (tp-moves world placed pos
                       (or turn-of
                           (fn [x] [(own-turn x) own-rotation]))
                       look)
             (pos-report eid placed pos)))))

(defn- tp-pos-deltas [world eid placed pos]
  (tp-at world eid placed pos))

(defn- entity-report [eid placed d]
  (if (= 1 (count placed))
    (say eid "commands.teleport.success.entity.single"
         (entity-name (nth (first placed) 2)) (entity-name d))
    (say eid "commands.teleport.success.entity.multiple"
         (count placed) (entity-name d))))

(defn- to-entity [world eid placed dim d]
  (let [pos (vec (xyz (:pos d))) turn [(:yaw d 0.0) (:pitch d 0.0)]]
    (concat (mapcat #(moved world % dim pos turn :entity) placed)
            (entity-report eid placed d))))

(defn- tp-entity-deltas [world eid placed sel]
  (let [[_ dim d] (first (selected world eid sel))]
    (cond
      (or (nil? d) (empty? placed))
      (fail eid "argument.entity.notfound.entity")
      (not (spawnable? (xyz (:pos d))))
      (fail eid "commands.teleport.invalidPosition")
      :else (to-entity world eid placed dim d))))

(defn- self [world eid]
  (selected world eid {:self? true :entities? true :limit 1}))

(defn- tp-deltas [world eid pos]
  (tp-pos-deltas world eid (self world eid) (vec pos)))

(defn- tp-to-deltas [world eid [sel]]
  (tp-entity-deltas world eid (self world eid) sel))

(defn- tp-targets-deltas [world eid [sel & pos]]
  (tp-pos-deltas world eid (selected world eid sel) (vec pos)))

(declare f32 pitch-set look-angles anchored rotated)

(defn- source-turn [world eid [ry yv] [rp pv]]
  (let [src (get-in world [:entities eid])]
    [(f32 (+ (double yv) (if ry (double (:yaw src 0.0)) 0.0)))
     (f32 (+ (double pv) (if rp (double (:pitch src 0.0)) 0.0)))]))

(defn- rotated-turn [[_ _ e] [yaw pitch] ry rp]
  (let [old-y (double (:yaw e 0.0)) old-p (double (:pitch e 0.0))]
    [[(if ry (+ old-y (wrapped (- (double yaw) old-y))) (wrapped yaw))
      (pitch-set (if rp (+ old-p (wrapped (- (double pitch) old-p)))
                     (wrapped pitch)))]
     (cond-> #{} ry (conj :y-rot) rp (conj :x-rot))]))

(defn- tp-rotated-deltas [world eid [sel x y z yaw pitch]]
  (let [turn (source-turn world eid yaw pitch)]
    (tp-at world eid (selected world eid sel) [x y z]
           #(rotated-turn % turn (first yaw) (first pitch)) nil)))

(defn- look-from [world target fx-of]
  (fn [[id dim e] pos]
    (let [[yaw pitch] (look-angles pos target)]
      (rotated world id dim yaw pitch
               (when (= :player (:type e)) (fx-of target))))))

(defn- tp-facing-deltas [world eid [sel x y z fx fy fz]]
  (let [target [fx fy fz]]
    (tp-at world eid (selected world eid sel) [x y z] nil
           (look-from world target #(out/look-at :feet % nil nil)))))

(defn- tp-facing-entity-deltas [world eid [sel x y z other anchor]]
  (let [[oid _ o] (first (selected world eid other))
        anchor (or anchor :feet)]
    (if o
      (tp-at world eid (selected world eid sel) [x y z] nil
             (look-from world (anchored o anchor)
                        #(out/look-at :feet % oid anchor)))
      (fail eid "argument.entity.notfound.entity"))))

(defn- tp-targets-to-deltas [world eid [sel dest]]
  (tp-entity-deltas world eid (selected world eid sel) dest))

(defn- given [world eid [id dim e] proto n]
  (let [lv (level-view world dim)
        inv (or (:inventory e) {})
        item (:item proto)
        [changes left] (items/add-stack inv (assoc proto :count n))
        drop #(items/dropped lv id % true 0)]
    (concat
      (in-level world dim
                (concat
                  (map (fn [[slot stack]] [:set-slot id slot stack])
                       changes)
                  (when left [[:spawn-entity (drop left)]])))
      (say eid "commands.give.success.single"
           n (item-name item) (entity-name e)))))

(defn- give-limit [proto]
  (* 100 (long (or (stack/component proto :max-stack-size) 1))))

(defn- give-deltas [world eid [sel input n]]
  (let [xs (player-selected world eid sel)
        proto (item-args/item-stack input 1)
        most (when-not (cmd-reader/error? proto) (give-limit proto))]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (cmd-reader/error? proto)
      (apply fail eid (:key proto) (:args proto))
      (> (long n) (long most))
      (fail eid "commands.give.failed.toomanyitems" most
            (item-name (:item proto)))
      :else (mapcat #(given world eid % proto n) xs))))

(defn- killed [world [id dim e]]
  (in-level world dim
            (cond
              (not= :player (:type e)) [[:remove-entity id]]
              (player/client-loaded? e (long (:tick world)))
              [[:merge-entity id {:health 0.0}]])))

(defn- kill-report [eid xs]
  (if (= 1 (count xs))
    (say eid "commands.kill.success.single"
         (entity-name (nth (first xs) 2)))
    (say eid "commands.kill.success.multiple" (count xs))))

(defn- kill-deltas [world eid [sel]]
  (let [xs (selected world eid sel)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (concat (mapcat #(killed world %) xs)
              (kill-report eid xs)))))

(defn- summoned [world eid type at]
  (let [t (:tick world)
        dim (source-dim world)
        kind {:translate (str "entity.minecraft." (name type))}
        msg {:translate "commands.summon.success" :with [kind]}
        mob (mobs/command-mob type at [t eid :summon at] t dim)]
    (concat (in-level world dim [[:spawn-entity mob]])
            (success [(out/to eid (out/system-chat msg))]))))

(defn- summon-deltas [world eid [type x y z]]
  (let [p (source-pos world)
        at [(double (or x (nth p 0)))
            (double (or y (nth p 1)))
            (double (or z (nth p 2)))]]
    (cond
      (not (mobs/mob-type? type)) (fail eid "commands.summon.failed")
      (spawnable? at) (summoned world eid type at)
      :else (fail eid "commands.summon.invalidPosition"))))

(defn- place-needed? [world pos st mode]
  (let [old (edit/block-at (source-level world) pos)
        left (if (block/air-type? old) old (block/emptied old))]
    (or (not= "destroy" mode) (not (block/air-type? st))
        (not (block/air-type? left)))))

(defn- block-set [world eid [x y z :as pos] st mode]
  (let [put? (place-needed? world pos st mode)
        [_ ds _ placed] (edited world [[pos (when put? st)]]
                                (mode-opts mode nil))]
    (if (and put? (zero? (long placed)))
      (fail eid "commands.setblock.failed")
      (concat ds (say eid "commands.setblock.success" x y z)))))

(defn- setblock-deltas [world eid [x y z block mode]]
  (let [pos [x y z]
        lv (source-level world)
        k (pos-error lv pos)]
    (cond
      k (fail eid k)
      (and (= "keep" mode)
           (not (block/air-type? (edit/block-at lv pos))))
      (fail eid "commands.setblock.failed")
      :else (block-set world eid pos (:state block) mode))))

(defn- block-under [world [x y z]]
  (let [p (source-pos world)
        at #(long (Math/floor (double (nth p %))))]
    [(long (or x (at 0)))
     (long (or y (at 1)))
     (long (or z (at 2)))]))

(defn- dimension-id [dim] (str "minecraft:" (data/snake dim)))

(defn- wrapped [a]
  (let [r (float (rem (float a) (float 360.0)))]
    (cond
      (>= r (float 180.0)) (float (- r (float 360.0)))
      (< r (float -180.0)) (float (+ r (float 360.0)))
      :else r)))

(defn- turn-of [world eid yaw pitch]
  (let [e (get-in world [:entities eid])
        get (fn [[rel? v] own]
              (let [base (if rel? (double (or own 0.0)) 0.0)]
                (float (+ (double v) base))))
        y (if yaw (get yaw (:yaw e)) (float 0.0))
        p (if pitch (get pitch (:pitch e)) (float 0.0))]
    [(wrapped y) (float (max -90.0 (min 90.0 (double p))))]))

(defn- out-of-bounds? [pos]
  (and (every? some? pos) (not (spawnable? pos))))

(defn- same-spawn? [world dim at turn]
  (and (= dim (:world-spawn-dimension world :overworld))
       (= at (vec (:world-spawn world)))
       (= (mapv double turn) (player/spawn-turn world))))

(defn- world-spawn-set [world eid at [yaw pitch :as turn]]
  (let [dim (source-dim world)
        with (conj (mapv str at) (str yaw) (str pitch)
                   (dimension-id dim))
        msg {:translate "commands.setworldspawn.success" :with with}]
    (concat [[:set-world-spawn dim at [yaw pitch]]]
            (when-not (same-spawn? world dim at turn)
              [(out/everyone (out/default-spawn dim at yaw pitch))])
            (success [(out/to eid (out/system-chat msg))]))))

(defn- world-spawn-deltas
  [world eid [x y z yaw pitch]]
  (if (out-of-bounds? [x y z])
    (fail eid "argument.pos.outofbounds")
    (world-spawn-set world eid (block-under world [x y z])
                     (turn-of world eid yaw pitch))))

(defn- spawn-report [eid xs with]
  (if (= 1 (count xs))
    (say* eid "commands.spawnpoint.success.single"
          (conj with (entity-name (nth (first xs) 2))))
    (say* eid "commands.spawnpoint.success.multiple"
          (conj with (count xs)))))

(defn- spawnpoint-set [world eid xs at [yaw pitch]]
  (let [dim (source-dim world)
        spawn {:dimension dim :pos at :yaw (double yaw)
               :pitch (double pitch)}
        set (fn [[id d]]
              (in-level world d
                        [[:merge-entity id {:forced-spawn spawn}]]))
        with (conj (mapv str at) (str yaw) (str pitch)
                   (dimension-id dim))]
    (concat (mapcat set xs) (spawn-report eid xs with))))

(defn- spawnpoint-deltas [world eid [sel x y z yaw pitch]]
  (let [xs (player-selected world eid sel)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (out-of-bounds? [x y z]) (fail eid "argument.pos.outofbounds")
      :else (let [at (block-under world [x y z])
                  turn (turn-of world eid yaw pitch)]
              (spawnpoint-set world eid xs at turn)))))

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
    (cons [:set-weather m]
          (success [(out/to eid (out/system-chat msg))]))))

(defn- clock-sent [world k c]
  (let [on (clock/advancing? world)]
    [[:set-clock k c]
     (out/all (out/time (long (:tick world))
                        {k (clock/network-state c on)}))]))

(defn- marker-key [id]
  (str "ResourceKey[minecraft:clock_time_marker / " id "]"))

(defn- to-marker [world eid k id]
  (let [c0 (clock/state world k)
        c (clock/moved-to c0 k id)]
    (concat (clock-sent world k (or c c0))
            (if c
              (say eid "commands.time.set.time_marker"
                   (data/wire k) id)
              (fail eid "commands.time.no_time_marker_found"
                    (marker-key id) (data/wire k))))))

(defn- paused [p] (fn [c _] (assoc c :paused p)))

(def ^:private clock-edits
  {:time-set [clock/set-ticks "commands.time.set.absolute"
              (fn [_ v] [v])]
   :time-add [clock/added "commands.time.set.absolute"
              (fn [c _] [(bigint (:total-ticks c))])]
   :time-pause [(paused true) "commands.time.pause" (fn [_ _] [])]
   :time-resume [(paused false) "commands.time.resume"
                 (fn [_ _] [])]
   :time-rate [(fn [c v] (assoc c :rate (double v)))
               "commands.time.rate" (fn [_ v] [v])]})

(defn- clock-changed [world eid k v [f key with]]
  (let [c (f (clock/state world k) v)]
    (concat (clock-sent world k c)
            (say* eid key (into [(data/wire k)] (with c v))))))

(defn- in-timeline [world eid k id f key]
  (let [t (clock/timeline-of (data/wire id))
        n (clock/ticks world k)]
    (if (= k (:clock t))
      (answer (say eid key (data/wire id) (bigint (f t n))))
      (fail eid "commands.time.wrong_timeline_for_clock"
            (data/wire id) (data/wire k)))))

(def ^:private timeline-reads
  {:time-timeline [clock/timeline-ticks
                   "commands.time.query.timeline"]
   :time-repetition [clock/repetitions
                     "commands.time.query.timeline.repetitions"]})

(defn- clock-query [world eid k]
  (answer (say eid "commands.time.query.absolute" (data/wire k)
               (bigint (clock/ticks world k)))))

(defn- clock-deltas [world eid op k v]
  (if-let [e (clock-edits op)]
    (clock-changed world eid k v e)
    (case op
      :time-marker (to-marker world eid k v)
      :time-query (clock-query world eid k)
      (let [[f key] (timeline-reads op)]
        (in-timeline world eid k v f key)))))

(defn- time-deltas [world eid op [k v]]
  (let [dim (source-dim world)
        k (or k (clock/default-of dim))]
    (cond
      (= :time-gametime op)
      (answer (say eid "commands.time.query.gametime"
                   (bigint (:tick world))))
      (nil? k) (fail eid "commands.time.no_default_clock"
                     (data/wire dim))
      :else (clock-deltas world eid op k v))))

(defn- weather-command-deltas [world eid op args]
  (let [given (first args)]
    (case op
      :weather-clear (weather-deltas world eid :clear given)
      :weather-rain (weather-deltas world eid :rain given)
      :weather-thunder (weather-deltas world eid :thunder given))))

(defn- mode-name [mode]
  {:translate (str "gameMode." (name mode))})

(defn- told-of-mode [world id mode]
  (when (get-in world [:rules :send-command-feedback] true)
    [(out/to id (out/system-chat
                  {:translate "gameMode.changed"
                   :with [(mode-name mode)]}))]))

(defn- mode-report
  [world eid [id _ e] mode]
  (if (= id eid)
    (say eid "commands.gamemode.success.self" (mode-name mode))
    (concat (told-of-mode world id mode)
            (say eid "commands.gamemode.success.other"
                 (entity-name e) (mode-name mode)))))

(defn- mode-change [world [id dim e] mode]
  (game-mode/change (level-view world dim) id e mode))

(defn- mode-set
  [world eid mode [_ dim :as x]]
  (when-let [ds (seq (mode-change world x mode))]
    (concat (in-level world dim ds) (mode-report world eid x mode))))

(defn- gamemode-deltas [world eid [mode sel]]
  (let [xs (player-selected world eid sel)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapcat #(mode-set world eid mode %) xs))))

(defn- enforced [world mode]
  (when mode
    (mapcat (fn [[_ dim :as x]]
              (in-level world dim (mode-change world x mode)))
            (player-entries world))))

(defn- default-mode-deltas
  [world eid [mode]]
  (let [cfg (assoc (:config world) :game-mode mode)
        forced (game-mode/forced-mode (assoc world :config cfg))]
    (concat [[:set-config cfg]]
            (enforced world forced)
            (say eid "commands.defaultgamemode.success"
                 (mode-name mode)))))

(defn- reload-deltas [_world eid _args]
  (cons (out/to eid (out/reload))
        (say eid "commands.reload.success")))

(defn- camera-set
  [world [id dim e :as x] [tid tdim t]]
  (if (or (nil? t) (= dim tdim))
    (let [lv (level-view world dim)]
      (in-level world dim (camera/set-deltas lv id e tid)))
    (concat (moved world x tdim (vec (xyz (:pos t))) (own-turn x)
                   own-rotation)
            (in-level world tdim
                      [[:merge-entity id {:camera tid}]
                       (out/to id (out/camera tid))]))))

(defn- spectate-report [eid t]
  (let [k (if t "started" "stopped")
        msg {:translate (str "commands.spectate.success." k)
             :with (if t [(entity-name t)] [])}]
    (answer [(out/to eid (out/system-chat msg))])))

(defn- spectated
  [world eid [id _ e :as x] [tid _ t :as target]]
  (cond
    (= id tid) (fail eid "commands.spectate.self")
    (not (game-mode/spectator? e))
    (fail eid "commands.spectate.not_spectator" (entity-name e))
    :else (concat (camera-set world x target)
                  (spectate-report eid t))))

(defn- spectate-deltas [world eid [target player]]
  (let [x (first (player-selected world eid player))
        t (when target (first (selected world eid target)))]
    (cond
      (nil? x) (fail eid "argument.entity.notfound.player")
      (and target (nil? t))
      (fail eid "argument.entity.notfound.entity")
      :else (spectated world eid x t))))

(defn- entity-tp-deltas [world eid u]
  (let [x [eid (:dim world) (get-in world [:entities eid])]
        [_ dim d] (by-uuid world false u)]
    (when (and d (game-mode/spectator? (nth x 2)))
      (concat (camera/set-deltas world eid (nth x 2) nil)
              (moved world x dim (vec (xyz (:pos d)))
                     [(:yaw d 0.0) (:pitch d 0.0)] :entity)))))

(defn- effect-title [k]
  {:translate (str "effect.minecraft." (data/snake k))})

(defn- given-ticks ^long [k secs]
  (cond
    (nil? secs) (if (effect/instant? k) 1 600)
    (= :infinite secs) effect/infinite
    (effect/instant? k) (long secs)
    :else (* 20 (long secs))))

(defn- effect-changed [world [id dim e] f]
  (when (effects/living? e)
    (let [acc (f (effects/account id e))]
      (when (:landed? acc)
        (or (in-level world dim (effects/deltas acc e)) [])))))

(defn- effect-report [eid xs n key-of with]
  (if (= 1 (count xs))
    (apply say eid (key-of "single")
           (with (entity-name (nth (first xs) 2))))
    (apply say eid (key-of "multiple") (with n))))

(defn- effect-run [world eid xs f fail-key key-of with]
  (let [dss (keep #(effect-changed world % f) xs)]
    (if (empty? dss)
      (fail eid fail-key)
      (concat (apply concat dss)
              (effect-report eid xs (count xs) key-of with)))))

(defn- effect-give-deltas [world eid [sel k secs amp hide]]
  (let [xs (selected world eid sel)
        d (given-ticks k secs)
        i (effect/instance d (or amp 0) false (not hide))
        key-of #(str "commands.effect.give.success." %)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (effect-run world eid xs #(effects/land % k i)
                  "commands.effect.give.failed" key-of
                  (fn [who] [(effect-title k) who (quot d 20)])))))

(defn- clear-parts [k]
  (if k
    ["specific" #(effects/take-off % k) #(vector (effect-title k) %)]
    ["everything" effects/take-all vector]))

(defn- effect-clear-deltas [world eid [sel k]]
  (let [xs (if sel (selected world eid sel) (self world eid))
        [what f with] (clear-parts k)
        base (str "commands.effect.clear." what)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.entity")
      (effect-run world eid xs f (str base ".failed")
                  #(str base ".success." %) with))))

(defn- chimes [e acc]
  (for [vol (:chimes acc)]
    (out/all (out/sound :player/levelup (:pos e) vol 1.0))))

(defn- xp-of [world e]
  (xp/account e (- (long (:tick world)) (long (:born e 0)))))

(defn- xp-changed [world [id dim e] f]
  (let [acc (f (xp-of world e))
        ds (cons [:merge-entity id (xp/marks acc)] (chimes e acc))]
    (in-level world dim ds)))

(defn- xp-unit [unit] (or unit "points"))

(defn- xp-report [eid xs op unit amount]
  (let [base (str "commands.experience." op "." unit ".success.")]
    (if (= 1 (count xs))
      (say eid (str base "single") amount
           (entity-name (nth (first xs) 2)))
      (say eid (str base "multiple") amount (count xs)))))

(defn- xp-add-deltas [world eid [sel amount unit]]
  (let [xs (player-selected world eid sel)
        unit (xp-unit unit)
        f (if (= "levels" unit) xp/give-levels xp/give-points)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (concat (mapcat #(xp-changed world % (fn [a] (f a amount))) xs)
              (xp-report eid xs "add" unit amount)))))

(defn- settable? [world unit amount [_ _ e]]
  (or (= "levels" unit)
      (< (long amount) (xp/needed (:level (xp-of world e))))))

(defn- xp-set-deltas [world eid [sel amount unit]]
  (let [xs (player-selected world eid sel)
        unit (xp-unit unit)
        f (if (= "levels" unit) xp/set-levels xp/set-points)
        g (fn [a] (f a amount))
        ok (filter #(settable? world unit amount %) xs)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (empty? ok) (fail eid "commands.experience.set.points.invalid")
      :else (concat (mapcat #(xp-changed world % g) ok)
                    (xp-report eid xs "set" unit amount)))))

(defn- xp-query-deltas [world eid [sel unit]]
  (if-let [[_ _ e] (first (player-selected world eid sel))]
    (let [acc (xp-of world e)
          n (if (= "levels" unit) (:level acc) (xp/points acc))]
      (say eid (str "commands.experience.query." unit)
           (entity-name e) n))
    (fail eid "argument.entity.notfound.player")))

(def ^:private separator {:text ", " :color "gray"})

(defn- name-list [names]
  (case (count names)
    0 ""
    1 (first names)
    {:text "" :extra (vec (interpose separator names))}))

(defn- names-of [world eid sel]
  (name-list (map #(entity-name (nth % 2)) (selected world eid sel))))

(defn- message-step [world eid ^String text]
  (fn [[acc at] [a b sel]]
    [(cond-> acc
       (< (long at) (long a)) (conj (subs text at a))
       :always (conj (names-of world eid sel)))
     b]))

(defn- message-content
  "The text of a message argument with its selectors named."
  [world eid {:keys [^String text parts]}]
  (if (empty? parts)
    text
    (let [s0 (first (first parts))
          step (message-step world eid text)
          [extra end] (reduce step [[] s0] parts)
          tail? (< (long end) (count text))]
      {:text (subs text 0 s0)
       :extra (cond-> extra tail? (conj (subs text end)))})))

(defn- decorated
  "The chat line of chat type id with params by name."
  [id params]
  (let [pack (get (data/pack "chat_type") (str "minecraft:" id))
        {:strs [translation_key parameters style]} (get pack "chat")]
    (merge {:translate translation_key :with (mapv params parameters)}
           (update-keys style data/kebab))))

(defn- sender-name [world eid]
  (entity-name (get-in world [:entities eid])))

(defn- broadcast [id]
  (fn [world eid [m]]
    (let [content (message-content world eid m)
          who (sender-name world eid)
          line (decorated id {"sender" who "content" content})]
      [(out/everyone (out/player-chat line))])))

(defn- whispered [who content [id _ e]]
  (let [to {"target" (entity-name e) "content" content}
        from {"sender" (:name who) "content" content}
        line #(out/player-chat (decorated %1 %2))]
    [(out/to (:eid who) (line "msg_command_outgoing" to))
     (out/to id (line "msg_command_incoming" from))]))

(defn- msg-deltas [world eid [sel m]]
  (let [xs (player-selected world eid sel)
        who {:eid eid :name (sender-name world eid)}]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapcat #(whispered who (message-content world eid m) %) xs))))

(defn- tellraw-deltas [world eid [sel text]]
  (let [xs (player-selected world eid sel)]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (mapv (fn [[id]] (out/to id (out/system-chat text))) xs))))

(defn- name-and-id [[id _ e]]
  {:translate "commands.list.nameAndId"
   :with [(:name e) (str (entity/uuid-of id e))]})

(defn- list-deltas [f]
  (fn [world eid _]
    (let [xs (player-entries world)
          cfg (merge config/defaults (:config world))
          most (:max-players cfg)]
      (answer (say eid "commands.list.players" (count xs) most
                   (name-list (map f xs)))))))

(defn- title-report [eid xs key]
  (if (= 1 (count xs))
    (say eid (str key ".single") (entity-name (nth (first xs) 2)))
    (say eid (str key ".multiple") (count xs))))

(defn- titled [key fx]
  (fn [world eid [sel & more]]
    (let [xs (player-selected world eid sel)
          m (apply fx more)]
      (if (empty? xs)
        (fail eid "argument.entity.notfound.player")
        (concat (map (fn [[id]] (out/to id m)) xs)
                (title-report eid xs key))))))

(defn- shown [kind]
  (titled (str "commands.title.show." (name kind))
          #(out/title kind %)))

(defn- help-deltas [world eid [text]]
  (let [lv (player/permission-level (get-in world [:entities eid]))
        extra (cmd/extra-of world)
        lines (cmd/help-lines lv text (source-pos world) extra)]
    (if (nil? lines)
      (fail eid "commands.help.failed")
      (answer (mapv #(out/to eid (out/system-chat %)) lines)))))

(defn- sound-range-sq ^double [volume]
  (let [v (float volume)
        r (float (if (> v (float 1.0)) (* (float 16.0) v) 16.0))]
    (double (* r r))))

(defn- heard
  "The position and volume player e hears a sound at, nil when out of
  range (PlaySoundCommand.playSound)."
  [e pos volume min-volume]
  (let [[px py pz] (xyz (:pos e))
        d (mapv - pos [px py pz])
        sq (reduce + (map * d d))]
    (cond
      (<= sq (sound-range-sq volume)) [pos volume]
      (<= (double min-volume) 0.0) nil
      :else (let [n (Math/sqrt sq)]
              [(mapv #(+ %1 (* (/ %2 n) 2.0)) [px py pz] d)
               min-volume]))))

(defn- sound-listeners [world eid sel]
  (let [dim (source-dim world)
        xs (if sel (player-selected world eid sel) (self world eid))]
    (filter #(= dim (nth % 1)) xs)))

(defn- sound-report [eid played id]
  (if (= 1 (count played))
    (say eid "commands.playsound.success.single" id
         (entity-name (nth (first played) 2)))
    (say eid "commands.playsound.success.multiple" id
         (count played))))

(defn- playsound-result [world eid sel id played]
  (cond
    (and sel (empty? (player-selected world eid sel)))
    (fail eid "argument.entity.notfound.player")
    (empty? played) (fail eid "commands.playsound.failed")
    :else (concat (map second played)
                  (sound-report eid (map first played) id))))

(defn- playsound-deltas
  [world eid [src id sel x y z volume pitch min-volume]]
  (let [pos (if (some? x) [x y z] (vec (source-pos world)))
        volume (float (or volume 1.0))
        least (or min-volume 0.0)
        seed (random/mix64 (hash [(:tick world) eid id]))
        src (or src "master") pitch (or pitch 1.0)
        sound #(out/named-sound id src %1 %2 pitch seed)
        play (fn [[pid _ e :as x]]
               (when-let [[at v] (heard e pos volume least)]
                 [x (out/to pid (sound at v))]))
        played (keep play (sound-listeners world eid sel))]
    (playsound-result world eid sel id played)))

(defn- stop-report [src id]
  (let [src (when (not= "*" src) src)]
    (cond
      (and src id) ["commands.stopsound.success.source.sound" id src]
      src ["commands.stopsound.success.source.any" src]
      id ["commands.stopsound.success.sourceless.sound" id]
      :else ["commands.stopsound.success.sourceless.any"])))

(defn- stopsound-deltas [world eid [src sel id]]
  (let [xs (player-selected world eid sel)
        m (out/stop-sound id (when (not= "*" src) src))]
    (if (empty? xs)
      (fail eid "argument.entity.notfound.player")
      (concat (map (fn [[pid]] (out/to pid m)) xs)
              (apply say eid (stop-report src id))))))

(def ^:private inventory-order
  "Menu slots in the order Inventory counts them: hotbar, main,
  feet to head, offhand, then the crafting grid."
  (vec (concat (range 36 45) (range 9 36) [8 7 6 5 45] [1 2 3 4])))

(defn- taken ^long [pred ^long limit ^long counted stack]
  (let [n (stack/size stack)]
    (cond
      (not (and stack (item-args/matches? pred stack))) 0
      (zero? limit) n
      (neg? (- limit counted)) n
      :else (min (- limit counted) n))))

(defn- shrunk [stack ^long k]
  (when (< k (stack/size stack)) (update stack :count - k)))

(defn- clear-step [pred limit]
  (fn [[n changes] [slot stack]]
    (let [k (taken pred limit n stack)]
      [(+ (long n) k)
       (cond-> changes
         (and (pos? k) (not (zero? (long limit))))
         (conj [slot (shrunk stack k)]))])))

(defn- cleared
  "How many items pred matches on player e, and the slot changes
  that take at most limit of them; limit 0 only counts, -1 takes
  all (Inventory.clearOrCountMatchingItems)."
  [e pred limit]
  (let [inv (:inventory e)
        stacks (conj (mapv (fn [s] [s (get inv s)]) inventory-order)
                     [:carried (:carried e)])]
    (reduce (clear-step (or pred {:type nil :tests []}) limit)
            [0 []] stacks)))

(defn- clear-change [id [slot stack]]
  (if (= :carried slot)
    [:merge-entity id {:carried stack}]
    [:set-slot id slot stack]))

(defn- clear-report [eid xs n limit]
  (let [kind (if (zero? (long limit)) "test" "success")]
    (if (= 1 (count xs))
      (say eid (str "commands.clear." kind ".single") n
           (entity-name (nth (first xs) 2)))
      (say eid (str "commands.clear." kind ".multiple") n
           (count xs)))))

(defn- clear-failure [eid xs]
  (if (= 1 (count xs))
    (fail eid "clear.failed.single" (:name (nth (first xs) 2)))
    (fail eid "clear.failed.multiple" (count xs))))

(defn- cleared-deltas [world [id dim [_ cs]]]
  (in-level world dim (map #(clear-change id %) cs)))

(defn- clear-deltas [world eid [sel pred limit]]
  (let [xs (if sel (player-selected world eid sel) (self world eid))
        limit (long (or limit -1))
        rs (map (fn [[id dim e]] [id dim (cleared e pred limit)]) xs)
        n (reduce + (map #(first (nth % 2)) rs))]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.player")
      (zero? (long n)) (clear-failure eid xs)
      :else (concat (mapcat #(cleared-deltas world %) rs)
                    (clear-report eid xs n limit)))))

(def ^:private tag-limit 1024)

(defn- tag-added [world name [id dim e]]
  (let [tags (or (:tags e) #{})]
    (when-not (or (contains? tags name)
                  (>= (count tags) (long tag-limit)))
      (in-level world dim
                [[:merge-entity id {:tags (conj tags name)}]]))))

(defn- tag-removed [world name [id dim e]]
  (when (contains? (:tags e) name)
    (in-level world dim
              [[:merge-entity id {:tags (disj (:tags e) name)}]])))

(defn- tag-report [eid xs base name]
  (if (= 1 (count xs))
    (say eid (str base "single") name
         (entity-name (nth (first xs) 2)))
    (say eid (str base "multiple") name (count xs))))

(defn- tag-changed [f op]
  (fn [world eid [sel name]]
    (let [xs (selected world eid sel)
          dss (keep #(f world name %) xs)
          base (str "commands.tag." op ".success.")]
      (cond
        (empty? xs) (fail eid "argument.entity.notfound.entity")
        (empty? dss) (fail eid (str "commands.tag." op ".failed"))
        :else (concat (apply concat dss)
                      (tag-report eid xs base name))))))

(defn- tag-names [tags]
  (name-list (mapv (fn [t] {:text t :color "green"}) (sort tags))))

(defn- tag-list [eid xs tags]
  (let [one? (= 1 (count xs))
        who (when one? (entity-name (nth (first xs) 2)))
        n (count tags)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.entity")
      (and one? (zero? n))
      (say eid "commands.tag.list.single.empty" who)
      one? (say eid "commands.tag.list.single.success" who n
                (tag-names tags))
      (zero? n)
      (say eid "commands.tag.list.multiple.empty" (count xs))
      :else (say eid "commands.tag.list.multiple.success" (count xs)
                 n (tag-names tags)))))

(defn- tag-list-deltas [world eid [sel]]
  (let [xs (selected world eid sel)
        tags (into #{} (mapcat #(:tags (nth % 2))) xs)]
    (answer (tag-list eid xs tags))))

(defn- swung [world hand [id dim e]]
  (when (effects/living? e)
    (let [ds (player/swing-deltas id e hand (:tick world) true)]
      (or (in-level world dim ds) []))))

(defn- swing-report [eid xs n]
  (if (= 1 n)
    (say eid "commands.swing.success.single"
         (entity-name (nth (first xs) 2)))
    (say eid "commands.swing.success.multiple" n)))

(defn- swing-deltas [world eid [hand sel]]
  (let [xs (if sel (selected world eid sel) (self world eid))
        dss (keep #(swung world (or hand :main) %) xs)]
    (cond
      (empty? xs) (fail eid "argument.entity.notfound.entity")
      (empty? dss) (fail eid "commands.swing.failed.notliving")
      :else (concat (apply concat dss)
                    (swing-report eid xs (count dss))))))

(defn- f32 ^double [x] (double (float x)))

(defn- pitch-set ^double [x]
  (f32 (max -90.0 (min 90.0 (f32 (rem (float x) (float 360.0)))))))

(defn- turned-to
  "The yaw and pitch of Entity.forceSetRotation from the rotation
  argument, and the values the player packet carries."
  [world eid e [ry yv] [rp pv]]
  (let [src (get-in world [:entities eid])
        arg #(f32 (if %1 (+ (double %2) (double (or %3 0.0))) %2))
        y (arg ry yv (:yaw src)) x (arg rp pv (:pitch src))
        dy (if ry (f32 (- y (f32 (:yaw e 0.0)))) y)
        dx (if rp (f32 (- x (f32 (:pitch e 0.0)))) x)
        ay (if ry (f32 (+ (f32 (:yaw e 0.0)) dy)) dy)
        ax (if rp (f32 (+ (f32 (:pitch e 0.0)) dx)) dx)
        sent [dy (boolean ry) dx (boolean rp)]]
    [ay (pitch-set (max -90.0 (min 90.0 ax))) sent]))

(defn- rotated [world id dim yaw pitch fx]
  (let [turn {:yaw yaw :head-yaw yaw :pitch pitch}]
    (in-level world dim
              (cond-> [[:merge-entity id turn]]
                fx (conj (out/to id fx))))))

(defn- rotate-report [eid e]
  (say eid "commands.rotate.success" (entity-name e)))

(defn- rotate-deltas [world eid [sel yaw pitch]]
  (if-let [[id dim e] (first (selected world eid sel))]
    (let [[ay ax sent] (turned-to world eid e yaw pitch)
          fx (when (= :player (:type e))
               (apply out/player-rotation sent))]
      (concat (rotated world id dim ay ax fx) (rotate-report eid e)))
    (fail eid "argument.entity.notfound.entity")))

(def ^:private deg (f32 (/ 180.0 (f32 Math/PI))))

(defn- look-angles
  "The yaw and pitch of Entity.lookAt from feet at from to pos."
  [[fx fy fz] [px py pz]]
  (let [xd (- (double px) (double fx)) yd (- (double py) (double fy))
        zd (- (double pz) (double fz))
        sd (Math/sqrt (+ (* xd xd) (* zd zd)))
        pitch (wrapped (float (- (* (Steer/atan2 yd sd) deg))))
        turn (float (* (Steer/atan2 zd xd) deg))
        yaw (wrapped (- turn (float 90.0)))]
    [(f32 yaw) (pitch-set pitch)]))

(defn- anchored [e anchor]
  (let [[x y z] (xyz (:pos e))]
    (if (= :eyes anchor)
      [x (+ (double y) (double (float (entity/eye-height e)))) z]
      [x y z])))

(defn- faced [world eid [id dim e] pos fx]
  (let [[yaw pitch] (look-angles (xyz (:pos e)) pos)]
    (concat (rotated world id dim yaw pitch
                     (when (= :player (:type e)) fx))
            (rotate-report eid e))))

(defn- facing-deltas [world eid [sel x y z]]
  (if-let [x0 (first (selected world eid sel))]
    (faced world eid x0 [x y z] (out/look-at :feet [x y z] nil nil))
    (fail eid "argument.entity.notfound.entity")))

(defn- facing-entity-deltas [world eid [sel other anchor]]
  (let [x0 (first (selected world eid sel))
        [oid _ o] (first (selected world eid other))
        anchor (or anchor :feet)]
    (if (and x0 o)
      (let [pos (anchored o anchor)]
        (faced world eid x0 pos (out/look-at :feet pos oid anchor)))
      (fail eid "argument.entity.notfound.entity"))))

(defn- version-lines
  "The lines of VersionCommand.dumpVersion after its header."
  [{:keys [id data series protocol build-time resource-pack data-pack
           stable] :as v}]
  (let [t (fn [k with]
            {:translate (str "commands.version." k) :with with})
        stability (if stable "yes" "no")]
    [(t "id" [id]) (t "name" [(:name v)]) (t "data" [data])
     (t "series" [series])
     (t "protocol" [protocol (str "0x" (Long/toHexString protocol))])
     (t "build_time" [(str (Date. (long build-time)))])
     (t "pack.resource" [resource-pack]) (t "pack.data" [data-pack])
     {:translate (str "commands.version.stable." stability)}]))

(defn- version-deltas [_world eid _]
  (mapv #(out/to eid (out/system-chat %))
        (cons {:translate "commands.version.header"}
              (version-lines (data/version)))))

(def ^:private chat-commands
  {:say (broadcast "say_command") :me (broadcast "emote_command")
   :msg msg-deltas :tellraw tellraw-deltas :help help-deltas
   :playsound playsound-deltas :stopsound stopsound-deltas
   :clear clear-deltas
   :tag-add (tag-changed tag-added "add")
   :tag-remove (tag-changed tag-removed "remove")
   :tag-list tag-list-deltas :swing swing-deltas
   :rotate rotate-deltas :rotate-facing facing-deltas
   :rotate-facing-entity facing-entity-deltas :version version-deltas
   :list (list-deltas #(entity-name (nth % 2)))
   :list-uuids (list-deltas name-and-id)
   :title-clear (titled "commands.title.cleared"
                        #(out/clear-titles false))
   :title-reset (titled "commands.title.reset"
                        #(out/clear-titles true))
   :title-title (shown :title) :title-subtitle (shown :subtitle)
   :title-actionbar (shown :actionbar)
   :title-times (titled "commands.title.times" out/title-times)})

(def ^:private commands
  {:xp-add xp-add-deltas :xp-set xp-set-deltas
   :xp-query xp-query-deltas
   :effect-give effect-give-deltas :effect-clear effect-clear-deltas
   :tp tp-deltas :tp-to tp-to-deltas :tp-targets tp-targets-deltas
   :tp-targets-to tp-targets-to-deltas
   :tp-targets-rotated tp-rotated-deltas
   :tp-targets-facing tp-facing-deltas
   :tp-targets-facing-entity tp-facing-entity-deltas
   :give give-deltas
   :kill kill-deltas
   :summon summon-deltas :setblock setblock-deltas
   :setworldspawn world-spawn-deltas :spawnpoint spawnpoint-deltas
   :fill fill-deltas :fill-where fill-where-deltas
   :clone clone-deltas
   :reload reload-deltas
   :gamemode gamemode-deltas :defaultgamemode default-mode-deltas
   :spectate spectate-deltas})

(defn- world-command-deltas [world eid [tag op & args]]
  (if-let [f (if (identical? :plugin tag)
               op
               (or (commands op) (chat-commands op)))]
    (f world eid args)
    (case op
      :gamerule (rule-deltas world eid (first args) (second args))
      (:weather-clear :weather-rain :weather-thunder)
      (weather-command-deltas world eid op args)
      (:time-set :time-add :time-marker :time-pause :time-resume
       :time-rate :time-query :time-timeline :time-repetition
       :time-gametime)
      (time-deltas world eid op args)
      (tell eid (str "unknown world command: " op)))))

(defn- sourced [world r origin]
  (let [pos (if (contains? r :origin) (:origin r) origin)]
    (assoc world :source
           {:dim (:dim r (:dim world)) :pos pos
            :relative (:relative r #{})})))

(def ^:private here
  {:translate "command.context.here" :color "red" :italic true})

(defn- context [^String s at]
  (let [c (min (long at) (count s))]
    {:text "" :color "gray"
     :click {:action :suggest-command :command (str "/" s)}
     :extra (cond-> (if (> c 10) ["..."] [])
              :always (conj (subs s (max 0 (- c 10)) c))
              (< c (count s))
              (conj {:text (subs s c) :color "red"
                     :underlined true})
              :always (conj here))}))

(defn- parse-failed [eid text r]
  (let [at (:cursor r)]
    (cond-> [(out/to eid (failure (:failure r)))]
      at (conj (out/to eid (failure (context (subs text 1) at)))))))

(defn- feedback? [world ds]
  (reduce (fn [on d]
            (if (= [:set-rule :send-command-feedback] (take 2 d))
              (nth d 2)
              on))
          (get-in world [:rules :send-command-feedback] true) ds))

(defn- admin-report [world eid m]
  (when-let [e (get-in world [:entities eid])]
    (out/except eid (out/system-chat
                      {:translate "chat.type.admin"
                       :with [(entity-name e) (:text m)]
                       :color "gray" :italic true}))))

(defn- report-deltas [world eid on? d]
  (let [m (when (= :fx (nth d 0)) (nth d 1))]
    (cond (not (:feedback m)) [d]
          (not on?) nil
          :else (cond-> [[:fx (dissoc m :feedback :admins)]]
                  (:admins m) (conj (admin-report world eid m))))))

(defn- reported [world eid ds]
  (let [on? (feedback? world ds)]
    (into [] (comp (mapcat #(report-deltas world eid on? %))
                   (remove nil?))
          ds)))

(defn- gamemaster? [world eid]
  (when-let [e (get-in world [:entities eid])]
    (<= (long cmd/gamemaster) (player/permission-level e))))

(defn- level [world eid]
  (player/permission-level (get-in world [:entities eid])))

(defn- parsed [world eid text origin]
  (cmd/parse text origin (:dim world :overworld) (level world eid)
             [0.0 0.0] (cmd/extra-of world)))

(defn- command-deltas [world eid text]
  (let [origin (when-let [p (get-in world [:entities eid :pos])]
                 [(v/x p) (v/y p) (v/z p)])
        r (parsed world eid text origin)]
    (cond
      (:failure r) (parse-failed eid text r)
      (:error r) (tell eid (:error r))
      :else (let [w (sourced world r origin)
                  ds (world-command-deltas w eid (:delta r))]
              (reported w eid ds)))))

(defn- public-deltas [world eid text]
  (when-let [e (get-in world [:entities eid])]
    [(out/all (out/player-chat
                {:translate "chat.type.text"
                 :with [(entity-name e) text]}))]))

(defn- said-deltas [world eid raw]
  (let [text (str/trim (str raw))]
    (cond
      (str/blank? text) nil
      (str/starts-with? text "/") (command-deltas world eid text)
      :else (public-deltas world eid text))))

(defn- tab-deltas [world eid text target id]
  (let [lv (level world eid)
        {:keys [start texts]} (cmd/suggestions world text target lv)
        len (- (count text) (long start))]
    [(out/to eid (out/suggestions (or id 0) start len texts))]))

(defn- mode-changed [world eid mode]
  (when (gamemaster? world eid)
    (let [x [eid (:dim world) (get-in world [:entities eid])]]
      (reported world eid (mode-set world eid mode x)))))

(defn- event-deltas [world [tag eid text target id]]
  (case tag
    :chat (said-deltas world eid text)
    :tab-complete (tab-deltas world eid text target id)
    :change-game-mode (mode-changed world eid text)
    :teleport-to-entity (entity-tp-deltas world eid text)
    nil))

(defn- distance-fx [old new k fx]
  (let [n (get new k)]
    (when (not= (get old k) n) [(out/everyone (fx n))])))

(defn- config-loaded [world m]
  (let [old (:config world)]
    (concat [[:set-config (merge old m)]
             (out/everyone (out/reloaded))]
            (distance-fx old m :view-distance out/view-distance)
            (distance-fx old m :simulation-distance
              out/simulation-distance))))

(defn- commit-synced
  [world [_ commit]]
  [[:set-config (assoc (:config world) :commit commit)]])

(defn- config-event-deltas [world [tag eid m :as ev]]
  (case tag
    :config-loaded (config-loaded world m)
    :config-failed (fail eid "commands.reload.failure")
    :commit-synced (commit-synced world ev)
    nil))

(defn- one-deltas [world ev]
  (vec (concat (event-deltas world ev)
               (rules-event-deltas world ev)
               (config-event-deltas world ev))))

(defn- chat-deltas [world events]
  (apply/fold-events world events one-deltas))

(defn chat
  "Turns the chat lines and commands of this tick into deltas."
  {:wake {:events #{:chat :tab-complete :change-game-mode
                    :teleport-to-entity :rules-request :set-rules
                    :config-loaded :config-failed :commit-synced}}}
  [world d]
  (let [events (:input d)]
    (deltas/of-vec (chat-deltas world events))))
