(ns collider.game.commands.fill
  "The commands that set blocks: fill, setblock and clone."
  (:require [collider.game.changes :as changes]
            [collider.game.command.args :as cmd-args]
            [collider.game.command.args.block :as block-args]
            [collider.game.command.reader :as cmd-reader]
            [collider.game.command.selector :as sel]
            [collider.game.commands.clone :as clone]
            [collider.game.commands.pos :as pos]
            [collider.game.commands.reply :refer [fail say]]
            [collider.game.gamerules :as rules]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn- box [[ax ay az bx by bz]]
  (mapv (fn [a b] (sort [(long a) (long b)])) [ax ay az] [bx by bz]))

(defn- shell? [[[x1 x2] [y1 y2] [z1 z2]] [x y z]]
  (or (= x x1) (= x x2) (= y y1) (= y y2) (= z z1) (= z z2)))

(defn- cell-change [bounds st mode]
  (fn [p]
    (cond (shell? bounds p) [p st]
          (= "outline" mode) nil
          (= "hollow" mode) [p 0]
          :else [p st])))

(defn- fill-changes
  [[[x1 x2] [y1 y2] [z1 z2] :as bounds] st mode]
  (into [] (keep (cell-change bounds st mode))
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
        read (fn [id] [id (level/read-absent world id)])
        loaded (into {} (comp (remove in?) (map read))
                     (box-ids bounds))]
    [(reduce-kv #(assoc %1 %2 (:chunk %3)) (:chunks world) loaded)
     (level/read-absent-deltas loaded)]))

(defn- mode-opts [mode test]
  (cond-> {}
    test (assoc :test test)
    (= "keep" mode) (assoc :test block/air-type?)
    (= "destroy" mode) (assoc :destroy? true)
    (= "strict" mode) (assoc :strict? true)))

(defn- changes-box [changes]
  (box (into (ffirst changes) (first (peek changes)))))

(defn- edited [world changes opts]
  (let [dim (sel/source-dim world)
        lv (sel/level-view world dim)
        [chunks adds] (fetched lv (changes-box changes))
        [ds n placed] (changes/command-deltas
                        (assoc lv :chunks chunks) changes opts)]
    [(sel/in-level world dim adds) (sel/in-level world dim ds) n
     placed]))

(defn- area ^long [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (inc (- (long x2) (long x1))) (inc (- (long y2) (long y1)))
     (inc (- (long z2) (long z1)))))

(defn- rule-value [world k]
  (get-in world [:rules k] (rules/defaults k)))

(defn- too-big
  "Returns the error of a command that would change more cells than
  the rule max_block_modifications allows."
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

(defn- fill-by [world eid corners block mode test]
  (let [bounds (box corners)]
    (if-let [k (some #(pos/pos-error (sel/source-level world) %)
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
  (if id (cmd-args/dimension id schema/dims) (sel/source-dim world)))

(defn- dim-error [d]
  (when (cmd-reader/error? d) (into [(:key d)] (:args d))))

(defn- corner-error [world dim p]
  (when-let [k (pos/pos-error (sel/level-view world dim) p)] [k]))

(defn- clone-error
  "Returns the first error of the arguments of /clone, in the order
  it reads them."
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
  "Returns true when the chunks between corners a and b are loaded."
  [lv [_ y0 _ :as a] [_ y1 _ :as b]]
  (and (>= (long y1) (chunk/level-min-y lv))
       (<= (long y0) (chunk/level-max-y lv))
       (not (pos/unloaded? lv (chunk-corners a b)))))

(defn- opened-level [world dim boxes]
  (reduce (fn [[lv adds] bounds]
            (let [[chunks more] (fetched lv bounds)]
              [(assoc lv :chunks chunks) (into adds more)]))
          [(sel/level-view world dim) []] boxes))

(defn- clone-test [{:keys [filter]} filtered?]
  (cond filtered? (fn [st _] (block-args/matches? filter st nil))
        (= "masked" filter) (fn [st _] (not (block/air-type? st)))))

(defn- copied-tail [p]
  (cond-> (into [] (mapcat (fn [[q e]] (changes/be-changed q e)))
                (:entities p))
    (seq (:ticks p)) (conj [:schedule-copied (:ticks p)])))

(defn- moved-run [world [fd from fadds] [td to tadds] p]
  (let [[cds] (when (seq (:clear p))
                (changes/ops-deltas from (:clear p)))
        [ds n] (changes/ops-deltas to (:place p))
        placed (concat tadds ds (copied-tail p))]
    [(concat (sel/in-level world fd (concat fadds cds))
             (sel/in-level world td placed))
     n]))

(defn- clone-run
  "Returns the deltas of a clone planned as p and the cells it set."
  [world [fd _ _ :as from] [td to tadds :as dest] p]
  (if (= fd td)
    (let [ops (into (vec (:clear p)) (:place p))
          [ds n] (changes/ops-deltas to ops)]
      [(sel/in-level world td (concat tadds ds (copied-tail p))) n])
    (moved-run world from dest p)))

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
  (and (chunks-at? (sel/level-view world fd) (:begin a) (:end a))
       (chunks-at? (sel/level-view world td) (:dest a)
                   (mapv peek d))))

(defn- overlapping [world eid a fd td [b _ d]]
  (when (and (not (#{"force" "move"} (:mode a))) (= fd td)
             (overlap? b d))
    (fail eid "commands.clone.overlap")))

(defn- clone-deltas [world eid [opts :as xs]]
  (let [a (clone-args xs)
        fd (dim-of world (:from a))
        td (dim-of world (:to a))
        [b :as boxes] (clone-boxes a)]
    (if-let [[k & with] (clone-error world a fd td)]
      (apply fail eid k with)
      (or (overlapping world eid a fd td boxes)
          (too-big world eid "commands.clone.toobig" b)
          (when-not (clone-loaded? world a fd td boxes)
            (fail eid "argument.pos.unloaded"))
          (cloned world eid a opts fd td boxes)))))

(defn- place-needed? [world p st mode]
  (let [old (changes/block-at (sel/source-level world) p)
        left (if (block/air-type? old) old (block/emptied old))]
    (or (not= "destroy" mode) (not (block/air-type? st))
        (not (block/air-type? left)))))

(defn- block-set [world eid [x y z :as p] st mode]
  (let [put? (place-needed? world p st mode)
        opts (mode-opts mode nil)
        [_ ds _ placed] (edited world [[p (when put? st)]] opts)]
    (if (and put? (zero? (long placed)))
      (fail eid "commands.setblock.failed")
      (concat ds (say eid "commands.setblock.success" x y z)))))

(defn- setblock-deltas [world eid [x y z block mode]]
  (let [p [x y z]
        lv (sel/source-level world)
        k (pos/pos-error lv p)]
    (cond
      k (fail eid k)
      (and (= "keep" mode)
           (not (block/air-type? (changes/block-at lv p))))
      (fail eid "commands.setblock.failed")
      :else (block-set world eid p (:state block) mode))))

(def handlers
  "The block commands by name."
  {:fill fill-deltas :fill-where fill-where-deltas
   :setblock setblock-deltas :clone clone-deltas})
