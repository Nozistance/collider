(ns collider.game.systems.blocks.dig
  "Breaking blocks."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.block.blockentity :as be]
            [collider.game.block.lid :as lid]
            [collider.game.dig :as dig]
            [collider.game.entity :as entity]
            [collider.game.food :as food]
            [collider.game.item :as item]
            [collider.game.loot :as loot]
            [collider.game.inventory :as inventory]
            [collider.game.mode :as game-mode]
            [collider.game.player :as player]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.blocks.use :as use]
            [collider.game.reach :as reach]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.halves :as halves]
            [collider.world.chunk :as chunk]
            [collider.world.env.attribute :as attribute]))

(set! *warn-on-reflection* true)

(def ^:const start 0)

(def ^:const abort 1)

(def ^:const finish 2)

(defn- first-half? [old]
  (let [{:keys [half part]} (block/props-of old)
        t (block/type-of old)]
    (or (and (= :upper half) (contains? halves/half-types t))
        (and (= :bed t) (= :foot part)))))

(defn- kept-partner [world e pos old]
  (when (and (player/infinite-materials? e) (first-half? old))
    (halves/partner (:chunks world) pos old)))

(defn- shulker-drop [world pos e]
  (let [item (block/block-of (changes/block-at world pos))
        vel (entity/pop-velocity [(:tick world) pos :shulker])]
    [:spawn-entity
     (entity/item (v/centre pos) vel (be/to-stack item e))]))

(defn- shulker-break-deltas [world e pos]
  (let [be (be/at world pos)]
    (when (and (= :shulker-box (:kind be)) (player/infinite-materials? e))
      (concat
        (when (lid/animation world pos)
          [[:shulker-anim pos nil]])
        (when (some some? (:items be))
          [(shulker-drop world pos be)])))))

(defn- break-shown [eid pos old]
  (if (block/fire? old)
    [(out/all (out/extinguish pos))]
    [(out/except eid (out/break-effect pos old))]))

(defn- gone-deltas
  [world eid pos old [ppos pst :as kept]]
  (let [gone (cond->> [[pos (block/emptied old)]]
               kept (cons [ppos (block/emptied pst)]))]
    (concat
      (break-shown eid pos old)
      (changes/change-deltas world gone)
      (when kept [(out/except eid (out/break-effect ppos pst))]))))

(defn- break-deltas [world eid pos]
  (let [old (changes/block-at world pos)
        e (get-in world [:entities eid])
        kept (kept-partner world e pos old)]
    (if (pos? old)
      (into (vec (shulker-break-deltas world e pos))
            (gone-deltas world eid pos old kept))
      [(edit/own-change world eid pos)])))

(defn- tool-breaks? [e]
  (let [it (get (data/items) (:item (player/hand-stack e :main)))]
    (not (or (false? (:breaks? it))
             (and (player/infinite-materials? e)
                  (false? (:creative-break? it)))))))

(defn- permitted? [world e pos]
  (or (not (edit/game-master-block? (changes/block-at world pos)))
      (edit/game-master? e)))

(defn- restricted [world eid status pos]
  (when (= start (long status)) [(edit/own-change world eid pos)]))

(defn- staged [eid pos stage]
  (out/except eid (out/destroy-stage eid pos stage)))

(defn- stage-of ^long [^double p] (long (num/f32 (* p 10.0))))

(defn- worn-deltas [world eid e st]
  (let [stack (player/hand-stack e :main)
        t (get-in (data/items) [(:item stack) :tool])
        hard? (not (zero? (double (:hardness (get (data/blocks)
                                                  (block/block-of st))
                                             0.0))))
        n (long (:per-block t 1))]
    (when (and t (not (player/infinite-materials? e)))
      (cons [:award eid (keyword "used" (name (:item stack))) 1]
            (when (and (pos? n) (or hard? (= :shears (:item stack))))
              (inventory/hurt-item-deltas (:tick world) eid e :main n))))))

(defn- loot-deltas [world e pos st]
  (when (get-in world [:rules :block-drops] true)
    (let [ctx {:state st :pos pos :entity e
               :tool (player/hand-stack e :main)
               :block-entity (be/at world pos)
               :block-at #(changes/block-at world %)}
          roll #(random/of-key (:tick world) pos :mined %)]
      (map-indexed (fn [i stack]
                     [:spawn-entity (item/popped world pos stack [:mined i])])
                   (loot/block-drops ctx roll)))))

(defn- silk? [e]
  (contains? (get-in (player/hand-stack e :main)
                     [:components :enchantments])
             :silk-touch))

(defn- melts? [world e pos st]
  (let [below (changes/block-at world (mapv + pos [0 -1 0]))]
    (and (= :ice (block/block-of st)) (not (silk? e))
         (not (attribute/water-evaporates? (:dim world)))
         (or (block/blocks-motion? below) (block/liquid? below)))))

(defn- egg-left-deltas [world pos st]
  (let [n (block/prop-long st :eggs)
        pitch (+ 0.9 (* 0.2 (random/of-key (:tick world) pos :egg)))]
    (when (and (= :turtle-egg (block/block-of st)) (> n 1))
      (concat [(out/all (out/block-sound :entity.turtle.egg-break pos 0.7 pitch))]
              (changes/change-deltas
                world [[pos (block/with-long st :eggs (dec n))]])
              [(out/all (out/break-effect pos st))]))))

(defn- after-deltas [world e pos st]
  (if (melts? world e pos st)
    (changes/change-deltas world [[pos (block/state :water)]])
    (egg-left-deltas world pos st)))

(defn- mined-deltas [world eid e pos st]
  (when (dig/correct-tool? e st)
    (concat [[:award eid (keyword "mined" (name (block/block-of st))) 1]
             [:merge-entity eid (select-keys (food/exhausted e 0.005)
                                             [:exhaustion])]]
            (loot-deltas world e pos st)
            (changes/xp-deltas world pos st (player/hand-stack e :main))
            (after-deltas world e pos st))))

(defn- destroyed [world eid e pos]
  (let [st (changes/block-at world pos)]
    (concat (break-deltas world eid pos)
            (when (and (pos? st) (not (player/infinite-materials? e)))
              (concat (mined-deltas world eid e pos st)
                      (worn-deltas world eid e st))))))

(defn- dig-state [e] (:dig e))

(defn- started [world eid e pos]
  (let [st (changes/block-at world pos)
        air? (zero? st)
        p (if air? 1.0 (dig/progress e st))
        d (dig-state e)]
    (concat
      (when-not air? (use/attack-deltas world eid pos))
      (if (and (not air?) (>= p 1.0))
        (destroyed world eid e pos)
        (concat
          (when (:active? d) [(edit/own-change world eid (:pos d))])
          [[:merge-entity eid {:dig {:pos pos :start (:tick world)
                                     :active? true :stage (stage-of p)}}]
           (staged eid pos (stage-of p))])))))

(defn- stopped [world eid e pos]
  (let [d (dig-state e)
        st (changes/block-at world pos)]
    (when (and (= pos (:pos d)) (pos? st))
      (let [n (inc (- (long (:tick world)) (long (:start d))))
            prog (num/f32 (* (dig/progress e st) n))]
        (cond
          (>= prog (num/f32 0.7))
          (concat [[:merge-entity eid {:dig (assoc d :active? false)}]
                   (staged eid pos -1)]
                  (destroyed world eid e pos))
          (nil? (:delayed e))
          [[:merge-entity eid
            {:dig (assoc d :active? false)
             :delayed {:pos pos :start (:start d)}}]])))))

(defn- aborted [eid e pos]
  (let [d (dig-state e)]
    (concat [[:merge-entity eid {:dig (assoc d :active? false)}]]
            (when (and d (not= pos (:pos d))) [(staged eid (:pos d) -1)])
            [(staged eid pos -1)])))

(defn- timed? [e] (not (:instabuild? (game-mode/abilities e))))

(defn- survival-deltas [world eid e status pos]
  (condp = (long status)
    start (started world eid e pos)
    finish (stopped world eid e pos)
    abort (aborted eid e pos)
    nil))

(defn- creative-deltas [world eid e status pos]
  (when (#{start finish} status)
    (if (and (tool-breaks? e) (permitted? world e pos))
      (break-deltas world eid pos)
      [(edit/own-change world eid pos)])))

(defn- checked-deltas [world eid e status pos]
  (let [[may? kept] (if (= abort (long status))
                      [true nil]
                      (edit/break-check world eid e pos))]
    (concat
      kept
      (cond
        (not may?) (restricted world eid status pos)
        (timed? e) (survival-deltas world eid e status pos)
        :else (creative-deltas world eid e status pos)))))

(defn dig-deltas
  "Returns the deltas of a player who digs at a block. Out of
  creative the block breaks when the dig has gone on long enough."
  [world [eid status pos _face]]
  (let [e (get-in world [:entities eid])
        below-top? (<= (long (nth pos 1)) (chunk/level-max-y world))]
    (when (reach/in-reach? e pos)
      (if below-top?
        (checked-deltas world eid e status pos)
        [(edit/own-change world eid pos)]))))

(defn- ticked-progress ^double [world e st start]
  (let [n (+ 2 (- (long (:tick world)) (long start)))]
    (num/f32 (* (dig/progress e st) n))))

(defn- restaged [eid d pos ^double prog]
  (let [stage (stage-of prog)]
    (when (not= stage (:stage d))
      [[:merge-entity eid {:dig (assoc d :stage stage)}]
       (staged eid pos stage)])))

(defn- delayed-deltas [world eid e]
  (let [{:keys [pos start]} (:delayed e)
        st (changes/block-at world pos)
        prog (ticked-progress world e st start)]
    (cond
      (zero? st) [[:merge-entity eid {:delayed nil}]]
      (>= prog 1.0)
      (concat (restaged eid (dig-state e) pos prog)
              [[:merge-entity eid {:delayed nil}]]
              (destroyed world eid e pos))
      :else (restaged eid (dig-state e) pos prog))))

(defn- active-deltas [world eid e]
  (let [{:keys [pos start] :as d} (dig-state e)
        st (changes/block-at world pos)]
    (if (zero? st)
      [[:merge-entity eid {:dig (assoc d :active? false :stage -1)}]
       (staged eid pos -1)]
      (restaged eid d pos (ticked-progress world e st start)))))

(defn player-deltas
  "Returns the deltas of the dig of player p in its tick. A delayed
  break goes on until the block is gone, else the cracks of the dig
  go on."
  [world [eid e]]
  (cond
    (:delayed e) (delayed-deltas world eid e)
    (:active? (dig-state e)) (active-deltas world eid e)))
