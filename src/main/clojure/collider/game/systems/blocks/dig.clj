(ns collider.game.systems.blocks.dig
  "Breaking blocks."
  (:require [collider.data :as data]
            [collider.game.changes :as changes]
            [collider.game.block.blockentity :as be]
            [collider.game.block.lid :as lid]
            [collider.game.entity :as entity]
            [collider.game.player :as player]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.reach :as reach]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.halves :as halves]
            [collider.world.chunk :as chunk]))

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

(defn- shulker-break-deltas [world pos]
  (let [e (be/at world pos)]
    (when (= :shulker-box (:kind e))
      (concat
        (when (lid/animation world pos)
          [[:shulker-anim pos nil]])
        (when (some some? (:items e))
          [(shulker-drop world pos e)])))))

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
      (into (vec (shulker-break-deltas world pos))
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

(def ^:private breaking-actions #{start finish})

(defn dig-deltas
  "Returns the deltas of a player who digs at a block.
  Only the start and the finish of the dig break the block at pos."
  [world [eid status pos _face]]
  (let [e (get-in world [:entities eid])
        below-top? (<= (long (nth pos 1)) (chunk/level-max-y world))]
    (when (and (breaking-actions status) (reach/in-reach? e pos))
      (if-not below-top?
        [(edit/own-change world eid pos)]
        (let [[may? kept] (edit/break-check world eid e pos)]
          (concat
            kept
            (cond
              (not may?) (restricted world eid status pos)
              (and (tool-breaks? e) (permitted? world e pos))
              (break-deltas world eid pos)
              :else [(edit/own-change world eid pos)])))))))
