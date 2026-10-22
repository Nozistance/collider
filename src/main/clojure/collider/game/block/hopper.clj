(ns collider.game.block.hopper
  "A hopper moves one item toward the block it faces and takes
  one from above."
  (:require [collider.game.block.blockentity :as be]
            [collider.game.changes :as changes]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.sections :as sections]
            [collider.game.stack :as stack]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- size ^long [s] (if s (stack/size s) 0))

(defn- full? [items]
  (every? #(and % (= (size %) (stack/max-size %))) items))

(defn- vacant? [items] (not-any? some? items))

(defn- shrink [s]
  (let [n (dec (size s))]
    (when (pos? n) (assoc s :count n))))

(defn- one [s] (assoc s :count 1))

(defn- indices [kind face taking? n]
  (if-not (#{:furnace :blast-furnace :smoker} kind)
    (range n)
    (case face
      :up [0]
      :down (if taking? [2] [])
      (if taking? [1] [1]))))

(defn- room? [cur s]
  (and cur (stack/same-kind? cur s)
       (< (size cur) (stack/max-size s))))

(defn- accept [items s allowed]
  (or (some (fn [i]
              (when (room? (nth items i) s)
                (update items i update :count inc)))
            allowed)
      (some (fn [i]
              (when (nil? (nth items i))
                (assoc items i (one s))))
            allowed)))

(defn- offer [items allowed]
  (some (fn [i]
          (when-let [s (nth items i)]
            [i (shrink s) (one s)]))
        allowed))

(defn- pour [items s allowed]
  (loop [items items s s]
    (if-not (and s (pos? (size s)))
      [items nil]
      (if-let [next (accept items s allowed)]
        (recur next (shrink s))
        [items s]))))

(defn- held [world pos]
  (let [st (chunk/at (:chunks world) pos)
        k (be/kind st)
        e (when k (be/at world pos))]
    (when (:items e)
      {:pos pos :kind k :items (vec (:items e))})))

(defn- face-of [hopper-facing] (dir/opposite hopper-facing))

(defn- move-one [from to face]
  (let [n (count (:items from))
        give (indices (:kind from) face true n)
        take (indices (:kind to) face false (count (:items to)))]
    (when-let [[i left piece] (offer (:items from) give)]
      (when-let [into (accept (:items to) piece take)]
        [(assoc-in from [:items i] left)
         (assoc to :items into)]))))

(defn- push [world pos e]
  (let [face (block/facing-of (chunk/at (:chunks world) pos))
        there (mapv + pos (dir/offset face))
        to (held world there)]
    (when (and to (not (vacant? (:items e))))
      (when-let [[from to] (move-one {:kind :hopper :items (:items e)}
                                     to (face-of face))]
        [(assoc e :items (:items from))
         [:set-block-entity there (assoc (be/at world there)
                                         :items (:items to))]
         [:output-signal there]]))))

(defn- pull [world pos e]
  (let [above (update pos 1 inc)
        from (held world above)]
    (when (and from (not (full? (:items e))))
      (when-let [[from to] (move-one from
                                     {:kind :hopper :items (:items e)}
                                     :down)]
        [(assoc e :items (:items to))
         [:set-block-entity above (assoc (be/at world above)
                                         :items (:items from))]
         [:output-signal above]]))))

(defn- blocks-items? [chunks above]
  (let [st (chunk/at chunks above)]
    (and (block/full-cube? st)
         (not (block/tagged? st "does_not_block_hoppers")))))

(defn- meets? [box e]
  (let [[a0 a1 a2 a3 a4 a5] (bodies/box-of e)
        [x0 y0 z0 x1 y1 z1] box]
    (and (< (double a0) (double x1)) (> (double a3) (double x0))
         (< (double a1) (double y1)) (> (double a4) (double y0))
         (< (double a2) (double z1)) (> (double a5) (double z0)))))

(defn- suck-box [pos]
  (let [x (double (pos 0)) y (double (pos 1)) z (double (pos 2))]
    [x (+ y (/ 11.0 16.0)) z (+ x 1.0) (+ y 2.0) (+ z 1.0)]))

(defn- above-items [world pos]
  (let [box (suck-box pos)
        [x0 y0 z0 x1 y1 z1] box
        ids (sections/within (sections/index world)
                             (v/v3 x0 y0 z0) (v/v3 x1 y1 z1))]
    (for [id (sort ids)
          :let [e (get (:entities world) id)]
          :when (and e (= :item (:type e)) (meets? box e)
                     (pos? (size (:stack e))))]
      [id e])))

(defn- sucked [world pos e]
  (when-not (or (full? (:items e))
                (held world (update pos 1 inc))
                (blocks-items? (:chunks world) (update pos 1 inc)))
    (when-let [[id item] (first (above-items world pos))]
      (let [allowed (range (count (:items e)))
            [items left] (pour (:items e) (:stack item) allowed)]
        (when (not= items (:items e))
          [(assoc e :items items)
           (if left
             [:merge-entity id {:stack left}]
             [:remove-entity id])])))))

(defn- pack [[e & ds]]
  [e (vec (remove nil? ds))])

(defn- moved [world pos e]
  (or (when-let [r (push world pos e)] (pack r))
      (when-not (full? (:items e))
        (when-let [r (or (pull world pos e) (sucked world pos e))]
          (pack r)))))

(defn- cooled [e ^long cd moved?]
  (assoc e :cooldown (if moved? 8 cd)))

(defn- transfer-deltas [world pos e e' extra]
  (let [be? #(= :set-block-entity (first %))
        sig? #(= :output-signal (first %))
        ops (when (not= (:items e) (:items e'))
              (into [[:output-signal pos]] (filter sig? extra)))]
    (concat
      (changes/entities-changed-deltas
        world (into [[:set-block-entity pos e']] (filter be? extra))
        ops (:tick world))
      (remove #(or (be? %) (sig? %)) extra))))

(defn tick-deltas
  [world [pos e]]
  (let [st (chunk/at (:chunks world) pos)
        cd (dec (long (:cooldown e -1)))
        open? (= :true (:enabled (block/props-of st)))
        [e' extra] (when (and (not (pos? cd)) open?)
                     (moved world pos e))
        moved? (some? e')]
    (transfer-deltas world pos e (cooled (or e' e) cd moved?) extra)))
