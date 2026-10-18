(ns collider.game.block.piston
  "The moves of a piston on its block events and the ticks of the
  blocks it moves."
  (:require [collider.cell :as cell]
            [collider.game.changes :as changes]
            [collider.game.out :as out]
            [collider.hash-order :as hash-order]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.piston :as piston]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- kind [^long st]
  (if (= :sticky-piston (block/block-of st)) :sticky :normal))

(defn- moving [f t]
  (block/state :moving-piston {:facing f :type t}))

(defn- head [f t]
  (block/state :piston-head {:facing f :type t}))

(defn- reversed [ps sts] (rseq (mapv vector ps sts)))

(defn- moved
  "The block entity of a moving piston that carries st toward f."
  [st f extending? source?]
  {:kind :piston :moved st :facing f :extending? extending?
   :source? source? :progress 0 :progress-o 0})

(defn- sound [w pos kind ^double spread]
  (let [r (random/of-key (:tick w) pos kind)]
    (out/all (out/block-sound kind pos 0.5 (+ 0.6 (* spread r))))))

(defn- sent [deltas pos b0 b1]
  (conj (vec deltas) (out/all (out/block-event pos b0 b1))))

(defn- reshaped [w pos st]
  (or (connect/reshape (:chunks w) pos st (:tick w)) st))

(defn- left
  "Returns [cell st] of the cells of push no block moves into, in the
  order a hash map keyed by them iterates. The arm, when given, is
  taken."
  [chunks push d arm]
  (let [taken (cond-> (set (map #(dir/toward % d) push))
                arm (conj arm))
        ks (filterv (complement taken) push)]
    (mapv #(let [q (ks %)] [q (chunk/at chunks q)])
          (hash-order/of (map cell/hashed ks)))))

(defn- left-ops
  "Returns the ops that empty the cells of left and afterwards tell
  the cells around them."
  [left]
  (concat (for [[q] left] [:set [q 0] 82])
          (for [[q o] left
                op [[:indirect q o 2] [:shapes q 0 2]]]
            op)))

(defn- carried [chunks q d f ext?]
  [:set [(dir/toward q d) (moving f :normal)
         [[:carry (moved (chunk/at chunks q) f ext? false)]]]
   324])

(defn- arm-ops [pos f t]
  (let [arm (dir/toward pos f)
        m (moving f t)]
    [[:set [arm m [[:carry (moved (head f t) f true true)]]] 324]]))

(defn- move-ops
  "Returns the ops of moveBlocks of the piston at pos facing f of
  type t, extending when ext?, for its resolved cells."
  [chunks pos f t ext? {:keys [push destroy]}]
  (let [d (if ext? f (dir/opposite f))
        arm (dir/toward pos f)
        gone (reversed destroy (mapv #(chunk/at chunks %) destroy))
        left (left chunks push d (when ext? arm))]
    (concat
      (for [[q o] gone] [:set [q 0 [[:drop o]]] 18])
      (for [q (rseq push)] (carried chunks q d f ext?))
      (when ext? (arm-ops pos f t))
      (left-ops left)
      (for [[q o] gone
            op [[:removal q o false] [:indirect q o 2]
                [:notify q nil o]]]
        op)
      (for [q (rseq push)] [:notify q nil (chunk/at chunks q)])
      (when ext? [[:notify arm nil (head f t)]]))))

(defn- seen
  "Returns w with the block entities that ops carry where none stands
  yet, so that the run of ops reads them."
  [w ops]
  (reduce (fn [w [q _ fx]]
            (let [k [:block-entities (chunk/block-chunk q) q]
                  e (some #(when (= :carry (first %)) (second %)) fx)]
              (if (or (nil? e) (get-in w k)) w (assoc-in w k e))))
          w (keep #(when (= :set (first %)) (second %)) ops)))

(defn- extended [w pos st f]
  (when-let [r (piston/resolved (:chunks w) pos f true)]
    (let [ops (move-ops (:chunks w) pos f (kind st) true r)
          on (block/with st :extended :true)]
      (concat
        (changes/set-deltas
          (seen w ops) (concat ops [[:set [pos on] 67]]))
        [(sound w pos :block.piston.extend 0.25)]))))

(defn- final-step
  "The step that puts the block a moving piston at p carries in place
  at once, as its block entity e ends early."
  [w p e]
  (fn [chunks]
    (when (piston/moving? (chunk/at chunks p))
      (let [st (if (:source? e) 0 (reshaped w p (:moved e)))]
        [[p st nil 3] [:changed p st nil]]))))

(defn- early? [e]
  (and (= :piston (:kind e)) (< (long (:progress-o e)) 2)))

(defn- block-entity [w p]
  (get-in w [:block-entities (chunk/block-chunk p) p]))

(defn- cleared [arm]
  (fn [chunks] [[arm (block/emptied (chunk/at chunks arm)) nil 3]]))

(defn- pull-ops [pos f]
  (let [arm (dir/toward pos f)]
    [(fn [chunks]
       (when (piston/head? (chunk/at chunks arm))
         [[:set [arm 0] 276]]))
     (fn [chunks]
       (when-let [r (piston/resolved chunks pos f false)]
         (move-ops chunks pos f :sticky false r)))]))

(defn- pulled
  "The step of a sticky piston at pos facing f that pulls back the
  block two ahead of it on event b0."
  [w pos f b0]
  (fn [chunks]
    (let [two (dir/toward pos f 2)
          st (chunk/at chunks two)
          e (block-entity w two)]
      (cond
        (and (piston/moving? st) (= :piston (:kind e))
             (= f (:facing e)) (:extending? e))
        [(final-step w two e)]
        (or (not= 1 b0) (block/air-type? st)
            (not (piston/pushable? st two (dir/opposite f) false f)))
        [(cleared (dir/toward pos f))]
        :else (pull-ops pos f)))))

(defn- retracted [w pos st f b0 b1]
  (let [arm (dir/toward pos f)
        e (block-entity w arm)
        t (kind st)
        m (moving f t)
        to (dir/from-index (bit-and (long b1) 7))
        base (moved (block/state (block/block-of st) {:facing to})
                    f false true)]
    (concat
      (changes/set-deltas
        w (concat (when (early? e) [(final-step w arm e)])
                  [[:set [pos m [[:carry base]]] 276]
                   [:notify pos nil m] [:shapes pos m 2]
                   (if (= :sticky t)
                     (pulled w pos f b0)
                     (cleared arm))]))
      [(sound w pos :block.piston.contract 0.15)])))

(defn triggered
  "Returns the deltas of block event b0 b1 of the piston st at pos,
  with its packet when the piston moves."
  [w pos st b0 b1]
  (let [f (block/facing-of st)
        on (piston/powered? (:chunks w) pos f)
        b0 (long b0)
        up (block/with st :extended :true)]
    (cond
      (and on (pos? b0)) (changes/set-deltas w [[pos up nil 2]])
      (pos? b0) (sent (retracted w pos st f b0 b1) pos b0 b1)
      on (some-> (extended w pos st f) (sent pos b0 b1)))))

(defn- dried [^long st]
  (if (block/waterlogged? st) (block/with st :waterlogged :false) st))

(defn- placed [pos moved ^long st]
  (if (block/air-type? st)
    [[:set [pos moved] 340] (block/destroyed pos moved)]
    [[pos (dried st) nil 67] [:changed pos (dried st) nil]]))

(defn- finished [w pos e]
  (if (piston/moving? (chunk/at (:chunks w) pos))
    (changes/set-deltas
      w (placed pos (:moved e) (reshaped w pos (:moved e))))
    [[:set-block-entity pos nil]]))

(defn tick-deltas
  "Returns the deltas of the tick of the moving piston e at pos."
  [w [pos e]]
  (let [n (long (:progress e))]
    (if (< n 2)
      [[:set-block-entity pos
        (assoc e :progress-o n :progress (inc n) :ticked (:tick w))]]
      (finished w pos e))))
