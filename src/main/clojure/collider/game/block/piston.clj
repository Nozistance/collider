(ns collider.game.block.piston
  "The moves of a piston on its block events and the ticks of the
  blocks it moves."
  (:require [collider.game.changes :as changes]
            [collider.game.out :as out]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.piston :as piston]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- moving [f]
  (block/state :moving-piston {:facing f :type :normal}))

(defn- head [f]
  (block/state :piston-head {:facing f :type :normal}))

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

(defn- extend-ops [chunks pos f {:keys [push destroy]}]
  (let [arm (dir/toward pos f)
        gone (reversed destroy (mapv #(chunk/at chunks %) destroy))
        old (reversed push (mapv #(chunk/at chunks %) push))
        m (moving f)]
    (concat
      (for [[q o] gone] [:set [q 0 [[:drop o]]] 18])
      (for [q (rseq push)] [:set [(dir/toward q f) m] 324])
      [[:set [arm m] 324]]
      (for [[q o] gone
            op [[:removal q o false] [:indirect q o 2]
                [:notify q nil o]]]
        op)
      (for [[q o] old] [:notify q nil o])
      [[:notify arm nil (head f)]])))

(defn- carried [chunks pos f {:keys [push]}]
  (conj (mapv (fn [q] [:set-block-entity (dir/toward q f)
                       (moved (chunk/at chunks q) f true false)])
              (rseq push))
        [:set-block-entity (dir/toward pos f)
         (moved (head f) f true true)]))

(defn- seen
  "Returns w with the block entities of bes where none stands yet, so
  that the run that moves their blocks reads them."
  [w bes]
  (reduce (fn [w [_ q e]]
            (let [k [:block-entities (chunk/block-chunk q) q]]
              (if (get-in w k) w (assoc-in w k e))))
          w bes))

(defn- extended [w pos st f]
  (when-let [r (piston/resolved (:chunks w) pos f)]
    (let [chunks (:chunks w)
          on (block/with st :extended :true)
          bes (carried chunks pos f r)]
      (concat
        (changes/set-deltas
          (seen w bes)
          (concat (extend-ops chunks pos f r) [[:set [pos on] 67]]))
        bes
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

(defn- retracted [w pos f b1]
  (let [arm (dir/toward pos f)
        e (get-in w [:block-entities (chunk/block-chunk arm) arm])
        m (moving f)
        to (dir/from-index (bit-and (long b1) 7))
        base (block/state :piston {:facing to})
        clear (fn [chunks]
                [[arm (block/emptied (chunk/at chunks arm)) nil 3]])]
    (concat
      (changes/set-deltas
        w (concat (when (early? e) [(final-step w arm e)])
                  [[:set [pos m] 276] [:notify pos nil m]
                   [:shapes pos m 2] clear]))
      [[:set-block-entity pos (moved base f false true)]
       (sound w pos :block.piston.contract 0.15)])))

(defn triggered
  "Returns the deltas of block event b0 b1 of the piston st at pos,
  with its packet when the piston moves."
  [w pos st b0 b1]
  (when (= :sticky-piston (block/block-of st))
    (throw (ex-info "sticky pistons do not move yet" {:pos pos})))
  (let [f (block/facing-of st)
        on (piston/powered? (:chunks w) pos f)
        b0 (long b0)
        up (block/with st :extended :true)]
    (cond
      (and on (pos? b0)) (changes/set-deltas w [[pos up nil 2]])
      (pos? b0) (sent (retracted w pos f b1) pos b0 b1)
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
