(ns collider.world.neighbors
  "Level.setBlock with the neighbour updates it runs at once, as
  CollectingNeighborUpdater orders them: onPlace of the block set,
  then neighborChanged of the six around it, then their
  updateShape. A block set while updates run has its own updates
  wait until the update that set it is done; then they run before
  the rest. The level is plain chunks; the result tells what was
  written, with the effects of each change where vanilla shows
  them, and the ticks asked for, in the order they were asked."
  (:require [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.chunk :as chunk]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private update-order
  "NeighborUpdater.UPDATE_ORDER: west, east, down, up, north,
  south; each with the side of the change seen from there."
  [[[-1 0 0] :east] [[1 0 0] :west] [[0 -1 0] :up]
   [[0 1 0] :down] [[0 0 -1] :south] [[0 0 1] :north]])

(def ^:private shape-order
  "BlockBehaviour.UPDATE_SHAPE_ORDER: west, east, north, south,
  down, up."
  [[[-1 0 0] :east] [[1 0 0] :west] [[0 0 -1] :south]
   [[0 0 1] :north] [[0 -1 0] :up] [[0 1 0] :down]])

(def ^:const update-limit
  "How deep updateShape goes, Block.UPDATE_LIMIT."
  512)

(defn- shifted [[x y z] [dx dy dz]]
  [(+ (long x) (long dx)) (+ (long y) (long dy))
   (+ (long z) (long dz))])

(defn- block-at ^long [s p]
  (if (chunk/in-range? (long (p 1)))
    (chunk/chunks-get-block (:chunks s) p)
    0))

(defn- ticked
  "Returns s with a tick of list k asked for at tick at, never
  sooner than the next tick."
  [s ctx k at p ty]
  (if (and at ty)
    (let [at (max (long at) (inc (long (:tick ctx))))]
      (update s :ticks conj [k at (chunk/block-pos->id p) ty]))
    s))

(declare set-block)

(defn- destroys?
  "Tells whether a change from updateShape takes the block away:
  updateOrDestroy destroys it then, with full updates."
  [old st]
  (and (not (block/liquid? old))
       (or (zero? (long st)) (block/liquid? st))))

(defn- shape-flags ^long [old st]
  (if (destroys? old st) 3 2))

(defn- reacted
  "Returns s after the changes a block made in reply to a
  neighbour. flags-of gives the flags of each change."
  [s ctx changes flags-of limit]
  (reduce (fn [s [p st :as c]]
            (let [old (block-at s p)]
              (set-block s ctx c (flags-of old st) limit)))
          s changes))

(defn- rule-woken
  "The rule owning p answers a change: a tick, or its reply at
  once."
  [s ctx p old side flags-of limit]
  (let [{:keys [chunks]} s
        {:keys [dim tick]} ctx
        st (block-at s p)
        at (rules/wake-tick chunks dim st tick p old side)]
    (if (= :neighbor at)
      (let [ctx' (assoc ctx :side side)]
        (reacted s ctx (rules/reshape-changes chunks st p ctx')
                 flags-of limit))
      (ticked s ctx :block-ticks at p (block/block-of st)))))

(defn- fluid-woken [s ctx p old side]
  (let [{:keys [dim tick]} ctx
        st (block-at s p)
        at (rules/fluid-wake-tick (:chunks s) dim st tick p old side)]
    (ticked s ctx :fluid-ticks at p (rules/fluid-of st))))

(defn- liquid-woken
  "LiquidBlock.onPlace and neighborChanged: the liquid at p mixes,
  or else asks for its tick."
  [s ctx p old side]
  (let [st (block-at s p)
        s' (rule-woken s ctx p old side (constantly 3) update-limit)]
    (if (= st (block-at s' p))
      (fluid-woken s' ctx p old side)
      s')))

(defn- placed
  "onPlace of the block now at p: its rule and its fluid see the
  change at their own cell."
  [s ctx p old]
  (if (block/liquid? (block-at s p))
    (liquid-woken s ctx p old nil)
    (-> (rule-woken s ctx p old nil (constantly 3) update-limit)
        (fluid-woken ctx p old nil))))

(defn- neighbor-changed
  "neighborChanged of the block at q, for a change on side."
  [s ctx q old side]
  (let [st (block-at s q)]
    (cond
      (zero? st) s
      (block/liquid? st) (liquid-woken s ctx q old side)
      (= :neighbor (rules/update-pass st))
      (rule-woken s ctx q old side (constantly 3) update-limit)
      :else s)))

(defn- reshaped
  "The shape a connecting block at q takes for a change on side;
  one that goes is destroyed."
  [s ctx q st side limit]
  (if-let [new (connect/reshape (:chunks s) q st (:tick ctx) #{side})]
    (let [c (if (destroys? st new) (block/destroyed q st) [q new])]
      (set-block s ctx c (shape-flags st new) limit))
    s))

(defn- source-fluid? [st]
  (or (block/waterlogged? st) (block/source-state? st)))

(defn- liquid-shaped
  "LiquidBlock.updateShape: a tick when a source is on either
  side of the change."
  [s ctx q old side st nst]
  (if (or (block/source-state? st) (source-fluid? nst))
    (fluid-woken s ctx q old side)
    s))

(defn- shape-changed
  "updateShape of the block at q, for a change on side to nst."
  [s ctx q old side nst limit]
  (let [st (block-at s q)]
    (cond
      (zero? st) s
      (block/liquid? st) (liquid-shaped s ctx q old side st nst)
      :else
      (let [s (fluid-woken s ctx q old side)
            s (if (= :shape (rules/update-pass st))
                (rule-woken s ctx q old side shape-flags limit)
                s)]
        (if (= st (block-at s q))
          (reshaped s ctx q st side limit)
          s)))))

(defn- run-next
  "Runs the next update of item, NeighborUpdates.runNext. Returns
  [s item'], item' nil when the item is done."
  [s ctx {:keys [kind p old i side nst limit] :as item}]
  (if (= :multi kind)
    (let [[d side] (nth update-order i)
          s (neighbor-changed s ctx (shifted p d) old side)
          i (inc (long i))]
      [s (when (< i (count update-order)) (assoc item :i i))])
    [(shape-changed s ctx p old side nst limit) nil]))

(defn- run-top
  "Runs the item on top of the stack until it is done or adds
  updates of its own."
  [s ctx]
  (let [[s item] (run-next s ctx (peek (:stack s)))
        s (update s :stack pop)]
    (cond
      (nil? item) s
      (seq (:added s)) (update s :stack conj item)
      :else (recur (update s :stack conj item) ctx))))

(defn- run-updates
  "CollectingNeighborUpdater.runUpdates: what an update adds runs
  before the rest of it, the first added first."
  [s ctx]
  (loop [s s]
    (let [s (-> s
                (update :stack into (rseq (:added s)))
                (assoc :added []))]
      (if (empty? (:stack s))
        (assoc s :count 0)
        (recur (run-top s ctx))))))

(defn- add-and-run
  "CollectingNeighborUpdater.addAndRun: an update made while
  others run waits for the one running to finish."
  [s ctx item]
  (if (pos? (long (:count s)))
    (update s :added conj item)
    (-> (assoc s :count 1 :stack (list item))
        (run-updates ctx))))

(defn- neighbors-changed [s ctx p old]
  (add-and-run s ctx {:kind :multi :p p :old old :i 0}))

(defn- shape-item [p old nst limit [d side]]
  {:kind :shape :p (shifted p d) :old old :side side :nst nst
   :limit limit})

(defn- shapes-changed [s ctx p old limit]
  (let [nst (block-at s p)
        item #(shape-item p old nst limit %)]
    (reduce #(add-and-run %1 ctx (item %2)) s shape-order)))

(defn- written [s p old st fx]
  (-> s
      (update :chunks chunk/chunks-set-blocks [[p st]])
      (update :records conj (if fx [p st fx] [p st]))
      (update :writes conj [p old st])))

(defn- updated
  "The updates of a block set at p: onPlace, then, while the block
  stays, neighborChanged with flag 1 and updateShape while the
  depth lasts."
  [s ctx p old st flags limit]
  (let [s (placed s ctx p old)
        limit (long limit)]
    (if (not= (long st) (block-at s p))
      s
      (cond-> s
        (odd? (long flags)) (neighbors-changed ctx p old)
        (pos? limit) (shapes-changed ctx p old (dec limit))))))

(defn- settable? [s p]
  (and (chunk/in-range? (long (p 1)))
       (contains? (:chunks s) (chunk/block-chunk p))))

(defn set-block
  "Returns s after Level.setBlock of change [p st fx] with flags,
  1 for neighborChanged and 2 without, and the depth left to
  updateShape. A change to the state already there sets nothing
  but still shows its effects."
  [s ctx [p st fx] flags limit]
  (let [old (block-at s p)]
    (cond
      (not (settable? s p)) s
      (= old (long st))
      (cond-> s (seq fx) (update :records conj [p st fx]))
      :else (-> (written s p old st fx)
                (updated ctx p old st flags limit)))))

(defn- placement-state
  "The state of a change as placed: the shape it takes from its
  neighbours, getStateForPlacement of vanilla."
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (or st' st) fx]))

(def ^:private blank
  {:records [] :writes [] :ticks [] :placed [] :stack () :added []
   :count 0})

(defn- placed-with [flags]
  (fn [s ctx c]
    (set-block s ctx (placement-state (:chunks s) ctx c)
               flags update-limit)))

(defn- run-each [chunks ctx changes f]
  (reduce #(f %1 ctx %2) (assoc blank :chunks chunks) changes))

(defn set-blocks
  "Returns {:chunks :records :writes :ticks}: chunks after each
  change [pos st fx] was placed in order with full updates, the
  changes as made, the reactions among them with their effects,
  each write as [pos old st], and the ticks asked for as
  [list at id type] in order. ctx gives :tick, :dim, :rules and
  what the rules read."
  [chunks ctx changes]
  (run-each chunks ctx changes (placed-with 3)))

(defn- op-run [s ctx [op x flags]]
  (case op
    :set (set-block s ctx x flags update-limit)
    :notify (neighbors-changed s ctx x (block-at s x))))

(defn run
  "Returns what set-blocks does, for ops run in order: [:set c
  flags], Level.setBlock of change c as it is, not shaped by its
  neighbours; [:notify pos], Level.updateNeighborsAt, the six
  around pos told of a change there."
  [chunks ctx ops]
  (run-each chunks ctx ops op-run))

(defn- command-state
  "The state BlockInput.place sets: the shape st takes from its
  neighbours, st itself when that shape is air."
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (if (and st' (not (zero? (long st')))) st' st) fx]))

(defn- command-placed
  "BlockInput.place with flags 2 and 256: s with the change set,
  its cell noted under :placed with the state it held, when the
  place changed it."
  [s ctx [p :as c]]
  (let [n (count (:writes s))
        c' (command-state (:chunks s) ctx c)
        s' (set-block s ctx c' 2 update-limit)
        [q old] (get (:writes s') n)]
    (if (= p q)
      (update s' :placed conj [p old])
      s')))

(defn commanded
  "Returns what set-blocks does, with :placed, for the changes of
  /setblock or /fill: each set with flags 2 and 256, then for each
  that changed its cell, in order, updateNeighboursOnBlockSet, the
  neighbours told of it. :placed holds those cells as [pos old]."
  [chunks ctx changes]
  (let [s (run-each chunks ctx changes command-placed)]
    (reduce (fn [s [p old]] (neighbors-changed s ctx p old))
            s (:placed s))))
