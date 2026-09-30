(ns collider.world.neighbors
  "Block changes with the neighbour updates they run at once."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.rail :as rail]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.rules :as rules])
  (:import (collider.world ChunkIndex Neighbors)))

(set! *warn-on-reflection* true)

(def ^:private update-sides
  (object-array [:east :west :up :down :south :north]))

(def ^:private shape-sides
  (object-array [:east :west :south :north :up :down]))

(def ^:const update-limit
  "How deep shape updates go."
  512)

(defn- chunks ^ChunkIndex [^Neighbors s] (.chunks s))

(defn- with-chunks ^Neighbors [^Neighbors s chunks]
  (set! (.chunks s) chunks)
  s)

(defn- add-record ^Neighbors [^Neighbors s x] (.record s x))

(defn- add-write ^Neighbors [^Neighbors s x] (.write s x))

(defn- add-tick ^Neighbors [^Neighbors s x] (.tick s x))

(defn- add-sent ^Neighbors [^Neighbors s x] (.send s x))

(defn- add-placed ^Neighbors [^Neighbors s x] (.addPlaced s x))

(defn- write-count ^long [^Neighbors s] (.writeCount s))

(defn- write-at [^Neighbors s ^long n] (.writeAt s n))

(defn- running? [^Neighbors s] (.running s))

(defn- place! ^long [^Neighbors s p ^long st ^long flags]
  (.place s p (int (p 0)) (int (p 1)) (int (p 2)) st flags
          chunk/min-y chunk/max-y))

(defn- shape-deaf? [^long st]
  (or (zero? st)
      (not (or (block/liquid? st) (rules/fluid-of st)
               (= :shape (rules/update-pass st))
               (connect/connecting? st)))))

(defn- neighbor-deaf? [^long st]
  (or (zero? st)
      (not (or (block/liquid? st)
               (= :neighbor (rules/update-pass st))))))

(defn- plain?
  "Returns true when placing st asks nothing of st itself: no rule
  owns it, it holds no fluid, keeps its shape and sets off no effect."
  [^long st]
  (not (or (rules/update-pass st) (block/liquid? st)
           (rules/fluid-of st) (connect/connecting? st)
           (geyser/placed-fx st))))

(defn- by-state [f]
  (boolean-array (map (comp boolean f)
                      (range (inc (data/block-state-count))))))

(def ^:private states
  (delay {:shape (by-state shape-deaf?)
          :neighbor (by-state neighbor-deaf?)
          :plain (by-state plain?)
          :rail (by-state rail/rail?)}))

(defn- marked? [k ^long st]
  (aget ^booleans (k @states) st))

(defn- deaf-to-neighbor?
  "Returns true when a neighbour update of a block in state st
  changes nothing."
  [^long st]
  (marked? :neighbor st))

(defn- deaf-around? [^Neighbors s k p]
  (.deafAround s (long (p 0)) (long (p 1)) (long (p 2)) (k @states)
               chunk/min-y chunk/max-y))

(defn- shifted [[x y z] [dx dy dz]]
  [(+ (long x) (long dx)) (+ (long y) (long dy))
   (+ (long z) (long dz))])

(defn- block-at ^long [s p]
  (if (chunk/in-range? (long (p 1)))
    (chunk/chunks-get-block (chunks s) p)
    0))

(defn- ticked [s ctx k at p ty]
  (if (and at ty)
    (let [at (max (long at) (inc (long (:tick ctx))))]
      (add-tick s [k at (chunk/block-pos->id p) ty]))
    s))

(declare set-block)

(defn- destroys? [old st]
  (and (not (block/liquid? old))
       (or (zero? (long st)) (block/liquid? st))))

(defn- shape-flags ^long [^long flags old st]
  (if (destroys? old st) 3 (bit-and flags -33)))

(defn- reacted
  "Sets the changes of a rule in order. A change may name its own
  flags. A step, a function of the level, gives the changes that
  follow the ones before it."
  [s ctx changes flags-of limit]
  (reduce (fn [s c]
            (if (fn? c)
              (reacted s ctx (c (chunks s)) flags-of limit)
              (let [[p st] c
                    f (get c 3 (flags-of (block-at s p) st))]
                (set-block s ctx c f limit))))
          s changes))

(defn- rule-woken [s ctx p old side flags-of limit]
  (let [cs (chunks s)
        {:keys [dim tick]} ctx
        st (block-at s p)
        at (rules/wake-tick cs dim st tick p old side)]
    (if (= :neighbor at)
      (let [ctx' (assoc ctx :side side :old old)]
        (reacted s ctx (rules/reshape-changes cs st p ctx')
                 flags-of limit))
      (ticked s ctx :block-ticks at p (block/block-of st)))))

(defn- fluid-woken [s ctx p old side]
  (let [{:keys [dim tick]} ctx
        st (block-at s p)
        at (rules/fluid-wake-tick (chunks s) dim st tick p old side)]
    (ticked s ctx :fluid-ticks at p (rules/fluid-of st))))

(defn- column-woken [s ctx p below]
  (let [st (block-at s p)
        at (liquid/column-wake st below (:tick ctx))]
    (ticked s ctx :block-ticks at p (block/block-of st))))

(defn- liquid-woken [s ctx p old side]
  (let [st (block-at s p)
        s' (if (block/lava? st)
             (rule-woken s ctx p old side (constantly 3) update-limit)
             s)]
    (if (= st (block-at s' p))
      (-> (fluid-woken s' ctx p old side)
          (column-woken ctx p (block-at s' (shifted p [0 -1 0]))))
      s')))

(defn- placed [s ctx p old]
  (if (block/liquid? (block-at s p))
    (liquid-woken s ctx p old nil)
    (rule-woken s ctx p old nil (constantly 3) update-limit)))

(defn- state-changed [s ctx q st old side]
  (cond
    (zero? (long st)) s
    (block/liquid? st) (liquid-woken s ctx q old side)
    (= :neighbor (rules/update-pass st))
    (rule-woken s ctx q old side (constantly 3) update-limit)
    :else s))

(defn- reshaped [s ctx q st side flags limit]
  (if-let [new (connect/reshape (chunks s) q st (:tick ctx) #{side})]
    (let [c (if (destroys? st new)
              (block/destroyed q st)
              [q new (some-> (geyser/shaped-fx st new) vector)])]
      (set-block s ctx c (shape-flags flags st new) limit))
    s))

(defn- source-fluid? [st]
  (or (block/waterlogged? st) (block/source-state? st)))

(defn- liquid-shaped [s ctx q old side st nst]
  (cond-> s
    (or (block/source-state? st) (source-fluid? nst))
    (fluid-woken ctx q old side)
    (= :down side) (column-woken ctx q nst)))

(defn- block-shaped [s ctx q st old side flags limit]
  (let [s (fluid-woken s ctx q old side)
        f #(shape-flags flags %1 %2)
        s (if (= :shape (rules/update-pass st))
            (rule-woken s ctx q old side f limit)
            s)]
    (if (= st (block-at s q))
      (reshaped s ctx q st side flags limit)
      s)))

(defn- shape-changed [s ctx q st old side nst flags limit]
  (cond
    (zero? (long st)) s
    (block/liquid? st) (liquid-shaped s ctx q old side st nst)
    :else (block-shaped s ctx q st old side flags limit)))

(defn- run-next [s ctx [_ p old]]
  (state-changed s ctx p old old nil)
  nil)

(defn- stepper [ctx]
  (fn [s item] (run-next s ctx item)))

(defn- add-and-run [^Neighbors s ctx item]
  (.addAndRun s item (stepper ctx)))

(defn- passed [^Neighbors s ctx p shape? told]
  (let [^booleans deaf ((if shape? :shape :neighbor) @states)
        ^objects sides (if shape? shape-sides update-sides)]
    (.pass s (long (p 0)) (long (p 1)) (long (p 2)) (boolean shape?)
           deaf sides told (stepper ctx) chunk/min-y chunk/max-y)))

(defn- neighbors-changed [s ctx p old]
  (if (and (not (running? s)) (deaf-around? s :neighbor p))
    s
    (passed s ctx p false #(state-changed %1 ctx %2 %4 old %3))))

(defn- called [s ctx fx]
  (if (seq fx)
    (reduce (fn [s [k q st]]
              (if (and (= :neighbor-changed k)
                       (not (deaf-to-neighbor? st)))
                (add-and-run s ctx [:full q st])
                s))
            s (filter vector? fx))
    s))

(defn- shapes-changed [s ctx p old flags limit]
  (if (and (not (running? s)) (deaf-around? s :shape p))
    s
    (let [nst (block-at s p)
          f (bit-and (long flags) -34)]
      (passed s ctx p true
              #(shape-changed %1 ctx %2 %4 old %3 nst f limit)))))

(defn- run-by-level? [e]
  (or (= :fluid-tick e)
      (and (vector? e) (= :neighbor-changed (e 0)))))

(defn- shown [fx]
  (if (seq fx) (into [] (remove run-by-level?) fx) []))

(defn- written [s p old st fx flags]
  (let [fx (into (shown fx) (geyser/placed-fx st))]
    (-> (with-chunks s (chunk/chunks-set-block (chunks s) p st))
        (add-record (if (seq fx) [p st fx] [p st]))
        (add-write [p old st flags]))))

(defn- updated [s ctx p old st flags limit]
  (let [s (if (bit-test (long flags) 9) s (placed s ctx p old))
        limit (long limit)]
    (if (not= (long st) (block-at s p))
      s
      (cond-> s
        (bit-test (long flags) 1) (add-sent p)
        (odd? (long flags)) (neighbors-changed ctx p old)
        (and (pos? limit) (not (bit-test (long flags) 4)))
        (shapes-changed ctx p old flags (dec limit))))))

(defn- removed
  "Returns s after a rail old that st replaced at p tells the cells
  around it, as its affectNeighborsAfterRemoval does."
  [s ctx p old st flags]
  (let [flags (long flags)]
    (if (and (marked? :rail old) (odd? flags) (not (bit-test flags 6))
             (not= (block/block-of old) (block/block-of st)))
      (reduce #(neighbors-changed %1 ctx %2 old) s
              (rail/removal-notified p old))
      s)))

(defn- kept?
  "Tells whether p still holds the block of st after old left it."
  [s p old st]
  (or (not (marked? :rail old))
      (= (block/block-of st) (block/block-of (block-at s p)))))

(defn- liquid-placed [s ctx p old fx]
  (if (some #{:fluid-tick} fx)
    (fluid-woken s ctx p old nil)
    s))

(defn- settable? [s p]
  (and (chunk/in-range? (long (p 1)))
       (contains? (chunks s) (chunk/block-chunk p))))

(defn flags-of
  "Returns the flags that a change [p st fx flags] carries, or flags
  when it names none."
  ^long [c ^long flags]
  (long (get c 3 flags)))

(defn- stored
  [s ctx [p st fx] flags limit]
  (let [old (block-at s p)]
    (cond
      (not (settable? s p)) s
      (= old (long st))
      (let [fx (shown fx)]
        (cond-> s (seq fx) (add-record [p st fx])))
      :else (let [s (-> (written s p old st fx flags)
                        (removed ctx p old st flags))]
              (if (kept? s p old st)
                (-> (updated s ctx p old st flags limit)
                    (liquid-placed ctx p old fx))
                s)))))

(defn- plain-set [s ctx p st flags limit]
  (let [old (place! s p st flags)
        flags (long flags)
        limit (long limit)
        s (if (neg? old) s (removed s ctx p old st flags))]
    (if (or (neg? old) (not (kept? s p old st)))
      s
      (cond-> s
        (odd? flags) (neighbors-changed ctx p old)
        (and (pos? limit) (not (bit-test flags 4)))
        (shapes-changed ctx p old flags (dec limit))))))

(defn set-block
  "Returns s after change [p st fx] is set with flags and depth.
  Flag 1 tells the neighbours. Flag 2 tells the clients. Flag 16 skips
  shape updates. Flag 512 skips the placement reply. Shape replies
  keep the flags without 1 and 32. The neighbour calls that fx names
  run last. The cells that the clients hear of go to :sent, and each
  write to :writes as [p old st flags]."
  [s ctx [p st fx :as c] flags limit]
  (if (and (empty? fx) (marked? :plain st))
    (plain-set s ctx p st flags limit)
    (-> (stored s ctx c flags limit)
        (called ctx fx))))

(defn- placement-state
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (or st' st) fx]))

(defn- placed-with [flags]
  (fn [s ctx c]
    (set-block s ctx (placement-state (chunks s) ctx c)
               flags update-limit)))

(defn- level [^Neighbors s open?]
  (let [[records writes ticks sent placed] (.collected s)
        cs (chunks s)]
    {:chunks (if open? cs (chunk/frozen cs)) :records records
     :writes writes :ticks ticks :sent sent :placed placed}))

(defn- opened [chunks]
  (Neighbors.
    (if (chunk/editing? chunks) chunks (chunk/editable chunks))))

(defn- run-each
  "Runs f over the changes in one window of edits of chunks. Chunks
  already open stay open for the caller to freeze; others come out
  frozen again."
  [chunks ctx changes f]
  (level (reduce #(f %1 ctx %2) (opened chunks) changes)
         (chunk/editing? chunks)))

(defn set-blocks
  "Returns the level after changes [pos st fx] are placed in order
  with full updates. The result also holds the writes, the ticks asked
  for and the cells that the clients hear of. ctx gives the tick, the
  dimension, the rules and what the rules read."
  [chunks ctx changes]
  (run-each chunks ctx changes (placed-with 3)))

(defn- edged
  "StructureTemplate.updateShapeAtEdge on one face: p takes the shape
  that the cell on side gives, then that cell the shape p gives."
  [s ctx p side]
  (let [q (shifted p (dir/offset side))
        n (block-at s q)
        s (shape-changed s ctx p (block-at s p) n side n 2 update-limit)
        st (block-at s p)]
    (shape-changed s ctx q n st (dir/opposite side) st 2 update-limit)))

(defn- set-told
  "ServerLevel.updateNeighboursOnBlockSet: the cells around p hear of
  the change there from old."
  [s ctx p old]
  (-> (removed s ctx p old (block-at s p) 1)
      (neighbors-changed ctx p old)))

(defn- op-run [s ctx [op x y]]
  (case op
    (:set :put) (set-block s ctx x y update-limit)
    :notify (neighbors-changed s ctx x (block-at s x))
    :tell (set-told s ctx x y)
    :edge (edged s ctx x y)
    :send (add-sent s x)))

(defn run
  "Returns the level after ops run in order.
  An op [:set c flags] sets change c as it is, not shaped by its
  neighbours; [:put c flags] does the same. An op [:notify pos] tells
  the six blocks around pos of a change there, [:tell pos old] of a
  change from old. An op [:edge pos side] updates the shapes of pos
  and the cell on side against each other with flags 2. An op
  [:send pos] tells the clients of pos."
  [chunks ctx ops]
  (run-each chunks ctx ops op-run))

(defn- op-counted [[s n] ctx [op :as o]]
  (let [w (write-count s)
        s (op-run s ctx o)]
    [s (if (and (= :put op) (< w (write-count s)))
         (inc (long n))
         n)]))

(defn run-counted
  "Returns the level after ops run in order, as run does. Its :count
  holds the ops [:put c flags] that changed their cell."
  [chunks ctx ops]
  (let [start [(opened chunks) 0]
        [s n] (reduce #(op-counted %1 ctx %2) start ops)]
    (assoc (level s (chunk/editing? chunks)) :count n)))

(defn- command-state
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (if (and st' (not (zero? (long st')))) st' st) fx]))

(defn- command-placed
  [s ctx [p :as c]]
  (let [n (write-count s)
        c' (command-state (chunks s) ctx c)
        s' (set-block s ctx c' 258 update-limit)]
    (if (< n (write-count s'))
      (let [[q old] (write-at s' n)]
        (if (= p q) (add-placed s' [p old]) s'))
      s')))

(defn- commanded-each [^Neighbors s ctx changes]
  (.command s changes 258 (:plain @states) (:shape @states)
            #(shapes-changed %1 ctx %2 %3 258 (dec update-limit))
            #(command-placed %1 ctx %2) chunk/min-y chunk/max-y))

(defn- told [^Neighbors s ctx]
  (.tell s (:neighbor @states) #(neighbors-changed %1 ctx %2 %3)
         chunk/min-y chunk/max-y))

(defn- cell-destroyed [s ctx p]
  (let [old (block-at s p) n (write-count s)]
    (if (block/air-type? old)
      [s false]
      (let [s (set-block s ctx (block/destroyed p old) 3 update-limit)]
        [s (< n (write-count s))]))))

(defn- cell-placed [s ctx [p st :as c] strict?]
  (let [n (write-count s)
        c (if strict? c (command-state (chunks s) ctx c))
        s (set-block s ctx c (if strict? 818 258) update-limit)
        [q old] (when (< n (write-count s)) (write-at s n))]
    (if (= p q) [(add-placed s [p old]) true] [s false])))

(defn- cell [ctx {:keys [strict? destroy? test]}]
  (fn [[s n] [p st :as c]]
    (if (and test (not (test (block-at s p))))
      [s n]
      (let [[s hit] (if destroy? (cell-destroyed s ctx p) [s false])
            [s put] (if st (cell-placed s ctx c strict?) [s false])]
        [s (if (or hit put) (inc (long n)) n)]))))

(defn- commanded-by [chunks ctx changes opts]
  (let [[s n] (reduce (cell ctx opts) [(opened chunks) 0] changes)
        s (if (:strict? opts) s (told s ctx))]
    (assoc (level s (chunk/editing? chunks)) :count n)))

(defn commanded
  "Returns the level after the changes of a command.
  Each change is set with flags 258. Each change that alters its cell
  tells its neighbours once all changes are set. :placed holds those
  cells as [pos old] and :count the cells the command affected. With
  :test only cells whose state passes it change. With :destroy? each
  cell is first destroyed with its drops. With :strict? each change is
  set as it is with flags 818 and tells no one. A change of state nil
  places nothing."
  ([chunks ctx changes] (commanded chunks ctx changes nil))
  ([chunks ctx changes opts]
   (if (seq opts)
     (commanded-by chunks ctx changes opts)
     (let [l (level (told (commanded-each (opened chunks) ctx changes)
                          ctx)
                    (chunk/editing? chunks))]
       (assoc l :count (count (:placed l)))))))
