(ns collider.world.neighbors
  "Block changes with the neighbour updates they run at once."
  (:require [collider.cell :as cell]
            [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.blocks.button :as button]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.diode :as diode]
            [collider.world.blocks.dust :as dust]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.blocks.lectern :as lectern]
            [collider.world.blocks.lightning-rod :as rod]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.blocks.rail :as rail]
            [collider.world.blocks.torch :as torch]
            [collider.world.blocks.wire :as wire]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.rules :as rules]
            [collider.world.update :as update])
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

(defn- set-chunks! ^Neighbors [^Neighbors s chunks]
  (set! (.chunks s) chunks)
  s)

(defn- add-record! ^Neighbors [^Neighbors s x] (.record s x))

(defn- add-write! ^Neighbors [^Neighbors s x] (.write s x))

(defn- add-tick! ^Neighbors [^Neighbors s x] (.tick s x))

(defn- add-sent! ^Neighbors [^Neighbors s x] (.send s x))

(defn- add-placed! ^Neighbors [^Neighbors s p old]
  (.addPlaced s p (long old)))

(defn- write-count ^long [^Neighbors s] (.writeCount s))

(defn- record-count ^long [^Neighbors s] (.recordCount s))

(defn- write-at [^Neighbors s ^long n] (.writeAt s n))

(defn- running? [^Neighbors s] (.running s))

(defn- place! ^long [^Neighbors s p ^long st ^long flags]
  (.place s p (int (p 0)) (int (p 1)) (int (p 2)) st flags
          chunk/min-y chunk/max-y))

(defn- hears? [st pass]
  (let [p (rules/update-pass st)]
    (or (= pass p) (= :both p))))

(defn- shape-deaf? [^long st]
  (or (zero? st)
      (not (or (block/liquid? st) (rules/fluid-of st)
               (hears? st :shape)
               (connect/connecting? st)))))

(defn- neighbor-deaf? [^long st]
  (or (zero? st)
      (not (or (block/liquid? st) (hears? st :neighbor)))))

(defn- analog? [^long st]
  (and (< st (data/block-state-count)) (block/analog-output? st)))

(def ^:private emptied-types
  #{:furnace :blast-furnace :smoker :barrel :brewing-stand :chest
    :trapped-chest :copper-chest :weathering-copper-chest
    :chiseled-book-shelf :crafter :creaking-heart :decorated-pot
    :dispenser :dropper :hopper :jukebox :shelf :shulker-box
    :copper-golem-statue :weathering-copper-golem-statue})

(defn- front-ops
  "Returns the ops of a diode st at p that tell the cell it faces
  away from, or nil when st is no diode."
  [p ^long st]
  (when (diode/diode? st)
    (let [f (:facing (block/props-of st))
          q (dir/toward p (dir/opposite f))]
      [[:changed q st f] [:notify q f st]])))

(defn- placed-ops
  "Returns the ops that block st set at p runs, or nil."
  [p ^long st]
  (if (torch/torch? st) (torch/told p st) (front-ops p st)))

(defn- removal-ops
  "Returns the ops that block old at p runs when it goes."
  [p ^long old moved?]
  (let [t (block/type-of old)
        {:keys [powered]} (block/props-of old)]
    (cond
      (contains? emptied-types t) [[:output-signal p]]
      (lectern/lectern? old)
      (when (= :true powered) [[:notify (dir/toward p :down) nil old]])
      (rod/rod? old)
      (when (= :true powered)
        [[:notify (rod/front p old) nil old]])
      moved? nil
      (button/switch? old)
      (when (button/powered? old) (button/told p old))
      (diode/diode? old) (front-ops p old)
      (torch/torch? old) (torch/told p old)
      (= :redstone-wire t) (dust/removed p old)
      (rail/rail? old)
      (mapv #(vector :notify % nil old) (rail/removal-notified p old)))))

(defn- hooked? [^long st]
  (boolean (seq (removal-ops [0 0 0] st false))))

(defn- plain?
  "Returns true when placing st needs no reply from st."
  [^long st]
  (not (or (rules/update-pass st) (block/liquid? st)
           (rules/fluid-of st) (connect/connecting? st)
           (geyser/placed-fx st) (analog? st))))

(defn- by-state [f]
  (boolean-array (map (comp boolean f)
                      (range (inc (data/block-state-count))))))

(def ^:private states
  (delay {:shape (by-state shape-deaf?)
          :neighbor (by-state neighbor-deaf?)
          :plain (by-state plain?)
          :analog (by-state analog?)
          :hooked (by-state hooked?)
          :loud (by-state #(or (analog? %) (hooked? %)))
          :none (boolean-array (inc (data/block-state-count)))
          :wire (by-state wire/wire?)}))

(defn- marked? [k ^long st]
  (aget ^booleans (k @states) st))

(defn- has? [^long flags ^long flag]
  (not (zero? (bit-and flags flag))))

(defn- deaf-of ^booleans [ctx k]
  (if (:trace-updates ctx) (:none @states) (k @states)))

(defn- deaf-around? [^Neighbors s ctx k p]
  (.deafAround s (long (p 0)) (long (p 1)) (long (p 2))
               (deaf-of ctx k) chunk/min-y chunk/max-y))

(defn- block-at ^long [s p]
  (if (chunk/in-range? (long (p 1)))
    (chunk/at (chunks s) p)
    0))

(def ^:private told-all (constantly update/all))

(defn- ticked [s k at p ty priority]
  (if (and at ty)
    (add-tick! s [k (long at) (cell/pack p) ty (long priority)])
    s))

(declare set-block op-run)

(defn- destroys? [old st]
  (and (not (block/liquid? old))
       (or (zero? (long st)) (block/liquid? st))))

(defn- shape-flags ^long [^long flags old st]
  (if (destroys? old st)
    update/all
    (bit-and-not flags update/suppress-drops)))

(defn- reacted
  "Returns s after the changes of a rule. A change may name its own
  flags. A step fn gives the changes that follow the ones before it."
  [s ctx changes flags-for limit]
  (reduce (fn [s c]
            (cond
              (fn? c) (reacted s ctx (c (chunks s)) flags-for limit)
              (keyword? (c 0)) (op-run s ctx c)
              :else (let [[p st] c
                          f (get c 3 (flags-for (block-at s p) st))]
                      (set-block s ctx c f limit))))
          s changes))

(defn- rule-woken [s ctx p old side flags-for limit pass]
  (let [cs (chunks s)
        {:keys [dim tick]} ctx
        st (block-at s p)
        at (rules/wake-tick cs dim st tick p old side)]
    (if (= :neighbor at)
      (let [ctx' (assoc ctx :side side :old old :pass pass)]
        (reacted s (dissoc ctx :moved? :given)
                 (rules/reshape-changes cs st p ctx')
                 flags-for limit))
      (ticked s :block-ticks at p (block/block-of st) 0))))

(defn- fluid-woken [s ctx p old side]
  (let [{:keys [dim tick]} ctx
        st (block-at s p)
        at (rules/fluid-wake-tick (chunks s) dim st tick p old side)]
    (ticked s :fluid-ticks at p (rules/fluid-of st) 0)))

(defn- column-woken [s ctx p below]
  (let [st (block-at s p)
        at (liquid/column-wake st below (:tick ctx))]
    (ticked s :block-ticks at p (block/block-of st) 0)))

(defn- neighbor-woken [s ctx p old side]
  (rule-woken s ctx p old side told-all update-limit :neighbor))

(defn- liquid-woken [s ctx p old side]
  (let [st (block-at s p)
        s' (if (block/lava? st) (neighbor-woken s ctx p old side) s)]
    (if (= st (block-at s' p))
      (-> (fluid-woken s' ctx p old side)
          (column-woken ctx p (block-at s' (dir/toward p :down))))
      s')))

(defn- placed [s ctx p old moved?]
  (let [st (block-at s p)]
    (cond
      (block/liquid? st) (liquid-woken s ctx p old nil)
      (rules/hears-place? st)
      (rule-woken s (cond-> ctx moved? (assoc :moved? true)) p old nil
                  told-all update-limit :place)
      :else s)))

(defn- state-changed [s ctx q st old side]
  (cond
    (zero? (long st)) s
    (block/liquid? st) (liquid-woken s ctx q old side)
    (hears? st :neighbor)
    (neighbor-woken s (assoc ctx :given st) q old side)
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
        s (if (hears? st :shape)
            (rule-woken s ctx q old side f limit :shape)
            s)]
    (if (= st (block-at s q))
      (reshaped s ctx q st side flags limit)
      s)))

(defn- shape-changed [s ctx q st old side nst flags limit]
  (cond
    (zero? (long st)) s
    (and (has? flags update/skip-wire-shape)
         (= :redstone-wire (block/type-of st)))
    s
    (block/liquid? st) (liquid-shaped s ctx q old side st nst)
    :else (block-shaped s ctx q st old side flags limit)))

(defn- run-next! [s ctx [k p src x flags limit]]
  (case k
    :changed (state-changed s ctx p (block-at s p) src x)
    :shape (shape-changed s ctx p (block-at s p) src x src flags limit)
    (state-changed s (cond-> ctx x (assoc :moved? true))
                   p src src nil)))

(defn- stepper [ctx]
  (fn [s item] (run-next! s ctx item)))

(defn- add-and-run! [^Neighbors s ctx item]
  (.addAndRun s item (stepper ctx)))

(def ^:private skip-index
  {:west 0 :east 1 :down 2 :up 3 :north 4 :south 5})

(defn- passed [^Neighbors s ctx p shape? skip told]
  (let [^booleans deaf ((if shape? :shape :neighbor) @states)
        ^objects sides (if shape? shape-sides update-sides)]
    (.pass s (long (p 0)) (long (p 1)) (long (p 2)) (boolean shape?)
           (int (skip-index skip -1)) deaf sides told (stepper ctx)
           chunk/min-y chunk/max-y)))

(defn- neighbors-changed
  ([s ctx p src] (neighbors-changed s ctx p src nil))
  ([s ctx p src skip]
   (if (and (not (running? s)) (deaf-around? s ctx :neighbor p))
     s
     (passed s ctx p false skip
             #(state-changed %1 ctx %2 %4 src %3)))))

(defn- called [s ctx fx]
  (if (seq fx)
    (reduce (fn [s [k q st moved?]]
              (if (= :neighbor-changed k)
                (add-and-run! s ctx [:full q st moved?])
                s))
            s (filter vector? fx))
    s))

(defn- settable? [s p]
  (and (chunk/in-range? (long (p 1)))
       (contains? (chunks s) (chunk/block-chunk p))))

(defn- comparator? [^long st]
  (= :comparator (block/type-of st)))

(defn- output-signaled
  "Returns s after the comparators beside p, or behind a conductor
  beside it, hear of a change of the signal p gives them."
  [s ctx p]
  (reduce (fn [s side]
            (let [q (dir/toward p side)
                  st (block-at s q)
                  r (dir/toward q side)]
              (cond
                (not (settable? s q)) s
                (comparator? st) (add-and-run! s ctx [:full q st])
                (and (block/conductor? st)
                     (comparator? (block-at s r)))
                (add-and-run! s ctx [:full r (block-at s r)])
                :else s)))
          s [:north :east :south :west]))

(def ^:private ^:const shape-cleared
  (bit-or update/neighbors update/suppress-drops))

(defn- indirect-shaped
  "Returns s after the wires a step up or down beside wire st at p,
  past a side it runs to, take the shape of the cell beside them."
  [s ctx p st flags limit]
  (if (wire/wire? st)
    (let [f (bit-and-not (long flags) shape-cleared)]
      (reduce
        (fn [s d]
          (let [q (dir/toward p d)]
            (if (or (= :none (wire/side-of st d)) (wire/wire? (block-at s q)))
              s
              (reduce (fn [s v]
                        (let [r (dir/toward q v)]
                          (if (wire/wire? (block-at s r))
                            (let [n (dir/toward r (dir/opposite d))]
                              (add-and-run! s ctx [:shape r (block-at s n)
                                                   (dir/opposite d) f limit]))
                            s)))
                      s [:down :up]))))
        s [:north :east :south :west]))
    s))

(defn- shapes-changed [s ctx p old flags limit]
  (if (and (not (running? s)) (deaf-around? s ctx :shape p))
    s
    (let [nst (block-at s p)
          f (bit-and-not (long flags) shape-cleared)]
      (passed s ctx p true nil
              #(shape-changed %1 ctx %2 %4 old %3 nst f limit)))))

(defn- run-by-level? [e]
  (or (= :fluid-tick e)
      (and (vector? e) (= :neighbor-changed (e 0)))))

(defn- shown [fx]
  (if (seq fx) (into [] (remove run-by-level?) fx) []))

(defn- written [s p old st fx flags]
  (let [fx (into (shown fx) (geyser/placed-fx st))]
    (-> (set-chunks! s (chunk/chunks-set-block (chunks s) p st))
        (add-record! (if (seq fx) [p st fx] [p st]))
        (add-write! [p old st flags]))))

(defn- hooked [s ctx ops]
  (reacted s ctx ops told-all update-limit))

(defn- updated [s ctx p old st flags limit]
  (let [flags (long flags)
        s (if (has? flags update/skip-on-place)
            s
            (-> (placed s ctx p old
                        (has? flags update/moved-by-piston))
                (hooked ctx (placed-ops p st))))
        limit (long limit)]
    (if (not= (long st) (block-at s p))
      s
      (cond-> s
        (has? flags update/clients) (add-sent! p)
        (has? flags update/neighbors) (neighbors-changed ctx p old)
        (and (has? flags update/neighbors) (marked? :analog st))
        (output-signaled ctx p)
        (and (pos? limit) (not (has? flags update/known-shape)))
        (-> (indirect-shaped ctx p old flags (dec limit))
            (shapes-changed ctx p old flags (dec limit))
            (indirect-shaped ctx p st flags (dec limit)))))))

(defn- after-removal [s ctx p old moved?]
  (hooked s ctx (removal-ops p old moved?)))

(defn- removed
  "Returns s after block old that st replaced at p tells the cells
  around it that it went."
  [s ctx p old st flags]
  (let [flags (long flags)
        moved? (has? flags update/moved-by-piston)]
    (if (and (marked? :hooked old)
             (or (not= (block/block-of old) (block/block-of st))
                 (rail/rail? st))
             (or moved? (has? flags update/neighbors)))
      (after-removal s ctx p old moved?)
      s)))

(defn- kept? [s p st]
  (= (block/block-of st) (block/block-of (block-at s p))))

(defn- liquid-placed [s ctx p old fx]
  (if (some #{:fluid-tick} fx)
    (fluid-woken s ctx p old nil)
    s))

(defn flags-of
  ^long [c ^long flags]
  (long (get c 3 flags)))

(defn- stored
  [s ctx [p st fx] flags limit]
  (let [old (block-at s p)]
    (cond
      (not (settable? s p)) s
      (= old (long st))
      (let [fx (shown fx)]
        (cond-> s (seq fx) (add-record! [p st fx])))
      :else (let [s (-> (written s p old st fx flags)
                        (removed ctx p old st flags))]
              (if (kept? s p st)
                (-> (updated s ctx p old st flags limit)
                    (liquid-placed ctx p old fx))
                s)))))

(defn- plain-set [s ctx p st flags limit]
  (let [old (place! s p st flags)
        flags (long flags)
        limit (long limit)
        s (if (neg? old) s (removed s ctx p old st flags))]
    (if (or (neg? old) (not (kept? s p st)))
      s
      (cond-> s
        (has? flags update/neighbors) (neighbors-changed ctx p old)
        (and (pos? limit) (not (has? flags update/known-shape)))
        (-> (indirect-shaped ctx p old flags (dec limit))
            (shapes-changed ctx p old flags (dec limit))
            (indirect-shaped ctx p st flags (dec limit)))))))

(defn set-block
  "Returns s after change [p st fx] is set with update flags to depth
  limit. The neighbour calls that fx names run last. The cells that
  the clients hear of go to :sent, and each write to :writes as
  [p old st flags]."
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
  (let [[records writes ticks sent placed updates] (.collected s)
        cs (chunks s)]
    {:chunks (if open? cs (chunk/frozen cs)) :records records
     :writes writes :ticks ticks :sent sent :placed placed
     :updates updates}))

(defn unheard
  "Returns the distinct cells of the records of level l that the
  clients do not hear of, in order."
  [l]
  (Neighbors/unheard (:records l) (:sent l)))

(def ^:const chain-limit
  "How many neighbour updates a chain runs by default."
  1000000)

(defn- opened [chunks ctx]
  (Neighbors.
    (if (chunk/editing? chunks) chunks (chunk/editable chunks))
    (long (:max-chained-neighbor-updates ctx chain-limit))
    (boolean (:trace-updates ctx))))

(defn- run-each
  "Returns the level after f runs over changes in one edit window of
  chunks. The chunks come out open only when they came in open."
  [chunks ctx changes f]
  (level (reduce #(f %1 ctx %2) (opened chunks ctx) changes)
         (chunk/editing? chunks)))

(defn set-blocks
  "Returns the level after changes [pos st fx] are placed in order
  with full updates. The result also holds the writes, the ticks asked
  for and the cells that the clients hear of. ctx gives the tick, the
  dimension, the rules and what the rules read."
  [chunks ctx changes]
  (run-each chunks ctx changes (placed-with update/all)))

(defn- edged
  "Returns s after p and the cell on side take each other's shape."
  [s ctx p side]
  (let [q (dir/toward p side)
        n (block-at s q)
        f update/clients
        lim update-limit
        s (shape-changed s ctx p (block-at s p) n side n f lim)
        st (block-at s p)]
    (shape-changed s ctx q n st (dir/opposite side) st f
                   update-limit)))

(defn- set-told
  [s ctx p old]
  (let [st (block-at s p)]
    (cond-> s
      (and (marked? :hooked old)
           (not= (block/block-of old) (block/block-of st)))
      (after-removal ctx p old false)
      true (neighbors-changed ctx p st)
      (marked? :analog st) (output-signaled ctx p))))

(defn- op-run [s ctx [op x y z ty]]
  (case op
    (:set :put) (set-block s ctx x y update-limit)
    :notify (neighbors-changed s ctx x (or z (block-at s x)) y)
    :changed (add-and-run! s ctx [:changed x y z])
    :output-signal (output-signaled s ctx x)
    :tell (set-told s ctx x y)
    :edge (edged s ctx x y)
    :send (add-sent! s x)
    :tick (ticked s :block-ticks y x ty z)
    :signal (-> (set-chunks! s (chunk/chunks-set-signal (chunks s) x y))
                (add-record! [x (block-at s x) [[:signal y]]]))))

(defn- stepped [s ctx o]
  (if (fn? o)
    (reacted s ctx (o (chunks s)) told-all update-limit)
    (op-run s ctx o)))

(defn run
  "Returns the level after ops run in order. An op [:set c flags] or
  [:put c flags] sets change c as it is, not shaped by its neighbours.
  An op [:notify pos] tells the six blocks around pos of a change
  there, and [:tell pos old] tells them of a change to the block now
  at pos, with old as the block that went. An op
  [:edge pos side] updates the shapes of pos and the cell on side
  against each other with flags 2. An op [:send pos] tells the
  clients of pos. An op [:notify pos side] tells the blocks around
  pos but the one on side, [:notify pos side src] tells them of a
  change of block src, [:changed pos src side] tells the block at pos
  of a change of block src on side, [:output-signal pos] tells the
  comparators that read pos, [:tick pos at priority ty] asks for a
  tick of type ty at pos at tick at, and [:signal pos v] keeps v as
  the signal of the comparator at pos. A step fn of chunks gives
  the changes that follow the ops before it."
  [chunks ctx ops]
  (run-each chunks ctx ops stepped))

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
  (let [start [(opened chunks ctx) 0]
        [s n] (reduce #(op-counted %1 ctx %2) start ops)]
    (assoc (level s (chunk/editing? chunks)) :count n)))

(defn- run-added [runs by ^long n]
  (let [top (peek runs)]
    (cond (zero? n) runs
          (not= by (nth top 0 ::none)) (conj runs [by n])
          :else (conj (pop runs) [by (+ n (long (nth top 1)))]))))

(defn- authored-run [ctx ops]
  (fn [[s i runs] [by n]]
    (let [i (long i) j (+ i (long n)) r (record-count s)
          s (reduce #(op-run %1 ctx %2) s (subvec ops i j))]
      [s j (run-added runs by (- (record-count s) r))])))

(defn run-authored
  "Returns the level after ops run in order, as run does. Given runs
  [by n], n ops in a row of author by, its :authors holds runs [by n]
  of its records."
  [chunks ctx ops runs]
  (let [start [(opened chunks ctx) 0 []]
        [s _ out] (reduce (authored-run ctx ops) start runs)]
    (assoc (level s (chunk/editing? chunks)) :authors out)))

(defn- command-state
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (if (and st' (not (zero? (long st')))) st' st) fx]))

(defn- command-placed
  [s ctx [p :as c]]
  (let [n (write-count s)
        c' (command-state (chunks s) ctx c)
        s' (set-block s ctx c' update/silent update-limit)]
    (if (< n (write-count s'))
      (let [[q old] (write-at s' n)]
        (if (= p q) (add-placed! s' p old) s'))
      s')))

(def ^:private ^:const shape-depth (dec update-limit))

(defn- command-shaped [s ctx p old]
  (-> (indirect-shaped s ctx p old update/silent shape-depth)
      (shapes-changed ctx p old update/silent shape-depth)))

(defn- commanded-each [^Neighbors s ctx changes]
  (.command s changes update/silent (:plain @states)
            (deaf-of ctx :shape) (:wire @states)
            #(command-shaped %1 ctx %2 %3)
            #(command-placed %1 ctx %2) chunk/min-y chunk/max-y))

(defn- told [^Neighbors s ctx]
  (.tell s (deaf-of ctx :neighbor) (:loud @states)
         #(set-told %1 ctx %2 %3) chunk/min-y chunk/max-y))

(defn- cell-destroyed [s ctx p]
  (let [old (block-at s p) n (write-count s)]
    (if (block/air-type? old)
      [s false]
      (let [c (block/destroyed p old)
            s (set-block s ctx c update/all update-limit)]
        [s (< n (write-count s))]))))

(defn- cell-placed [s ctx [p :as c] strict?]
  (let [n (write-count s)
        c (if strict? c (command-state (chunks s) ctx c))
        f (if strict? update/strict update/silent)
        s (set-block s ctx c f update-limit)
        [q old] (when (< n (write-count s)) (write-at s n))]
    (if (= p q) [(add-placed! s p old) true] [s false])))

(defn- cell [ctx {:keys [strict? destroy? test]}]
  (fn [[s n] [p st :as c]]
    (if (and test (not (test (block-at s p))))
      [s n]
      (let [[s hit] (if destroy? (cell-destroyed s ctx p) [s false])
            [s put] (if st (cell-placed s ctx c strict?) [s false])]
        [s (if (or hit put) (inc (long n)) n)]))))

(defn- commanded-by [chunks ctx changes opts]
  (let [[s n] (reduce (cell ctx opts) [(opened chunks ctx) 0] changes)
        s (if (:strict? opts) s (told s ctx))]
    (assoc (level s (chunk/editing? chunks)) :count n)))

(defn- commanded-plain [chunks ctx changes]
  (let [s (-> (opened chunks ctx) (commanded-each ctx changes)
              (told ctx))
        l (level s (chunk/editing? chunks))]
    (assoc l :count (count (:placed l)))))

(defn commanded
  "Returns the level after the changes of a command.
  Each change is set with silent flags. Each change that alters its
  cell tells its neighbours once all changes are set. :placed holds
  those cells and :count the cells the command affected.
  With :test only cells whose state passes it change. With :destroy?
  each cell is first destroyed with its drops. With :strict? each
  change is set as it is with strict flags and tells no one. A change
  of state nil places nothing."
  ([chunks ctx changes] (commanded chunks ctx changes nil))
  ([chunks ctx changes opts]
   (if (seq opts)
     (commanded-by chunks ctx changes opts)
     (commanded-plain chunks ctx changes))))
