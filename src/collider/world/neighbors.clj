(ns collider.world.neighbors
  "Block changes with the neighbour updates they run at once."
  (:require [collider.world.block :as block]
            [collider.world.blocks.connect :as connect]
            [collider.world.blocks.geyser :as geyser]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private update-order
  [[[-1 0 0] :east] [[1 0 0] :west] [[0 -1 0] :up]
   [[0 1 0] :down] [[0 0 -1] :south] [[0 0 1] :north]])

(def ^:private shape-order
  [[[-1 0 0] :east] [[1 0 0] :west] [[0 0 -1] :south]
   [[0 0 1] :north] [[0 -1 0] :up] [[0 1 0] :down]])

(def ^:const update-limit
  "How deep shape updates go."
  512)

(defn- shifted [[x y z] [dx dy dz]]
  [(+ (long x) (long dx)) (+ (long y) (long dy))
   (+ (long z) (long dz))])

(defn- block-at ^long [s p]
  (if (chunk/in-range? (long (p 1)))
    (chunk/chunks-get-block (:chunks s) p)
    0))

(defn- ticked [s ctx k at p ty]
  (if (and at ty)
    (let [at (max (long at) (inc (long (:tick ctx))))]
      (update s :ticks conj [k at (chunk/block-pos->id p) ty]))
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
              (reacted s ctx (c (:chunks s)) flags-of limit)
              (let [[p st] c
                    f (get c 3 (flags-of (block-at s p) st))]
                (set-block s ctx c f limit))))
          s changes))

(defn- rule-woken [s ctx p old side flags-of limit]
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

(defn- column-woken [s ctx p below]
  (let [st (block-at s p)
        at (liquid/column-wake st below (:tick ctx))]
    (ticked s ctx :block-ticks at p (block/block-of st))))

(defn- liquid-woken [s ctx p old side]
  (let [st (block-at s p)
        s' (rule-woken s ctx p old side (constantly 3) update-limit)]
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

(defn- neighbor-changed [s ctx q old side]
  (state-changed s ctx q (block-at s q) old side))

(defn- reshaped [s ctx q st side flags limit]
  (if-let [new (connect/reshape (:chunks s) q st (:tick ctx) #{side})]
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

(defn- shape-changed [s ctx q old side nst flags limit]
  (let [st (block-at s q)]
    (cond
      (zero? st) s
      (block/liquid? st) (liquid-shaped s ctx q old side st nst)
      :else (block-shaped s ctx q st old side flags limit))))

(defn- run-next
  [s ctx {:keys [kind p old i side nst flags limit st] :as item}]
  (case kind
    :multi (let [[d side] (nth update-order i)
                 s (neighbor-changed s ctx (shifted p d) old side)
                 i (inc (long i))]
             [s (when (< i (count update-order)) (assoc item :i i))])
    :full [(state-changed s ctx p st st nil) nil]
    [(shape-changed s ctx p old side nst flags limit) nil]))

(defn- run-top [s ctx]
  (let [[s item] (run-next s ctx (peek (:stack s)))
        s (update s :stack pop)]
    (cond
      (nil? item) s
      (seq (:added s)) (update s :stack conj item)
      :else (recur (update s :stack conj item) ctx))))

(defn- run-updates [s ctx]
  (loop [s s]
    (let [s (-> s
                (update :stack into (rseq (:added s)))
                (assoc :added []))]
      (if (empty? (:stack s))
        (assoc s :count 0)
        (recur (run-top s ctx))))))

(defn- add-and-run [s ctx item]
  (if (pos? (long (:count s)))
    (update s :added conj item)
    (-> (assoc s :count 1 :stack (list item))
        (run-updates ctx))))

(defn- neighbors-changed [s ctx p old]
  (add-and-run s ctx {:kind :multi :p p :old old :i 0}))

(defn- called [s ctx fx]
  (reduce (fn [s [k q st]]
            (if (= :neighbor-changed k)
              (add-and-run s ctx {:kind :full :p q :st st})
              s))
          s (filter vector? fx)))

(defn- shape-item [p old nst flags limit [d side]]
  {:kind :shape :p (shifted p d) :old old :side side :nst nst
   :flags flags :limit limit})

(defn- shapes-changed [s ctx p old flags limit]
  (let [nst (block-at s p)
        f (bit-and (long flags) -34)
        item #(shape-item p old nst f limit %)]
    (reduce #(add-and-run %1 ctx (item %2)) s shape-order)))

(defn- run-by-level? [e]
  (or (= :fluid-tick e)
      (and (vector? e) (= :neighbor-changed (e 0)))))

(defn- shown [fx]
  (into [] (remove run-by-level?) fx))

(defn- written [s p old st fx flags]
  (let [fx (into (shown fx) (geyser/placed-fx st))]
    (-> s
        (update :chunks chunk/chunks-set-blocks [[p st]])
        (update :records conj (if (seq fx) [p st fx] [p st]))
        (update :writes conj [p old st flags]))))

(defn- updated [s ctx p old st flags limit]
  (let [s (if (bit-test (long flags) 9) s (placed s ctx p old))
        limit (long limit)]
    (if (not= (long st) (block-at s p))
      s
      (cond-> s
        (bit-test (long flags) 1) (update :sent conj p)
        (odd? (long flags)) (neighbors-changed ctx p old)
        (and (pos? limit) (not (bit-test (long flags) 4)))
        (shapes-changed ctx p old flags (dec limit))))))

(defn- liquid-placed [s ctx p old fx]
  (if (some #{:fluid-tick} fx)
    (fluid-woken s ctx p old nil)
    s))

(defn- settable? [s p]
  (and (chunk/in-range? (long (p 1)))
       (contains? (:chunks s) (chunk/block-chunk p))))

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
        (cond-> s (seq fx) (update :records conj [p st fx])))
      :else (-> (written s p old st fx flags)
                (updated ctx p old st flags limit)
                (liquid-placed ctx p old fx)))))

(defn set-block
  "Returns s after change [p st fx] is set with flags and depth.
  Flag 1 tells the neighbours. Flag 2 tells the clients. Flag 16 skips
  shape updates. Flag 512 skips the placement reply. Shape replies
  keep the flags without 1 and 32. The neighbour calls that fx names
  run last. The cells that the clients hear of go to :sent, and each
  write to :writes as [p old st flags]."
  [s ctx [_ _ fx :as c] flags limit]
  (-> (stored s ctx c flags limit)
      (called ctx fx)))

(defn- placement-state
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (or st' st) fx]))

(def ^:private blank
  {:records [] :writes [] :ticks [] :placed [] :sent [] :stack ()
   :added [] :count 0})

(defn- placed-with [flags]
  (fn [s ctx c]
    (set-block s ctx (placement-state (:chunks s) ctx c)
               flags update-limit)))

(defn- run-each [chunks ctx changes f]
  (reduce #(f %1 ctx %2) (assoc blank :chunks chunks) changes))

(defn set-blocks
  "Returns the level after changes [pos st fx] are placed in order
  with full updates. The result also holds the writes, the ticks asked
  for and the cells that the clients hear of. ctx gives the tick, the
  dimension, the rules and what the rules read."
  [chunks ctx changes]
  (run-each chunks ctx changes (placed-with 3)))

(defn- op-run [s ctx [op x flags]]
  (case op
    :set (set-block s ctx x flags update-limit)
    :notify (neighbors-changed s ctx x (block-at s x))))

(defn run
  "Returns the level after ops run in order.
  An op [:set c flags] sets change c as it is, not shaped by its
  neighbours. An op [:notify pos] tells the six blocks around pos of a
  change there."
  [chunks ctx ops]
  (run-each chunks ctx ops op-run))

(defn- command-state
  [chunks ctx [p st fx]]
  (let [st' (when (chunk/in-range? (long (p 1)))
              (connect/reshape chunks p st (:tick ctx)))]
    [p (if (and st' (not (zero? (long st')))) st' st) fx]))

(defn- command-placed
  [s ctx [p :as c]]
  (let [n (count (:writes s))
        c' (command-state (:chunks s) ctx c)
        s' (set-block s ctx c' 258 update-limit)
        [q old] (get (:writes s') n)]
    (if (= p q)
      (update s' :placed conj [p old])
      s')))

(defn commanded
  "Returns the level after the changes of a command.
  Each change is set with flags 258. Each change that alters its cell
  tells its neighbours once all changes are set. :placed holds those
  cells as [pos old]."
  [chunks ctx changes]
  (let [s (run-each chunks ctx changes command-placed)]
    (reduce (fn [s [p old]] (neighbors-changed s ctx p old))
            s (:placed s))))
