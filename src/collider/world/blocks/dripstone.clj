(ns collider.world.blocks.dripstone
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private grows-on {:pointed-dripstone :dripstone-block})
(def ^:private max-growth-length {:pointed-dripstone 7 :sulfur-spike 2})
(def ^:private cauldrons #{:cauldron :layered-cauldron :lava-cauldron})
(def ^:private water-chance 0.17578125)
(def ^:private lava-chance 0.05859375)
(def ^:private growth-chance 0.011377778)
(defn speleothem? [^long st] (block/tagged? st "speleothems"))
(defn- dir-of [^long st] (:vertical-direction (block/props-of st)))
(defn- thickness-of [^long st] (:thickness (block/props-of st)))
(defn- directed? [^long st dir] (and (speleothem? st) (= dir (dir-of st))))
(defn stalactite? [^long st] (directed? st :down))
(defn- stalagmite? [^long st] (directed? st :up))
(defn- tip? [^long st merged?]
  (and (speleothem? st)
       (or (= :tip (thickness-of st)) (and merged? (= :tip_merge (thickness-of st))))))

(defn- free-hanging? [^long st]
  (and (stalactite? st) (= :tip (thickness-of st)) (= :false (:waterlogged (block/props-of st)))))

(defn- unmerged-tip? [^long st dir self]
  (and (tip? st false) (= dir (dir-of st)) (= self (block/block-of st))))

(defn- fluid-of [^long st]
  (cond
    (not (pos? st)) nil
    (block/waterlogged? st) :water
    :else (liquid/liquid-class st)))

(defn- water-at? [chunks p] (= :water (fluid-of (gen/at-void chunks p))))
(defn- water-source-at? [chunks p]
  (let [st (gen/at-void chunks p)]
    (and (= :water (fluid-of st))
         (or (block/waterlogged? st) (liquid/source-state? st)))))

(defn valid-placement? [chunks p dir self]
  (let [b (gen/at-void chunks (mapv + p (dir/offset (dir/opposite dir))))]
    (and (not (neg? b))
         (or (block/face-sturdy? b dir)
             (and (directed? b dir) (= self (block/block-of b)))))))

(defn supported? [chunks p ^long st]
  (valid-placement? chunks p (dir-of st) (block/block-of st)))

(defn- thickness [chunks p dir merge? self]
  (let [base (dir/opposite dir)
        front (gen/at-void chunks (mapv + p (dir/offset dir)))]
    (cond
      (and (directed? front base) (= self (block/block-of front)))
      (if (and (not merge?) (not= :tip_merge (thickness-of front))) :tip :tip_merge)
      (not (directed? front dir)) :tip
      (contains? #{:tip :tip_merge} (thickness-of front)) :frustum
      (directed? (gen/at-void chunks (mapv + p (dir/offset base))) dir) :middle
      :else :base)))

(defn updated ^long [chunks p ^long st]
  (let [dir (dir-of st) self (block/block-of st)]
    (if-not (valid-placement? chunks p dir self)
      st
      (block/state self (assoc (block/props-of st)
                          :thickness (thickness chunks p dir (= :tip_merge (thickness-of st)) self))))))

(defn placed [chunks p st pitch sneaking?]
  (let [st (long st)
        self (block/block-of st)
        default (if (neg? (double pitch)) :down :up)
        dir (cond
              (valid-placement? chunks p default self) default
              (valid-placement? chunks p (dir/opposite default) self) (dir/opposite default))]
    (when dir
      (block/state self (assoc (block/props-of st)
                          :vertical-direction dir
                          :thickness (thickness chunks p dir (not sneaking?) self))))))

(defn- find-vertical [chunks p dir path? target? max-steps]
  (loop [i 1 q (mapv + p (dir/offset dir))]
    (when (< i (long max-steps))
      (let [st (gen/at-void chunks q)]
        (cond
          (neg? st) nil
          (target? st) q
          (path? st) (recur (inc i) (mapv + q (dir/offset dir)))
          :else nil)))))

(defn- find-tip [chunks p ^long st ^long max-len]
  (if (tip? st false)
    p
    (let [dir (dir-of st) self (block/block-of st)]
      (find-vertical chunks p dir
                     (fn [n] (and (= self (block/block-of n)) (= dir (dir-of n))))
                     (fn [n] (tip? n false))
                     max-len))))

(defn- root-pos [chunks p ^long st]
  (let [self (block/block-of st) dir (dir-of st)]
    (find-vertical chunks p :up
                   (fn [n] (and (= self (block/block-of n)) (= dir (dir-of n))))
                   (fn [n] (not= self (block/block-of n)))
                   11)))

(defn- fluid-above [chunks p ^long st]
  (when (stalactite? st)
    (when-let [root (root-pos chunks p st)]
      (let [q (dir/up root) n (gen/at-void chunks q)]
        (when-not (neg? n)
          {:pos q :state n :fluid (if (= :mud (block/block-of n)) :water (fluid-of n))})))))

(defn- drips-through? [^long st]
  (cond
    (zero? st) true
    (neg? st) false
    (block/solid-render? st) false
    (some? (fluid-of st)) false
    :else (not (some (fn [[x0 y0 z0 x1 y1 z1]]
                       (and (> (double x1) 6.0) (< (double x0) 10.0) (> (double y1) 0.0)
                            (< (double y0) 16.0) (> (double z1) 6.0) (< (double z0) 10.0)))
                     (block/collision-boxes st)))))

(defn- receives? [^long st fluid]
  (case (block/type-of st)
    :cauldron true
    :layered-cauldron (and (= :water fluid) (= :water-cauldron (block/block-of st)))
    false))

(defn- filled [^long st fluid]
  (case (block/type-of st)
    :cauldron (if (= :water fluid) (block/state :water-cauldron {:level :1}) (block/state :lava-cauldron))
    :layered-cauldron (let [lvl (block/prop-long st :level)]
                        (when (< lvl 3)
                          (block/state (block/block-of st) {:level (keyword (str (inc lvl)))})))
    nil))

(defn- start-pos? [chunks p ^long st]
  (and (stalactite? st) (not= (block/block-of st) (block/block-of (gen/at-void chunks (dir/up p))))))

(defn- fillable-cauldron [chunks tip fluid]
  (find-vertical chunks tip :down (fn [n] (drips-through? n)) (fn [n] (receives? n fluid)) 11))

(defn- tip-above-cauldron [chunks p]
  (find-vertical chunks p :up (fn [n] (drips-through? n)) (fn [n] (free-hanging? n)) 11))

(defn- fill-fluid [chunks tip]
  (let [f (:fluid (fluid-above chunks tip (gen/at-void chunks tip)))]
    (when (contains? #{:water :lava} f) f)))

(defn- transferred [chunks p ^long st ^double roll]
  (when-let [{:keys [pos state fluid]} (fluid-above chunks p st)]
    (let [prob (case fluid :water water-chance :lava lava-chance nil)]
      (when (and prob (< roll (double prob)))
        (when-let [tip (find-tip chunks p st 11)]
          (if (and (= :mud (block/block-of (long state))) (= :water fluid))
            {:tip tip :changes [[pos (block/state :clay)]]}
            (when-let [c (fillable-cauldron chunks tip fluid)]
              {:tip tip :cauldron c :delay (+ 50 (- (long (tip 1)) (long (c 1))))})))))))

(defn drip [chunks p ^long st roll]
  (when (= :pointed-dripstone (block/type-of st))
    (let [roll (double (roll :drip))]
      (when (and (< roll water-chance) (start-pos? chunks p st))
        (transferred chunks p st roll)))))

(defn- created [chunks q dir th self]
  (block/state self {:vertical-direction dir :thickness th
                     :waterlogged        (if (water-at? chunks q) :true :false)}))

(defn- merged [chunks q ^long tst self]
  (let [[a b] (if (= :up (dir-of tst)) [(dir/up q) q] [q (dir/down q)])]
    [[a (created chunks a :down :tip_merge self)]
     [b (created chunks b :up :tip_merge self)]]))

(defn- grown [chunks from dir self]
  (let [target (mapv + from (dir/offset dir)) n (gen/at-void chunks target)]
    (cond
      (unmerged-tip? n (dir/opposite dir) self) (merged chunks target n self)
      (or (zero? n) (= :water (block/block-of n))) [[target (created chunks target dir :tip self)]])))

(defn- can-tip-grow? [chunks q ^long tst]
  (let [dir (dir-of tst) n (gen/at-void chunks (mapv + q (dir/offset dir)))]
    (cond
      (neg? n) false
      (some? (fluid-of n)) false
      (zero? n) true
      :else (unmerged-tip? n (dir/opposite dir) (block/block-of tst)))))

(defn- stalagmite-below [chunks tip self]
  (loop [i 0 q (dir/down tip)]
    (when (< i 10)
      (let [n (gen/at-void chunks q)]
        (cond
          (neg? n) nil
          (some? (fluid-of n)) nil
          (and (unmerged-tip? n :up self) (can-tip-grow? chunks q n)) (grown chunks q :up self)
          (and (valid-placement? chunks q :up self) (not (water-at? chunks (dir/down q)))) (grown chunks (dir/down q) :up self)
          (not (drips-through? n)) nil
          :else (recur (inc i) (dir/down q)))))))

(defn- can-grow? [chunks p self]
  (and (= (grows-on self) (block/block-of (gen/at-void chunks (dir/up p))))
       (water-source-at? chunks (dir/up (dir/up p)))))

(defn- grow-changes [chunks p ^long st roll]
  (let [self (block/block-of st)]
    (when (can-grow? chunks p self)
      (when-let [tip (find-tip chunks p st (get max-growth-length (block/type-of st) 7))]
        (let [tst (gen/at-void chunks tip)]
          (when (and (free-hanging? tst) (can-tip-grow? chunks tip tst))
            (if (< (double (roll :which)) 0.5)
              (grown chunks tip :down self)
              (stalagmite-below chunks tip self))))))))

(defn random-changes [chunks p ^long st roll]
  (when (and (= :pointed-dripstone (block/type-of st))
             (< (double (roll :grow)) growth-chance)
             (start-pos? chunks p st))
    (grow-changes chunks p st roll)))

(defn- fall-changes [chunks p]
  (loop [q p acc []]
    (let [st (gen/at-void chunks q)]
      (if-not (stalactite? st)
        acc
        (let [acc (conj acc [q (block/emptied st)])]
          (if (tip? st true) acc (recur (dir/down q) acc)))))))

(def rule
  {:name   :dripstone
   :match? (fn [_chunks st _p] (speleothem? st))
   :wake   (fn [chunks tick p _old _self?]
             (let [st (gen/at-void chunks p)]
               (when-not (supported? chunks p st)
                 (+ (long tick) (if (stalactite? st) 2 1)))))
   :due    (fn [chunks p _ctx]
             (let [st (gen/at-void chunks p)]
               (if (and (stalagmite? st) (not (supported? chunks p st)))
                 [[p (block/emptied st)]]
                 (seq (fall-changes chunks p)))))})

(def cauldron-rule
  {:name   :cauldron-drip
   :match? (fn [_chunks st _p] (contains? cauldrons (block/type-of st)))
   :wake   (fn [_chunks _tick _p _old _self?] nil)
   :due    (fn [chunks p _ctx]
             (when-let [tip (tip-above-cauldron chunks p)]
               (when-let [fluid (fill-fluid chunks tip)]
                 (let [st (gen/at-void chunks p)]
                   (when (receives? st fluid)
                     (when-let [st' (filled st fluid)]
                       [[p st']]))))))})
