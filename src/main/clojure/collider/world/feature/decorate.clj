(ns collider.world.feature.decorate
  "The root placer and the decorators of trees."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.feature.level :as lv :refer [off]]))

(set! *warn-on-reflection* true)

(defn- shuffled [l xs]
  (random/shuffled #(lv/next-int l %) xs))

(defn- root-spot? [l m p]
  (or (lv/valid? l p)
      (lv/in? (lv/at l p) (:can-grow-through m))))

(defn- ahead [l m p d origin]
  (let [below (lv/at-dir p :down)
        n (lv/manhattan p origin)
        w (long (:max-root-width m))
        skew (:random-skew-chance m)]
    (cond
      (and (> n (- w 3)) (<= n w))
      (if (lv/chance? l skew)
        [below (lv/at-dir (lv/at-dir p d) :down)]
        [below])
      (> n w) [below]
      (lv/chance? l skew) [below]
      (lv/next-bool l) [(lv/at-dir p d)]
      :else [below])))

(declare simulate)

(defn- root-step [l m d origin layer found q]
  (if (root-spot? l m q)
    (or (simulate l m q d origin (conj found q) (inc (long layer)))
        (reduced nil))
    found))

(defn- simulate [l m p d origin found layer]
  (let [n (long (:max-root-length m))]
    (when (and (not= layer n) (<= (count found) n))
      (reduce #(root-step l m d origin layer %1 %2)
              found (ahead l m p d origin)))))

(defn- wet [l p st]
  (if (contains? (block/props-of st) :waterlogged)
    (block/with st :waterlogged (lv/water? l p))
    st))

(defn- above-root [l rp p]
  (let [above (:above-root-placement rp)
        up (lv/at-dir p :up)]
    (if (and above
             (lv/chance? l (:above-root-placement-chance above))
             (lv/air? l up))
      (let [st (lv/provide l (:above-root-provider above) up)]
        (lv/placed l :roots up (wet l up st)))
      l)))

(defn- root [l rp p]
  (let [m (:mangrove-root-placement rp)]
    (cond
      (lv/in? (lv/at l p) (:muddy-roots-in m))
      (let [st (lv/provide l (:muddy-roots-provider m) p)]
        (lv/placed l :roots p (wet l p st)))
      (root-spot? l m p)
      (let [st (lv/provide l (:root-provider rp) p)]
        (above-root (lv/placed l :roots p (wet l p st)) rp p))
      :else l)))

(defn- column-free? [l m origin trunk]
  (every? #(root-spot? l m (off origin 0 % 0))
          (range (- (long (trunk 1)) (long (origin 1))))))

(defn- side-roots [l m trunk cells d]
  (when cells
    (let [start (lv/at-dir trunk d)]
      (when-let [found (simulate l m start d trunk [] 0)]
        (conj (into cells found) start)))))

(defn roots
  "Returns l with the roots that root placer rp grows from origin to
  the trunk, or nil when they do not fit."
  [l rp origin trunk]
  (let [m (:mangrove-root-placement rp)
        side #(side-roots l m trunk %1 %2)
        start [(lv/at-dir trunk :down)]]
    (when (column-free? l m origin trunk)
      (when-let [cells (reduce side start lv/horizontal)]
        (reduce #(root %1 rp %2) l cells)))))

(defn trunk-origin
  "Returns where the trunk starts above origin, drawn by placer rp."
  [l rp origin]
  (off origin 0 (lv/sample l (:trunk-offset-y rp)) 0))

(defn- vine [l p side]
  (lv/placed l :decorations p (block/state :vine {side :true})))

(defn- hanging-vine [l p side]
  (loop [l (vine l p side) q (lv/at-dir p :down) n 4]
    (if (and (lv/air? l q) (pos? n))
      (recur (vine l q side) (lv/at-dir q :down) (dec n))
      l)))

(def ^:private vine-sides
  [[:west :east] [:east :west] [:north :south] [:south :north]])

(defn- leaf-vines [m l p]
  (reduce (fn [l [d side]]
            (if (and (lv/chance? l (:probability m))
                     (lv/air? l (lv/at-dir p d)))
              (hanging-vine l (lv/at-dir p d) side)
              l))
          l vine-sides))

(defn- leave-vines [l m ctx]
  (reduce #(leaf-vines m %1 %2) l (:leaves ctx)))

(defn- log-vines [l p]
  (reduce (fn [l [d side]]
            (if (and (pos? (lv/next-int l 3))
                     (lv/air? l (lv/at-dir p d)))
              (vine l (lv/at-dir p d) side)
              l))
          l vine-sides))

(defn- trunk-vines [l _m ctx]
  (reduce log-vines l (:logs ctx)))

(defn- hive-y ^long [l {:keys [leaves logs]}]
  (let [low (long ((first logs) 1))]
    (if (seq leaves)
      (max (dec (long ((first leaves) 1))) (inc low))
      (min (+ low 1 (lv/next-int l 3)) (long ((peek logs) 1))))))

(defn- skip-bee-draws [l]
  (dotimes [_ (+ 2 (lv/next-int l 2))] (lv/next-int l 599))
  l)

(defn- hive-spot? [l p]
  (and (lv/air? l p) (lv/air? l (lv/at-dir p :south))))

(defn- beehive [l m ctx]
  (if (or (empty? (:logs ctx)) (not (lv/chance? l (:probability m))))
    l
    (let [y (hive-y l ctx)
          spots (for [p (:logs ctx) :when (= y (long (p 1)))
                      d [:east :south :west]]
                  (lv/at-dir p d))
          nest (block/state :bee-nest {:facing :south})]
      (if-let [p (->> (shuffled l spots)
                      (filter #(hive-spot? l %))
                      first)]
        (skip-bee-draws (lv/placed l :decorations p nest))
        l))))

(defn- ground-at [l m p]
  (loop [dy 2 l l]
    (if (< dy -3)
      l
      (let [q (off p 0 dy 0)]
        (if-let [st (lv/optional l (:provider m) q)]
          (lv/placed l :decorations q st)
          (if (and (not (lv/air? l q)) (neg? dy))
            l
            (recur (dec dy) l)))))))

(def ^:private circle-cells
  (for [x (range -2 3) z (range -2 3)
        :when (not (and (= 2 (Math/abs (long x)))
                        (= 2 (Math/abs (long z)))))]
    [x z]))

(defn- circle [l m p]
  (reduce (fn [l [x z]] (ground-at l m (off p x 0 z)))
          l circle-cells))

(defn- lowest [{:keys [roots logs]}]
  (cond
    (empty? roots) logs
    (and (seq logs) (= ((first roots) 1) ((first logs) 1)))
    (into logs roots)
    :else roots))

(defn- edge-circle [m p l _]
  (let [n (lv/next-int l 64) x (mod n 8) z (quot n 8)]
    (if (or (= x 0) (= x 7) (= z 0) (= z 7))
      (circle l m (off p (- x 3) 0 (- z 3)))
      l)))

(defn- ground-rings [l m p]
  (let [l (reduce (fn [l [x z]] (circle l m (off p x 0 z)))
                  l [[-1 -1] [2 -1] [-1 2] [2 2]])]
    (reduce #(edge-circle m p %1 %2) l (range 5))))

(defn- alter-ground [l m ctx]
  (let [ps (lowest ctx)]
    (if (empty? ps)
      l
      (let [y ((first ps) 1)]
        (reduce #(ground-rings %1 m %2) l
                (filter #(= y (% 1)) ps))))))

(defn- empty-run? [l m p d]
  (every? #(lv/air? l (lv/toward p d %))
          (range 1 (inc (long (:required-empty-blocks m))))))

(defn- with-excluded [bl p m]
  (let [r (long (:exclusion-radius-xz m))
        ry (long (:exclusion-radius-y m))]
    (into bl (for [x (range (- r) (inc r)) y (range (- ry) (inc ry))
                   z (range (- r) (inc r))]
               (off p x y z)))))

(defn- attach-one [m [l bl] leaf]
  (let [d (lv/pick l (:directions m))
        p (lv/at-dir leaf d)]
    (if (and (not (bl p)) (lv/chance? l (:probability m))
             (empty-run? l m leaf d))
      [(lv/placed l :decorations p
                  (lv/provide l (:block-provider m) p))
       (with-excluded bl p m)]
      [l bl])))

(defn- attached [l m ctx]
  (first (reduce #(attach-one m %1 %2)
                 [l #{}] (shuffled l (:leaves ctx)))))

(defn decorate
  "Returns l with the decorators decs of the tree in ctx, in order."
  [l decs ctx]
  (reduce (fn [l m]
            ((case (:type m)
               :beehive beehive
               :alter-ground alter-ground
               :leave-vine leave-vines
               :trunk-vine trunk-vines
               :attached-to-leaves attached)
             l m ctx))
          l decs))
