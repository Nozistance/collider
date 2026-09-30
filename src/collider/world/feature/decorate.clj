(ns collider.world.feature.decorate
  "The root placer and the decorators of trees."
  (:require [collider.world.block :as block]
            [collider.world.feature.level :as lv]
            [collider.world.feature.trunk :refer [off]]))

(set! *warn-on-reflection* true)

(defn shuffled
  "Util.shuffle of the vector xs."
  [l xs]
  (reduce (fn [v i]
            (let [j (lv/next-int l i)
                  a (v (dec i))]
              (assoc v (dec i) (v j) j a)))
          (vec xs) (range (count xs) 1 -1)))

(defn- manhattan ^long [a b]
  (reduce + (map #(Math/abs (- (long %1) (long %2))) a b)))

(defn- root-spot? [l m p]
  (or (lv/valid? l p)
      (lv/in? (lv/at l p) (:can-grow-through m))))

(defn- ahead [l m p d origin]
  (let [below (lv/at-dir p :down)
        n (manhattan p origin)
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

(defn- simulate
  "MangroveRootPlacer.simulateRoots: the cells out, or nil when the
  roots fail."
  [l m p d origin found layer]
  (let [n (long (:max-root-length m))]
    (when (and (not= layer n) (<= (count found) n))
      (reduce (fn [found q]
                (if (root-spot? l m q)
                  (or (simulate l m q d origin (conj found q) (inc layer))
                      (reduced nil))
                  found))
              found (ahead l m p d origin)))))

(defn- wet [l p st]
  (if (contains? (block/props-of st) :waterlogged)
    (lv/with-prop st :waterlogged (lv/water? l p))
    st))

(defn- root [l rp p]
  (let [m (:mangrove-root-placement rp)]
    (cond
      (lv/in? (lv/at l p) (:muddy-roots-in m))
      (let [st (lv/provide l (:muddy-roots-provider m) p)]
        (lv/placed l :roots p (wet l p st)))
      (root-spot? l m p)
      (let [l (lv/placed l :roots p
                         (wet l p (lv/provide l (:root-provider rp) p)))
            above (:above-root-placement rp)
            up (lv/at-dir p :up)]
        (if (and above (lv/chance? l (:above-root-placement-chance above))
                 (lv/air? l up))
          (let [st (lv/provide l (:above-root-provider above) up)]
            (lv/placed l :roots up (wet l up st)))
          l))
      :else l)))

(defn- column-free? [l m origin trunk]
  (every? #(root-spot? l m (off origin 0 % 0))
          (range (- (long (trunk 1)) (long (origin 1))))))

(defn roots
  "RootPlacer.placeRoots: lv with the roots, or nil when they fail."
  [l rp origin trunk]
  (let [m (:mangrove-root-placement rp)
        side (fn [cells d]
               (when cells
                 (when-let [found (simulate l m (lv/at-dir trunk d) d trunk
                                            [] 0)]
                   (conj (into cells found) (lv/at-dir trunk d)))))]
    (when (column-free? l m origin trunk)
      (when-let [cells (reduce side [(lv/at-dir trunk :down)]
                               lv/horizontal)]
        (reduce #(root %1 rp %2) l cells)))))

(defn trunk-origin
  "RootPlacer.getTrunkOrigin."
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

(defn- leave-vines [l m ctx]
  (reduce (fn [l p]
            (reduce (fn [l [d side]]
                      (if (and (lv/chance? l (:probability m))
                               (lv/air? l (lv/at-dir p d)))
                        (hanging-vine l (lv/at-dir p d) side)
                        l))
                    l vine-sides))
          l (:leaves ctx)))

(defn- trunk-vines [l _dec ctx]
  (reduce (fn [l p]
            (reduce (fn [l [d side]]
                      (if (and (pos? (lv/next-int l 3))
                               (lv/air? l (lv/at-dir p d)))
                        (vine l (lv/at-dir p d) side)
                        l))
                    l vine-sides))
          l (:logs ctx)))

(defn- hive-y ^long [l {:keys [leaves logs]}]
  (let [low (long ((first logs) 1))]
    (if (seq leaves)
      (max (dec (long ((first leaves) 1))) (inc low))
      (min (+ low 1 (lv/next-int l 3)) (long ((peek logs) 1))))))

(defn- bees [l]
  (dotimes [_ (+ 2 (lv/next-int l 2))] (lv/next-int l 599))
  l)

(defn- beehive [l m ctx]
  (if (or (empty? (:logs ctx)) (not (lv/chance? l (:probability m))))
    l
    (let [y (hive-y l ctx)
          spots (for [p (:logs ctx) :when (= y (long (p 1)))
                      d [:east :south :west]]
                  (lv/at-dir p d))]
      (if-let [p (->> (shuffled l spots)
                      (filter #(and (lv/air? l %)
                                    (lv/air? l (lv/at-dir % :south))))
                      first)]
        (bees (lv/placed l :decorations p
                         (block/state :bee-nest {:facing :south})))
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

(defn- circle [l m p]
  (reduce (fn [l [x z]] (ground-at l m (off p x 0 z)))
          l (for [x (range -2 3) z (range -2 3)
                  :when (not (and (= 2 (Math/abs (long x)))
                                  (= 2 (Math/abs (long z)))))]
              [x z])))

(defn- lowest [{:keys [roots logs]}]
  (cond
    (empty? roots) logs
    (and (seq logs) (= ((first roots) 1) ((first logs) 1))) (into logs roots)
    :else roots))

(defn- ground-rings [l m p]
  (let [l (reduce (fn [l [x z]] (circle l m (off p x 0 z)))
                  l [[-1 -1] [2 -1] [-1 2] [2 2]])]
    (reduce (fn [l _]
              (let [n (lv/next-int l 64) x (mod n 8) z (quot n 8)]
                (if (or (= x 0) (= x 7) (= z 0) (= z 7))
                  (circle l m (off p (- x 3) 0 (- z 3)))
                  l)))
            l (range 5))))

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

(defn- blocked [bl p m]
  (let [r (long (:exclusion-radius-xz m)) ry (long (:exclusion-radius-y m))]
    (into bl (for [x (range (- r) (inc r)) y (range (- ry) (inc ry))
                   z (range (- r) (inc r))]
               (off p x y z)))))

(defn- attached [l m ctx]
  (first
    (reduce (fn [[l bl] leaf]
              (let [d (lv/pick l (:directions m))
                    p (lv/at-dir leaf d)]
                (if (and (not (bl p)) (lv/chance? l (:probability m))
                         (empty-run? l m leaf d))
                  [(lv/placed l :decorations p
                              (lv/provide l (:block-provider m) p))
                   (blocked bl p m)]
                  [l bl])))
            [l #{}] (shuffled l (:leaves ctx)))))

(defn decorate
  "TreeDecorator.place of each decorator in order."
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
