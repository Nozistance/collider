(ns collider.game.craft
  "Crafting recipes and their matching against a crafting grid."
  (:refer-clojure :exclude [find])
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(defn- proto [item k] (get-in (data/items) [item :components k]))

(defn- component [stack k]
  (let [cs (:components stack)]
    (cond (contains? cs k) (get cs k)
          (contains? (:removed stack) k) nil
          :else (proto (:item stack) k))))

(defn- has? [stack k] (some? (component stack k)))

(defn- tidy [stack]
  (cond-> stack
    (empty? (:components stack)) (dissoc :components)
    (empty? (:removed stack)) (dissoc :removed)))

(defn- put [stack k v]
  (let [s (update stack :components dissoc k)]
    (tidy (cond (= v (proto (:item stack) k))
                (update s :removed disj k)
                (nil? v) (update s :removed (fnil conj #{}) k)
                :else (-> s
                          (update :components assoc k v)
                          (update :removed disj k))))))

(defn- with-patch [stack patch]
  (reduce #(put %1 %2 nil)
          (reduce-kv put stack (:components patch))
          (:removed patch)))

(defn- valid [stack]
  (let [most (or (component stack :max-stack-size) 1)]
    (when-not (or (and (has? stack :max-damage) (> most 1))
                  (> (:count stack) most))
      stack)))

(defn- normal [stack]
  (when (and stack (pos? (:count stack 1)))
    (with-patch {:item (:item stack) :count (:count stack 1)} stack)))

(defn- create [template]
  (valid (with-patch (select-keys template [:item :count]) template)))

(defn- create-over [template n patch]
  (-> {:item (:item template) :count n}
      (with-patch patch)
      (with-patch template)
      valid))

(defn- test? [ingredient stack]
  (and (some? stack) (contains? ingredient (:item stack))))

(defn- remainder [stack]
  (some-> (get-in (data/items) [(:item stack) :remainder]) create))

(def ^:private empty-input
  {:w 0 :h 0 :stacks [] :left 0 :top 0 :n 0})

(defn- row-span [[l r t b] xs y]
  (if (seq xs)
    [(min l (first xs)) (max r (last xs)) (min t y) (max b y)]
    [l r t b]))

(defn- span [w h stacks]
  (reduce (fn [acc y]
            (let [xs (filter #(nth stacks (+ % (* y w))) (range w))]
              (row-span acc xs y)))
          [(dec w) 0 (dec h) 0]
          (range h)))

(defn- cut [stacks w l t nw nh]
  (vec (for [y (range nh) x (range nw)]
         (nth stacks (+ x l (* (+ y t) w))))))

(defn- positioned [grid w h stacks l t]
  (merge grid {:w w :h h :stacks stacks :left l :top t
               :n (count (filter some? stacks))}))

(defn trim
  "Returns grid without its empty rows and columns, and where the
   rest starts."
  [{:keys [w h stacks] :as grid}]
  (if (or (zero? w) (zero? h))
    (merge grid empty-input)
    (let [stacks (mapv normal stacks)
          [l r t b] (span w h stacks)
          nw (inc (- r l))
          nh (inc (- b t))]
      (cond (or (<= nw 0) (<= nh 0)) (merge grid empty-input)
            (and (= nw w) (= nh h)) (positioned grid w h stacks l t)
            :else (positioned grid nw nh
                              (cut stacks w l t nw nh) l t)))))

(defmulti matches?
  "Returns true if recipe crafts from input."
  (fn [recipe _] (:type recipe)))

(defmulti assemble
  "Returns the stack that recipe crafts from input."
  (fn [recipe _] (:type recipe)))

(defmulti remainders
  "Returns what stays in each slot of input after recipe crafts."
  (fn [recipe _] (:type recipe)))

(defmethod remainders :default [_ input]
  (mapv #(some-> % remainder) (:stacks input)))

(defmethod assemble :default [recipe _] (create (:result recipe)))

(defn- cells-match? [{:keys [w h cells]} stacks flip?]
  (every? (fn [i]
            (let [x (rem i w)
                  cell (nth cells (if flip? (+ (- w x 1) (- i x)) i))
                  s (nth stacks i)]
              (if cell (test? cell s) (nil? s))))
          (range (* w h))))

(defn- pattern-matches? [pattern {:keys [w h n stacks]}]
  (and (= n (count (filter some? (:cells pattern))))
       (= w (:w pattern))
       (= h (:h pattern))
       (or (and (not (:symmetric? pattern))
                (cells-match? pattern stacks true))
           (cells-match? pattern stacks false))))

(defmethod matches? :shaped [recipe input]
  (pattern-matches? recipe input))

(defn- take-one [freq item]
  (if (= 1 (freq item)) (dissoc freq item) (update freq item dec)))

(defn- fits? [ingredients k freq]
  (or (= k (count ingredients))
      (let [ing (nth ingredients k)]
        (boolean
          (some (fn [[item _]]
                  (and (contains? ing item)
                       (fits? ingredients (inc k)
                              (take-one freq item))))
                freq)))))

(defmethod matches? :shapeless [{:keys [ingredients]} input]
  (let [{:keys [n stacks]} input]
    (and (= n (count ingredients))
         (if (and (= 1 (count stacks)) (= 1 n))
           (test? (first ingredients) (first stacks))
           (and (every? seq ingredients)
                (fits? ingredients 0
                       (frequencies (keep :item stacks))))))))

(defn- in-bounds? [{:keys [min max]} v]
  (and (or (nil? min) (<= min v)) (or (nil? max) (<= v max))))

(defn- transmuted [{:keys [result]} input extra]
  (create-over result (+ (:count result) extra) input))

(defn- transmute-scan [{:keys [input material]} stacks most]
  (reduce (fn [[found m] s]
            (cond (nil? s) [found m]
                  (test? input s) (if found (reduced nil) [s m])
                  (not (test? material s)) (reduced nil)
                  (> (inc m) most) (reduced nil)
                  :else [found (inc m)]))
          [nil 0]
          stacks))

(defn- same-stack? [a b]
  (= (dissoc a :count) (dissoc b :count)))

(defn- transmute-result? [recipe found m]
  (let [n (:count (:result recipe))]
    (or (not= 1 (if (:add-material-count? recipe) (+ m n) n))
        (let [made (transmuted recipe found 0)]
          (and (some? made) (not (same-stack? found made)))))))

(defmethod matches? :transmute [recipe {:keys [n stacks]}]
  (let [bounds (:material-count recipe)
        hi (:max bounds 8)]
    (boolean
      (when (<= (inc (:min bounds 1)) n (inc hi))
        (when-let [[found m] (transmute-scan recipe stacks hi)]
          (and found
               (in-bounds? bounds m)
               (transmute-result? recipe found m)))))))

(defn- transmute-count [{:keys [input material]} stacks]
  (reduce (fn [[in m] s]
            (cond (nil? s) [in m]
                  (test? input s) [s m]
                  (test? material s) [in (inc m)]
                  :else [in m]))
          [nil 0]
          stacks))

(defmethod assemble :transmute [recipe {:keys [stacks]}]
  (if (:add-material-count? recipe)
    (let [[in m] (transmute-count recipe stacks)]
      (transmuted recipe in m))
    (when-let [in (first (filter #(test? (:input recipe) %) stacks))]
      (transmuted recipe in 0))))

(defn- dye-of [stack] (or (component stack :dye) :white))

(defn- dye-scan [{:keys [target dye]} stacks]
  (reduce (fn [[t d] s]
            (cond (nil? s) [t d]
                  (test? target s) (if t (reduced nil) [true d])
                  (and (test? dye s) (has? s :dye)) [t true]
                  :else (reduced nil)))
          [false false]
          stacks))

(defmethod matches? :dye [recipe {:keys [n stacks]}]
  (and (>= n 2)
       (let [[t d] (dye-scan recipe stacks)] (boolean (and t d)))))

(defn- error [f a b]
  (Math/abs (- (* (double f) (double b)) (double a))))

(defn- fdiv [a b]
  (let [q (/ (double a) (double b))
        f (float q)
        g (Math/nextAfter (float f) q)]
    (cond (== q (double f)) f
          (< (error f a b) (error g a b)) f
          (< (error g a b) (error f a b)) g
          (even? (Float/floatToIntBits f)) f
          :else g)))

(defn- fmul [a b] (float (* (double a) (double b))))

(defn- rgb [c]
  (mapv #(bit-and (bit-shift-right c %) 255) [16 8 0]))

(defn- mix-dyes [current dyes]
  (let [diffuse #(get-in (data/recipes) [:dyes % :diffuse])
        colors (map diffuse dyes)
        rgbs (map rgb (cond->> colors current (cons current)))
        n (count rgbs)
        light (reduce + (map #(apply max %) rgbs))
        avg (fdiv (float light) (float n))
        [r g b] (map #(quot % n) (apply map + rgbs))
        top (float (max r g b))
        scale #(int (fdiv (fmul (float %) avg) top))]
    (bit-or (bit-shift-left (scale r) 16)
            (bit-shift-left (scale g) 8)
            (scale b))))

(defn- dye-parts [{:keys [target dye]} stacks]
  (reduce (fn [[t ds] s]
            (cond (nil? s) [t ds]
                  (test? target s) (if t (reduced nil) [s ds])
                  (test? dye s) [t (conj ds (dye-of s))]
                  :else (reduced nil)))
          [nil []]
          stacks))

(defmethod assemble :dye [{:keys [result] :as recipe} input]
  (when-let [[t ds] (dye-parts recipe (:stacks input))]
    (when (and t (seq ds))
      (some-> (create-over result (:count result) t)
              (put :dyed-color
                   (mix-dyes (component t :dyed-color) ds))))))

(defmethod matches? :imbue [{:keys [source material]} input]
  (let [{:keys [w h n stacks]} input]
    (and (= 3 w) (= 3 h) (= 9 n)
         (every? (fn [i]
                   (test? (if (= 4 i) source material)
                          (nth stacks i)))
                 (range 9)))))

(defmethod assemble :imbue [recipe {:keys [w stacks]}]
  (let [source (nth stacks (+ 1 w))]
    (some-> (create (:result recipe))
            (put :potion-contents
                 (component source :potion-contents)))))

(defmethod matches? :decorated-pot [recipe {:keys [w h n stacks]}]
  (and (= 3 w) (= 3 h) (= 4 n)
       (every? (fn [[k i]] (test? (recipe k) (nth stacks i)))
               [[:back 1] [:left 3] [:right 5] [:front 7]])))

(defmethod assemble :decorated-pot [{:keys [result]} {:keys [stacks]}]
  (let [sides (mapv #(:item (nth stacks %) :air) [1 3 5 7])]
    (create-over result (:count result)
                 {:components {:pot-decorations sides}})))

(defn- layers [stack] (count (component stack :banner-patterns)))

(defn- banner-color [stack]
  (get-in (data/items) [(:item stack) :banner-color]))

(defn- banner-step [banner [color src tgt] s]
  (let [c (banner-color s)
        k (layers s)]
    (cond (nil? s) [color src tgt]
          (not (and (test? banner s) c)) (reduced nil)
          (and color (not= color c)) (reduced nil)
          (> k 6) (reduced nil)
          (pos? k) (if src (reduced nil) [c true tgt])
          :else (if tgt (reduced nil) [c src true]))))

(defmethod matches? :banner-duplicate [{:keys [banner]} input]
  (and (= 2 (:n input))
       (let [[_ src tgt] (reduce (partial banner-step banner)
                                 [nil false false]
                                 (:stacks input))]
         (boolean (and src tgt)))))

(defmethod assemble :banner-duplicate [{:keys [result]} input]
  (when-let [s (first (filter #(and % (<= 1 (layers %) 6))
                              (:stacks input)))]
    (create-over result (:count result) s)))

(defmethod remainders :banner-duplicate [_ {:keys [stacks]}]
  (mapv (fn [s]
          (when s
            (or (remainder s)
                (when (pos? (layers s)) (assoc s :count 1)))))
        stacks))

(defn- copyable? [{:keys [allowed-generations]} content]
  (and (some? content)
       (in-bounds? allowed-generations (:generation content))))

(defn- book-step [{:keys [source material] :as r} [src mat] s]
  (cond (nil? s) [src mat]
        (test? source s)
        (if (and (copyable? r (component s :written-book-content))
                 (not src))
          [true mat]
          (reduced nil))
        (test? material s) [src true]
        :else (reduced nil)))

(defmethod matches? :book-cloning [recipe {:keys [n stacks]}]
  (and (>= n 2)
       (let [[src mat] (reduce (partial book-step recipe)
                               [false false]
                               stacks)]
         (boolean (and src mat)))))

(defn- book-parts [{:keys [source material]} stacks]
  (reduce (fn [[src k] s]
            (cond (nil? s) [src k]
                  (and (test? source s)
                       (has? s :written-book-content))
                  (if src (reduced nil) [s k])
                  (test? material s) [src (inc k)]
                  :else (reduced nil)))
          [nil 0]
          stacks))

(defmethod assemble :book-cloning [{:keys [result] :as recipe} input]
  (when-let [[src k] (book-parts recipe (:stacks input))]
    (when-let [content (component src :written-book-content)]
      (some-> (create-over result (+ (:count result) (dec k)) src)
              (put :written-book-content
                   (update content :generation inc))))))

(defmethod remainders :book-cloning [_ {:keys [stacks]}]
  (loop [out (vec (repeat (count stacks) nil))
         i 0]
    (if (= i (count stacks))
      out
      (let [s (nth stacks i)]
        (if-let [r (some-> s remainder)]
          (recur (assoc out i r) (inc i))
          (if (has? s :written-book-content)
            (assoc out i (assoc s :count 1))
            (recur out (inc i))))))))

(defn- rocket-step [{:keys [shell fuel star]} [sh f] s]
  (cond (nil? s) [sh f]
        (test? shell s) (if sh (reduced nil) [true f])
        (test? fuel s) (if (> (inc f) 3) (reduced nil) [sh (inc f)])
        (test? star s) [sh f]
        :else (reduced nil)))

(defmethod matches? :firework-rocket [recipe {:keys [n stacks]}]
  (and (>= n 2)
       (let [[sh f] (reduce (partial rocket-step recipe)
                            [false 0]
                            stacks)]
         (boolean (and sh (pos? f))))))

(defmethod assemble :firework-rocket [recipe input]
  (let [{:keys [fuel star result]} recipe
        ss (remove nil? (:stacks input))
        stars (remove #(test? fuel %) ss)
        booms (vec (keep #(when (test? star %)
                            (component % :firework-explosion))
                         stars))
        flight (- (count ss) (count stars))]
    (create-over result (:count result)
                 {:components {:fireworks {:flight-duration flight
                                           :explosions booms}}})))

(defn- shape-of [{:keys [shapes]} s]
  (some (fn [[shape ing]] (when (test? ing s) shape)) shapes))

(defn- star-step [r [fu d sh tr tw] s]
  (let [{:keys [twinkle trail fuel dye]} r]
    (cond (nil? s) [fu d sh tr tw]
          (test? twinkle s) (if tw (reduced nil) [fu d sh tr true])
          (test? trail s) (if tr (reduced nil) [fu d sh true tw])
          (test? fuel s) (if fu (reduced nil) [true d sh tr tw])
          (and (test? dye s) (has? s :dye)) [fu true sh tr tw]
          (or (nil? (shape-of r s)) sh) (reduced nil)
          :else [fu d true tr tw])))

(defmethod matches? :firework-star [recipe {:keys [n stacks]}]
  (and (>= n 2)
       (let [[fu d] (reduce (partial star-step recipe)
                            [false false false false false]
                            stacks)]
         (boolean (and fu d)))))

(defn- firework-color [stack]
  (get-in (data/recipes) [:dyes (dye-of stack) :firework]))

(defn- star-part [r [shape tr tw cs] s]
  (let [found (shape-of r s)]
    (cond (nil? s) [shape tr tw cs]
          found [found tr tw cs]
          (test? (:twinkle r) s) [shape tr true cs]
          (test? (:trail r) s) [shape true tw cs]
          (test? (:dye r) s)
          [shape tr tw (conj cs (firework-color s))]
          :else [shape tr tw cs])))

(defmethod assemble :firework-star [recipe {:keys [stacks]}]
  (let [[shape tr tw cs] (reduce (partial star-part recipe)
                                 [:small-ball false false []]
                                 stacks)]
    (some-> (create (:result recipe))
            (put :firework-explosion
                 {:shape shape :colors cs :fade-colors []
                  :trail tr :twinkle tw}))))

(defn- fade-step [{:keys [target dye]} [d t] s]
  (cond (nil? s) [d t]
        (and (test? dye s) (has? s :dye)) [true t]
        (or (not (test? target s)) t) (reduced nil)
        :else [d true]))

(defmethod matches? :firework-star-fade [recipe {:keys [n stacks]}]
  (and (>= n 2)
       (let [[d t] (reduce (partial fade-step recipe)
                           [false false]
                           stacks)]
         (boolean (and d t)))))

(def ^:private plain-explosion
  {:shape :small-ball :colors [] :fade-colors []
   :trail false :twinkle false})

(defn- fade-parts [{:keys [target dye]} stacks]
  (reduce (fn [[t cs] s]
            (cond (test? dye s) [t (conj cs (firework-color s))]
                  (test? target s) [s cs]
                  :else [t cs]))
          [nil []]
          stacks))

(defmethod assemble :firework-star-fade [{:keys [result] :as r} input]
  (let [[t cs] (fade-parts r (:stacks input))]
    (when (and t (seq cs))
      (when-let [made (create-over result (:count result) t)]
        (put made :firework-explosion
             (assoc (or (component made :firework-explosion)
                        plain-explosion)
                    :fade-colors cs))))))

(defn- combinable? [a b]
  (and (= (:item a) (:item b))
       (= 1 (:count a) (:count b))
       (every? #(and (has? a %) (has? b %)) [:max-damage :damage])))

(defn- repair-pair [{:keys [n stacks]}]
  (when (= 2 n)
    (let [[a b] (remove nil? stacks)]
      (when (combinable? a b) [a b]))))

(defmethod matches? :repair-item [_ input]
  (some? (repair-pair input)))

(defn- max-damage [s] (or (component s :max-damage) 0))

(defn- left-uses [s]
  (let [m (max-damage s)]
    (- m (min (max (or (component s :damage) 0) 0) m))))

(defn- enchant-key [s]
  (if (= :enchanted-book (:item s))
    :stored-enchantments
    :enchantments))

(defn- curses [a b]
  (let [ea (or (component a (enchant-key a)) {})
        eb (or (component b (enchant-key b)) {})
        curse (set (get-in (data/tags) ["enchantment" "curse"]))]
    (into {}
          (for [e (distinct (concat (keys ea) (keys eb)))
                :when (curse e)
                :let [lvl (max (get ea e 0) (get eb e 0))]
                :when (pos? lvl)]
            [e (min lvl 255)]))))

(defn- enchant [stack a b]
  (let [k (enchant-key stack)]
    (if-some [old (component stack k)]
      (put stack k (merge old (curses a b)))
      stack)))

(defmethod assemble :repair-item [_ input]
  (when-let [[a b] (repair-pair input)]
    (let [most (max (max-damage a) (max-damage b))
          left (+ (left-uses a) (left-uses b) (quot (* most 5) 100))
          s (-> (normal {:item (:item a) :count 1})
                (put :max-damage most))]
      (-> s
          (put :damage (min (max (- most left) 0) (max-damage s)))
          (enchant a b)))))

(defn- map-pattern [{:keys [map material]}]
  (let [m material]
    {:w 3 :h 3 :symmetric? true :cells [m m m m map m m m m]}))

(defn- filled-map [stacks]
  (first (filter #(has? % :map-id) stacks)))

(defn- extendable? [maps s]
  (when-let [saved (get maps (component s :map-id))]
    (and (not (:exploration? saved)) (< (:scale saved) 4))))

(defmethod matches? :map-extending [recipe input]
  (boolean
    (when (pattern-matches? (map-pattern recipe) input)
      (when-let [s (filled-map (:stacks input))]
        (extendable? (:maps input) s)))))

(defmethod assemble :map-extending [{:keys [result]} {:keys [stacks]}]
  (some-> (create-over result (:count result) (filled-map stacks))
          (put :map-post-processing :scale)))

(defn- shield-step [{:keys [banner target]} [clear pat] s]
  (cond (nil? s) [clear pat]
        (and (test? banner s) (banner-color s))
        (if pat (reduced nil) [clear true])
        (or (not (test? target s)) clear (pos? (layers s)))
        (reduced nil)
        :else [true pat]))

(defmethod matches? :shield-decoration [recipe {:keys [n stacks]}]
  (and (= 2 n)
       (let [[clear pat] (reduce (partial shield-step recipe)
                                 [false false]
                                 stacks)]
         (boolean (and clear pat)))))

(defn- shield-parts [{:keys [banner target]} stacks]
  (reduce (fn [[ps base t] s]
            (cond (nil? s) [ps base t]
                  (and (test? banner s) (banner-color s))
                  [(component s :banner-patterns) (banner-color s) t]
                  (test? target s) [ps base s]
                  :else [ps base t]))
          [nil :white nil]
          stacks))

(defmethod assemble :shield-decoration [{:keys [result] :as r} input]
  (let [[ps base t] (shield-parts r (:stacks input))]
    (some-> (create-over result (:count result) t)
            (put :banner-patterns ps)
            (put :base-color base))))

(defn- bucket [{:keys [type w h cells ingredients]}]
  (case type
    :shaped [:shaped [w h (count (filter some? cells))]]
    :shapeless [:shapeless (count ingredients)]
    [:other]))

(defn index-of [recipes]
  (reduce (fn [idx r] (update-in idx (bucket r) (fnil conj []) r))
          {:by-id (into {} (map (juxt :id identity)) recipes)}
          (sort-by :order recipes)))

(def ^:private ^:table crafting-index
  (delay (index-of (:crafting (data/recipes)))))

(defn index [] @crafting-index)

(defn- candidates [idx {:keys [w h n]}]
  (sort-by :order (concat (get-in idx [:shaped [w h n]])
                          (get-in idx [:shapeless n])
                          (:other idx))))

(defn find
  "Returns the recipe that crafts input, the hinted recipe first if it
   does."
  [idx input hint]
  (let [hinted (get-in idx [:by-id hint])]
    (if (and hinted (matches? hinted input))
      hinted
      (when (pos? (:n input))
        (some #(when (matches? % input) %) (candidates idx input))))))
