(ns collider.data.recipes
  "The crafting, cooking, smithing and stonecutting recipes read from
  the recipes of a pack."
  (:require [collider.data.pack :as pack]))

(set! *warn-on-reflection* true)

(def ^:private property-sets
  (let [smithing #{"minecraft:smithing_transform"
                   "minecraft:smithing_trim"}
        campfire #{"minecraft:campfire_cooking"}]
    {"furnace_input"       [#{"minecraft:smelting"} "ingredient"]
     "blast_furnace_input" [#{"minecraft:blasting"} "ingredient"]
     "smoker_input"        [#{"minecraft:smoking"} "ingredient"]
     "campfire_input"      [campfire "ingredient"]
     "smithing_base"       [smithing "base"]
     "smithing_template"   [smithing "template"]
     "smithing_addition"   [smithing "addition"]}))

(defn- stonecutting-entry [json]
  (when (= "minecraft:stonecutting" (get json "type"))
    (let [r (get json "result")
          r (if (string? r) {"id" r} r)
          n (get r "count" 1)]
      {:in  (pack/ingredient (get json "ingredient"))
       :out (cond-> {:item (pack/kw (get r "id"))}
              (not= 1 n) (assoc :count n))})))

(defn- stonecutting [recipes]
  (into [] (keep stonecutting-entry) recipes))

(defn- property-set [tags recipes [types field]]
  (let [want? (fn [json]
                (and (contains? types (get json "type"))
                     (contains? json field)))
        pick (fn [json]
               (when (want? json)
                 (let [i (pack/ingredient (get json field))]
                   (if (map? i) (or (tags (:tag i)) []) i))))]
    (into (sorted-set) (mapcat pick) recipes)))

(def ^:private crafting-types
  {"minecraft:crafting_shaped"                    :shaped
   "minecraft:crafting_shapeless"                 :shapeless
   "minecraft:crafting_transmute"                 :transmute
   "minecraft:crafting_dye"                       :dye
   "minecraft:crafting_imbue"                     :imbue
   "minecraft:crafting_decorated_pot"             :decorated-pot
   "minecraft:crafting_special_bannerduplicate"   :banner-duplicate
   "minecraft:crafting_special_bookcloning"       :book-cloning
   "minecraft:crafting_special_firework_rocket"   :firework-rocket
   "minecraft:crafting_special_firework_star"     :firework-star
   "minecraft:crafting_special_firework_star_fade" :firework-star-fade
   "minecraft:crafting_special_repairitem"        :repair-item
   "minecraft:crafting_special_mapextending"      :map-extending
   "minecraft:crafting_special_shielddecoration"  :shield-decoration})

(defn- raw-ingredient [v]
  (let [i (pack/ingredient v)] (if (map? i) [:tag (:tag i)] (vec i))))

(defn- stew-effects [v]
  (mapv (fn [e]
          {:effect (pack/kw (get e "id"))
           :duration (get e "duration" 160)})
        v))

(defn- result-component [[k v]]
  (if (= k "minecraft:suspicious_stew_effects")
    [:suspicious-stew-effects (stew-effects v)]
    (throw (pack/unknown "result component not modelled" {k v}))))

(defn- result-components [cs]
  (into (sorted-map) (map result-component) cs))

(defn- result [r]
  (let [r (if (string? r) {"id" r} r)
        cs (get r "components")]
    (cond-> {:item (pack/kw (get r "id")) :count (get r "count" 1)}
      (seq cs) (assoc :components (result-components cs)))))

(def ^:private smithing-types
  {"minecraft:smithing_transform" :transform
   "minecraft:smithing_trim"      :trim})

(defn- smithing-recipe [tags id json type]
  (cond-> (sorted-map :id (pack/kw id) :type type
                      :base (pack/item-set tags (get json "base")))
    (get json "template")
    (assoc :template (pack/item-set tags (get json "template")))
    (get json "addition")
    (assoc :addition (pack/item-set tags (get json "addition")))
    (= :trim type) (assoc :pattern (pack/kw (get json "pattern")))
    (= :transform type) (assoc :result (result (get json "result")))))

(defn- smithing [recipes tags]
  (into []
        (keep (fn [[id json]]
                (when-let [t (smithing-types (get json "type"))]
                  (smithing-recipe tags id json t))))
        recipes))

(defn- bounds [v default]
  (cond (nil? v) default
        (number? v) {:min v :max v}
        :else (cond-> {}
                (contains? v "min") (assoc :min (get v "min"))
                (contains? v "max") (assoc :max (get v "max")))))

(defn- shrink-step [[left right top bottom] [i ^String line]]
  (let [first-non (count (take-while #(= \space %) line))
        trail (count (take-while #(= \space %) (reverse line)))
        last-non (- (count line) 1 trail)]
    [(min left first-non) (max right last-non)
     (if (and (neg? last-non) (= top i)) (inc top) top)
     (if (neg? last-non) (inc bottom) 0)]))

(defn- shrink [pattern]
  (let [[left right top bottom]
        (reduce shrink-step [Integer/MAX_VALUE 0 0 0]
                (map-indexed vector pattern))
        n (count pattern)]
    (if (= n bottom)
      []
      (mapv #(subs (nth pattern (+ % top)) left (inc right))
            (range (- n bottom top))))))

(defn- symmetric? [w h cells]
  (let [cell (fn [x y] (nth cells (+ x (* y w))))
        mirrored? (fn [[x y]] (= (cell x y) (cell (- w 1 x) y)))
        pairs (for [y (range h) x (range (quot w 2))] [x y])]
    (or (= 1 w) (every? mirrored? pairs))))

(defn- cell-key [json ch]
  (when (not= \space ch)
    (or (get-in json ["key" (str ch)])
        (throw (pack/unknown "undefined symbol" {:symbol ch})))))

(defn- shaped [tags json]
  (let [rows (shrink (get json "pattern"))
        raw (mapv #(cell-key json %) (apply str rows))
        w (count (first rows))
        h (count rows)
        cells (mapv #(some->> % (pack/item-set tags)) raw)
        mirror (mapv #(some-> % raw-ingredient) raw)]
    {:w w :h h :cells cells :symmetric? (symmetric? w h mirror)}))

(def ^:private ingredient-fields
  {:transmute          ["input" "material"]
   :dye                ["target" "dye"]
   :imbue              ["source" "material"]
   :decorated-pot      ["back" "left" "right" "front"]
   :banner-duplicate   ["banner"]
   :book-cloning       ["source" "material"]
   :firework-rocket    ["shell" "fuel" "star"]
   :firework-star      ["trail" "twinkle" "fuel" "dye"]
   :firework-star-fade ["target" "dye"]
   :map-extending      ["map" "material"]
   :shield-decoration  ["banner" "target"]})

(defn- extra-fields [tags type json]
  (case type
    :transmute
    {:material-count
     (bounds (get json "material_count") {:min 1 :max 1})
     :add-material-count?
     (get json "add_material_count_to_result" false)}
    :book-cloning
    {:allowed-generations
     (bounds (get json "allowed_generations") {:min 0 :max 1})}
    :firework-star
    {:shapes (mapv (fn [[k v]] [(pack/kw k) (pack/item-set tags v)])
                   (get json "shapes"))}
    {}))

(defn- fields-of [tags type json]
  (into (extra-fields tags type json)
        (map (fn [f] [(pack/kw f) (pack/item-set tags (get json f))]))
        (ingredient-fields type)))

(defn- shapeless [tags json]
  {:ingredients (mapv #(pack/item-set tags %)
                      (get json "ingredients"))})

(defn- crafting-recipe [tags order [id json]]
  (let [type (crafting-types (get json "type"))
        r (get json "result")]
    (cond-> (merge {:id (pack/kw id) :order order :type type}
                   (case type
                     :shaped (shaped tags json)
                     :shapeless (shapeless tags json)
                     (fields-of tags type json)))
      r (assoc :result (result r)))))

(defn- crafting [recipes tags]
  (into []
        (map-indexed #(crafting-recipe tags %1 %2))
        (filter #(crafting-types (get (val %) "type")) recipes)))

(def ^:private cooking-types
  {"minecraft:smelting"         [:smelting 200]
   "minecraft:blasting"         [:blasting 100]
   "minecraft:smoking"          [:smoking 100]
   "minecraft:campfire_cooking" [:campfire 100]})

(defn- cooking-recipe [id json [type default]]
  (let [r (get json "result")
        r (if (string? r) {"id" r} r)]
    {:id       (pack/kw id)
     :type     type
     :in       (pack/ingredient (get json "ingredient"))
     :out      {:item (pack/kw (get r "id")) :count (get r "count" 1)}
     :time     (get json "cookingtime" default)
     :xp       (pack/flt (get json "experience" 0.0))
     :category (pack/kw (get json "category" "misc"))}))

(defn- cooking [recipes]
  (into []
        (keep (fn [[id json]]
                (when-let [t (cooking-types (get json "type"))]
                  (cooking-recipe id json t))))
        recipes))

(defn- property-sets-of [tags recipes]
  (into (sorted-map)
        (map (fn [[k v]] [k (vec (property-set tags recipes v))]))
        property-sets))

(defn recipes
  "Returns the recipes of a pack by kind, from its recipes by id in
  id order. Tags gives the items of an item tag by name, or nil for
  no such tag."
  [pack tags]
  (let [named (sort-by key pack)
        rs (map val named)]
    (pack/plain
     {:stonecutting  (stonecutting rs)
      :property-sets (property-sets-of tags rs)
      :crafting      (crafting named tags)
      :cooking       (cooking named)
      :smithing      (smithing named tags)})))
