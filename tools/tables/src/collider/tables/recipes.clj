(ns collider.tables.recipes
  "Reading the recipes of crafting, cooking and smithing."
  (:require [clojure.string :as str]
            [collider.tables.brewing :as brewing]
            [collider.tables.files :refer [jsons]]
            [collider.tables.tags :refer [ingredient item-set]]
            [collider.tables.value
             :refer [flt kw sorted-vals unknown]]))

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
      {:in  (ingredient (get json "ingredient"))
       :out (cond-> {:item (kw (get r "id"))}
              (not= 1 n) (assoc :count n))})))

(defn- stonecutting [recipes]
  (into [] (keep stonecutting-entry) recipes))

(defn- property-set [tags recipes [types field]]
  (let [want? (fn [json]
                (and (contains? types (get json "type"))
                     (contains? json field)))
        pick (fn [json]
               (when (want? json)
                 (let [i (ingredient (get json field))]
                   (if (map? i)
                     (get-in tags ["item" (:tag i)] [])
                     i))))]
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
  (let [i (ingredient v)] (if (map? i) [:tag (:tag i)] (vec i))))

(defn- stew-effects [v]
  (mapv (fn [e]
          {:effect (kw (get e "id"))
           :duration (get e "duration" 160)})
        v))

(defn- result-component [[k v]]
  (if (= k "minecraft:suspicious_stew_effects")
    [:suspicious-stew-effects (stew-effects v)]
    (throw (unknown "result component not modelled" {k v}))))

(defn- result-components [cs]
  (into (sorted-map) (map result-component) cs))

(defn- result [r]
  (let [r (if (string? r) {"id" r} r)
        cs (get r "components")]
    (cond-> {:item (kw (get r "id")) :count (get r "count" 1)}
      (seq cs) (assoc :components (result-components cs)))))

(def ^:private smithing-types
  {"minecraft:smithing_transform" :transform
   "minecraft:smithing_trim"      :trim})

(defn- smithing-recipe [tags id json type]
  (cond-> (sorted-map :id (kw id) :type type
                      :base (item-set tags (get json "base")))
    (get json "template")
    (assoc :template (item-set tags (get json "template")))
    (get json "addition")
    (assoc :addition (item-set tags (get json "addition")))
    (= :trim type) (assoc :pattern (kw (get json "pattern")))
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
        (throw (unknown "undefined symbol" {:symbol ch})))))

(defn- shaped [tags json]
  (let [rows (shrink (get json "pattern"))
        raw (mapv #(cell-key json %) (apply str rows))
        w (count (first rows))
        h (count rows)
        cells (mapv #(some->> % (item-set tags)) raw)
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
    {:shapes (mapv (fn [[k v]] [(kw k) (item-set tags v)])
                   (get json "shapes"))}
    {}))

(defn- fields-of [tags type json]
  (into (extra-fields tags type json)
        (map (fn [f] [(kw f) (item-set tags (get json f))]))
        (ingredient-fields type)))

(defn- shapeless [tags json]
  {:ingredients (mapv #(item-set tags %) (get json "ingredients"))})

(defn- crafting-recipe [tags order [id json]]
  (let [type (crafting-types (get json "type"))
        r (get json "result")]
    (cond-> (merge {:id (kw id) :order order :type type}
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
    {:id       (kw id)
     :type     type
     :in       (ingredient (get json "ingredient"))
     :out      {:item (kw (get r "id")) :count (get r "count" 1)}
     :time     (get json "cookingtime" default)
     :xp       (flt (get json "experience" 0.0))
     :category (kw (get json "category" "misc"))}))

(defn- cooking [recipes]
  (into []
        (keep (fn [[id json]]
                (when-let [t (cooking-types (get json "type"))]
                  (cooking-recipe id json t))))
        recipes))

(def ^:private fuel-values
  [[:lava-bucket 20000] [:coal-block 16000] [:blaze-rod 2400]
   [:coal 1600] [:charcoal 1600]
   [{:tag "logs"} 300] [{:tag "bamboo_blocks"} 300]
   [{:tag "planks"} 300] [:bamboo-mosaic 300]
   [{:tag "wooden_stairs"} 300] [:bamboo-mosaic-stairs 300]
   [{:tag "wooden_slabs"} 150] [:bamboo-mosaic-slab 150]
   [{:tag "wooden_trapdoors"} 300]
   [{:tag "wooden_pressure_plates"} 300]
   [{:tag "wooden_shelves"} 300] [{:tag "wooden_fences"} 300]
   [{:tag "fence_gates"} 300] [:note-block 300] [:bookshelf 300]
   [:chiseled-bookshelf 300] [:lectern 300] [:jukebox 300]
   [:chest 300] [:trapped-chest 300] [:crafting-table 300]
   [:daylight-detector 300] [{:tag "banners"} 300] [:bow 300]
   [:fishing-rod 300] [:ladder 300]
   [{:tag "signs"} 200] [{:tag "hanging_signs"} 800]
   [:wooden-shovel 200] [:wooden-sword 200] [:wooden-spear 200]
   [:wooden-hoe 200] [:wooden-axe 200] [:wooden-pickaxe 200]
   [{:tag "wooden_doors"} 200] [{:tag "boats"} 1200]
   [{:tag "wool"} 100] [{:tag "wooden_buttons"} 100] [:stick 100]
   [{:tag "saplings"} 100] [:bowl 100]
   [{:tag "wool_carpets"} 67] [:dried-kelp-block 4001]
   [:crossbow 300] [:bamboo 50] [:dead-bush 100]
   [:short-dry-grass 100] [:tall-dry-grass 100] [:scaffolding 50]
   [:loom 300] [:barrel 300] [:cartography-table 300]
   [:fletching-table 300] [:smithing-table 300] [:composter 300]
   [:azalea 100] [:flowering-azalea 100] [:mangrove-roots 300]
   [:leaf-litter 100]])

(defn- fuel-targets [tags target]
  (if (map? target) (get-in tags ["item" (:tag target)] []) [target]))

(defn- fuel [tags items]
  (let [add (fn [m [target time]]
              (into m (comp (filter items) (map #(vector % time)))
                    (fuel-targets tags target)))]
    (apply dissoc (reduce add (sorted-map) fuel-values)
           (get-in tags ["item" "non_flammable_wood"] []))))

(defn- property-sets-of [tags recipes]
  (sorted-vals property-sets #(vec (property-set tags recipes %))))

(defn recipes [zf tags dyes items potions]
  (let [named (->> (jsons zf "data/minecraft/recipe/")
                   (remove #(str/includes? (key %) "/")))
        rs (map val named)]
    {:stonecutting  (stonecutting rs)
     :property-sets (property-sets-of tags rs)
     :crafting      (crafting named tags)
     :cooking       (cooking named)
     :smithing      (smithing named tags)
     :fuel          (fuel tags items)
     :brewing       (brewing/brewing tags items potions)
     :dyes          dyes}))
