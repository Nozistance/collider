(ns collider.game.commands
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def block-names
  {"air" 0 "stone" 1 "grass" 2 "dirt" 3 "cobblestone" 4 "planks" 5 "sapling" 6
   "bedrock" 7 "flowing_water" 8 "water" 9 "flowing_lava" 10 "lava" 11
   "sand" 12 "gravel" 13 "gold_ore" 14 "iron_ore" 15 "coal_ore" 16 "log" 17
   "leaves" 18 "sponge" 19 "glass" 20 "lapis_ore" 21 "lapis_block" 22
   "sandstone" 24 "web" 30 "tallgrass" 31 "wool" 35 "gold_block" 41
   "iron_block" 42 "stone_slab" 44 "brick_block" 45 "tnt" 46 "bookshelf" 47
   "mossy_cobblestone" 48 "obsidian" 49 "torch" 50 "fire" 51 "oak_stairs" 53
   "chest" 54 "diamond_ore" 56 "diamond_block" 57 "crafting_table" 58
   "farmland" 60 "furnace" 61 "ladder" 65 "rail" 66 "stone_stairs" 67
   "snow_layer" 78 "ice" 79 "snow" 80 "clay" 82 "fence" 85 "pumpkin" 86
   "netherrack" 87 "soul_sand" 88 "glowstone" 89 "lit_pumpkin" 91
   "stonebrick" 98 "glass_pane" 102 "melon_block" 103 "nether_brick" 112
   "end_stone" 121 "wooden_slab" 126 "emerald_ore" 129 "emerald_block" 133
   "beacon" 138 "cobblestone_wall" 139 "redstone_block" 152 "quartz_block" 155
   "hay_block" 170 "carpet" 171 "hardened_clay" 172 "coal_block" 173
   "packed_ice" 174 "prismarine" 168 "sea_lantern" 169 "slime" 165})

(def commands [

   [:time "change or query the time of day"
    [:set "set the time" [[:value [:named-int {:min 0 :max 2147483647
                                               :names {"day" 1000 "night" 13000}}]]]
     [:world :time-set]]
    [:add "advance the time" [[:value [:int {:min 0 :max 2147483647}]]]
     [:world :time-add]]
    [:query "read the clock" [[:clock [:enum {:values #{"daytime" "gametime"}}]]]
     [:world :time-query]]]
   [:fill "fill a box with a block (~ = your position)"
    [[:x1 [:coord {:min -10000 :max 10000 :axis 0}]]
     [:y1 [:coord {:min 0 :max 255 :axis 1}]]
     [:z1 [:coord {:min -10000 :max 10000 :axis 2}]]
     [:x2 [:coord {:min -10000 :max 10000 :axis 0}]]
     [:y2 [:coord {:min 0 :max 255 :axis 1}]]
     [:z2 [:coord {:min -10000 :max 10000 :axis 2}]]
     [:block [:named-int {:min 0 :max 255 :default 1 :names block-names}]]]
    [:world :fill]]])

(defn- subcommands? [form] (keyword? (first (nth form 2))))
(defn- cmd-name [form] (name (first form)))
(defn- find-form [forms nm] (first (filter #(= nm (cmd-name %)) forms)))
(defn- label [[nm [kind {:keys [min max]}]]]
  (case kind
    :int (str "<" (name nm) " " min "-" max ">")
    (:coord :named-int :enum) (str "<" (name nm) ">")))

(defn- parse-long* [^String s]
  (try (Long/parseLong s) (catch NumberFormatException _ nil)))

(defn- in-range [nm n {:keys [min max]}]
  (if (<= (long min) (long n) (long max))
    [:ok n]
    [:err (str (name nm) ": give a value from " min " to " max ", not " n)]))

(defn- as-int [nm s opts _origin]
  (if-let [n (parse-long* s)]
    (in-range nm n opts)
    [:err (str (name nm) ": give a whole number, not \"" s "\"")]))

(defn- as-named-int [nm s {:keys [names] :as opts} _origin]
  (if-let [n (let [k (str/lower-case (str/replace (str s) #"^minecraft:" ""))]
               (or (get names k) (parse-long* s)))]
    (in-range nm n opts)
    [:err (str (name nm) ": give a whole number or "
               (str/join "/" (take 6 (sort (keys names))))
               (when (> (count names) 6) "/...")
               ", not \"" s "\"")]))

(defn- as-coord [nm s {:keys [axis] :as opts} origin]
  (let [rel? (str/starts-with? s "~")
        n    (if rel?
               (when origin
                 (when-let [off (if (= "~" s) 0 (parse-long* (subs s 1)))]
                   (+ (long (nth origin axis)) (long off))))
               (parse-long* s))]
    (cond
      (nil? n) [:err (str (name nm) ": give a whole number or ~, not \"" s "\"")]
      (<= (long (:min opts)) (long n) (long (:max opts))) [:ok n]
      :else [:err (str (name nm) ": " n " is out of " (:min opts) ".." (:max opts))])))

(defn- as-enum [nm s {:keys [values]} _origin]
  (if (contains? values s)
    [:ok s]
    [:err (str (name nm) ": give one of " (str/join ", " (sort values)) ", not \"" s "\"")]))

(def ^:private coercers
  {:int as-int, :named-int as-named-int, :coord as-coord, :enum as-enum})

(defn- coerce [[nm [kind opts] :as arg] s origin]
  (if (nil? s)
    (if-let [d (:default opts)] [:ok d] [:err (str "give the argument " (label arg))])
    ((coercers kind as-enum) nm s opts origin)))

(defn- arg-values [[_ [kind {:keys [min max values default axis names]}]] target]
  (case kind
    :int (->> [default min (quot (+ (long min) (long max)) 2) max]
              (remove nil?) (map str) distinct vec)
    :coord (if target [(str (nth target axis))] [])
    :named-int (let [dn (some (fn [[k v]] (when (= v default) k)) names)]
                 (into (if dn [dn] []) (sort (remove #{dn} (keys names)))))
    :enum (vec (sort values))))

(defn usage [path]
  (let [form (loop [forms commands, [nm & more] path]
               (let [f (find-form forms nm)]
                 (if (seq more) (recur (drop 2 f) more) f)))
        [_ doc args] form]
    (str "**/" (str/join " " path) "**"
         (when (seq args) (str " " (str/join " " (map label args))))
         " - " doc)))

(defn- usage-lines [form path]
  (if (subcommands? form)
    (mapcat #(usage-lines % (conj path (cmd-name %))) (drop 2 form))
    [(usage path)]))

(defn- parse-args [args tokens path origin]
  (loop [as args, ts tokens, acc []]
    (if-let [a (first as)]
      (let [[st v] (coerce a (first ts) origin)]
        (if (= :err st)
          {:error (str v "\n" (usage path))}
          (recur (next as) (next ts) (conj acc v))))
      {:args acc})))

(defn- delta-of [form tokens path origin]
  (let [r (parse-args (nth form 2) tokens path origin)]
    (if (:error r)
      r
      {:delta (into (nth form 3) (:args r))})))

(defn- no-subcommand [form nm sub]
  {:error (str (if sub (str "unknown subcommand \"" sub "\"") "give a subcommand")
               "\n" (str/join "\n" (usage-lines form [nm])))})

(defn- parse-subcommand [form nm [sub & arg-tokens] origin]
  (if-let [sform (find-form (drop 2 form) sub)]
    (delta-of sform arg-tokens [nm sub] origin)
    (no-subcommand form nm sub)))

(defn parse
  ([text] (parse text nil))
  ([text origin]
   (let [[nm & more] (remove str/blank? (str/split (subs text 1) #"\s+"))
         form (find-form commands nm)]
     (cond
       (nil? form) {:error (str "unknown command" (when nm (str " \"/" nm "\"")))}
       (subcommands? form) (parse-subcommand form nm more origin)
       :else (delta-of form more [nm] origin)))))

(defn- starting-with [prefix xs]
  (let [p (str/lower-case prefix)]
    (vec (filter #(str/starts-with? (str/lower-case %) p) xs))))

(defn- suggest-player [world prefix]
  (starting-with prefix (sort (keys (:players world)))))

(defn- suggest-command [prefix]
  (mapv #(str "/" %) (starting-with prefix (sort (map cmd-name commands)))))

(defn- suggest-subcommand [form prefix]
  (starting-with prefix (sort (map cmd-name (drop 2 form)))))

(defn- suggest-arg [form tokens i target]
  (if-let [a (nth (nth form 2 []) i nil)]
    (starting-with (last tokens) (arg-values a target))
    []))

(defn- suggest-after-command [form tokens target]
  (cond
    (not (subcommands? form)) (suggest-arg form tokens (dec (count tokens)) target)
    (= 1 (count tokens)) (suggest-subcommand form (first tokens))
    :else (suggest-arg (find-form (drop 2 form) (first tokens))
                       tokens
                       (- (count tokens) 2)
                       target)))

(defn suggest
  ([world text] (suggest world text nil))
  ([world text target]
   (let [text (or text "")]
     (if-not (str/starts-with? text "/")
       (suggest-player world (last (str/split text #" " -1)))
       (let [[nm & more] (str/split (subs text 1) #" " -1)
             form (find-form commands nm)]
         (cond
           (empty? more) (suggest-command nm)
           (nil? form) []
           :else (suggest-after-command form more target)))))))
