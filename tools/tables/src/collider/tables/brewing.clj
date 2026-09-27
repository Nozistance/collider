(ns collider.tables.brewing
  "The effects, the potions and the mixes of a brewing stand.")

(set! *warn-on-reflection* true)

(def ^:private effect-colors
  {:speed                  [3402751 :beneficial]
   :slowness               [9154528 :harmful]
   :haste                  [14270531 :beneficial]
   :mining-fatigue         [4866583 :harmful]
   :strength               [16762624 :beneficial]
   :instant-health         [16262179 :beneficial]
   :instant-damage         [11101546 :harmful]
   :jump-boost             [16646020 :beneficial]
   :nausea                 [5578058 :harmful]
   :regeneration           [13458603 :beneficial]
   :resistance             [9520880 :beneficial]
   :fire-resistance        [16750848 :beneficial]
   :water-breathing        [10017472 :beneficial]
   :invisibility           [16185078 :beneficial]
   :blindness              [2039587 :harmful]
   :night-vision           [12779366 :beneficial]
   :hunger                 [5797459 :harmful]
   :weakness               [4738376 :harmful]
   :poison                 [8889187 :harmful]
   :wither                 [7561558 :harmful]
   :health-boost           [16284963 :beneficial]
   :absorption             [2445989 :beneficial]
   :saturation             [16262179 :beneficial]
   :glowing                [9740385 :neutral]
   :levitation             [13565951 :harmful]
   :luck                   [5882118 :beneficial]
   :unluck                 [12624973 :harmful]
   :slow-falling           [15978425 :beneficial]
   :conduit-power          [1950417 :beneficial]
   :dolphins-grace         [8954814 :beneficial]
   :bad-omen               [745784 :neutral]
   :hero-of-the-village    [4521796 :beneficial]
   :darkness               [2696993 :harmful]
   :trial-omen             [1484454 :neutral]
   :raid-omen              [14565464 :neutral]
   :wind-charged           [12438015 :harmful]
   :weaving                [7891290 :harmful]
   :oozing                 [10092451 :harmful]
   :infested               [9214860 :harmful]
   :breath-of-the-nautilus [65518 :beneficial]})

(def ^:private instant-effects
  #{:instant-health :instant-damage :saturation})

(def ^:private potion-effects
  {:water                []
   :mundane              []
   :thick                []
   :awkward              []
   :night-vision         [[:night-vision 3600 0]]
   :long-night-vision    [[:night-vision 9600 0]]
   :invisibility         [[:invisibility 3600 0]]
   :long-invisibility    [[:invisibility 9600 0]]
   :leaping              [[:jump-boost 3600 0]]
   :long-leaping         [[:jump-boost 9600 0]]
   :strong-leaping       [[:jump-boost 1800 1]]
   :fire-resistance      [[:fire-resistance 3600 0]]
   :long-fire-resistance [[:fire-resistance 9600 0]]
   :swiftness            [[:speed 3600 0]]
   :long-swiftness       [[:speed 9600 0]]
   :strong-swiftness     [[:speed 1800 1]]
   :slowness             [[:slowness 1800 0]]
   :long-slowness        [[:slowness 4800 0]]
   :strong-slowness      [[:slowness 400 3]]
   :turtle-master        [[:slowness 400 3] [:resistance 400 2]]
   :long-turtle-master   [[:slowness 800 3] [:resistance 800 2]]
   :strong-turtle-master [[:slowness 400 5] [:resistance 400 3]]
   :water-breathing      [[:water-breathing 3600 0]]
   :long-water-breathing [[:water-breathing 9600 0]]
   :healing              [[:instant-health 1 0]]
   :strong-healing       [[:instant-health 1 1]]
   :harming              [[:instant-damage 1 0]]
   :strong-harming       [[:instant-damage 1 1]]
   :poison               [[:poison 900 0]]
   :long-poison          [[:poison 1800 0]]
   :strong-poison        [[:poison 432 1]]
   :regeneration         [[:regeneration 900 0]]
   :long-regeneration    [[:regeneration 1800 0]]
   :strong-regeneration  [[:regeneration 450 1]]
   :strength             [[:strength 3600 0]]
   :long-strength        [[:strength 9600 0]]
   :strong-strength      [[:strength 1800 1]]
   :weakness             [[:weakness 1800 0]]
   :long-weakness        [[:weakness 4800 0]]
   :luck                 [[:luck 6000 0]]
   :slow-falling         [[:slow-falling 1800 0]]
   :long-slow-falling    [[:slow-falling 4800 0]]
   :wind-charged         [[:wind-charged 3600 0]]
   :weaving              [[:weaving 3600 0]]
   :oozing               [[:oozing 3600 0]]
   :infested             [[:infested 3600 0]]})

(defn- effect-entry [[k [color category]]]
  (sorted-map :category category :color color
              :instant? (contains? instant-effects k)))

(defn effect-table [known]
  (into (sorted-map)
        (keep (fn [[k :as e]] (when (known k) [k (effect-entry e)])))
        effect-colors))

(defn- instance [[effect duration amplifier]]
  (sorted-map :amplifier amplifier :duration duration :effect effect))

(defn potion-table [known]
  (into (sorted-map)
        (keep (fn [[k rows]]
                (when (known k) [k (mapv instance rows)])))
        potion-effects))

(def ^:private brewing-containers
  [:potion :splash-potion :lingering-potion])

(def ^:private container-recipes
  [[:potion :gunpowder :splash-potion]
   [:splash-potion :dragon-breath :lingering-potion]])

(def ^:private potion-mixes
  [[:water :glowstone-dust :thick]
   [:water :redstone :mundane]
   [:water :nether-wart :awkward]
   [:start :breeze-rod :wind-charged]
   [:start :slime-block :oozing]
   [:start :stone :infested]
   [:start :cobweb :weaving]
   [:awkward :golden-carrot :night-vision]
   [:night-vision :redstone :long-night-vision]
   [:night-vision :fermented-spider-eye :invisibility]
   [:long-night-vision :fermented-spider-eye :long-invisibility]
   [:invisibility :redstone :long-invisibility]
   [:start :magma-cream :fire-resistance]
   [:fire-resistance :redstone :long-fire-resistance]
   [:start :rabbit-foot :leaping]
   [:leaping :redstone :long-leaping]
   [:leaping :glowstone-dust :strong-leaping]
   [:leaping :fermented-spider-eye :slowness]
   [:long-leaping :fermented-spider-eye :long-slowness]
   [:slowness :redstone :long-slowness]
   [:slowness :glowstone-dust :strong-slowness]
   [:awkward :turtle-helmet :turtle-master]
   [:turtle-master :redstone :long-turtle-master]
   [:turtle-master :glowstone-dust :strong-turtle-master]
   [:swiftness :fermented-spider-eye :slowness]
   [:long-swiftness :fermented-spider-eye :long-slowness]
   [:start :sugar :swiftness]
   [:swiftness :redstone :long-swiftness]
   [:swiftness :glowstone-dust :strong-swiftness]
   [:awkward :pufferfish :water-breathing]
   [:water-breathing :redstone :long-water-breathing]
   [:start :glistering-melon-slice :healing]
   [:healing :glowstone-dust :strong-healing]
   [:healing :fermented-spider-eye :harming]
   [:strong-healing :fermented-spider-eye :strong-harming]
   [:harming :glowstone-dust :strong-harming]
   [:poison :fermented-spider-eye :harming]
   [:long-poison :fermented-spider-eye :harming]
   [:strong-poison :fermented-spider-eye :strong-harming]
   [:start :spider-eye :poison]
   [:poison :redstone :long-poison]
   [:poison :glowstone-dust :strong-poison]
   [:start :ghast-tear :regeneration]
   [:regeneration :redstone :long-regeneration]
   [:regeneration :glowstone-dust :strong-regeneration]
   [:start :blaze-powder :strength]
   [:strength :redstone :long-strength]
   [:strength :glowstone-dust :strong-strength]
   [:water :fermented-spider-eye :weakness]
   [:weakness :redstone :long-weakness]
   [:awkward :phantom-membrane :slow-falling]
   [:slow-falling :redstone :long-slow-falling]])

(defn- expand-mix [[from ingredient to]]
  (if (= :start from)
    [[:water ingredient :mundane] [:awkward ingredient to]]
    [[from ingredient to]]))

(defn- mix-entry [[from ingredient to]]
  {:from from :ingredient ingredient :to to})

(defn- mixes [rows known?]
  (into [] (comp (filter #(every? known? %)) (map mix-entry)) rows))

(defn brewing [tags items potions]
  (let [mix? (fn [[from ingredient to]]
               (and (potions from) (items ingredient) (potions to)))
        xf (comp (filter mix?) (map mix-entry))
        rows (mapcat expand-mix potion-mixes)]
    {:containers      (filterv items brewing-containers)
     :container-mixes (mixes container-recipes items)
     :potion-mixes    (into [] xf rows)
     :fuel            (vec (get-in tags ["item" "brewing_fuel"]))}))
