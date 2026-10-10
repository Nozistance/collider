(ns collider.data
  "Game data tables."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.data.items :as items]
            [collider.data.loot :as loot]
            [collider.data.recipes :as recipes]
            [collider.data.tags :as tags]
            [collider.num :as num])
  (:import (clojure.lang PersistentArrayMap)
           (java.io File PushbackReader)
           (java.util.concurrent ExecutionException)))

(set! *warn-on-reflection* true)

(def game "26.2")

(def layout 23)

(defn- stamp-of [d]
  (try (edn/read-string (slurp (io/file d "stamp.edn")))
       (catch Exception _ nil)))

(defn complete?
  "Returns true when d holds a set of tables of this game and layout."
  [d]
  (= {:game game :layout layout}
     (select-keys (stamp-of d) [:game :layout])))

(defn dir
  "Returns the first place that holds a full set of tables of this
  version, or nil when none does."
  []
  (->> [(System/getProperty "collider.data") "target/data" "data"]
       (remove nil?)
       (filter complete?)
       first))

(defn- tables-of [ns]
  (for [[_ v] (ns-interns ns) :when (:table (meta v))] @v))

(defn- all-tables []
  (for [ns (all-ns)
        :when (str/starts-with? (str (ns-name ns)) "collider.")
        t (tables-of ns)]
    t))

(defn- wait [f]
  (try @f
       (catch ExecutionException e (throw (ex-cause e)))))

(defn load!
  "Reads every table that the loaded namespaces declare.
  A table made from other tables waits for them."
  []
  (run! wait (mapv #(future @%) (all-tables))))

(defn no-tables
  "Returns the error for a missing or stale set of tables."
  []
  (let [why (str "No complete set of tables for " game
                 " in target/data or data")
        how (str "Make them with:"
                 " clojure -T:build tables")]
    (ex-info "no tables"
             {:what    "no game data"
              :why     why
              :command how})))

(def ^:private nbt-readers
  {'nbt/b byte 'nbt/s short 'nbt/i int 'nbt/l long 'nbt/f float
   'nbt/d double 'nbt/B byte-array 'nbt/I int-array
   'nbt/L long-array})

(defn read-edn
  "Returns the table in file name. Throws when no full set of tables
  is found."
  [name]
  (let [d (or (dir) (throw (no-tables)))]
    (with-open [r (io/reader (io/file d name))]
      (edn/read {:readers nbt-readers} (PushbackReader. r)))))

(defn pack
  "Returns the entries of registry path of the game pack by id, as
  the codec of the registry writes them."
  [path]
  (read-edn (str "pack/" path ".edn")))

(def ^:private item-facts
  [:compost :wall-blocks :place-sounds :remainders :banner-colors
   :non-breakers :item-names :mob-buckets])

(def ^:private table-names
  (into [:packets :registries :blocks :synced :light :fire :fuel
         :brewing :dyes :sounds :potions :effects :entities
         :version :block-entities :growers
         :attributes :spawns]
        item-facts))

(def ^:private ^:table tables
  (delay (into {}
               (map (fn [k]
                      [k (read-edn (str (name k) ".edn"))]))
               table-names)))

(defn packets
  "Returns the packet ids by connection state, direction and name."
  [] (:packets @tables))

(defn version
  "Returns the version facts of the game.
  The build time is in epoch millis."
  [] (:version @tables))

(defn registries
  "Returns the ids of the entries the client knows, by registry."
  [] (:registries @tables))

(defn blocks [] (:blocks @tables))

(defn spawns
  "Returns the facts that natural spawning reads."
  [] (:spawns @tables))

(defn growers
  [] (:growers @tables))

(defn light
  "Returns how block states pass and emit light."
  [] (:light @tables))

(defn fire
  "Returns how blocks catch fire and burn."
  [] (:fire @tables))

(defn attributes
  "Returns the default attribute values of each living entity type."
  [] (:attributes @tables))

(def ^:private ^:table loot-tables
  (delay (let [t (pack "loot_table")]
           {:drops (loot/block-drops t)
            :entity-drops (loot/entity-drops t)})))

(defn drops
  "Returns the loot table of each block by name."
  [] (:drops @loot-tables))

(defn entity-drops
  "Returns the loot tables of the mobs.
  A shearing table has the name of the mob with the suffix -shear."
  [] (:entity-drops @loot-tables))

(defn sounds [] (:sounds @tables))

(def sound-table
  "The sound event and the channel of each sound kind."
  {:player/hurt                   [:entity.player.hurt 7]
   :player/hurt-on-fire           [:entity.player.hurt-on-fire 7]
   :player/hurt-drown             [:entity.player.hurt-drown 7]
   :player/hurt-freeze            [:entity.player.hurt-freeze 7]
   :player/hurt-sweet-berry-bush
   [:entity.player.hurt-sweet-berry-bush 7]
   :player/death                  [:entity.player.death 7]
   :sheep/say                     [:entity.sheep.ambient 6]
   :sheep/step                    [:entity.sheep.step 6]
   :sheep/hurt                    [:entity.sheep.hurt 6]
   :sheep/death                   [:entity.sheep.death 6]
   :sheep/shear                   [:entity.sheep.shear 7]
   :cow/say                       [:entity.cow.ambient 6]
   :cow/step                      [:entity.cow.step 6]
   :cow/hurt                      [:entity.cow.hurt 6]
   :cow/death                     [:entity.cow.death 6]
   :cow/milk                      [:entity.cow.milk 7]
   :cow-moody/say                 [:entity.cow-moody.ambient 6]
   :cow-moody/step                [:entity.cow-moody.step 6]
   :cow-moody/hurt                [:entity.cow-moody.hurt 6]
   :cow-moody/death               [:entity.cow-moody.death 6]
   :mooshroom/milk                [:entity.mooshroom.milk 6]
   :mooshroom/suspicious
   [:entity.mooshroom.suspicious-milk 6]
   :mooshroom/shear               [:entity.mooshroom.shear 7]
   :mooshroom/eat                 [:entity.mooshroom.eat 6]
   :mooshroom/convert             [:entity.mooshroom.convert 6]
   :pig/say                       [:entity.pig.ambient 6]
   :pig/step                      [:entity.pig.step 6]
   :pig/hurt                      [:entity.pig.hurt 6]
   :pig/death                     [:entity.pig.death 6]
   :pig/eat                       [:entity.pig.eat 6]
   :pig-big/say                   [:entity.pig-big.ambient 6]
   :pig-big/hurt                  [:entity.pig-big.hurt 6]
   :pig-big/death                 [:entity.pig-big.death 6]
   :pig-big/eat                   [:entity.pig-big.eat 6]
   :pig-mini/say                  [:entity.pig-mini.ambient 6]
   :pig-mini/hurt                 [:entity.pig-mini.hurt 6]
   :pig-mini/death                [:entity.pig-mini.death 6]
   :pig-mini/eat                  [:entity.pig-mini.eat 6]
   :baby-pig/say                  [:entity.baby-pig.ambient 6]
   :baby-pig/step                 [:entity.baby-pig.step 6]
   :baby-pig/hurt                 [:entity.baby-pig.hurt 6]
   :baby-pig/death                [:entity.baby-pig.death 6]
   :baby-pig/eat                  [:entity.baby-pig.eat 6]
   :rabbit/say                    [:entity.rabbit.ambient 6]
   :rabbit/hurt                   [:entity.rabbit.hurt 6]
   :rabbit/death                  [:entity.rabbit.death 6]
   :rabbit/jump                   [:entity.rabbit.jump 6]
   :armadillo/say                 [:entity.armadillo.ambient 6]
   :armadillo/step                [:entity.armadillo.step 6]
   :armadillo/hurt                [:entity.armadillo.hurt 6]
   :armadillo/hurt-reduced
   [:entity.armadillo.hurt-reduced 6]
   :armadillo/death               [:entity.armadillo.death 6]
   :armadillo/eat                 [:entity.armadillo.eat 6]
   :armadillo/roll                [:entity.armadillo.roll 6]
   :armadillo/land                [:entity.armadillo.land 6]
   :armadillo/unroll-start
   [:entity.armadillo.unroll-start 6]
   :armadillo/unroll-finish
   [:entity.armadillo.unroll-finish 6]
   :armadillo/scute-drop          [:entity.armadillo.scute-drop 6]
   :armadillo/brush               [:entity.armadillo.brush 6]
   :goat/say                      [:entity.goat.ambient 6]
   :goat/step                     [:entity.goat.step 6]
   :goat/hurt                     [:entity.goat.hurt 6]
   :goat/death                    [:entity.goat.death 6]
   :goat/eat                      [:entity.goat.eat 6]
   :goat/milk                     [:entity.goat.milk 7]
   :goat/long-jump                [:entity.goat.long-jump 6]
   :goat/prepare-ram              [:entity.goat.prepare-ram 6]
   :goat/ram-impact               [:entity.goat.ram-impact 6]
   :goat/horn-break               [:entity.goat.horn-break 6]
   :goat-screaming/say            [:entity.goat.screaming.ambient 6]
   :goat-screaming/hurt           [:entity.goat.screaming.hurt 6]
   :goat-screaming/death          [:entity.goat.screaming.death 6]
   :goat-screaming/eat            [:entity.goat.screaming.eat 6]
   :goat-screaming/milk           [:entity.goat.screaming.milk 7]
   :goat-screaming/long-jump
   [:entity.goat.screaming.long-jump 6]
   :goat-screaming/prepare-ram
   [:entity.goat.screaming.prepare-ram 6]
   :goat-screaming/ram-impact
   [:entity.goat.screaming.ram-impact 6]
   :chicken/say                   [:entity.chicken.ambient 6]
   :chicken/step                  [:entity.chicken.step 6]
   :chicken/hurt                  [:entity.chicken.hurt 6]
   :chicken/death                 [:entity.chicken.death 6]
   :chicken/egg                   [:entity.chicken.egg 6]
   :chicken-picky/say             [:entity.chicken-picky.ambient 6]
   :chicken-picky/hurt            [:entity.chicken-picky.hurt 6]
   :chicken-picky/death           [:entity.chicken-picky.death 6]
   :baby-chicken/say              [:entity.baby-chicken.ambient 6]
   :baby-chicken/step             [:entity.baby-chicken.step 6]
   :baby-chicken/hurt             [:entity.baby-chicken.hurt 6]
   :baby-chicken/death            [:entity.baby-chicken.death 6]
   :tnt/primed                    [:entity.tnt.primed 4]
   :snowball/throw                [:entity.snowball.throw 6]
   :egg/throw                     [:entity.egg.throw 7]
   :ender-pearl/throw             [:entity.ender-pearl.throw 6]
   :wind-charge/throw             [:entity.wind-charge.throw 6]
   :wind-charge/burst             [:entity.wind-charge.wind-burst 4]
   :splash-potion/throw           [:entity.splash-potion.throw 7]
   :lingering-potion/throw        [:entity.lingering-potion.throw 6]
   :experience-bottle/throw       [:entity.experience-bottle.throw 6]
   :player/levelup                [:entity.player.levelup 7]
   :player/teleport               [:entity.player.teleport 7]
   :hoe/till                      [:item.hoe.till 4]
   :candle/extinguish             [:block.candle.extinguish 4]
   :eyeblossom/open               [:block.eyeblossom.open 4]
   :eyeblossom/close              [:block.eyeblossom.close 4]
   :eyeblossom/open-long          [:block.eyeblossom.open-long 4]
   :eyeblossom/close-long         [:block.eyeblossom.close-long 4]
   :cake/add-candle               [:block.cake.add-candle 4]
   :cave-vines/pick-berries       [:block.cave-vines.pick-berries 4]
   :big-dripleaf/tilt-down        [:block.big-dripleaf.tilt-down 4]
   :big-dripleaf/tilt-up          [:block.big-dripleaf.tilt-up 4]
   :sweet-berry-bush/pick-berries
   [:block.sweet-berry-bush.pick-berries 4]
   :bottle/fill                   [:item.bottle.fill 4]
   :bottle/empty                  [:item.bottle.empty 4]
   :copper-golem/statue
   [:entity.copper-golem-become-statue 4]
   :axe/strip                     [:item.axe.strip 4]
   :axe/scrape                    [:item.axe.scrape 4]
   :axe/wax-off                   [:item.axe.wax-off 4]
   :dye/use                       [:item.dye.use 4]
   :golden-dandelion/use          [:item.golden-dandelion.use 7]
   :golden-dandelion/unuse        [:item.golden-dandelion.unuse 7]
   :glow-ink/use                  [:item.glow-ink-sac.use 4]
   :ink-sac/use                   [:item.ink-sac.use 4]
   :sign/waxed                    [:block.sign.waxed-interact-fail 4]
   :decorated-pot/insert          [:block.decorated-pot.insert 4]
   :decorated-pot/insert-fail     [:block.decorated-pot.insert-fail 4]
   :shelf/place-item              [:block.shelf.place-item 4]
   :shelf/single-swap             [:block.shelf.single-swap 4]
   :shelf/take-item               [:block.shelf.take-item 4]
   :bookshelf/insert              [:block.chiseled-bookshelf.insert 4]
   :bookshelf/insert-enchanted
   [:block.chiseled-bookshelf.insert.enchanted 4]
   :bookshelf/pickup              [:block.chiseled-bookshelf.pickup 4]
   :bookshelf/pickup-enchanted
   [:block.chiseled-bookshelf.pickup.enchanted 4]
   :bell/use                      [:block.bell.use 4]
   :bucket/empty                  [:item.bucket.empty 4]
   :bucket/fill                   [:item.bucket.fill 4]
   :bucket/empty-lava             [:item.bucket.empty-lava 4]
   :bucket/fill-lava              [:item.bucket.fill-lava 4]
   :bucket/empty-snow             [:item.bucket.empty-powder-snow 4]
   :bucket/fill-snow              [:item.bucket.fill-powder-snow 4]
   :pumpkin/carve                 [:block.pumpkin.carve 4]
   :composter/ready               [:block.composter.ready 4]
   :composter/empty               [:block.composter.empty 4]
   :shovel/flatten                [:item.shovel.flatten 4]
   :fire/ignite                   [:item.flintandsteel.use 4]
   :firecharge/use                [:item.firecharge.use 4]
   :generic/extinguish-fire       [:entity.generic.extinguish-fire 4]
   :wet-sponge/dries              [:block.wet-sponge.dries 4]
   :generic/burn                  [:entity.generic.burn 8]
   :explosion                     [:entity.generic.explode 4]
   :splash                        [:entity.generic.splash 6]
   :swim                          [:entity.generic.swim 6]})

(defn entities
  "Returns the width, height and eye height of each entity type.
  The sizes of its baby are under :baby."
  [] (:entities @tables))

(defn block-entities
  "Returns the tags a fresh block entity of each type saves and sends,
  and whether it sends a block entity data packet."
  [] (:block-entities @tables))

(defn potions
  "Returns the effect instances every potion gives."
  [] (:potions @tables))

(defn mob-effects
  "Returns the colour, category and immediacy of every effect."
  [] (:effects @tables))

(defn snake
  "Returns the name of k with dashes as underscores."
  ^String [k]
  (str/replace (name k) \- \_))

(defn wire
  "Returns k as a resource location with the default namespace."
  ^String [k]
  (str (or (namespace k) "minecraft") ":" (snake k)))

(defn kebab
  "Returns resource location s as a keyword.
  The default namespace goes away and underscores become dashes."
  [^String s]
  (let [s (str/lower-case s)
        i (str/index-of s ":")
        ns (if i (subs s 0 i) "minecraft")
        nm (str/replace (if i (subs s (inc i)) s) \_ \-)]
    (if (= ns "minecraft") (keyword nm) (keyword ns nm))))

(defn full-id
  "Returns resource location s, in the default namespace when it has
  no namespace."
  ^String [^String s]
  (if (str/includes? s ":") s (str "minecraft:" s)))

(def validate? (Boolean/getBoolean "collider.validate"))

(defn packet-id ^long [state dir name]
  (or (get-in (packets) [state dir name])
      (throw (ex-info "unknown packet"
                      {:state state :dir dir :name name}))))

(defn registry-id ^long [registry entry]
  (or (get-in (registries) [registry entry])
      (throw (ex-info "unknown registry entry"
                      {:registry registry :entry entry}))))

(defn- file-order [id]
  (let [i (str/index-of id ":")]
    [(str (subs id (inc i)) ".json") (subs id 0 i)]))

(defn- entry-names [registry]
  (into [] (map kebab) (sort-by file-order (keys (pack registry)))))

(def ^:private ^:table datapack-map
  (delay (PersistentArrayMap.
          (object-array
           (into [] (mapcat (fn [r] [r (entry-names r)]))
                 (:synced @tables))))))

(defn datapack
  "Returns the entries the server sends to the client, by registry,
  in the order the server sends them."
  [] @datapack-map)

(defn- index-entries [entries]
  (into {} (map-indexed (fn [i e] [e (long i)])) entries))

(def ^:private ^:table datapack-index
  (delay
    (into {}
          (map (fn [[registry entries]]
                 [registry (index-entries entries)]))
          (datapack))))

(defn datapack-id
  "Returns the place of entry in the datapack order of registry.
  Throws for an unknown entry."
  ^long [registry entry]
  (or (get (get @datapack-index registry) entry)
      (throw (ex-info "unknown datapack entry"
                      {:registry registry :entry entry}))))

(defn entry-id
  "Returns the network id of entry in registry, built in or from the
  datapack. Throws for an unknown entry."
  ^long [registry entry]
  (if (contains? (registries) registry)
    (registry-id registry entry)
    (datapack-id registry entry)))

(defn known-entry-id
  "Returns the network id of entry in registry.
  Returns nil for an unknown entry."
  [registry entry]
  (if (contains? (registries) registry)
    (get-in (registries) [registry entry])
    (get (get @datapack-index registry) entry)))

(defn- invert-ids [entries]
  (into {} (map (fn [[k v]] [(long v) k])) entries))

(def ^:private ^:table by-id
  (delay
    (into {}
          (map (fn [[registry entries]]
                 [registry (invert-ids entries)]))
          (registries))))

(defn- unknown-id [registry ^long id]
  (ex-info "unknown registry id" {:registry registry :id id}))

(defn- unknown-registry [registry]
  (ex-info "unknown registry" {:registry registry}))

(defn entry-name
  "Returns the entry of registry with network id id. Throws for an
  unknown registry or id."
  [registry ^long id]
  (let [m (get @by-id registry)
        v (get (datapack) registry)]
    (cond
      m (or (get m id) (throw (unknown-id registry id)))
      (nil? v) (throw (unknown-registry registry))
      (< -1 id (count v)) (nth v id)
      :else (throw (unknown-id registry id)))))

(defn- tag-paths
  []
  (let [root (io/file (or (dir) (throw (no-tables))) "pack" "tags")
        skip (inc (count (str root)))]
    (into (sorted-set)
          (comp (map #(str/replace (str %) File/separatorChar \/))
                (filter #(str/ends-with? % ".edn"))
                (map #(subs % skip (- (count %) 4))))
          (file-seq root))))

(defn- element-exists [path]
  (if-let [ids (get (registries) path)]
    #(contains? ids (kebab %))
    (let [ids (set (keys (pack path)))] #(contains? ids %))))

(defn tag-name
  "Returns tag id without its mark and without the default namespace."
  ^String [^String id]
  (let [id (if (str/starts-with? id "#") (subs id 1) id)]
    (if (str/starts-with? id "minecraft:") (subs id 10) id)))

(defn- built-tags [path]
  (let [files (pack (str "tags/" path))
        built (tags/build files (element-exists path))]
    (into {}
          (map (fn [[id vs]] [(tag-name id) (mapv kebab vs)]))
          (sort-by (comp tag-name key) built))))

(def ^:private ^:table all-tags
  (delay (into {} (map (fn [p] [p (built-tags p)])) (tag-paths))))

(defn- sent-tags? [[path ts]]
  (and (seq ts)
       (or (contains? (registries) path)
           (contains? (datapack) path))))

(def ^:private ^:table sent-tags
  (delay (into {} (filter sent-tags?) (sort-by key @all-tags))))

(defn tags
  "Returns the tags the server sends to the client, by registry."
  [] @sent-tags)

(defn registry-tags
  "Returns the tags of registry path by name, or nil when it has
  no tags."
  [path]
  (get @all-tags path))

(defn tag-values [registry tag]
  (get (registry-tags registry) tag []))

(defn holder-set
  "Returns the entries of registry that holder set v names. A string
  with a mark names a tag, one without names a single entry."
  [registry v]
  (cond (vector? v) (mapv kebab v)
        (str/starts-with? v "#") (tag-values registry (tag-name v))
        :else [(kebab v)]))

(def ^:private ^:table brewing-table
  (delay (assoc (:brewing @tables)
                :fuel (tag-values "item" "brewing_fuel"))))

(def ^:private ^:table recipe-table
  (delay
    (let [items (registry-tags "item")
          own (select-keys @tables [:fuel :brewing :dyes])]
      (merge (recipes/recipes (pack "recipe") items)
             (assoc own :brewing @brewing-table)))))

(defn recipes
  "Returns the recipes by kind, with the fuel, the brewing mixes and
  the dye colours."
  [] @recipe-table)

(defn cooking-recipes
  "Returns the cooking recipes in search order."
  [] (:cooking (recipes)))

(defn fuel
  "Returns how many ticks each item burns for in a furnace."
  [] (:fuel @tables))

(defn brewing
  "Returns the containers, mixes and fuel of a brewing stand."
  [] @brewing-table)

(defn smithing-recipes
  "Returns the transform and trim recipes of a smithing table."
  [] (:smithing (recipes)))

(def ^:private ^:table item-table
  (delay (items/items (pack "components/item") registry-tags
                      (select-keys @tables item-facts))))

(defn items [] @item-table)

(defn cooldown-group
  "Returns the group whose cooldown locks item, item itself when it
  names no group."
  [item]
  (get-in (items) [item :use-cooldown :group] item))

(defn use-cooldown
  "Returns the cooldown group and the ticks item locks it for.
  Returns nil when the item has no cooldown."
  [item]
  (when-let [c (get-in (items) [item :use-cooldown])]
    [(cooldown-group item) (long (* 20.0 (double (:seconds c))))]))

(defn max-stack ^long [item]
  (long (get-in (items) [item :max-stack] 64)))

(defn jukebox-song [item]
  (get-in (items) [item :jukebox-song]))

(defn equip-slot [item]
  (get-in (items) [item :equip]))

(defn swap-slot
  "Returns the slot a player puts item on by using it, or nil."
  [item]
  (get-in (items) [item :swap]))

(defn mob-bucket
  "Returns the fluid and the sound of a bucket of a mob, or nil."
  [item]
  (get-in (items) [item :mob-bucket]))

(defn equip-sound [item]
  (get-in (items) [item :equip-sound] :item.armor.equip-generic))

(defn dye-color [item]
  (get-in (items) [item :dye]))

(defn pattern-tag [item]
  (get-in (items) [item :patterns]))

(defn compost
  "Returns the chance item raises a composter, else nil."
  [item]
  (get-in (items) [item :compost]))

(defn item-name
  "Returns the name an item shows when it has no custom one."
  [item]
  (get-in (items) [item :name]))

(defn item-title
  "Returns the text component an item is called by."
  [item]
  (get-in (items) [item :title]))

(defn rarity
  "Returns the rarity of item, :common when it has none."
  [item]
  (get-in (items) [item :rarity] :common))

(defn repairable
  "Returns the items that mend item on an anvil, else nil."
  [item]
  (get-in (items) [item :repairable]))

(defn trim-material
  "Returns the trim material item gives, else nil."
  [item]
  (get-in (items) [item :trim-material]))

(defn resists
  "Returns the tag of the damage item shrugs off, else nil."
  [item]
  (get-in (items) [item :resists]))

(defn state-count
  "Returns how many states the block of facts b has."
  ^long [b]
  (reduce * 1 (map count (vals (:props b)))))

(def ^:private ^:table state-total
  (delay
    (long (reduce (fn [n [_ b]]
                    (max n (+ (long (:first b)) (state-count b))))
                  0 (blocks)))))

(defn block-state-count ^long []
  @state-total)

(defn info
  "Returns the facts known about block. Throws for an unknown one."
  [block]
  (or (get (blocks) block)
      (throw (ex-info "unknown block" {:block block}))))

(defn placed-sound [block item]
  (let [t (get (sounds) (:sound (info block)))]
    {:kind   (or (get-in (items) [item :place-sound]) (:place t))
     :volume (num/f32 (/ (+ 1.0 (num/f32 (:volume t 1.0))) 2.0))
     :pitch  (num/f32 (* (num/f32 0.8) (num/f32 (:pitch t 1.0))))}))

(defn open-sound [block open?]
  (get (info block) (if open? :open :close)))

(defn by-hand?
  "Returns true when block drops when broken without a tool."
  [block]
  (get (info block) :hand? true))
