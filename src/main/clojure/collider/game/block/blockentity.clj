(ns collider.game.block.blockentity
  "Block entities, the data of a block besides its state."
  (:require [collider.data :as data]
            [collider.game.block.sign :as sign]
            [collider.hash-order :as hash-order]
            [collider.game.stack :as stack]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(def ^:private block-kinds
  {:banner              :banner :wall-banner :banner
   :skull               :skull :wall-skull :skull
   :wither-skull        :skull :wither-wall-skull :skull
   :piglinwallskull     :skull
   :player-head         :skull :player-wall-head :skull
   :decorated-pot       :decorated-pot
   :jukebox             :jukebox
   :shelf               :shelf
   :chiseled-book-shelf :chiseled-bookshelf
   :bell                :bell
   :chest               :chest :copper-chest :chest
   :weathering-copper-chest :chest
   :trapped-chest       :trapped-chest
   :ender-chest         :ender-chest
   :barrel              :barrel
   :shulker-box         :shulker-box
   :lectern             :lectern
   :furnace             :furnace
   :blast-furnace       :blast-furnace
   :smoker              :smoker
   :brewing-stand       :brewing-stand
   :campfire            :campfire
   :potent-sulfur       :potent-sulfur
   :enchantment-table   :enchanting-table
   :end-portal          :end-portal
   :end-gateway         :end-gateway
   :beacon              :beacon
   :conduit             :conduit
   :spawner             :mob-spawner
   :trial-spawner       :trial-spawner
   :vault               :vault
   :brushable           :brushable-block
   :copper-golem-statue :copper-golem-statue
   :weathering-copper-golem-statue :copper-golem-statue
   :command             :command-block
   :structure           :structure-block
   :jigsaw              :jigsaw
   :test                :test-block
   :test-instance       :test-instance-block})

(def container-kinds #{:chest :trapped-chest :barrel :shulker-box})

(def furnace-kinds #{:furnace :blast-furnace :smoker})

(def menu-kinds
  (conj (into container-kinds furnace-kinds) :brewing-stand))

(defn kind [^long st]
  (or (sign/kind st) (get block-kinds (block/type-of st))))

(defn removed?
  "Returns true when the block entity of old goes as st takes its
  cell with flags. The block and the kind of block entity change,
  and flag 256 is not set."
  [old st flags]
  (let [k (kind old)]
    (boolean
      (and k (not (bit-test (long flags) 8))
           (not= (block/block-of old) (block/block-of (long st)))
           (not= k (kind (long st)))))))

(defn in-chunk [world id]
  (get (:block-entities world) id))

(defn at [world pos]
  (get-in world [:block-entities (chunk/block-chunk pos) pos]))

(defn type-id ^long [e]
  (data/registry-id "block_entity_type" (:kind e)))

(defn identifier [item]
  (data/wire item))

(defn- ordered
  "Returns the entries with a value, written in order, as a vanilla
  compound holds them in its hash map."
  [entries]
  (let [es (filterv (comp some? second) entries)
        code (fn [[k]] (.hashCode ^String (name k)))]
    (apply array-map (mapcat #(nth es %) (hash-order/of (map code es))))))

(defn- deep-ordered [x]
  (if (map? x) (ordered (map (fn [[k v]] [k (deep-ordered v)]) x)) x))

(defn- filterable-nbt [{:keys [raw filtered]}]
  (ordered [[:raw raw] [:filtered filtered]]))

(defn- book-nbt [{:keys [title author generation pages resolved]}]
  (ordered [[:title (filterable-nbt title)] [:author author]
            [:generation (when (pos? (long generation)) (int generation))]
            [:pages (mapv filterable-nbt pages)]
            [:resolved (when resolved true)]]))

(def ^:private component-nbt
  {:written-book-content book-nbt
   :writable-book-content
   #(ordered [[:pages (not-empty (mapv filterable-nbt (:pages %)))]])
   :custom-name identity})

(defn- components-nbt
  "Returns the tag of components cs. Only the contents of books and the
  name are written, all a lectern or a jukebox holds."
  [cs]
  (not-empty (ordered (keep (fn [[k v]]
                              (when-let [f (component-nbt k)]
                                [(data/wire k) (f v)]))
                            cs))))

(defn stack-nbt [stack]
  (when stack
    (ordered [[:id (identifier (:item stack))]
              [:count (int (:count stack 1))]
              [:components (components-nbt (:components stack))]])))

(defn- filterable-of [m] {:raw (:raw m) :filtered (:filtered m)})

(def ^:private component-of
  {:written-book-content
   (fn [m]
     {:title (filterable-of (:title m)) :author (:author m)
      :generation (long (:generation m 0))
      :pages (mapv filterable-of (:pages m))
      :resolved (boolean (:resolved m))})
   :writable-book-content #(hash-map :pages (mapv filterable-of (:pages %)))
   :custom-name identity})

(defn- nbt-stack [m]
  (let [cs (into {} (keep (fn [[k v]]
                            (let [k (data/kebab (name k))]
                              (when-let [f (component-of k)] [k (f v)]))))
                 (:components m))]
    (cond-> {:item (data/kebab (:id m)) :count (long (:count m 1))}
      (seq cs) (assoc :components cs))))

(defn items-nbt [items]
  (let [slot (fn [i s] (when s (assoc (stack-nbt s) :Slot (byte i))))]
    (into [] (keep-indexed slot) items)))

(defn- dye-name [color] (data/snake color))

(defn- pattern-nbt [{:keys [pattern color]}]
  {:pattern (if (map? pattern)
              {:asset_id (identifier (:asset (:direct pattern)))
               :translation_key (:translation-key (:direct pattern))}
              (identifier pattern))
   :color   (dye-name color)})

(defn- banner-nbt [e]
  (cond-> {}
    (seq (:patterns e))
    (assoc :patterns (mapv pattern-nbt (:patterns e)))))

(defn- uuid-ints ^ints [^UUID u]
  (let [hi (.getMostSignificantBits u)
        lo (.getLeastSignificantBits u)]
    (int-array [(unsigned-bit-shift-right hi 32) hi
                (unsigned-bit-shift-right lo 32) lo])))

(defn- property-nbt [{:keys [name value signature]}]
  (cond-> (array-map :name name :value value)
    signature (assoc :signature signature)))

(defn- skin-nbt [skin]
  (cond-> {}
          (:body skin) (assoc :texture (identifier (:body skin)))
          (:cape skin) (assoc :cape (identifier (:cape skin)))
          (:elytra skin) (assoc :elytra (identifier (:elytra skin)))
          (:model skin) (assoc :model (name (:model skin)))))

(defn- properties-nbt [p]
  (cond-> {}
    (:name p) (assoc :name (:name p))
    (:id p) (assoc :id (uuid-ints (:id p)))
    (seq (:properties p))
    (assoc :properties (mapv property-nbt (:properties p)))))

(defn profile-nbt [profile]
  (let [p (or (:left (:profile profile)) (:right (:profile profile)))]
    (merge (properties-nbt p) (skin-nbt (:skin profile)))))

(defn- skull-nbt [e]
  (cond-> {}
    (:profile e) (assoc :profile (profile-nbt (:profile e)))))

(defn- pot-nbt [e]
  (cond-> {}
          (seq (:sherds e))
          (assoc :sherds (mapv identifier (:sherds e)))
          (:item e) (assoc :item (stack-nbt (:item e)))))

(defn- shelf-nbt [e]
  {:Items (items-nbt (:items e))
   :align_items_to_bottom (boolean (:align-bottom? e))})

(defn- fresh-tags [k] (get (data/block-entities) k))

(defn- tags [e which] (get (fresh-tags (:kind e)) which))

(defn- age ^long [e t] (- (long t) (long (:born e))))

(defn nbt [e t]
  (case (:kind e)
    (:sign :hanging-sign) (sign/nbt e)
    :banner (banner-nbt e)
    :skull (skull-nbt e)
    :decorated-pot (pot-nbt e)
    :shelf (shelf-nbt e)
    :campfire {:Items (items-nbt (:items e))}
    :end-gateway (assoc (tags e :update) :Age (age e t))
    :structure-block (assoc (tags e :update) :author (:author e ""))
    (tags e :update)))

(defn full-nbt
  "Returns the tag of block entity e at pos with its id and place.
  Only signs and containers save their whole body."
  [e [x y z] t]
  (cond-> (assoc (or (nbt e t) {}) :components {}
                 :id (identifier (:kind e))
                 :x (int x) :y (int y) :z (int z))
    (and (:items e) (not= :campfire (:kind e)))
    (assoc :Items (items-nbt (:items e)))))

(def ^:private fixed
  #{:beacon :conduit :mob-spawner :trial-spawner :vault
    :brushable-block :copper-golem-statue :enchanting-table
    :end-portal})

(defn- custom [e t]
  (cond
    (= :end-gateway (:kind e)) (assoc (tags e :custom) :Age (age e t))
    (fixed (:kind e)) (tags e :custom)))

(def ^:private furnace-fields
  {:lit-remaining :lit_time_remaining :lit-total :lit_total_time
   :cook :cooking_time_spent :cook-total :cooking_total_time})

(def ^:private tag-fields
  {:chiseled-bookshelf {:last-slot :last_interacted_slot}
   :potent-sulfur      {:countdown :countdown}
   :brewing-stand      {:brew :BrewTime :fuel :Fuel}
   :campfire           {:cook :CookingTimes
                        :cook-total :CookingTotalTimes}
   :furnace            furnace-fields
   :blast-furnace      furnace-fields
   :smoker             furnace-fields})

(defn- tag-value [v] (if (number? v) (long v) (mapv long v)))

(def ^:private data-types
  {:lit_time_remaining short :lit_total_time short
   :cooking_time_spent short :cooking_total_time short
   :BrewTime short :Fuel byte :CookingTimes int-array
   :CookingTotalTimes int-array :last_interacted_slot int :countdown int})

(defn- fields-nbt [e]
  (for [[field k] (get tag-fields (:kind e))]
    [k ((data-types k) (get e field))]))

(defn- recipes-nbt [used]
  (ordered (map (fn [[id n]] [(data/wire id) (int n)]) used)))

(defn- data-nbt [e t]
  (let [k (:kind e)]
    (cond
      (furnace-kinds k)
      (ordered (conj (vec (fields-nbt e))
                     [:RecipesUsed (recipes-nbt (:used e))]))
      (contains? tag-fields k) (ordered (fields-nbt e))
      (= :shelf k)
      (ordered [[:align_items_to_bottom (boolean (:align-bottom? e))]])
      (= :lectern k)
      (when-let [b (:book e)]
        (ordered [[:Book (stack-nbt b)] [:Page (int (:page e 0))]]))
      (= :jukebox k)
      (when-let [r (:record e)]
        (ordered [[:RecordItem (stack-nbt r)]
                  [:ticks_since_song_started
                   (when (:song e) (- (long t) (long (:started e))))]]))
      (#{:sign :hanging-sign} k) (deep-ordered (sign/nbt e))
      :else (custom e t))))

(defn entity-data [e t]
  (when-let [d (not-empty (data-nbt e t))]
    {:type (:kind e) :data d}))

(def ^:private op-only
  #{:command-block :lectern :sign :hanging-sign :mob-spawner
    :trial-spawner})

(defn op-only?
  "Returns true when only a game master places the data of e."
  [e]
  (contains? op-only (:kind e)))

(defn on-wire? [e]
  (boolean (tags e :update-packet?)))

(def ^:private component-fields
  {:banner        {:patterns :banner-patterns}
   :skull         {:profile :profile}
   :decorated-pot {:sherds :pot-decorations}})

(defn contents [items]
  (let [last-at (fn [acc [i s]] (if s (long i) acc))
        top (reduce last-at -1 (map-indexed vector items))]
    (mapv stack/template (take (inc top) items))))

(defn- items-of [cs size]
  (vec (take size (concat (map stack/of-template cs) (repeat nil)))))

(defn- flag? [v] (if (boolean? v) v (pos? (long v))))

(defn- fields-of [e d]
  (reduce-kv (fn [e field k]
               (if-some [v (get d k)] (assoc e field (tag-value v)) e))
             e (get tag-fields (:kind e))))

(defn- with-data [e d]
  (let [k (:kind e)]
    (cond
      (furnace-kinds k)
      (cond-> (fields-of e d)
        (:RecipesUsed d)
        (assoc :used (into {} (map (fn [[id n]] [(data/kebab (name id))
                                                 (long n)]))
                           (:RecipesUsed d))))
      (contains? tag-fields k) (fields-of e d)
      (= :shelf k)
      (cond-> e (some? (:align_items_to_bottom d))
        (assoc :align-bottom? (flag? (:align_items_to_bottom d))))
      (= :lectern k)
      (cond-> e (:Book d)
        (assoc :book (nbt-stack (:Book d)) :page (long (:Page d 0))))
      (= :jukebox k)
      (cond-> e (:RecordItem d) (assoc :record (nbt-stack (:RecordItem d))))
      (#{:sign :hanging-sign} k) (sign/loaded e d)
      :else e)))

(defn- with-container [e cs]
  (cond
    (nil? cs) e
    (vector? (:items e)) (assoc e :items (items-of cs (count (:items e))))
    (= :decorated-pot (:kind e)) (assoc e :item (stack/of-template (first cs)))
    :else e))

(defn from-stack
  "Returns block entity e set from stack. Its block entity data comes
  first, when data? and of the kind of e, and its components go over."
  ([e stack] (from-stack e stack true))
  ([e stack data?]
   (let [cs (:components stack)
         d (:block-entity-data cs)
         e (if (and data? d (= (:type d) (:kind e))) (with-data e (:data d)) e)]
     (reduce-kv (fn [e field component]
                  (if-let [v (get cs component)] (assoc e field v) e))
                (with-container e (:container cs))
                (get component-fields (:kind e) {})))))

(defn placed-by
  "Returns block entity e placed by player p. A structure block takes
  the name of p as its author."
  [e p]
  (cond-> e
    (= :structure-block (:kind e)) (assoc :author (:name p))))

(defn- blank? [v] (or (nil? v) (and (coll? v) (empty? v))))

(defn to-stack [item e]
  (let [put (fn [m field component]
              (let [v (get e field)]
                (if (blank? v) m (assoc m component v))))
        cs (reduce-kv put {} (get component-fields (:kind e) {}))]
    (cond-> {:item item :count 1}
            (= :shulker-box (:kind e))
            (assoc-in [:components :container] (contents (:items e)))
            (seq cs) (update :components merge cs))))

(defn picked
  "Returns the stack of item block entity e gives when picked with its
  data at tick t. What it holds is a container, the rest is data."
  [item e t]
  (let [held (if (= :decorated-pot (:kind e)) [(:item e)] (:items e))
        cs (not-empty (contents held))
        d (entity-data e t)]
    (cond-> (update (to-stack item e) :components dissoc :container)
      d (assoc-in [:components :block-entity-data] d)
      cs (assoc-in [:components :container] cs)
      true (update :components not-empty))))

(def ^:private blank-furnace {:items [nil nil nil] :used {}})

(def ^:private blank
  {:banner             {:patterns []}
   :decorated-pot      {:sherds [] :item nil}
   :jukebox            {:record nil}
   :shelf              {:items [nil nil nil]}
   :chiseled-bookshelf {:items (vec (repeat 6 nil))}
   :lectern            {:book nil :page 0}
   :brewing-stand      {:items (vec (repeat 5 nil))}
   :campfire           {:items (vec (repeat 4 nil))}
   :furnace            blank-furnace
   :blast-furnace      blank-furnace
   :smoker             blank-furnace})

(defn- from-tags [k]
  (let [kept (:custom (fresh-tags k))]
    (update-vals (get tag-fields k {}) #(tag-value (get kept %)))))

(defn fresh [k editor]
  (case k
    (:sign :hanging-sign) (sign/fresh k editor)
    (:chest :trapped-chest :barrel :shulker-box)
    {:kind k :items (vec (repeat 27 nil))}
    (merge (blank k) (from-tags k) {:kind k})))

(defn made [k t]
  (cond-> (fresh k nil) (= :end-gateway k) (assoc :born t)))

(defn saved [e t]
  (if (= :end-gateway (:kind e))
    (-> e (dissoc :born) (assoc :age (age e t)))
    e))

(defn loaded [e t]
  (if (= :end-gateway (:kind e))
    (-> e (dissoc :age) (assoc :born (- (long t) (long (:age e)))))
    e))

(defn wire [entries t]
  (let [entry (fn [[pos e]] [pos {:type (type-id e) :nbt (nbt e t)}])]
    (into {} (map entry) entries)))
