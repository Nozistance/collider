(ns collider.tables.items
  "The facts of items that only their classes know."
  (:require [collider.tables.reflect
             :refer [call call-static cls elements hidden-field key-of
                     registry static-field]]
            [collider.tables.value :refer [flt kw unknown]]))

(set! *warn-on-reflection* true)

(defn- instances [c]
  (let [items (registry "ITEM")
        c (cls c)]
    (for [i (elements items) :when (Class/.isInstance c i)]
      [(key-of items i) i])))

(defn wall-blocks
  "Returns the block each standing item puts on a wall."
  []
  (let [blocks (registry "BLOCK")
        wall #(hidden-field (class %) % "wallBlock")]
    (into (sorted-map)
          (map (fn [[k i]] [k (key-of blocks (wall i))]))
          (instances "world.item.StandingAndWallBlockItem"))))

(defn place-sounds
  "Returns the sound each bucket of a solid block places with."
  []
  (let [c (cls "world.item.SolidBucketItem")]
    (into (sorted-map)
          (map (fn [[k i]]
                 (let [e (hidden-field c i "placeSound")]
                   [k (kw (str (call e "location")))])))
          (instances "world.item.SolidBucketItem"))))

(defn mob-buckets
  "Returns the fluid each bucket of a mob pours and the sound it
  empties with."
  []
  (let [c (cls "world.item.MobBucketItem")
        fluids (registry "FLUID")
        sound #(kw (str (call % "location")))
        fact (fn [i]
               {:fluid (key-of fluids (hidden-field c i "content"))
                :sound (sound (hidden-field c i "emptySound"))})]
    (into (sorted-map)
          (map (fn [[k i]] [k (fact i)]))
          (instances "world.item.MobBucketItem"))))

(defn compost
  "Returns the chance each item raises a composter by."
  []
  (let [items (registry "ITEM")]
    (into (sorted-map)
          (map (fn [[i v]] [(key-of items i) (flt v)]))
          (static-field "world.level.block.ComposterBlock"
                        "COMPOSTABLES"))))

(defn- template [reg t]
  (when-not (call (call t "components") "isEmpty")
    (throw (unknown "remainder with components" {:template (str t)})))
  {:item (key-of reg (call (call t "item") "value"))
   :count (call t "count")})

(defn remainders
  "Returns what each item leaves in a crafting grid."
  []
  (let [items (registry "ITEM")]
    (into (sorted-map)
          (for [i (elements items)
                :let [t (call i "getCraftingRemainder")]
                :when t]
            [(key-of items i) (template items t)]))))

(defn non-breakers
  "Returns the items that never break a block."
  []
  (let [items (registry "ITEM")
        stone (call (static-field "world.level.block.Blocks" "STONE")
                    "defaultBlockState")]
    (into (sorted-set)
          (for [i (elements items)
                :let [s (call i "getDefaultInstance")
                      args [s stone nil nil nil]]
                :when (not (apply call i "canDestroyBlock" args))]
            (key-of items i)))))

(defn banner-colors
  "Returns the colour of each banner item."
  []
  (let [color #(call (call % "getColor") "getSerializedName")]
    (into (sorted-map)
          (map (fn [[k i]] [k (kw (color i))]))
          (instances "world.item.BannerItem"))))

(defn- translated [lang c]
  (let [text (call c "getContents")
        kind (cls "network.chat.contents.TranslatableContents")]
    (when-not (Class/.isInstance kind text)
      (throw (unknown "item name not translated" {:name (str c)})))
    (let [k (call text "getKey")]
      (when (call lang "has" k) (call lang "getOrDefault" k)))))

(defn- default-name [lang kind i]
  (some->> (call (call i "components") "get" kind)
           (translated lang)))

(defn item-names
  "Returns the name each item shows in the language of the server.
  The components of the items must be bound."
  []
  (let [items (registry "ITEM")
        lang (call-static "locale.Language" "getInstance")
        components "core.component.DataComponents"
        kind (static-field components "ITEM_NAME")
        name-of #(default-name lang kind %)]
    (into (sorted-map)
          (keep (fn [i]
                  (when-let [n (name-of i)] [(key-of items i) n])))
          (elements items))))
