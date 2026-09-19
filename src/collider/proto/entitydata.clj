(ns collider.proto.entitydata
  "Synched fields of the vanilla entity classes we spawn.")

(set! *warn-on-reflection* true)

(def classes
  "Vanilla class -> its parent and its `defineId` fields, in order.
  A field is a name, an `EntityDataSerializers` type and the default
  from `defineSynchedData`; the wire index is the position of the
  field in the chain of the class."
  {:entity
   {:fields [[:shared-flags :byte 0]
             [:air-supply :int 300]
             [:custom-name :optional-component nil]
             [:custom-name-visible :boolean false]
             [:silent :boolean false]
             [:no-gravity :boolean false]
             [:pose :pose 0]
             [:ticks-frozen :int 0]]}
   :living
   {:parent :entity
    :fields [[:living-flags :byte 0]
             [:health :float 1.0]
             [:effect-particles :particles []]
             [:effect-ambience :boolean false]
             [:arrow-count :int 0]
             [:stinger-count :int 0]
             [:sleeping-pos :optional-block-pos nil]]}
   :mob {:parent :living :fields [[:mob-flags :byte 0]]}
   :pathfinder-mob {:parent :mob :fields []}
   :ageable-mob
   {:parent :pathfinder-mob
    :fields [[:baby :boolean false] [:age-locked :boolean false]]}
   :animal {:parent :ageable-mob :fields []}
   :sheep {:parent :animal :fields [[:wool :byte 0]]}
   :abstract-cow {:parent :animal :fields []}
   :cow
   {:parent :abstract-cow
    :fields [[:variant :cow-variant :temperate]
             [:sound-variant :cow-sound-variant :classic]]}
   :mushroom-cow {:parent :abstract-cow :fields [[:type :int 0]]}
   :avatar
   {:parent :living
    :fields [[:main-hand :humanoid-arm :right]
             [:mode-customisation :byte 0]]}
   :player
   {:parent :avatar
    :fields [[:absorption :float 0.0]
             [:score :int 0]
             [:shoulder-parrot-left :optional-unsigned-int nil]
             [:shoulder-parrot-right :optional-unsigned-int nil]]}
   :item-entity {:parent :entity :fields [[:item :item nil]]}
   :primed-tnt
   {:parent :entity
    :fields [[:fuse :int 80] [:block-state :block-state :tnt]]}
   :falling-block
   {:parent :entity :fields [[:start-pos :block-pos [0 0 0]]]}
   :projectile {:parent :entity :fields []}
   :throwable-projectile {:parent :projectile :fields []}
   :throwable-item-projectile
   {:parent :throwable-projectile :fields [[:item :item nil]]}
   :area-effect-cloud
   {:parent :entity
    :fields [[:radius :float 3.0]
             [:waiting :boolean false]
             [:particle :particle :entity-effect]]}})

(defn- chain [cls]
  (if-let [p (:parent (classes cls))] (conj (chain p) cls) [cls]))

(defn- numbered [cls]
  (into {}
        (map-indexed (fn [i [n t d]] [n [i t d]]))
        (mapcat #(:fields (classes %)) (chain cls))))

(def fields
  "Vanilla class -> field name -> `[index type default]`."
  (into {} (map (juxt identity numbered)) (keys classes)))

(defn default
  "Returns what the class holds in the field before anything sets it."
  [cls k]
  (nth (get-in fields [cls k]) 2))

(defn entries
  "Metadata entries `[index type value]` of a class from named fields.
  The order is the one `SynchedEntityData` packs them in."
  [cls m]
  (let [fs (fields cls)]
    (->> m
         (map (fn [[k v]]
                (if-let [[i t] (fs k)]
                  [i t v]
                  (throw (ex-info "no such synched field"
                                  {:class cls :field k})))))
         (sort-by first)
         vec)))
