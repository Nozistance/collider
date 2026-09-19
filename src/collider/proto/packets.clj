(ns collider.proto.packets
  "Readers and writers of every packet, by connection state and name."
  (:require [collider.data :as data]
            [collider.game.delta :as delta]
            [collider.proto.buf :as buf]
            [collider.proto.chunk :as chunk]
            [collider.proto.codec :as c]
            [collider.proto.wire :as wire]
            [malli.core :as m]
            [malli.error :as me])
  (:import (collider.java Buf)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- write-registry-entries! [^Buf buf names]
  (c/write-varint buf (count names))
  (doseq [n names]
    (c/write-id buf n)
    (buf/write-boolean! buf false)))

(defn- write-spawn-info [^Buf buf m]
  (c/write-holder-ref buf (long (:dimension-type m)))
  (c/write-id buf :overworld)
  (buf/write-long! buf 0)
  (buf/write-byte! buf 1)
  (buf/write-byte! buf -1)
  (buf/write-boolean! buf false)
  (buf/write-boolean! buf true)
  (buf/write-boolean! buf false)
  (c/write-varint buf 0)
  (c/write-varint buf 63))

(defn- node-flags ^long [type executable?]
  (bit-or (case type :root 0 :literal 1 :argument 2)
          (if executable? 4 0)))

(defn- write-int-range! [^Buf buf props]
  (buf/write-byte! buf 3)
  (buf/write-int! buf (int (:min props)))
  (buf/write-int! buf (int (:max props))))

(defn- write-double-range! [^Buf buf props]
  (buf/write-byte! buf 3)
  (buf/write-double! buf (double (:min props)))
  (buf/write-double! buf (double (:max props))))

(defn- entity-flags ^long [props]
  (bit-or (if (:single? props) 1 0) (if (:players? props) 2 0)))

(defn- write-parser! [^Buf buf parser props]
  (let [id (data/registry-id "command_argument_type" parser)]
    (c/write-varint buf id))
  (case (name parser)
    "brigadier:integer" (write-int-range! buf props)
    "brigadier:double" (write-double-range! buf props)
    "brigadier:string" (c/write-varint buf (long (:kind props 0)))
    "time" (buf/write-int! buf (int (:min props 0)))
    "entity" (buf/write-byte! buf (int (entity-flags props)))
    "resource" (c/write-string buf (:registry props))
    nil))

(defn- write-node! [^Buf buf node]
  (let [{:keys [type name parser props executable? children]} node]
    (buf/write-byte! buf (int (node-flags type executable?)))
    (c/write-varint buf (count children))
    (doseq [c children] (c/write-varint buf (long c)))
    (when (not= :root type) (c/write-string buf name))
    (when (= :argument type) (write-parser! buf parser props))))

(defn- stat-registry [type]
  (case type
    :custom "custom_stat"
    :mined "block"
    (:killed :killed-by) "entity_type"
    "item"))

(defn- write-stat! [^Buf buf k n]
  (let [type (keyword (namespace k))
        key (keyword (name k))]
    (c/write-varint buf (data/registry-id "stat_type" type))
    (c/write-varint buf (data/registry-id (stat-registry type) key))
    (c/write-varint buf (long n))))

(defn- read-pair [^Buf buf]
  [(c/read-string buf) (c/read-string buf)])

(defn- read-vec3 [^Buf buf]
  [(buf/read-double buf) (buf/read-double buf) (buf/read-double buf)])

(defn- read-float-vec3 [^Buf buf]
  [(buf/read-float buf) (buf/read-float buf) (buf/read-float buf)])

(def ^:private Varint
  [:int {:min -2147483648 :max 2147483647}])

(def ^:private Id [:or :keyword :string])

(def ^:private Component [:or :string :map])

(def ^:private Stacks [:sequential [:maybe delta/Stack]])

(def ^:private Modifiers
  [:maybe [:sequential [:tuple :keyword number? :int]]])

(def ^:private Attributes
  [:or [:map-of :keyword number?]
   [:sequential [:tuple :keyword number? Modifiers]]])

(def ^:private Node
  [:map [:type [:enum :root :literal :argument]]
   [:name {:optional true} :string]
   [:parser {:optional true} :keyword]
   [:props {:optional true} [:maybe :map]]
   [:executable? {:optional true} [:maybe :boolean]]
   [:children [:sequential :int]]])

(def ^:private Player
  [:map [:uuid :uuid]
   [:name {:optional true} :string]
   [:gamemode {:optional true} [:maybe :int]]
   [:ping {:optional true} [:maybe :int]]])

(def ^:private Stonecutting
  [:map [:in [:sequential :keyword]]
   [:out [:map [:item :keyword] [:count {:optional true} :int]]]])

(def ^:private EntityData
  [:sequential [:tuple :int
                [:enum :byte :int :float :item :boolean :block-pos
                 :optional-block-pos :block-state :particle :pose] :any]])

(def ^:private table
  {[:handshake :intention]
   {:schema [:map [:protocol wire/varint]
             [:address [wire/string {:max 255}]]
             [:port wire/unsigned-short] [:next wire/varint]]
    :read :wire}

   [:status :status-response]
   {:schema [:map [:json :string]]
    :write (fn [^Buf buf m] (c/write-string buf (:json m)))}
   [:status :ping-request]
   {:schema [:map [:payload wire/long]]
    :read :wire}
   [:status :pong-response]
   {:schema [:map [:payload wire/long]]
    :write :wire}

   [:login :hello]
   {:schema [:map [:name [wire/string {:max 16}]] [:uuid wire/uuid]]
    :read :wire}
   [:login :login-compression]
   {:schema [:map [:threshold Varint]]
    :write (fn [^Buf buf m] (c/write-varint buf (long (:threshold m))))}
   [:login :login-finished]
   {:schema [:map [:uuid wire/uuid] [:name [wire/string {:max 16}]]
             [:properties {:optional true} [:= {:wire wire/varint} 0]]
             [:session {:optional true}
              [:= {:wire wire/uuid} (UUID. 0 0)]]]
    :write :wire}
   [:login :login-disconnect]
   {:schema [:map [:json :string]]
    :write (fn [^Buf buf m] (c/write-string buf (:json m)))}

   [:configuration :custom-payload]
   {:schema [:map [:channel Id] [:value :string]]
    :write (fn [^Buf buf m] (c/write-id buf (:channel m)) (c/write-string buf (:value m)))}
   [:configuration :update-enabled-features]
   {:schema [:map [:features [:sequential Id]]]
    :write (fn [^Buf buf m] (c/write-list buf (:features m) c/write-id))}
   [:configuration :select-known-packs]
   {:schema [:map [:packs [:sequential [:sequential :string]]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (count (:packs m)))
             (doseq [pack (:packs m)]
               (doseq [s pack] (c/write-string buf s))))}
   [:configuration :registry-data]
   {:schema [:map [:registry Id] [:names [:sequential Id]]]
    :write (fn [^Buf buf m]
             (c/write-id buf (:registry m))
             (write-registry-entries! buf (:names m)))}
   [:configuration :update-tags]
   {:schema [:map [:tags [:map-of Id [:map-of Id [:sequential :keyword]]]]]
    :write (fn [^Buf buf m]
             (let [tags (:tags m)]
               (c/write-varint buf (count tags))
               (doseq [[registry ts] tags]
                 (c/write-id buf registry)
                 (c/write-varint buf (count ts))
                 (doseq [[tag entries] ts]
                   (c/write-id buf tag)
                   (let [ids (keep #(try (data/entry-id registry %) (catch Exception _ nil)) entries)]
                     (c/write-varint buf (count ids))
                     (doseq [id ids] (c/write-varint buf id)))))))}
   [:configuration :finish-configuration]
   {:schema [:map]
    :write (fn [_ _] nil)}
   [:configuration :disconnect]
   {:schema [:map [:text Component]]
    :write (fn [^Buf buf m] (c/write-component buf (:text m)))}

   [:play :login]
   {:schema [:map [:eid :int] [:max-players Varint]
             [:view-distance Varint] [:simulation-distance Varint]
             [:dimension-type :int]]
    :write (fn [^Buf buf m]
             (buf/write-int! buf (int (:eid m)))
             (buf/write-boolean! buf false)
             (c/write-list buf [:overworld] c/write-id)
             (c/write-varint buf (long (:max-players m)))
             (c/write-varint buf (long (:view-distance m)))
             (c/write-varint buf (long (:simulation-distance m)))
             (buf/write-boolean! buf false)
             (buf/write-boolean! buf true)
             (buf/write-boolean! buf false)
             (write-spawn-info buf m)
             (buf/write-boolean! buf false)
             (buf/write-boolean! buf false))}
   [:play :respawn]
   {:schema [:map [:dimension-type :int] [:keep {:optional true} :int]]
    :write (fn [^Buf buf m]
             (write-spawn-info buf m)
             (buf/write-byte! buf (int (:keep m 0))))}
   [:play :player-abilities]
   {:schema [:map [:flags :int]
             [:flying-speed {:optional true} number?]
             [:walking-speed {:optional true} number?]]
    :read  (fn [^Buf buf] {:flags (buf/read-byte buf)})
    :write (fn [^Buf buf m]
             (buf/write-byte! buf (int (:flags m)))
             (buf/write-float! buf (float (:flying-speed m)))
             (buf/write-float! buf (float (:walking-speed m))))}
   [:play :set-default-spawn-position]
   {:schema [:map [:pos delta/Pos]
             [:yaw {:optional true} number?]
             [:pitch {:optional true} number?]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-id buf :overworld)
               (c/write-block-pos buf x y z)
               (buf/write-float! buf (float (:yaw m 0.0)))
               (buf/write-float! buf (float (:pitch m 0.0)))))}
   [:play :set-health]
   {:schema [:map [:health number?] [:food Varint] [:saturation number?]]
    :write (fn [^Buf buf m]
             (buf/write-float! buf (float (:health m)))
             (c/write-varint buf (long (:food m)))
             (buf/write-float! buf (float (:saturation m))))}
   [:play :set-experience]
   {:schema [:map [:progress number?] [:level Varint] [:total Varint]]
    :write (fn [^Buf buf m]
             (buf/write-float! buf (float (:progress m)))
             (c/write-varint buf (long (:level m)))
             (c/write-varint buf (long (:total m))))}
   [:play :game-event]
   {:schema [:map [:event :int] [:value {:optional true} number?]]
    :write (fn [^Buf buf m]
             (buf/write-byte! buf (int (:event m)))
             (buf/write-float! buf (float (:value m 0.0))))}
   [:play :commands]
   {:schema [:map [:nodes [:sequential Node]]]
    :write (fn [^Buf buf {:keys [nodes]}]
             (c/write-varint buf (count nodes))
             (doseq [node nodes] (write-node! buf node))
             (c/write-varint buf (dec (count nodes))))}
   [:play :command-suggestions]
   {:schema [:map [:id Varint] [:start Varint]
             [:length Varint] [:matches [:sequential :string]]]
    :write (fn [^Buf buf {:keys [id start length matches]}]
             (c/write-varint buf (long id))
             (c/write-varint buf (long start))
             (c/write-varint buf (long length))
             (c/write-varint buf (count matches))
             (doseq [m matches]
               (c/write-string buf m)
               (buf/write-boolean! buf false)))}
   [:play :command-suggestion]
   {:schema [:map [:id Varint] [:text [:string {:max 32500}]]]
    :read (fn [^Buf buf] {:id (c/read-varint buf) :text (c/read-string buf 32500)})}
   [:play :award-stats]
   {:schema [:map [:stats [:map-of :keyword :int]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (count (:stats m)))
             (doseq [[k n] (:stats m)] (write-stat! buf k n)))}
   [:play :game-rule-values]
   {:schema [:map [:values [:map-of :string :string]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (count (:values m)))
             (doseq [[k v] (:values m)]
               (c/write-string buf k)
               (c/write-string buf v)))}
   [:play :set-game-rule]
   {:schema [:map [:entries [:sequential [:tuple :string :string]]]]
    :read (fn [^Buf buf]
            (let [n (c/read-count buf)]
              {:entries (vec (repeatedly n #(read-pair buf)))}))}
   [:play :ping-request]
   {:schema [:map [:payload :int]]
    :read (fn [^Buf buf] {:payload (buf/read-long buf)})}
   [:play :pong-response]
   {:schema [:map [:payload :int]]
    :write (fn [^Buf buf m] (buf/write-long! buf (long (:payload m))))}
   [:play :change-difficulty]
   {:schema [:map [:difficulty {:optional true} Varint]
             [:locked {:optional true} [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:difficulty m 0)))
             (buf/write-boolean! buf (boolean (:locked m))))}
   [:play :server-data]
   {:schema [:map [:motd Component]]
    :write (fn [^Buf buf m]
             (c/write-component buf (:motd m))
             (buf/write-boolean! buf false))}
   [:play :initialize-border]
   {:schema [:map [:size number?] [:max-size Varint]
             [:center-x {:optional true} number?]
             [:center-z {:optional true} number?]
             [:warning-blocks {:optional true} Varint]
             [:warning-time {:optional true} Varint]]
    :write (fn [^Buf buf m]
             (buf/write-double! buf (double (:center-x m 0.0)))
             (buf/write-double! buf (double (:center-z m 0.0)))
             (buf/write-double! buf (double (:size m)))
             (buf/write-double! buf (double (:size m)))
             (c/write-varlong buf 0)
             (c/write-varint buf (long (:max-size m)))
             (c/write-varint buf (long (:warning-blocks m 5)))
             (c/write-varint buf (long (:warning-time m 15))))}
   [:play :ticking-state]
   {:schema [:map [:rate {:optional true} number?]
             [:frozen? {:optional true} [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (buf/write-float! buf (float (:rate m 20.0)))
             (buf/write-boolean! buf (boolean (:frozen? m))))}
   [:play :ticking-step]
   {:schema [:map [:steps {:optional true} Varint]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:steps m 0))))}
   [:play :update-attributes]
   {:schema [:map [:eid Varint] [:attributes Attributes]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-varint buf (count (:attributes m)))
             (doseq [[attr base mods] (:attributes m)]
               (c/write-varint buf (data/registry-id "attribute" attr))
               (buf/write-double! buf (double base))
               (c/write-varint buf (count mods))
               (doseq [[id amount op] mods]
                 (c/write-string buf (data/wire id))
                 (buf/write-double! buf (double amount))
                 (c/write-varint buf (long op)))))}
   [:play :set-time]
   {:schema [:map [:age :int] [:time :int]]
    :write (fn [^Buf buf m]
             (buf/write-long! buf (long (:age m)))
             (c/write-varint buf 1)
             (c/write-varint buf (data/datapack-id "world_clock" :overworld))
             (c/write-varlong buf (long (:time m)))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 1.0)))}
   [:play :player-position]
   {:schema [:map [:teleport-id Varint] [:pos delta/Vec3]
             [:vel {:optional true} [:maybe delta/Vec3]]
             [:yaw {:optional true} number?]
             [:pitch {:optional true} number?]
             [:relative {:optional true} :int]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m) [vx vy vz] (:vel m [0.0 0.0 0.0])]
               (c/write-varint buf (long (:teleport-id m)))
               (buf/write-double! buf (double x)) (buf/write-double! buf (double y)) (buf/write-double! buf (double z))
               (buf/write-double! buf (double vx)) (buf/write-double! buf (double vy)) (buf/write-double! buf (double vz))
               (buf/write-float! buf (float (:yaw m 0.0)))
               (buf/write-float! buf (float (:pitch m 0.0)))
               (buf/write-int! buf (int (:relative m 0)))))}
   [:play :accept-teleportation]
   {:schema [:map [:id Varint]]
    :read (fn [^Buf buf] {:id (c/read-varint buf)})}
   [:play :set-chunk-cache-center]
   {:schema [:map [:cx :int] [:cz :int]]
    :write (fn [^Buf buf m] (c/write-varint buf (long (:cx m))) (c/write-varint buf (long (:cz m))))}
   [:play :level-chunk-with-light]
   {:schema [:map [:cx :int] [:cz :int] [:chunk :any]
             [:block-entities {:optional true} [:maybe :any]]]
    :write (fn [^Buf buf m] (chunk/write-chunk! buf (:cx m) (:cz m) (:chunk m) (:block-entities m)))}
   [:play :open-sign-editor]
   {:schema [:map [:pos delta/Pos] [:front? [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (buf/write-boolean! buf (boolean (:front? m))))}
   [:play :block-event]
   {:schema [:map [:pos delta/Pos] [:action :int] [:param :int]
             [:block Varint]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (buf/write-byte! buf (int (:action m)))
             (buf/write-byte! buf (int (:param m)))
             (c/write-varint buf (long (:block m))))}
   [:play :block-entity-data]
   {:schema [:map [:pos delta/Pos] [:type Varint]
             [:nbt [:maybe :map]]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:type m)))
             (c/write-nbt buf (:nbt m)))}
   [:play :forget-level-chunk]
   {:schema [:map [:cx :int] [:cz :int]]
    :write (fn [^Buf buf m]
             (let [lo (bit-and (long (:cx m)) 0xFFFFFFFF)
                   hi (bit-shift-left (long (:cz m)) 32)]
               (buf/write-long! buf (bit-or lo hi))))}
   [:play :chunk-batch-start]
   {:schema [:map]
    :write (fn [_ _] nil)}
   [:play :chunk-batch-finished]
   {:schema [:map [:size Varint]]
    :write (fn [^Buf buf m] (c/write-varint buf (long (:size m))))}
   [:play :chunk-batch-received]
   {:schema [:map [:rate number?]]
    :read (fn [^Buf buf] {:rate (buf/read-float buf)})}
   [:play :keep-alive]
   {:schema [:map [:id wire/long]]
    :read  :wire
    :write :wire}
   [:play :disconnect]
   {:schema [:map [:text Component]]
    :write (fn [^Buf buf m] (c/write-component buf (:text m)))}
   [:play :system-chat]
   {:schema [:map [:text Component]
             [:overlay {:optional true} [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (c/write-component buf (:text m))
             (buf/write-boolean! buf (boolean (:overlay m))))}
   [:play :block-update]
   {:schema [:map [:pos delta/Pos] [:state :int]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:state m))))}
   [:play :section-blocks-update]
   {:schema [:map [:section delta/Pos]
             [:changes [:sequential [:tuple :int :int]]]]
    :write (fn [^Buf buf m]
             (let [[sx sy sz] (:section m)]
               (buf/write-long! buf (c/section-pos (long sx) (long sy) (long sz))))
             (c/write-varint buf (count (:changes m)))
             (doseq [[at state] (:changes m)]
               (c/write-varlong buf (bit-or (bit-shift-left (long state) 12) (long at)))))}
   [:play :cooldown]
   {:schema [:map [:group Id] [:duration Varint]]
    :write (fn [^Buf buf m]
             (c/write-id buf (:group m))
             (c/write-varint buf (long (:duration m))))}
   [:play :block-changed-ack]
   {:schema [:map [:sequence Varint]]
    :write (fn [^Buf buf m] (c/write-varint buf (long (:sequence m))))}
   [:play :player-info-update]
   {:schema [:map [:action {:optional true} :keyword]
             [:players [:sequential Player]]]
    :write (fn [^Buf buf m]
             (if (= :latency (:action m))
               (do (buf/write-byte! buf 0x10)
                   (c/write-varint buf (count (:players m)))
                   (doseq [{:keys [uuid ping]} (:players m)]
                     (c/write-uuid buf uuid)
                     (c/write-varint buf (long (or ping 0)))))
               (do (buf/write-byte! buf 0x1D)
                   (c/write-varint buf (count (:players m)))
                   (doseq [{:keys [uuid name gamemode ping]} (:players m)]
                     (c/write-uuid buf uuid)
                     (c/write-string buf name)
                     (c/write-varint buf 0)
                     (c/write-varint buf (long (or gamemode 1)))
                     (buf/write-boolean! buf true)
                     (c/write-varint buf (long (or ping 0)))))))}
   [:play :level-event]
   {:schema [:map [:event :int] [:pos delta/Pos] [:data :int]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (buf/write-int! buf (int (:event m)))
               (c/write-block-pos buf (long x) (long y) (long z))
               (buf/write-int! buf (int (:data m)))
               (buf/write-boolean! buf false)))}
   [:play :player-info-remove]
   {:schema [:map [:uuids [:sequential :uuid]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (count (:uuids m)))
             (doseq [u (:uuids m)] (c/write-uuid buf u)))}
   [:play :tab-list]
   {:schema [:map [:header Component] [:footer Component]]
    :write (fn [^Buf buf m]
             (c/write-component buf (:header m))
             (c/write-component buf (:footer m)))}
   [:play :set-held-slot]
   {:schema [:map [:slot wire/varint]]
    :write :wire}
   [:play :container-set-content]
   {:schema [:map
             [:container {:optional true} Varint]
             [:state-id {:optional true} Varint]
             [:items Stacks] [:carried [:maybe delta/Stack]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (c/write-varint buf (count (:items m)))
             (doseq [st (:items m)] (c/write-item-stack buf st))
             (c/write-item-stack buf (:carried m)))}
   [:play :container-set-slot]
   {:schema [:map
             [:container {:optional true} Varint]
             [:state-id {:optional true} Varint]
             [:slot :int] [:stack [:maybe delta/Stack]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (buf/write-short! buf (int (:slot m)))
             (c/write-item-stack buf (:stack m)))}

   [:play :open-screen]
   {:schema [:map [:container Varint] [:menu Varint] [:title Component]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m)))
             (c/write-varint buf (long (:menu m)))
             (c/write-component buf (:title m)))}
   [:play :container-set-data]
   {:schema [:map [:container Varint] [:id :int] [:value :int]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m)))
             (buf/write-short! buf (int (:id m)))
             (buf/write-short! buf (int (:value m))))}
   [:play :container-button-click]
   {:schema [:map [:container Varint] [:button Varint]]
    :read (fn [^Buf buf] {:container (c/read-varint buf) :button (c/read-varint buf)})}
   [:play :update-recipes]
   {:schema [:map
             [:property-sets [:map-of Id [:sequential :keyword]]]
             [:stonecutting [:sequential Stonecutting]]]
    :write (fn [^Buf buf m]
             (let [sets (:property-sets m)
                   item (fn [i] (c/write-holder-ref buf (data/registry-id "item" i)))]
               (c/write-varint buf (count sets))
               (doseq [[k items] sets]
                 (c/write-id buf (data/kebab k))
                 (c/write-varint buf (count items))
                 (run! item items))
               (c/write-varint buf (count (:stonecutting m)))
               (doseq [{:keys [in out]} (:stonecutting m)]
                 (c/write-varint buf (inc (count in)))
                 (run! item in)
                 (c/write-varint buf (data/registry-id "slot_display" :item-stack))
                 (item (:item out))
                 (c/write-varint buf (long (:count out 1)))
                 (c/write-patch buf nil))))}
   [:play :container-close]
   {:schema [:map [:container Varint]]
    :read  (fn [^Buf buf] {:container (c/read-varint buf)})
    :write (fn [^Buf buf m] (c/write-varint buf (long (:container m))))}
   [:play :set-cursor-item]
   {:schema [:map [:stack [:maybe delta/Stack]]]
    :write (fn [^Buf buf m] (c/write-item-stack buf (:stack m)))}

   [:play :bundle-delimiter]
   {:schema [:map]
    :write (fn [_ _] nil)}
   [:play :add-entity]
   {:schema [:map [:eid Varint] [:uuid :uuid] [:type Varint]
             [:pos delta/Vec3] [:vel {:optional true} [:maybe delta/Vec3]]
             [:pitch {:optional true} number?]
             [:yaw {:optional true} number?]
             [:head-yaw {:optional true} number?]
             [:data {:optional true} Varint]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-uuid buf (:uuid m))
             (c/write-varint buf (long (:type m)))
             (c/write-vec3 buf (:pos m))
             (c/write-lp-vec3 buf (:vel m [0.0 0.0 0.0]))
             (c/write-angle buf (:pitch m 0.0))
             (c/write-angle buf (:yaw m 0.0))
             (c/write-angle buf (:head-yaw m 0.0))
             (c/write-varint buf (long (:data m 0))))}
   [:play :remove-entities]
   {:schema [:map [:eids [:sequential :int]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (count (:eids m)))
             (doseq [e (:eids m)] (c/write-varint buf (long e))))}
   [:play :set-entity-data]
   {:schema [:map [:eid wire/varint] [:data wire/entity-data]]
    :write :wire}
   [:play :move-entity-pos]
   {:schema [:map [:eid Varint] [:dx :int] [:dy :int]
             [:dz :int] [:on-ground [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-short! buf (int (:dx m))) (buf/write-short! buf (int (:dy m))) (buf/write-short! buf (int (:dz m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :move-entity-pos-rot]
   {:schema [:map [:eid Varint] [:dx :int] [:dy :int]
             [:dz :int] [:yaw :int] [:pitch :int]
             [:on-ground [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-short! buf (int (:dx m))) (buf/write-short! buf (int (:dy m))) (buf/write-short! buf (int (:dz m)))
             (buf/write-byte! buf (int (:yaw m))) (buf/write-byte! buf (int (:pitch m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :move-entity-rot]
   {:schema [:map [:eid Varint] [:yaw :int] [:pitch :int]
             [:on-ground [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:yaw m))) (buf/write-byte! buf (int (:pitch m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :rotate-head]
   {:schema [:map [:eid Varint] [:yaw :int]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:yaw m))))}
   [:play :entity-position-sync]
   {:schema [:map [:eid Varint] [:pos delta/Vec3]
             [:vel {:optional true} [:maybe delta/Vec3]]
             [:yaw {:optional true} number?]
             [:pitch {:optional true} number?]
             [:on-ground [:maybe :boolean]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-vec3 buf (:pos m))
             (c/write-vec3 buf (:vel m [0.0 0.0 0.0]))
             (buf/write-float! buf (float (:yaw m 0.0)))
             (buf/write-float! buf (float (:pitch m 0.0)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :set-entity-motion]
   {:schema [:map [:eid Varint] [:vel delta/Vec3]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-lp-vec3 buf (:vel m)))}
   [:play :set-equipment]
   {:schema [:map [:eid Varint]
             [:slots [:sequential [:tuple :int [:maybe delta/Stack]]]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (let [slots (vec (:slots m))]
               (doseq [[i [slot stack]] (map-indexed vector slots)]
                 (buf/write-byte! buf (int (if (< (inc i) (count slots)) (bit-or (long slot) 0x80) slot)))
                 (c/write-item-stack buf stack))))}
   [:play :animate]
   {:schema [:map [:eid Varint] [:action :int]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:action m))))}
   [:play :hurt-animation]
   {:schema [:map [:eid Varint] [:yaw {:optional true} number?]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-float! buf (float (:yaw m 0.0))))}
   [:play :entity-event]
   {:schema [:map [:eid :int] [:event :int]]
    :write (fn [^Buf buf m]
             (buf/write-int! buf (int (:eid m)))
             (buf/write-byte! buf (int (:event m))))}
   [:play :take-item-entity]
   {:schema [:map [:item Varint] [:collector Varint]
             [:amount {:optional true} Varint]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:item m)))
             (c/write-varint buf (long (:collector m)))
             (c/write-varint buf (long (:amount m 1))))}
   [:play :sound]
   {:schema [:map [:sound :int] [:source Varint] [:pos delta/Vec3]
             [:volume number?] [:pitch number?]
             [:seed {:optional true} :int]]
    :write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-holder-ref buf (long (:sound m)))
               (c/write-varint buf (long (:source m)))
               (buf/write-int! buf (int (* 8.0 (double x))))
               (buf/write-int! buf (int (* 8.0 (double y))))
               (buf/write-int! buf (int (* 8.0 (double z))))
               (buf/write-float! buf (float (:volume m)))
               (buf/write-float! buf (float (:pitch m)))
               (buf/write-long! buf (long (:seed m 0)))))}
   [:play :level-particles]
   {:schema [:map [:pos delta/Vec3] [:speed number?]
             [:count :int] [:particle Varint]
             [:state {:optional true} [:maybe :int]]]
    :write (fn [^Buf buf m]
             (buf/write-boolean! buf false)
             (buf/write-boolean! buf false)
             (c/write-vec3 buf (:pos m))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float (:speed m)))
             (buf/write-int! buf (int (:count m)))
             (c/write-varint buf (long (:particle m)))
             (when-some [st (:state m)] (c/write-varint buf (long st))))}
   [:play :explode]
   {:schema [:map [:center delta/Vec3] [:radius number?]
             [:blocks {:optional true} :int]
             [:knockback {:optional true} [:maybe delta/Vec3]]
             [:particle Varint] [:sound :int]
             [:block-particles {:optional true}
              [:sequential [:tuple :int number? number? :int]]]]
    :write (fn [^Buf buf m]
             (c/write-vec3 buf (:center m))
             (buf/write-float! buf (float (:radius m)))
             (buf/write-int! buf (int (:blocks m 0)))
             (if-let [k (:knockback m)]
               (do (buf/write-boolean! buf true) (c/write-vec3 buf k))
               (buf/write-boolean! buf false))
             (c/write-varint buf (long (:particle m)))
             (c/write-holder-ref buf (long (:sound m)))
             (c/write-varint buf (count (:block-particles m)))
             (doseq [[id scaling speed weight] (:block-particles m)]
               (c/write-varint buf (long id))
               (buf/write-float! buf (float scaling))
               (buf/write-float! buf (float speed))
               (c/write-varint buf (long weight))))}
   [:play :chat]
   {:schema [:map [:message [:string {:max 256}]]]
    :read (fn [^Buf buf] {:message (c/read-string buf 256)})}
   [:play :chat-command]
   {:schema [:map [:command :string]]
    :read (fn [^Buf buf] {:command (c/read-string buf)})}
   [:play :move-player-pos]
   {:schema [:map [:pos delta/Vec3] [:flags :int]]
    :read (fn [^Buf buf]
            {:pos   (read-vec3 buf)
             :flags (buf/read-byte buf)})}
   [:play :move-player-pos-rot]
   {:schema [:map [:pos delta/Vec3] [:yaw number?]
             [:pitch number?] [:flags :int]]
    :read (fn [^Buf buf]
            {:pos   (read-vec3 buf)
             :yaw   (buf/read-float buf)
             :pitch (buf/read-float buf)
             :flags (buf/read-byte buf)})}
   [:play :move-player-rot]
   {:schema [:map [:yaw number?] [:pitch number?] [:flags :int]]
    :read (fn [^Buf buf]
            {:yaw   (buf/read-float buf)
             :pitch (buf/read-float buf)
             :flags (buf/read-byte buf)})}
   [:play :move-player-status-only]
   {:schema [:map [:flags :int]]
    :read (fn [^Buf buf] {:flags (buf/read-byte buf)})}
   [:play :player-action]
   {:schema [:map [:action Varint] [:pos delta/Pos]
             [:face :int] [:sequence Varint]]
    :read (fn [^Buf buf]
            {:action   (c/read-varint buf)
             :pos      (c/read-block-pos buf)
             :face     (buf/read-unsigned-byte buf)
             :sequence (c/read-varint buf)})}
   [:play :use-item-on]
   {:schema [:map [:hand Varint] [:pos delta/Pos] [:face Varint]
             [:cursor delta/Vec3] [:inside :boolean] [:border :boolean]
             [:sequence Varint]]
    :read (fn [^Buf buf]
            {:hand     (c/read-varint buf)
             :pos      (c/read-block-pos buf)
             :face     (c/read-varint buf)
             :cursor   (read-float-vec3 buf)
             :inside   (buf/read-boolean buf)
             :border   (buf/read-boolean buf)
             :sequence (c/read-varint buf)})}
   [:play :use-item]
   {:schema [:map [:hand Varint] [:sequence Varint]
             [:yaw number?] [:pitch number?]]
    :read (fn [^Buf buf]
            {:hand     (c/read-varint buf)
             :sequence (c/read-varint buf)
             :yaw      (buf/read-float buf)
             :pitch    (buf/read-float buf)})}
   [:play :sign-update]
   {:schema [:map [:pos delta/Pos] [:front? :boolean]
             [:lines [:sequential [:string {:max 384}]]]]
    :read (fn [^Buf buf]
            {:pos    (c/read-block-pos buf)
             :front? (buf/read-boolean buf)
             :lines  (vec (repeatedly 4 #(c/read-string buf 384)))})}
   [:play :swing]
   {:schema [:map [:hand Varint]]
    :read (fn [^Buf buf] {:hand (c/read-varint buf)})}
   [:play :player-command]
   {:schema [:map [:eid Varint] [:action Varint] [:data Varint]]
    :read (fn [^Buf buf]
            {:eid    (c/read-varint buf)
             :action (c/read-varint buf)
             :data   (c/read-varint buf)})}
   [:play :player-input]
   {:schema [:map [:flags :int]]
    :read (fn [^Buf buf] {:flags (buf/read-byte buf)})}
   [:play :pick-item-from-block]
   {:schema [:map [:pos delta/Pos] [:include-data :boolean]]
    :read (fn [^Buf buf]
            {:pos          (c/read-block-pos buf)
             :include-data (buf/read-boolean buf)})}
   [:play :pick-item-from-entity]
   {:schema [:map [:id Varint] [:include-data :boolean]]
    :read (fn [^Buf buf]
            {:id           (c/read-varint buf)
             :include-data (buf/read-boolean buf)})}
   [:play :set-carried-item]
   {:schema [:map [:slot :int]]
    :read (fn [^Buf buf] {:slot (buf/read-short buf)})}
   [:play :container-click]
   {:schema [:map [:container wire/varint] [:state-id wire/varint]
             [:slot wire/short] [:button wire/byte]
             [:mode wire/varint]
             [:changed [:map-of wire/short wire/hashed-stack]]
             [:carried wire/hashed-stack]]
    :read :wire}
   [:play :set-creative-mode-slot]
   {:schema [:map [:slot wire/short]
             [:stack [wire/item-stack {:delimited true}]]]
    :read :wire}
   [:play :client-command]
   {:schema [:map [:action Varint]]
    :read (fn [^Buf buf] {:action (c/read-varint buf)})}
   [:play :interact]
   {:schema [:map [:target Varint] [:action Varint]
             [:hand {:optional true} Varint]
             [:at {:optional true} delta/Vec3] [:sneaking :boolean]]
    :read (fn [^Buf buf]
            (let [target (c/read-varint buf)
                  action (c/read-varint buf)]
              (case action
                0 (let [hand (c/read-varint buf)]
                    {:target target :action action :hand hand :sneaking (buf/read-boolean buf)})
                2 (let [at [(buf/read-float buf) (buf/read-float buf) (buf/read-float buf)]
                        hand (c/read-varint buf)]
                    {:target target :action action :at at :hand hand :sneaking (buf/read-boolean buf)})
                {:target target :action action :sneaking (buf/read-boolean buf)})))}})

(defn- compiled
  "Fills in the halves an entry leaves to its schema."
  [{:keys [schema] :as e}]
  (cond-> e
    (= :wire (:read e)) (assoc :read (wire/reader schema))
    (= :wire (:write e)) (assoc :write (wire/writer schema))))

(def packets
  "Every packet by connection state and name."
  (update-vals table compiled))

(defn- checker
  "Returns a fn that throws on a message not fitting schema.
  With validation off it returns nil instead."
  [nm schema]
  (when (and delta/validate? schema)
    (let [valid (delay (m/validator schema))
          explain (delay (m/explainer schema))]
      (fn [m]
        (when-not (@valid m)
          (let [info {:packet nm :message m
                      :why (me/humanize (@explain m))}]
            (throw (ex-info (str "invalid packet " nm) info))))))))

(defn- inbound-entry [state [nm id]]
  (let [e (get packets [state nm])
        k (checker nm (:schema e))]
    [(long id) (assoc e :packet nm :check k)]))

(defn- state-inbound [state dirs]
  (into {} (map #(inbound-entry state %)) (:serverbound dirs)))

(def ^:private ^:table inbound
  (delay
    (into {}
          (map (fn [[state dirs]] [state (state-inbound state dirs)]))
          (data/packets))))

(defn- outbound-entry [state [nm id]]
  (let [e (get packets [state nm])]
    (when-let [w (:write e)]
      [nm {:id (long id) :write w :check (checker nm (:schema e))}])))

(defn- state-outbound [state dirs]
  (into {} (keep #(outbound-entry state %)) (:clientbound dirs)))

(def ^:private ^:table outbound
  (delay
    (into {}
          (map (fn [[state dirs]]
                 [state (state-outbound state dirs)]))
          (data/packets))))

(defn decode [state ^Buf buf]
  (let [id (c/read-varint buf)]
    (when-let [e (get (get @inbound state) id)]
      (if-let [r (:read e)]
        (let [m (r buf)]
          (when-let [check (:check e)] (check m))
          (assoc m :packet (:packet e)))
        {:packet (:packet e)}))))

(defn encode! [state ^Buf buf m]
  (let [e (or (get (get @outbound state) (:packet m))
              (throw (ex-info "no writer for packet" {:state state :packet (:packet m)})))]
    (when-let [check (:check e)] (check m))
    (c/write-varint buf (long (:id e)))
    ((:write e) buf m)))
