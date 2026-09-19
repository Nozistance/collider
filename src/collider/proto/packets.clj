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

(def ^:private Id [:or :keyword :string])

(def ^:private Line [wire/string {:max 384}])

(def ^:private Signature [wire/bytes 256])

(def ^:private LastSeen
  [:map [:offset wire/varint] [:acknowledged [wire/bitset 20]]
   [:checksum wire/byte]])

(def ^:private Modifier
  [:tuple wire/id wire/double wire/varint])

(def ^:private Attribute
  [:tuple [wire/reg "attribute"] wire/double [:sequential Modifier]])

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

(def ^:private spawn-info
  "The fields every join and respawn repeats about the world entered."
  [[:dimension-type wire/holder-ref]
   [:dimension {:optional true} [:= {:wire wire/id} :overworld]]
   [:seed {:optional true} [:= {:wire wire/long} 0]]
   [:gamemode {:optional true} [:= {:wire wire/byte} 1]]
   [:last-gamemode {:optional true} [:= {:wire wire/byte} -1]]
   [:debug? {:optional true} [:= {:wire wire/boolean} false]]
   [:flat? {:optional true} [:= {:wire wire/boolean} true]]
   [:death {:optional true} [:= {:wire wire/boolean} false]]
   [:portal-cooldown {:optional true} [:= {:wire wire/varint} 0]]
   [:sea-level {:optional true} [:= {:wire wire/varint} 63]]])

(def ^:private Login
  (into [:map [:eid wire/int]
         [:hardcore {:optional true} [:= {:wire wire/boolean} false]]
         [:levels {:optional true}
          [:= {:wire [:sequential wire/id]} [:overworld]]]
         [:max-players wire/varint] [:view-distance wire/varint]
         [:simulation-distance wire/varint]
         [:reduced-debug {:optional true}
          [:= {:wire wire/boolean} false]]
         [:death-screen {:optional true}
          [:= {:wire wire/boolean} true]]
         [:limited-crafting {:optional true}
          [:= {:wire wire/boolean} false]]]
        (concat spawn-info
                [[:secure-chat {:optional true}
                  [:= {:wire wire/boolean} false]]
                 [:enforced {:optional true}
                  [:= {:wire wire/boolean} false]]])))

(def ^:private Abilities
  [:map [:flags wire/byte] [:flying-speed wire/float]
   [:walking-speed wire/float]])

(defn- tag-entry-ids [registry entries]
  (keep #(try (data/entry-id registry %)
              (catch Exception _ nil))
        entries))

(defn- section-change-long ^long [state at]
  (bit-or (bit-shift-left (long state) 12) (long at)))

(defn- write-item-ref! [^Buf buf i]
  (c/write-holder-ref buf (data/registry-id "item" i)))

(defn- write-latency-entry! [^Buf buf {:keys [uuid ping]}]
  (c/write-uuid buf uuid)
  (c/write-varint buf (long (or ping 0))))

(defn- write-player-entry! [^Buf buf p]
  (let [{:keys [uuid name gamemode ping]} p]
    (c/write-uuid buf uuid)
    (c/write-string buf name)
    (c/write-varint buf 0)
    (c/write-varint buf (long (or gamemode 1)))
    (buf/write-boolean! buf true)
    (c/write-varint buf (long (or ping 0)))))

(defn- write-player-info! [^Buf buf m]
  (let [latency? (= :latency (:action m))
        players (:players m)
        one (if latency? write-latency-entry! write-player-entry!)]
    (buf/write-byte! buf (if latency? 0x10 0x1D))
    (c/write-varint buf (count players))
    (doseq [p players] (one buf p))))

(def ^:private table
  {[:handshake :intention]
   {:schema [:map [:protocol wire/varint]
             [:address [wire/string {:max 255}]]
             [:port wire/unsigned-short] [:next wire/varint]]
    :read :wire}

   [:status :status-response]
   {:schema [:map [:json wire/string]]
    :write :wire}
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
   {:schema [:map [:threshold wire/varint]]
    :write :wire}
   [:login :login-finished]
   {:schema [:map [:uuid wire/uuid] [:name [wire/string {:max 16}]]
             [:properties {:optional true} [:= {:wire wire/varint} 0]]
             [:session {:optional true}
              [:= {:wire wire/uuid} (UUID. 0 0)]]]
    :write :wire}
   [:login :login-disconnect]
   {:schema [:map [:json [wire/string {:max 262144}]]]
    :write :wire}

   [:configuration :custom-payload]
   {:schema [:map [:channel wire/id] [:value wire/string]]
    :write :wire}
   [:configuration :update-enabled-features]
   {:schema [:map [:features [:sequential wire/id]]]
    :write :wire}
   [:configuration :select-known-packs]
   {:schema [:map
             [:packs [:sequential
                      [:tuple wire/string wire/string wire/string]]]]
    :write :wire}
   [:configuration :registry-data]
   {:schema [:map [:registry wire/id]
             [:names [:sequential [wire/bare wire/id]]]]
    :write :wire}
   [:configuration :update-tags]
   {:schema [:map
             [:tags [:map-of Id
                     [:map-of Id [:sequential :keyword]]]]]
    :write (fn [^Buf buf m]
             (let [tags (:tags m)]
               (c/write-varint buf (count tags))
               (doseq [[registry ts] tags]
                 (c/write-id buf registry)
                 (c/write-varint buf (count ts))
                 (doseq [[tag entries] ts]
                   (c/write-id buf tag)
                   (let [ids (tag-entry-ids registry entries)]
                     (c/write-varint buf (count ids))
                     (doseq [id ids] (c/write-varint buf id)))))))}
   [:configuration :finish-configuration]
   {:schema [:map]
    :write :wire}
   [:configuration :disconnect]
   {:schema [:map [:text wire/text]]
    :write :wire}

   [:play :login]
   {:schema Login
    :write :wire}
   [:play :respawn]
   {:schema (into [:map] (conj spawn-info [:keep wire/byte]))
    :write :wire}
   [:play :player-abilities]
   {:schema [:map [:flags :int]
             [:flying-speed {:optional true} number?]
             [:walking-speed {:optional true} number?]]
    :read  (wire/reader [:map [:flags wire/byte]])
    :write (wire/writer Abilities)}
   [:play :set-default-spawn-position]
   {:schema [:map
             [:dimension {:optional true}
              [:= {:wire wire/id} :overworld]]
             [:pos wire/block-pos] [:yaw wire/float]
             [:pitch wire/float]]
    :write :wire}
   [:play :set-health]
   {:schema [:map [:health wire/float] [:food wire/varint]
             [:saturation wire/float]]
    :write :wire}
   [:play :set-experience]
   {:schema [:map [:progress wire/float] [:level wire/varint]
             [:total wire/varint]]
    :write :wire}
   [:play :game-event]
   {:schema [:map [:event wire/unsigned-byte] [:value wire/float]]
    :write :wire}
   [:play :commands]
   {:schema [:map [:nodes [:sequential Node]]]
    :write (fn [^Buf buf {:keys [nodes]}]
             (c/write-varint buf (count nodes))
             (doseq [node nodes] (write-node! buf node))
             (c/write-varint buf (dec (count nodes))))}
   [:play :command-suggestions]
   {:schema [:map [:id wire/varint] [:start wire/varint]
             [:length wire/varint]
             [:matches [:sequential [wire/bare wire/string]]]]
    :write :wire}
   [:play :command-suggestion]
   {:schema [:map [:id wire/varint]
             [:text [wire/string {:max 32500}]]]
    :read :wire}
   [:play :award-stats]
   {:schema [:map [:stats [:map-of wire/stat wire/varint]]]
    :write :wire}
   [:play :game-rule-values]
   {:schema [:map [:values [:map-of wire/string wire/string]]]
    :write :wire}
   [:play :set-game-rule]
   {:schema [:map
             [:entries [:sequential
                        [:tuple wire/string wire/string]]]]
    :read :wire}
   [:play :ping-request]
   {:schema [:map [:payload wire/long]]
    :read :wire}
   [:play :pong-response]
   {:schema [:map [:payload wire/long]]
    :write :wire}
   [:play :change-difficulty]
   {:schema [:map [:difficulty wire/varint] [:locked wire/boolean]]
    :write :wire}
   [:play :server-data]
   {:schema [:map [:motd wire/text]
             [:icon {:optional true} [:= {:wire wire/boolean} false]]]
    :write :wire}
   [:play :initialize-border]
   {:schema [:map [:center-x wire/double] [:center-z wire/double]
             [:old-size wire/double] [:size wire/double]
             [:lerp {:optional true} [:= {:wire wire/varlong} 0]]
             [:max-size wire/varint] [:warning-blocks wire/varint]
             [:warning-time wire/varint]]
    :write :wire}
   [:play :ticking-state]
   {:schema [:map [:rate wire/float] [:frozen? wire/boolean]]
    :write :wire}
   [:play :ticking-step]
   {:schema [:map [:steps wire/varint]]
    :write :wire}
   [:play :update-attributes]
   {:schema [:map [:eid wire/varint]
             [:attributes [:sequential Attribute]]]
    :write :wire}
   [:play :set-time]
   {:schema [:map [:age wire/long]
             [:clocks {:optional true} [:= {:wire wire/varint} 1]]
             [:clock {:optional true}
              [:= {:wire [wire/reg "world_clock"]} :overworld]]
             [:time wire/varlong]
             [:rate {:optional true} [:= {:wire wire/float} 0.0]]
             [:scale {:optional true} [:= {:wire wire/float} 1.0]]]
    :write (fn [^Buf buf m]
             (buf/write-long! buf (long (:age m)))
             (c/write-varint buf 1)
             (c/write-varint
               buf (data/datapack-id "world_clock" :overworld))
             (c/write-varlong buf (long (:time m)))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 1.0)))}
   [:play :player-position]
   {:schema [:map [:teleport-id wire/varint] [:pos wire/vec3]
             [:vel wire/vec3] [:yaw wire/float] [:pitch wire/float]
             [:relative wire/int]]
    :write :wire}
   [:play :accept-teleportation]
   {:schema [:map [:id wire/varint]]
    :read :wire}
   [:play :set-chunk-cache-center]
   {:schema [:map [:cx wire/varint] [:cz wire/varint]]
    :write :wire}
   [:play :level-chunk-with-light]
   {:schema [:map [:cx :int] [:cz :int] [:chunk :any]
             [:block-entities {:optional true} [:maybe :any]]]
    :write (fn [^Buf buf m]
             (chunk/write-chunk!
               buf (:cx m) (:cz m) (:chunk m) (:block-entities m)))}
   [:play :open-sign-editor]
   {:schema [:map [:pos wire/block-pos] [:front? wire/boolean]]
    :write :wire}
   [:play :block-event]
   {:schema [:map [:pos wire/block-pos] [:action wire/unsigned-byte]
             [:param wire/unsigned-byte] [:block wire/varint]]
    :write :wire}
   [:play :block-entity-data]
   {:schema [:map [:pos wire/block-pos] [:type wire/varint]
             [:nbt wire/nbt]]
    :write :wire}
   [:play :forget-level-chunk]
   {:schema [:map [:cz wire/int] [:cx wire/int]]
    :write :wire}
   [:play :chunk-batch-start]
   {:schema [:map]
    :write :wire}
   [:play :chunk-batch-finished]
   {:schema [:map [:size wire/varint]]
    :write :wire}
   [:play :chunk-batch-received]
   {:schema [:map [:rate wire/float]]
    :read :wire}
   [:play :keep-alive]
   {:schema [:map [:id wire/long]]
    :read  :wire
    :write :wire}
   [:play :disconnect]
   {:schema [:map [:text wire/text]]
    :write :wire}
   [:play :system-chat]
   {:schema [:map [:text wire/text] [:overlay wire/boolean]]
    :write :wire}
   [:play :block-update]
   {:schema [:map [:pos wire/block-pos] [:state wire/varint]]
    :write :wire}
   [:play :section-blocks-update]
   {:schema [:map [:section wire/section-pos]
             [:changes [:sequential wire/section-change]]]
    :write (fn [^Buf buf m]
             (let [[sx sy sz] (:section m)]
               (buf/write-long!
                 buf (c/section-pos (long sx) (long sy) (long sz))))
             (c/write-varint buf (count (:changes m)))
             (doseq [[at state] (:changes m)]
               (c/write-varlong buf (section-change-long state at))))}
   [:play :cooldown]
   {:schema [:map [:group wire/id] [:duration wire/varint]]
    :write :wire}
   [:play :block-changed-ack]
   {:schema [:map [:sequence wire/varint]]
    :write :wire}
   [:play :player-info-update]
   {:schema [:map [:action {:optional true} :keyword]
             [:players [:sequential Player]]]
    :write write-player-info!}
   [:play :level-event]
   {:schema [:map [:event wire/int] [:pos wire/block-pos]
             [:data wire/int]
             [:global {:optional true}
              [:= {:wire wire/boolean} false]]]
    :write :wire}
   [:play :player-info-remove]
   {:schema [:map [:uuids [:sequential wire/uuid]]]
    :write :wire}
   [:play :tab-list]
   {:schema [:map [:header wire/text] [:footer wire/text]]
    :write :wire}
   [:play :set-held-slot]
   {:schema [:map [:slot wire/varint]]
    :write :wire}
   [:play :container-set-content]
   {:schema [:map [:container wire/varint] [:state-id wire/varint]
             [:items [:sequential wire/item-stack]]
             [:carried wire/item-stack]]
    :write :wire}
   [:play :container-set-slot]
   {:schema [:map [:container wire/varint] [:state-id wire/varint]
             [:slot wire/short] [:stack wire/item-stack]]
    :write :wire}

   [:play :open-screen]
   {:schema [:map [:container wire/varint] [:menu wire/varint]
             [:title wire/text]]
    :write :wire}
   [:play :container-set-data]
   {:schema [:map [:container wire/varint] [:id wire/short]
             [:value wire/short]]
    :write :wire}
   [:play :container-button-click]
   {:schema [:map [:container wire/varint] [:button wire/varint]]
    :read :wire}
   [:play :update-recipes]
   {:schema [:map
             [:property-sets [:map-of Id [:sequential :keyword]]]
             [:stonecutting [:sequential Stonecutting]]]
    :write (fn [^Buf buf m]
             (let [sets (:property-sets m)
                   item (fn [i] (write-item-ref! buf i))]
               (c/write-varint buf (count sets))
               (doseq [[k items] sets]
                 (c/write-id buf (data/kebab k))
                 (c/write-varint buf (count items))
                 (run! item items))
               (c/write-varint buf (count (:stonecutting m)))
               (doseq [{:keys [in out]} (:stonecutting m)]
                 (c/write-varint buf (inc (count in)))
                 (run! item in)
                 (c/write-varint
                   buf (data/registry-id "slot_display" :item-stack))
                 (item (:item out))
                 (c/write-varint buf (long (:count out 1)))
                 (c/write-patch buf nil))))}
   [:play :container-close]
   {:schema [:map [:container wire/varint]]
    :read  :wire
    :write :wire}
   [:play :set-cursor-item]
   {:schema [:map [:stack wire/item-stack]]
    :write :wire}

   [:play :bundle-delimiter]
   {:schema [:map]
    :write :wire}
   [:play :add-entity]
   {:schema [:map [:eid wire/varint] [:uuid wire/uuid]
             [:type wire/varint] [:pos wire/vec3] [:vel wire/lp-vec3]
             [:pitch wire/angle] [:yaw wire/angle]
             [:head-yaw wire/angle] [:data wire/varint]]
    :write :wire}
   [:play :remove-entities]
   {:schema [:map [:eids [:sequential wire/varint]]]
    :write :wire}
   [:play :set-entity-data]
   {:schema [:map [:eid wire/varint] [:data wire/entity-data]]
    :write :wire}
   [:play :move-entity-pos]
   {:schema [:map [:eid wire/varint] [:dx wire/short] [:dy wire/short]
             [:dz wire/short] [:on-ground wire/boolean]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-short! buf (int (:dx m)))
             (buf/write-short! buf (int (:dy m)))
             (buf/write-short! buf (int (:dz m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :move-entity-pos-rot]
   {:schema [:map [:eid wire/varint] [:dx wire/short] [:dy wire/short]
             [:dz wire/short] [:yaw wire/byte] [:pitch wire/byte]
             [:on-ground wire/boolean]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-short! buf (int (:dx m)))
             (buf/write-short! buf (int (:dy m)))
             (buf/write-short! buf (int (:dz m)))
             (buf/write-byte! buf (int (:yaw m)))
             (buf/write-byte! buf (int (:pitch m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :move-entity-rot]
   {:schema [:map [:eid wire/varint] [:yaw wire/byte]
             [:pitch wire/byte] [:on-ground wire/boolean]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:yaw m)))
             (buf/write-byte! buf (int (:pitch m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :rotate-head]
   {:schema [:map [:eid wire/varint] [:yaw wire/byte]]
    :write :wire}
   [:play :entity-position-sync]
   {:schema [:map [:eid wire/varint] [:pos wire/vec3] [:vel wire/vec3]
             [:yaw wire/float] [:pitch wire/float]
             [:on-ground wire/boolean]]
    :write :wire}
   [:play :set-entity-motion]
   {:schema [:map [:eid wire/varint] [:vel wire/lp-vec3]]
    :write :wire}
   [:play :set-equipment]
   {:schema [:map [:eid wire/varint]
             [:slots [:sequential
                      [:tuple :int [:maybe delta/Stack]]]]]
    :write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (let [slots (vec (:slots m))]
               (doseq [[i [slot stack]] (map-indexed vector slots)]
                 (let [more? (< (inc i) (count slots))
                       b (if more? (bit-or (long slot) 0x80) slot)]
                   (buf/write-byte! buf (int b)))
                 (c/write-item-stack buf stack))))}
   [:play :animate]
   {:schema [:map [:eid wire/varint] [:action wire/unsigned-byte]]
    :write :wire}
   [:play :hurt-animation]
   {:schema [:map [:eid wire/varint] [:yaw wire/float]]
    :write :wire}
   [:play :entity-event]
   {:schema [:map [:eid wire/int] [:event wire/byte]]
    :write :wire}
   [:play :take-item-entity]
   {:schema [:map [:item wire/varint] [:collector wire/varint]
             [:amount wire/varint]]
    :write :wire}
   [:play :sound]
   {:schema [:map [:sound wire/holder-ref] [:source wire/varint]
             [:pos wire/fixed-vec3] [:volume wire/float]
             [:pitch wire/float] [:seed wire/long]]
    :write :wire}
   [:play :level-particles]
   {:schema [:map
             [:limiter {:optional true}
              [:= {:wire wire/boolean} false]]
             [:always {:optional true}
              [:= {:wire wire/boolean} false]]
             [:pos wire/vec3]
             [:dx {:optional true} [:= {:wire wire/float} 0.0]]
             [:dy {:optional true} [:= {:wire wire/float} 0.0]]
             [:dz {:optional true} [:= {:wire wire/float} 0.0]]
             [:speed wire/float] [:count wire/int]
             [:particle wire/varint] [:state [wire/tail wire/varint]]]
    :write :wire}
   [:play :explode]
   {:schema [:map [:center wire/vec3] [:radius wire/float]
             [:blocks wire/int] [:knockback [:maybe wire/vec3]]
             [:particle wire/varint] [:sound wire/holder-ref]
             [:block-particles
              [:sequential
               [:tuple wire/varint wire/float wire/float
                wire/varint]]]]
    :write :wire}
   [:play :chat]
   {:schema [:map [:message [wire/string {:max 256}]]
             [:timestamp wire/long] [:salt wire/long]
             [:signature [:maybe Signature]] [:last-seen LastSeen]]
    :read :wire}
   [:play :chat-command]
   {:schema [:map [:command wire/string]]
    :read :wire}
   [:play :chat-command-signed]
   {:schema [:map [:command wire/string]
             [:timestamp wire/long] [:salt wire/long]
             [:signatures
              [:sequential {:max 8}
               [:tuple [wire/string {:max 16}] Signature]]]
             [:last-seen LastSeen]]
    :read :wire}
   [:play :move-player-pos]
   {:schema [:map [:pos wire/vec3] [:flags wire/unsigned-byte]]
    :read :wire}
   [:play :move-player-pos-rot]
   {:schema [:map [:pos wire/vec3] [:yaw wire/float]
             [:pitch wire/float] [:flags wire/unsigned-byte]]
    :read :wire}
   [:play :move-player-rot]
   {:schema [:map [:yaw wire/float] [:pitch wire/float]
             [:flags wire/unsigned-byte]]
    :read :wire}
   [:play :move-player-status-only]
   {:schema [:map [:flags wire/unsigned-byte]]
    :read :wire}
   [:play :player-action]
   {:schema [:map [:action wire/varint] [:pos wire/block-pos]
             [:face wire/unsigned-byte] [:sequence wire/varint]]
    :read :wire}
   [:play :use-item-on]
   {:schema [:map [:hand wire/varint] [:pos wire/block-pos]
             [:face wire/varint] [:cursor wire/float-vec3]
             [:inside wire/boolean] [:border wire/boolean]
             [:sequence wire/varint]]
    :read :wire}
   [:play :use-item]
   {:schema [:map [:hand wire/varint] [:sequence wire/varint]
             [:yaw wire/float] [:pitch wire/float]]
    :read :wire}
   [:play :sign-update]
   {:schema [:map [:pos wire/block-pos] [:front? wire/boolean]
             [:lines [:tuple Line Line Line Line]]]
    :read :wire}
   [:play :swing]
   {:schema [:map [:hand wire/varint]]
    :read :wire}
   [:play :attack]
   {:schema [:map [:target wire/varint]]
    :read :wire}
   [:play :player-command]
   {:schema [:map [:eid wire/varint] [:action wire/varint]
             [:data wire/varint]]
    :read :wire}
   [:play :player-input]
   {:schema [:map [:flags wire/byte]]
    :read :wire}
   [:play :pick-item-from-block]
   {:schema [:map [:pos wire/block-pos]
             [:include-data wire/boolean]]
    :read :wire}
   [:play :pick-item-from-entity]
   {:schema [:map [:id wire/varint] [:include-data wire/boolean]]
    :read :wire}
   [:play :set-carried-item]
   {:schema [:map [:slot wire/short]]
    :read :wire}
   [:play :container-click]
   {:schema [:map [:container wire/varint] [:state-id wire/varint]
             [:slot wire/short] [:button wire/byte]
             [:mode wire/varint]
             [:changed [:map-of {:max 128}
                        wire/short wire/hashed-stack]]
             [:carried wire/hashed-stack]]
    :read :wire}
   [:play :set-creative-mode-slot]
   {:schema [:map [:slot wire/short]
             [:stack [wire/item-stack {:delimited true}]]]
    :read :wire}
   [:play :client-command]
   {:schema [:map [:action wire/varint]]
    :read :wire}
   [:play :interact]
   {:schema [:map [:target wire/varint] [:hand wire/varint]
             [:at wire/lp-vec3] [:sneaking wire/boolean]]
    :read :wire}})

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

(defn- no-writer [state m]
  (ex-info "no writer for packet"
           {:state state :packet (:packet m)}))

(defn decode
  "Returns the packet read from buf in the connection state."
  [state ^Buf buf]
  (let [id (c/read-varint buf)]
    (when-let [e (get (get @inbound state) id)]
      (if-let [r (:read e)]
        (let [m (r buf)]
          (when-let [check (:check e)] (check m))
          (assoc m :packet (:packet e)))
        {:packet (:packet e)}))))

(defn encode!
  "Writes packet m into buf with the id of the connection state."
  [state ^Buf buf m]
  (let [e (or (get (get @outbound state) (:packet m))
              (throw (no-writer state m)))]
    (when-let [check (:check e)] (check m))
    (c/write-varint buf (long (:id e)))
    ((:write e) buf m)))
