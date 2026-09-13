(ns collider.proto.packets
  (:require [collider.data :as data]
            [collider.proto.chunk :as chunk]
            [collider.proto.codec :as c])
  (:import (collider.java Buf)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- write-registry-entries! [^Buf buf names]
  (c/write-varint buf (count names))
  (doseq [n names]
    (c/write-id buf n)
    (.writeBoolean buf false)))

(defn- write-spawn-info [^Buf buf m]
  (c/write-holder-ref buf (long (:dimension-type m)))
  (c/write-id buf :overworld)
  (.writeLong buf 0)
  (.writeByte buf 1)
  (.writeByte buf -1)
  (.writeBoolean buf false)
  (.writeBoolean buf true)
  (.writeBoolean buf false)
  (c/write-varint buf 0)
  (c/write-varint buf 63))

(def packets
  {[:handshake :intention]
   {:read (fn [^Buf buf] {:protocol (c/read-varint buf)
                          :address  (c/read-string buf 255)
                          :port     (.readUnsignedShort buf)
                          :next     (c/read-varint buf)})}

   [:status :status-response]
   {:write (fn [^Buf buf m] (c/write-string buf (:json m)))}
   [:status :ping-request]
   {:read (fn [^Buf buf] {:payload (.readLong buf)})}
   [:status :pong-response]
   {:write (fn [^Buf buf m] (.writeLong buf (long (:payload m))))}

   [:login :hello]
   {:read (fn [^Buf buf] {:name (c/read-string buf 16) :uuid (c/read-uuid buf)})}
   [:login :login-compression]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:threshold m))))}
   [:login :login-finished]
   {:write (fn [^Buf buf m]
             (c/write-uuid buf (:uuid m))
             (c/write-string buf (:name m))
             (c/write-varint buf 0)
             (c/write-uuid buf (UUID. 0 0)))}
   [:login :login-disconnect]
   {:write (fn [^Buf buf m] (c/write-string buf (:json m)))}

   [:configuration :custom-payload]
   {:write (fn [^Buf buf m] (c/write-id buf (:channel m)) (c/write-string buf (:value m)))}
   [:configuration :update-enabled-features]
   {:write (fn [^Buf buf m] (c/write-list buf (:features m) c/write-id))}
   [:configuration :select-known-packs]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:packs m)))
             (doseq [pack (:packs m)]
               (doseq [s pack] (c/write-string buf s))))}
   [:configuration :registry-data]
   {:write (fn [^Buf buf m]
             (c/write-id buf (:registry m))
             (write-registry-entries! buf (:names m)))}
   [:configuration :update-tags]
   {:write (fn [^Buf buf m]
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
   {:write (fn [_ _] nil)}
   [:configuration :disconnect]
   {:write (fn [^Buf buf m] (c/write-component buf (:text m)))}

   [:play :login]
   {:write (fn [^Buf buf m]
             (.writeInt buf (int (:eid m)))
             (.writeBoolean buf false)
             (c/write-list buf [:overworld] c/write-id)
             (c/write-varint buf (long (:max-players m)))
             (c/write-varint buf (long (:view-distance m)))
             (c/write-varint buf (long (:simulation-distance m)))
             (.writeBoolean buf false)
             (.writeBoolean buf true)
             (.writeBoolean buf false)
             (write-spawn-info buf m)
             (.writeBoolean buf false)
             (.writeBoolean buf false))}
   [:play :respawn]
   {:write (fn [^Buf buf m]
             (write-spawn-info buf m)
             (.writeByte buf (int (:keep m 0))))}
   [:play :player-abilities]
   {:read  (fn [^Buf buf] {:flags (.readByte buf)})
    :write (fn [^Buf buf m]
             (.writeByte buf (int (:flags m)))
             (.writeFloat buf (float (:flying-speed m)))
             (.writeFloat buf (float (:walking-speed m))))}
   [:play :set-default-spawn-position]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-id buf :overworld)
               (c/write-block-pos buf x y z)
               (.writeFloat buf (float (:yaw m 0.0)))
               (.writeFloat buf (float (:pitch m 0.0)))))}
   [:play :set-health]
   {:write (fn [^Buf buf m]
             (.writeFloat buf (float (:health m)))
             (c/write-varint buf (long (:food m)))
             (.writeFloat buf (float (:saturation m))))}
   [:play :set-experience]
   {:write (fn [^Buf buf m]
             (.writeFloat buf (float (:progress m)))
             (c/write-varint buf (long (:level m)))
             (c/write-varint buf (long (:total m))))}
   [:play :game-event]
   {:write (fn [^Buf buf m]
             (.writeByte buf (int (:event m)))
             (.writeFloat buf (float (:value m 0.0))))}
   [:play :commands]
   {:write (fn [^Buf buf {:keys [nodes]}]
             (c/write-varint buf (count nodes))
             (doseq [{:keys [type name parser props executable? children]} nodes]
               (.writeByte buf (int (bit-or (case type :root 0 :literal 1 :argument 2)
                                            (if executable? 4 0))))
               (c/write-varint buf (count children))
               (doseq [c children] (c/write-varint buf (long c)))
               (when (not= :root type)
                 (c/write-string buf name))
               (when (= :argument type)
                 (c/write-varint buf (data/registry-id "command_argument_type" parser))
                 (case (clojure.core/name parser)
                   "brigadier:integer" (do (.writeByte buf 3) (.writeInt buf (int (:min props))) (.writeInt buf (int (:max props))))
                   "brigadier:double" (do (.writeByte buf 3) (.writeDouble buf (double (:min props))) (.writeDouble buf (double (:max props))))
                   "brigadier:string" (c/write-varint buf (long (:kind props 0)))
                   "time" (.writeInt buf (int (:min props 0)))
                   "entity" (.writeByte buf (int (bit-or (if (:single? props) 1 0) (if (:players? props) 2 0))))
                   "resource" (c/write-string buf (:registry props))
                   nil)))
             (c/write-varint buf (dec (count nodes))))}
   [:play :command-suggestions]
   {:write (fn [^Buf buf {:keys [id start length matches]}]
             (c/write-varint buf (long id))
             (c/write-varint buf (long start))
             (c/write-varint buf (long length))
             (c/write-varint buf (count matches))
             (doseq [m matches]
               (c/write-string buf m)
               (.writeBoolean buf false)))}
   [:play :command-suggestion]
   {:read (fn [^Buf buf] {:id (c/read-varint buf) :text (c/read-string buf 32500)})}
   [:play :award-stats]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:stats m)))
             (doseq [[k n] (:stats m)
                     :let [type (keyword (namespace k)) key (keyword (name k))]]
               (c/write-varint buf (data/registry-id "stat_type" type))
               (c/write-varint buf (data/registry-id (case type
                                                       :custom "custom_stat"
                                                       :mined "block"
                                                       (:killed :killed-by) "entity_type"
                                                       "item")
                                                     key))
               (c/write-varint buf (long n))))}
   [:play :game-rule-values]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:values m)))
             (doseq [[k v] (:values m)]
               (c/write-string buf k)
               (c/write-string buf v)))}
   [:play :set-game-rule]
   {:read (fn [^Buf buf]
            {:entries (vec (repeatedly (c/read-count buf)
                                       #(vector (c/read-string buf) (c/read-string buf))))})}
   [:play :ping-request]
   {:read (fn [^Buf buf] {:payload (.readLong buf)})}
   [:play :pong-response]
   {:write (fn [^Buf buf m] (.writeLong buf (long (:payload m))))}
   [:play :change-difficulty]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:difficulty m 0)))
             (.writeBoolean buf (boolean (:locked m))))}
   [:play :server-data]
   {:write (fn [^Buf buf m]
             (c/write-component buf (:motd m))
             (.writeBoolean buf false))}
   [:play :initialize-border]
   {:write (fn [^Buf buf m]
             (.writeDouble buf (double (:center-x m 0.0)))
             (.writeDouble buf (double (:center-z m 0.0)))
             (.writeDouble buf (double (:size m)))
             (.writeDouble buf (double (:size m)))
             (c/write-varlong buf 0)
             (c/write-varint buf (long (:max-size m)))
             (c/write-varint buf (long (:warning-blocks m 5)))
             (c/write-varint buf (long (:warning-time m 15))))}
   [:play :ticking-state]
   {:write (fn [^Buf buf m]
             (.writeFloat buf (float (:rate m 20.0)))
             (.writeBoolean buf (boolean (:frozen? m))))}
   [:play :ticking-step]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:steps m 0))))}
   [:play :update-attributes]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-varint buf (count (:attributes m)))
             (doseq [[attr base] (:attributes m)]
               (c/write-varint buf (data/registry-id "attribute" attr))
               (.writeDouble buf (double base))
               (c/write-varint buf 0)))}
   [:play :set-time]
   {:write (fn [^Buf buf m]
             (.writeLong buf (long (:age m)))
             (c/write-varint buf 1)
             (c/write-varint buf (data/datapack-id "world_clock" :overworld))
             (c/write-varlong buf (long (:time m)))
             (.writeFloat buf (float 0.0))
             (.writeFloat buf (float 1.0)))}
   [:play :player-position]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m) [vx vy vz] (:vel m [0.0 0.0 0.0])]
               (c/write-varint buf (long (:teleport-id m)))
               (.writeDouble buf (double x)) (.writeDouble buf (double y)) (.writeDouble buf (double z))
               (.writeDouble buf (double vx)) (.writeDouble buf (double vy)) (.writeDouble buf (double vz))
               (.writeFloat buf (float (:yaw m 0.0)))
               (.writeFloat buf (float (:pitch m 0.0)))
               (.writeInt buf (int (:relative m 0)))))}
   [:play :accept-teleportation]
   {:read (fn [^Buf buf] {:id (c/read-varint buf)})}
   [:play :set-chunk-cache-center]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:cx m))) (c/write-varint buf (long (:cz m))))}
   [:play :level-chunk-with-light]
   {:write (fn [^Buf buf m] (chunk/write-chunk! buf (:cx m) (:cz m) (:chunk m) (:block-entities m)))}
   [:play :open-sign-editor]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (.writeBoolean buf (boolean (:front? m))))}
   [:play :block-event]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (.writeByte buf (int (:action m)))
             (.writeByte buf (int (:param m)))
             (c/write-varint buf (long (:block m))))}
   [:play :block-entity-data]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:type m)))
             (c/write-nbt buf (:nbt m)))}
   [:play :forget-level-chunk]
   {:write (fn [^Buf buf m]
             (.writeLong buf (bit-or (bit-and (long (:cx m)) 0xFFFFFFFF)
                                     (bit-shift-left (long (:cz m)) 32))))}
   [:play :chunk-batch-start]
   {:write (fn [_ _] nil)}
   [:play :chunk-batch-finished]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:size m))))}
   [:play :chunk-batch-received]
   {:read (fn [^Buf buf] {:rate (.readFloat buf)})}
   [:play :keep-alive]
   {:read  (fn [^Buf buf] {:id (.readLong buf)})
    :write (fn [^Buf buf m] (.writeLong buf (long (:id m))))}
   [:play :disconnect]
   {:write (fn [^Buf buf m] (c/write-component buf (:text m)))}
   [:play :system-chat]
   {:write (fn [^Buf buf m]
             (c/write-component buf (:text m))
             (.writeBoolean buf (boolean (:overlay m))))}
   [:play :block-update]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:state m))))}
   [:play :section-blocks-update]
   {:write (fn [^Buf buf m]
             (let [[sx sy sz] (:section m)]
               (.writeLong buf (c/section-pos (long sx) (long sy) (long sz))))
             (c/write-varint buf (count (:changes m)))
             (doseq [[at state] (:changes m)]
               (c/write-varlong buf (bit-or (bit-shift-left (long state) 12) (long at)))))}
   [:play :block-changed-ack]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:sequence m))))}
   [:play :player-info-update]
   {:write (fn [^Buf buf m]
             (if (= :latency (:action m))
               (do (.writeByte buf 0x10)
                   (c/write-varint buf (count (:players m)))
                   (doseq [{:keys [uuid ping]} (:players m)]
                     (c/write-uuid buf uuid)
                     (c/write-varint buf (long (or ping 0)))))
               (do (.writeByte buf 0x1D)
                   (c/write-varint buf (count (:players m)))
                   (doseq [{:keys [uuid name gamemode ping]} (:players m)]
                     (c/write-uuid buf uuid)
                     (c/write-string buf name)
                     (c/write-varint buf 0)
                     (c/write-varint buf (long (or gamemode 1)))
                     (.writeBoolean buf true)
                     (c/write-varint buf (long (or ping 0)))))))}
   [:play :level-event]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (.writeInt buf (int (:event m)))
               (c/write-block-pos buf (long x) (long y) (long z))
               (.writeInt buf (int (:data m)))
               (.writeBoolean buf false)))}
   [:play :player-info-remove]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:uuids m)))
             (doseq [u (:uuids m)] (c/write-uuid buf u)))}
   [:play :tab-list]
   {:write (fn [^Buf buf m]
             (c/write-component buf (:header m))
             (c/write-component buf (:footer m)))}
   [:play :set-held-slot]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:slot m))))}
   [:play :container-set-content]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (c/write-varint buf (count (:items m)))
             (doseq [st (:items m)] (c/write-item-stack buf st))
             (c/write-item-stack buf (:carried m)))}
   [:play :container-set-slot]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (.writeShort buf (int (:slot m)))
             (c/write-item-stack buf (:stack m)))}

   [:play :open-screen]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m)))
             (c/write-varint buf (long (:menu m)))
             (c/write-component buf (:title m)))}
   [:play :container-set-data]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m)))
             (.writeShort buf (int (:id m)))
             (.writeShort buf (int (:value m))))}
   [:play :container-button-click]
   {:read (fn [^Buf buf] {:container (c/read-varint buf) :button (c/read-varint buf)})}
   [:play :update-recipes]
   {:write (fn [^Buf buf m]
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
   {:read  (fn [^Buf buf] {:container (c/read-varint buf)})
    :write (fn [^Buf buf m] (c/write-varint buf (long (:container m))))}
   [:play :set-cursor-item]
   {:write (fn [^Buf buf m] (c/write-item-stack buf (:stack m)))}

   [:play :bundle-delimiter]
   {:write (fn [_ _] nil)}
   [:play :add-entity]
   {:write (fn [^Buf buf m]
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
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:eids m)))
             (doseq [e (:eids m)] (c/write-varint buf (long e))))}
   [:play :set-entity-data]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-entity-data buf (:data m)))}
   [:play :move-entity-pos]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeShort buf (int (:dx m))) (.writeShort buf (int (:dy m))) (.writeShort buf (int (:dz m)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :move-entity-pos-rot]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeShort buf (int (:dx m))) (.writeShort buf (int (:dy m))) (.writeShort buf (int (:dz m)))
             (.writeByte buf (int (:yaw m))) (.writeByte buf (int (:pitch m)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :move-entity-rot]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeByte buf (int (:yaw m))) (.writeByte buf (int (:pitch m)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :rotate-head]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeByte buf (int (:yaw m))))}
   [:play :entity-position-sync]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-vec3 buf (:pos m))
             (c/write-vec3 buf (:vel m [0.0 0.0 0.0]))
             (.writeFloat buf (float (:yaw m 0.0)))
             (.writeFloat buf (float (:pitch m 0.0)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :set-entity-motion]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-lp-vec3 buf (:vel m)))}
   [:play :set-equipment]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (let [slots (vec (:slots m))]
               (doseq [[i [slot stack]] (map-indexed vector slots)]
                 (.writeByte buf (int (if (< (inc i) (count slots)) (bit-or (long slot) 0x80) slot)))
                 (c/write-item-stack buf stack))))}
   [:play :animate]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeByte buf (int (:action m))))}
   [:play :hurt-animation]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeFloat buf (float (:yaw m 0.0))))}
   [:play :entity-event]
   {:write (fn [^Buf buf m]
             (.writeInt buf (int (:eid m)))
             (.writeByte buf (int (:event m))))}
   [:play :take-item-entity]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:item m)))
             (c/write-varint buf (long (:collector m)))
             (c/write-varint buf (long (:amount m 1))))}
   [:play :sound]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-holder-ref buf (long (:sound m)))
               (c/write-varint buf (long (:source m)))
               (.writeInt buf (int (* 8.0 (double x))))
               (.writeInt buf (int (* 8.0 (double y))))
               (.writeInt buf (int (* 8.0 (double z))))
               (.writeFloat buf (float (:volume m)))
               (.writeFloat buf (float (:pitch m)))
               (.writeLong buf (long (:seed m 0)))))}
   [:play :level-particles]
   {:write (fn [^Buf buf m]
             (.writeBoolean buf false)
             (.writeBoolean buf false)
             (c/write-vec3 buf (:pos m))
             (.writeFloat buf (float 0.0))
             (.writeFloat buf (float 0.0))
             (.writeFloat buf (float 0.0))
             (.writeFloat buf (float (:speed m)))
             (.writeInt buf (int (:count m)))
             (c/write-varint buf (long (:particle m)))
             (when-some [st (:state m)] (c/write-varint buf (long st))))}
   [:play :explode]
   {:write (fn [^Buf buf m]
             (c/write-vec3 buf (:center m))
             (.writeFloat buf (float (:radius m)))
             (.writeInt buf (int (:blocks m 0)))
             (if-let [k (:knockback m)]
               (do (.writeBoolean buf true) (c/write-vec3 buf k))
               (.writeBoolean buf false))
             (c/write-varint buf (long (:particle m)))
             (c/write-holder-ref buf (long (:sound m)))
             (c/write-varint buf 0))}
   [:play :chat]
   {:read (fn [^Buf buf] {:message (c/read-string buf 256)})}
   [:play :chat-command]
   {:read (fn [^Buf buf] {:command (c/read-string buf)})}
   [:play :move-player-pos]
   {:read (fn [^Buf buf] {:pos   [(.readDouble buf) (.readDouble buf) (.readDouble buf)]
                          :flags (.readByte buf)})}
   [:play :move-player-pos-rot]
   {:read (fn [^Buf buf] {:pos   [(.readDouble buf) (.readDouble buf) (.readDouble buf)]
                          :yaw   (.readFloat buf)
                          :pitch (.readFloat buf)
                          :flags (.readByte buf)})}
   [:play :move-player-rot]
   {:read (fn [^Buf buf] {:yaw   (.readFloat buf)
                          :pitch (.readFloat buf)
                          :flags (.readByte buf)})}
   [:play :move-player-status-only]
   {:read (fn [^Buf buf] {:flags (.readByte buf)})}
   [:play :player-action]
   {:read (fn [^Buf buf] {:action   (c/read-varint buf)
                          :pos      (c/read-block-pos buf)
                          :face     (.readUnsignedByte buf)
                          :sequence (c/read-varint buf)})}
   [:play :use-item-on]
   {:read (fn [^Buf buf] {:hand     (c/read-varint buf)
                          :pos      (c/read-block-pos buf)
                          :face     (c/read-varint buf)
                          :cursor   [(.readFloat buf) (.readFloat buf) (.readFloat buf)]
                          :inside   (.readBoolean buf)
                          :border   (.readBoolean buf)
                          :sequence (c/read-varint buf)})}
   [:play :use-item]
   {:read (fn [^Buf buf] {:hand     (c/read-varint buf)
                          :sequence (c/read-varint buf)
                          :yaw      (.readFloat buf)
                          :pitch    (.readFloat buf)})}
   [:play :sign-update]
   {:read (fn [^Buf buf] {:pos    (c/read-block-pos buf)
                          :front? (.readBoolean buf)
                          :lines  (vec (repeatedly 4 #(c/read-string buf 384)))})}
   [:play :swing]
   {:read (fn [^Buf buf] {:hand (c/read-varint buf)})}
   [:play :player-command]
   {:read (fn [^Buf buf] {:eid    (c/read-varint buf)
                          :action (c/read-varint buf)
                          :data   (c/read-varint buf)})}
   [:play :player-input]
   {:read (fn [^Buf buf] {:flags (.readByte buf)})}
   [:play :pick-item-from-block]
   {:read (fn [^Buf buf] {:pos          (c/read-block-pos buf)
                          :include-data (.readBoolean buf)})}
   [:play :pick-item-from-entity]
   {:read (fn [^Buf buf] {:id           (c/read-varint buf)
                          :include-data (.readBoolean buf)})}
   [:play :set-carried-item]
   {:read (fn [^Buf buf] {:slot (.readShort buf)})}
   [:play :container-click]
   {:read (fn [^Buf buf]
            (let [container (c/read-varint buf)
                  state-id (c/read-varint buf)
                  slot (.readShort buf)
                  button (.readByte buf)
                  mode (c/read-varint buf)
                  changed (into {} (repeatedly (c/read-varint buf)
                                               #(vector (long (.readShort buf)) (c/read-hashed-stack buf))))
                  carried (c/read-hashed-stack buf)]
              {:container container :state-id state-id :slot slot :button button
               :mode      mode :changed changed :carried carried}))}
   [:play :set-creative-mode-slot]
   {:read (fn [^Buf buf] {:slot  (.readShort buf)
                          :stack (c/read-item-stack buf)})}
   [:play :client-command]
   {:read (fn [^Buf buf] {:action (c/read-varint buf)})}
   [:play :interact]
   {:read (fn [^Buf buf]
            (let [target (c/read-varint buf)
                  action (c/read-varint buf)]
              (case action
                0 (let [hand (c/read-varint buf)]
                    {:target target :action action :hand hand :sneaking (.readBoolean buf)})
                2 (let [at [(.readFloat buf) (.readFloat buf) (.readFloat buf)]
                        hand (c/read-varint buf)]
                    {:target target :action action :at at :hand hand :sneaking (.readBoolean buf)})
                {:target target :action action :sneaking (.readBoolean buf)})))}})

(def ^:private name-by-id
  (into {} (for [[state dirs] data/packets
                 [nm id] (:serverbound dirs)]
             [[state (long id)] nm])))

(defn decode [state ^Buf buf]
  (let [id (c/read-varint buf)]
    (when-let [nm (name-by-id [state id])]
      (if-let [r (:read (packets [state nm]))]
        (assoc (r buf) :packet nm)
        {:packet nm}))))

(defn encode! [state ^Buf buf m]
  (let [nm (:packet m)
        w (or (:write (packets [state nm]))
              (throw (ex-info "no writer for packet" {:state state :packet nm})))]
    (c/write-varint buf (data/packet-id state :clientbound nm))
    (w buf m)))
