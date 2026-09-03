(ns collider.proto.packets
  "Readers and writers of 26.2 packets by [state name]; ids come from
   resources/mc/packets.edn."
  (:require [collider.data :as data]
            [collider.proto.chunk :as chunk]
            [collider.proto.codec :as c])
  (:import (io.netty.buffer ByteBuf)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- write-registry-entries! [^ByteBuf buf names]
  (c/write-varint buf (count names))
  (doseq [n names]
    (c/write-id buf n)
    (.writeBoolean buf false)))

(def packets
  {[:handshake :intention]
   {:read (fn [^ByteBuf buf] {:protocol (c/read-varint buf)
                              :address  (c/read-string buf)
                              :port     (.readUnsignedShort buf)
                              :next     (c/read-varint buf)})}

   [:status :status-response]
   {:write (fn [^ByteBuf buf m] (c/write-string buf (:json m)))}
   [:status :ping-request]
   {:read (fn [^ByteBuf buf] {:payload (.readLong buf)})}
   [:status :pong-response]
   {:write (fn [^ByteBuf buf m] (.writeLong buf (long (:payload m))))}

   [:login :hello]
   {:read (fn [^ByteBuf buf] {:name (c/read-string buf) :uuid (c/read-uuid buf)})}
   [:login :login-finished]
   {:write (fn [^ByteBuf buf m]
             (c/write-uuid buf (:uuid m))
             (c/write-string buf (:name m))
             (c/write-varint buf 0)
             (c/write-uuid buf (UUID. 0 0)))}
   [:login :login-disconnect]
   {:write (fn [^ByteBuf buf m] (c/write-string buf (:json m)))}

   [:configuration :custom-payload]
   {:write (fn [^ByteBuf buf m] (c/write-id buf (:channel m)) (c/write-string buf (:value m)))}
   [:configuration :update-enabled-features]
   {:write (fn [^ByteBuf buf m] (c/write-list buf (:features m) c/write-id))}
   [:configuration :select-known-packs]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (count (:packs m)))
             (doseq [pack (:packs m)]
               (doseq [s pack] (c/write-string buf s))))}
   [:configuration :registry-data]
   {:write (fn [^ByteBuf buf m]
             (c/write-id buf (:registry m))
             (write-registry-entries! buf (:names m)))}
   [:configuration :update-tags]
   {:write (fn [^ByteBuf buf m]
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
   {:write (fn [^ByteBuf buf m] (c/write-component buf (:text m)))}

   [:play :login]
   {:write (fn [^ByteBuf buf m]
             (.writeInt buf (int (:eid m)))
             (.writeBoolean buf false)
             (c/write-list buf [:overworld] c/write-id)
             (c/write-varint buf (long (:max-players m)))
             (c/write-varint buf (long (:view-distance m)))
             (c/write-varint buf (long (:simulation-distance m)))
             (.writeBoolean buf false)
             (.writeBoolean buf true)
             (.writeBoolean buf false)
             (c/write-holder-ref buf (long (:dimension-type m)))
             (c/write-id buf :overworld)
             (.writeLong buf 0)
             (.writeByte buf 1)
             (.writeByte buf -1)
             (.writeBoolean buf false)
             (.writeBoolean buf true)
             (.writeBoolean buf false)
             (c/write-varint buf 0)
             (c/write-varint buf 63)
             (.writeBoolean buf false)
             (.writeBoolean buf false))}
   [:play :player-abilities]
   {:read (fn [^ByteBuf buf] {:flags (.readByte buf)})
    :write (fn [^ByteBuf buf m]
             (.writeByte buf (int (:flags m)))
             (.writeFloat buf (float (:flying-speed m)))
             (.writeFloat buf (float (:walking-speed m))))}
   [:play :set-default-spawn-position]
   {:write (fn [^ByteBuf buf m]
             (let [[x y z] (:pos m)]
               (c/write-id buf :overworld)
               (c/write-block-pos buf x y z)
               (.writeFloat buf (float (:yaw m 0.0)))
               (.writeFloat buf (float (:pitch m 0.0)))))}
   [:play :set-health]
   {:write (fn [^ByteBuf buf m]
             (.writeFloat buf (float (:health m)))
             (c/write-varint buf (long (:food m)))
             (.writeFloat buf (float (:saturation m))))}
   [:play :set-experience]
   {:write (fn [^ByteBuf buf m]
             (.writeFloat buf (float (:progress m)))
             (c/write-varint buf (long (:level m)))
             (c/write-varint buf (long (:total m))))}
   [:play :game-event]
   {:write (fn [^ByteBuf buf m]
             (.writeByte buf (int (:event m)))
             (.writeFloat buf (float (:value m 0.0))))}
   [:play :set-time]
   {:write (fn [^ByteBuf buf m]
             (.writeLong buf (long (:age m)))
             (c/write-varint buf 1)
             (c/write-varint buf (data/datapack-id "world_clock" :overworld))
             (c/write-varlong buf (long (:time m)))
             (.writeFloat buf (float 0.0))
             (.writeFloat buf (float 1.0)))}
   [:play :player-position]
   {:write (fn [^ByteBuf buf m]
             (let [[x y z] (:pos m) [vx vy vz] (:vel m [0.0 0.0 0.0])]
               (c/write-varint buf (long (:teleport-id m)))
               (.writeDouble buf (double x)) (.writeDouble buf (double y)) (.writeDouble buf (double z))
               (.writeDouble buf (double vx)) (.writeDouble buf (double vy)) (.writeDouble buf (double vz))
               (.writeFloat buf (float (:yaw m 0.0)))
               (.writeFloat buf (float (:pitch m 0.0)))
               (.writeInt buf (int (:relative m 0)))))}
   [:play :accept-teleportation]
   {:read (fn [^ByteBuf buf] {:id (c/read-varint buf)})}
   [:play :set-chunk-cache-center]
   {:write (fn [^ByteBuf buf m] (c/write-varint buf (long (:cx m))) (c/write-varint buf (long (:cz m))))}
   [:play :level-chunk-with-light]
   {:write (fn [^ByteBuf buf m] (chunk/write-chunk! buf (:cx m) (:cz m) (:chunk m)))}
   [:play :forget-level-chunk]
   {:write (fn [^ByteBuf buf m]
             (.writeLong buf (bit-or (bit-and (long (:cx m)) 0xFFFFFFFF)
                                     (bit-shift-left (long (:cz m)) 32))))}
   [:play :chunk-batch-start]
   {:write (fn [_ _] nil)}
   [:play :chunk-batch-finished]
   {:write (fn [^ByteBuf buf m] (c/write-varint buf (long (:size m))))}
   [:play :chunk-batch-received]
   {:read (fn [^ByteBuf buf] {:rate (.readFloat buf)})}
   [:play :keep-alive]
   {:read  (fn [^ByteBuf buf] {:id (.readLong buf)})
    :write (fn [^ByteBuf buf m] (.writeLong buf (long (:id m))))}
   [:play :disconnect]
   {:write (fn [^ByteBuf buf m] (c/write-component buf (:text m)))}
   [:play :system-chat]
   {:write (fn [^ByteBuf buf m]
             (c/write-component buf (:text m))
             (.writeBoolean buf (boolean (:overlay m))))}
   [:play :block-update]
   {:write (fn [^ByteBuf buf m]
             (let [[x y z] (:pos m)]
               (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:state m))))}
   [:play :section-blocks-update]
   {:write (fn [^ByteBuf buf m]
             (let [[sx sy sz] (:section m)]
               (.writeLong buf (c/section-pos (long sx) (long sy) (long sz))))
             (c/write-varint buf (count (:changes m)))
             (doseq [[at state] (:changes m)]
               (c/write-varlong buf (bit-or (bit-shift-left (long state) 12) (long at)))))}
   [:play :block-changed-ack]
   {:write (fn [^ByteBuf buf m] (c/write-varint buf (long (:sequence m))))}
   [:play :player-info-update]
   {:write (fn [^ByteBuf buf m]
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
   {:write (fn [^ByteBuf buf m]
             (let [[x y z] (:pos m)]
               (.writeInt buf (int (:event m)))
               (c/write-block-pos buf (long x) (long y) (long z))
               (.writeInt buf (int (:data m)))
               (.writeBoolean buf false)))}
   [:play :player-info-remove]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (count (:uuids m)))
             (doseq [u (:uuids m)] (c/write-uuid buf u)))}
   [:play :tab-list]
   {:write (fn [^ByteBuf buf m]
             (c/write-component buf (:header m))
             (c/write-component buf (:footer m)))}
   [:play :set-held-slot]
   {:write (fn [^ByteBuf buf m] (c/write-varint buf (long (:slot m))))}
   [:play :container-set-content]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (c/write-varint buf (count (:items m)))
             (doseq [st (:items m)] (c/write-item-stack buf st))
             (c/write-item-stack buf (:carried m)))}
   [:play :container-set-slot]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (.writeShort buf (int (:slot m)))
             (c/write-item-stack buf (:stack m)))}

   [:play :bundle-delimiter]
   {:write (fn [_ _] nil)}
   [:play :add-entity]
   {:write (fn [^ByteBuf buf m]
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
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (count (:eids m)))
             (doseq [e (:eids m)] (c/write-varint buf (long e))))}
   [:play :set-entity-data]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-entity-data buf (:data m)))}
   [:play :move-entity-pos]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeShort buf (int (:dx m))) (.writeShort buf (int (:dy m))) (.writeShort buf (int (:dz m)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :move-entity-pos-rot]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeShort buf (int (:dx m))) (.writeShort buf (int (:dy m))) (.writeShort buf (int (:dz m)))
             (.writeByte buf (int (:yaw m))) (.writeByte buf (int (:pitch m)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :move-entity-rot]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeByte buf (int (:yaw m))) (.writeByte buf (int (:pitch m)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :rotate-head]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeByte buf (int (:yaw m))))}
   [:play :entity-position-sync]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-vec3 buf (:pos m))
             (c/write-vec3 buf (:vel m [0.0 0.0 0.0]))
             (.writeFloat buf (float (:yaw m 0.0)))
             (.writeFloat buf (float (:pitch m 0.0)))
             (.writeBoolean buf (boolean (:on-ground m))))}
   [:play :set-entity-motion]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-lp-vec3 buf (:vel m)))}
   [:play :set-equipment]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (let [slots (vec (:slots m))]
               (doseq [[i [slot stack]] (map-indexed vector slots)]
                 (.writeByte buf (int (if (< (inc i) (count slots)) (bit-or (long slot) 0x80) slot)))
                 (c/write-item-stack buf stack))))}
   [:play :animate]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeByte buf (int (:action m))))}
   [:play :hurt-animation]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:eid m)))
             (.writeFloat buf (float (:yaw m 0.0))))}
   [:play :entity-event]
   {:write (fn [^ByteBuf buf m]
             (.writeInt buf (int (:eid m)))
             (.writeByte buf (int (:event m))))}
   [:play :take-item-entity]
   {:write (fn [^ByteBuf buf m]
             (c/write-varint buf (long (:item m)))
             (c/write-varint buf (long (:collector m)))
             (c/write-varint buf (long (:amount m 1))))}
   [:play :sound]
   {:write (fn [^ByteBuf buf m]
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
   {:write (fn [^ByteBuf buf m]
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
   {:write (fn [^ByteBuf buf m]
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
   {:read (fn [^ByteBuf buf] {:message (c/read-string buf)})}
   [:play :chat-command]
   {:read (fn [^ByteBuf buf] {:command (c/read-string buf)})}
   [:play :move-player-pos]
   {:read (fn [^ByteBuf buf] {:pos   [(.readDouble buf) (.readDouble buf) (.readDouble buf)]
                              :flags (.readByte buf)})}
   [:play :move-player-pos-rot]
   {:read (fn [^ByteBuf buf] {:pos   [(.readDouble buf) (.readDouble buf) (.readDouble buf)]
                              :yaw   (.readFloat buf)
                              :pitch (.readFloat buf)
                              :flags (.readByte buf)})}
   [:play :move-player-rot]
   {:read (fn [^ByteBuf buf] {:yaw   (.readFloat buf)
                              :pitch (.readFloat buf)
                              :flags (.readByte buf)})}
   [:play :move-player-status-only]
   {:read (fn [^ByteBuf buf] {:flags (.readByte buf)})}
   [:play :player-action]
   {:read (fn [^ByteBuf buf] {:action   (c/read-varint buf)
                              :pos      (c/read-block-pos buf)
                              :face     (.readUnsignedByte buf)
                              :sequence (c/read-varint buf)})}
   [:play :use-item-on]
   {:read (fn [^ByteBuf buf] {:hand     (c/read-varint buf)
                              :pos      (c/read-block-pos buf)
                              :face     (c/read-varint buf)
                              :cursor   [(.readFloat buf) (.readFloat buf) (.readFloat buf)]
                              :inside   (.readBoolean buf)
                              :border   (.readBoolean buf)
                              :sequence (c/read-varint buf)})}
   [:play :use-item]
   {:read (fn [^ByteBuf buf] {:hand     (c/read-varint buf)
                              :sequence (c/read-varint buf)})}
   [:play :swing]
   {:read (fn [^ByteBuf buf] {:hand (c/read-varint buf)})}
   [:play :player-command]
   {:read (fn [^ByteBuf buf] {:eid    (c/read-varint buf)
                              :action (c/read-varint buf)
                              :data   (c/read-varint buf)})}
   [:play :player-input]
   {:read (fn [^ByteBuf buf] {:flags (.readByte buf)})}
   [:play :pick-item-from-block]
   {:read (fn [^ByteBuf buf] {:pos (c/read-block-pos buf)
                              :include-data (.readBoolean buf)})}
   [:play :pick-item-from-entity]
   {:read (fn [^ByteBuf buf] {:id (c/read-varint buf)
                              :include-data (.readBoolean buf)})}
   [:play :set-carried-item]
   {:read (fn [^ByteBuf buf] {:slot (.readShort buf)})}
   [:play :set-creative-mode-slot]
   {:read (fn [^ByteBuf buf] {:slot  (.readShort buf)
                              :stack (c/read-item-stack buf)})}
   [:play :client-command]
   {:read (fn [^ByteBuf buf] {:action (c/read-varint buf)})}
   [:play :interact]
   {:read (fn [^ByteBuf buf]
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
  (delay (into {} (for [[state dirs] @data/packets
                        [nm id] (:serverbound dirs)]
                    [[state (long id)] nm]))))

(defn decode
  "Reads one serverbound packet of the given connection state: a map with
   :packet name, or nil for an unknown id."
  [state ^ByteBuf buf]
  (let [id (c/read-varint buf)]
    (when-let [nm (@name-by-id [state id])]
      (if-let [r (:read (packets [state nm]))]
        (assoc (r buf) :packet nm)
        {:packet nm}))))

(defn encode!
  "Writes the clientbound packet m (:packet name) of the given connection state."
  [state ^ByteBuf buf m]
  (let [nm (:packet m)
        w  (or (:write (packets [state nm]))
               (throw (ex-info "no writer for packet" {:state state :packet nm})))]
    (c/write-varint buf (data/packet-id state :clientbound nm))
    (w buf m)))
