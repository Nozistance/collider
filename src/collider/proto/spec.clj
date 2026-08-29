(ns collider.proto.spec
  (:require [collider.proto.types :as t])
  (:import (io.netty.buffer ByteBuf)))

(set! *warn-on-reflection* true)

(defonce registry (atom {}))
(defonce by-key   (atom {}))
(defn register! [{:keys [key state dir id] :as codec}]
  (swap! registry assoc [state dir id] codec)
  (swap! by-key assoc key codec)
  key)

(def ^:private field-codecs
  {:varint   [`t/read-varint   `t/write-varint]
   :string   [`t/read-string   `t/write-string]
   :bytes    [`t/read-bytes    `t/write-bytes]
   :bool     [`t/read-bool     `t/write-bool]
   :byte     [`t/read-byte     `t/write-byte]
   :ubyte    [`t/read-ubyte    `t/write-ubyte]
   :short    [`t/read-short    `t/write-short]
   :ushort   [`t/read-ushort   `t/write-ushort]
   :int      [`t/read-int      `t/write-int]
   :long     [`t/read-long     `t/write-long]
   :float    [`t/read-float    `t/write-float]
   :double   [`t/read-double   `t/write-double]
   :position [`t/read-position `t/write-position]
   :angle    [`t/read-angle    `t/write-angle]
   :uuid     [`t/read-uuid     `t/write-uuid]
   :metadata [`t/read-metadata `t/write-metadata]})

(defmacro defpacket [key {:keys [state dir id]} & fields]
  (let [buf   (with-meta (gensym "buf") {:tag `ByteBuf})
        m     (gensym "m")
        binds (mapv (fn [[fk ft]]
                      (let [[rd wr] (or (field-codecs ft)
                                        (throw (ex-info "Unknown field type" {:packet key :field fk :type ft})))]
                        {:sym (gensym (name fk)) :fk fk :rd rd :wr wr}))
                    fields)]
    `(register!
      {:key ~key :state ~state :dir ~dir :id ~id
       :read (fn [~buf]
               (let [~@(mapcat (fn [{:keys [sym rd]}] [sym (list rd buf)]) binds)]
                 ~(into {:packet/key key}
                        (map (fn [{:keys [sym fk]}] [fk sym]))
                        binds)))
       :write (fn [~buf ~m]
                ~@(map (fn [{:keys [fk wr]}] (list wr buf (list fk m))) binds)
                nil)})))

(defn decode [state ^ByteBuf buf]
  (let [id (t/read-varint buf)]
    (if-let [codec (@registry [state :c->s id])]
      ((:read codec) buf)
      {:packet/key :packet/unknown :packet/state state :packet/id id})))

(defn encode [^ByteBuf buf m]
  (let [k     (:packet/key m)
        codec (or (@by-key k)
                  (throw (ex-info "Unknown outbound packet" {:key k})))]
    (t/write-varint buf (:id codec))
    ((:write codec) buf m)))
