(ns collider.net.frame
  (:require [collider.proto.types :as t])
  (:import (io.netty.buffer ByteBuf)
           (io.netty.channel ChannelHandlerContext)
           (io.netty.handler.codec ByteToMessageDecoder MessageToByteEncoder)
           (java.util List)))

(set! *warn-on-reflection* true)

(defn- read-frame-len
  ^long [^ByteBuf in]
  (loop [n 0 acc 0]
    (if-not (.isReadable in)
      -1
      (let [b   (long (.readByte in))
            acc (bit-or acc (bit-shift-left (bit-and b 0x7F) (* n 7)))]
        (cond
          (zero? (bit-and b 0x80)) acc
          (>= n 2) (throw (ex-info "Frame length varint too long" {}))
          :else (recur (inc n) acc))))))

(defn varint-frame-decoder []
  (proxy [ByteToMessageDecoder] []
    (decode [^ChannelHandlerContext _ctx ^ByteBuf in ^List out]
      (loop []
        (when (.isReadable in)
          (.markReaderIndex in)
          (let [len (read-frame-len in)]
            (if (or (neg? len) (< (.readableBytes in) len))
              (.resetReaderIndex in)
              (do (.add out (.readRetainedSlice in (int len)))
                  (recur)))))))))

(defn varint-frame-encoder []
  (proxy [MessageToByteEncoder] []
    (encode [^ChannelHandlerContext _ctx ^ByteBuf msg ^ByteBuf out]
      (t/write-varint out (.readableBytes msg))
      (.writeBytes out msg))))
