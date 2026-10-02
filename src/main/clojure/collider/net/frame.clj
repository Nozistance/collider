(ns collider.net.frame
  "Packet frames on a connection: length prefix and compression."
  (:require [collider.proto.buf :as buf]
            [collider.proto.codec :as c])
  (:import (collider.proto Buf)
           (java.io EOFException InputStream OutputStream)
           (java.util.zip Deflater Inflater)))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-uncompressed 8388608)

(def ^:private ^:const max-length-bytes 3)

(defn- read-length ^long [^InputStream in]
  (loop [n 0 acc 0]
    (let [b (.read in)]
      (when (neg? b) (throw (EOFException. "end of stream")))
      (let [acc (bit-or acc
                        (bit-shift-left (bit-and b 0x7F) (* n 7)))]
        (cond
          (zero? (bit-and b 0x80)) acc
          (>= (inc n) max-length-bytes)
          (throw (ex-info "frame length varint too long" {}))
          :else (recur (inc n) acc))))))

(def ^:private ^:const frame-keep 8192)

(defn read-frame!
  "Reads one packet from the stream into buf and returns buf."
  ^Buf [^InputStream in ^Buf buf]
  (let [len (read-length in)]
    (buf/clear! buf frame-keep)
    (buf/read-from! buf in len)
    buf))

(defn- bad-frame [info]
  (ex-info "badly compressed packet" info))

(defn- inflate! [^Buf buf ^Inflater inflater ^long n]
  (let [dst (byte-array n)]
    (buf/inflate-input! buf inflater)
    (let [got (try (.inflate inflater dst)
                   (finally (.reset inflater)))]
      (when (not= got n)
        (throw (bad-frame {:got got :expected n}))))
    (buf/adopt! buf dst n)))

(defn decompress!
  "Inflates the packet in buf if it came compressed, and returns buf.
  Throws when its size breaks the threshold rules."
  ^Buf [^Buf buf ^long threshold ^Inflater inflater]
  (when-not (neg? threshold)
    (let [n (c/read-varint buf)]
      (when (pos? n)
        (when (< n threshold)
          (throw (bad-frame {:size n :threshold threshold})))
        (when (> n max-uncompressed)
          (throw (bad-frame {:size n :max max-uncompressed})))
        (inflate! buf inflater n))))
  buf)

(def ^:private ^:const deflate-step 8192)

(defn- deflate-into! [^Buf body ^Deflater deflater]
  (loop []
    (buf/ensure! body deflate-step)
    (buf/deflate! body deflater)
    (when-not (.finished deflater) (recur))))

(defn- compress-into! [^Buf body ^Buf payload ^Deflater deflater]
  (c/write-varint body (buf/readable-bytes payload))
  (buf/deflate-input! payload deflater)
  (.finish deflater)
  (deflate-into! body deflater)
  (.reset deflater))

(defn- write-body!
  [^Buf body ^Buf payload ^long threshold ^Deflater deflater]
  (cond
    (neg? threshold) (buf/write-bytes! body payload)
    (< (buf/readable-bytes payload) threshold)
    (do (c/write-varint body 0) (buf/write-bytes! body payload))
    :else (compress-into! body payload deflater)))

(defn write-frame!
  "Writes the payload to the stream as one packet."
  [^OutputStream out ^Buf payload ^Buf body ^Buf head threshold
   ^Deflater deflater]
  (buf/clear! body)
  (buf/clear! head)
  (write-body! body payload threshold deflater)
  (c/write-varint head (buf/readable-bytes body))
  (buf/write-to! head out)
  (buf/write-to! body out))
