(ns collider.persist.snapshot
  (:require [clojure.data.int-map :as im]
            [clojure.java.io :as io]
            [collider.log :as log]
            [collider.world.chunk :as chunk]
            [taoensso.nippy :as nippy])
  (:import (collider.world.chunk Section)
           (java.io File)
           (java.nio.file CopyOption Files OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

(set! *warn-on-reflection* true)

(def ^:const format-version 3)
(def ^:private readable-formats #{1 2 3})
(nippy/extend-freeze Section ::section [^Section s out]
  (nippy/freeze-to-out! out (.blocks s))
  (nippy/freeze-to-out! out (.block-light s))
  (nippy/freeze-to-out! out (.sky-light s)))

(nippy/extend-thaw ::section [in]
  (let [blocks      (nippy/thaw-from-in! in)
        block-light (nippy/thaw-from-in! in)
        sky-light   (nippy/thaw-from-in! in)]
    (chunk/->Section blocks block-light sky-light)))

(def ^:private freeze-opts {:compressor nippy/lz4-compressor})
(defn changed-chunks [old new]
  (into {} (remove (fn [[k v]] (identical? v (get old k)))) new))

(defn write-snapshot!
  ^long [file snap]
  (let [^Path target (.toPath (io/file file))
        dir         (or (.getParent target) (.toPath (io/file ".")))
        ^bytes data (nippy/freeze (-> snap
                                      (update :chunks #(into {} %))
                                      (assoc :format format-version))
                                  freeze-opts)]
    (Files/createDirectories dir (make-array FileAttribute 0))
    (let [tmp  (Files/createTempFile dir "world-" ".tmp" (make-array FileAttribute 0))
          ^"[Ljava.nio.file.OpenOption;" open-opts (make-array OpenOption 0)]
      (try
        (Files/write tmp data open-opts)
        (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                       StandardCopyOption/REPLACE_EXISTING]))
        (catch Throwable t
          (Files/deleteIfExists tmp)
          (throw t))))
    (alength data)))

(defn- thaw-chunk-key [k]
  (if (vector? k) (chunk/pos->id (k 0) (k 1)) k))

(defn- thaw-chunk [c]
  (if (map? (:sections c))
    (assoc c :sections (reduce-kv (fn [sv si sec] (assoc sv (long si) sec))
                                  (vec (repeat 16 nil)) (:sections c)))
    c))

(defn- thaw-chunks [cs]
  (into (im/int-map)
        (map (fn [[k c]] [(thaw-chunk-key k) (thaw-chunk c)]))
        cs))

(defn- thaw-block-ticks [bt]
  (reduce (fn [acc [dt ps]]
            (update acc (max 1 (long dt))
                    (fnil into (im/int-set))
                    (map chunk/block-pos->id ps)))
          (im/int-map)
          bt))

(defn load-snapshot [file]
  (let [^File f (io/file file)]
    (when (.isFile f)
      (try
        (let [m (nippy/thaw (Files/readAllBytes (.toPath f)))]
          (if (contains? readable-formats (:format m))
            (cond-> (-> (select-keys m [:time-of-day :profiles])
                        (assoc :chunks (thaw-chunks (:chunks m))))
              (seq (:block-ticks m)) (assoc :block-ticks (thaw-block-ticks (:block-ticks m))))
            (do (log/info "snapshot: unknown format" (:format m) "- world not loaded")
                nil)))
        (catch Throwable t
          (log/info "snapshot: read failed" (str f) "-" (.getMessage t))
          nil)))))

(defn start-saver
  ([] (start-saver nil))
  ([initial-snap]
   {:agent   (agent {:snap initial-snap :writes 0 :bytes 0} :error-mode :continue)
    :pending (atom nil)}))

(defn- same-but-time? [a b]
  (and (identical? (:chunks a) (:chunks b))
       (= (seq (:block-ticks a)) (seq (:block-ticks b)))
       (= (not-empty (:profiles a)) (not-empty (:profiles b)))))

(defn- same-snap? [a b]
  (and (same-but-time? a b)
       (= (:time-of-day a) (:time-of-day b))))

(defn- consume! [state pending]
  (let [[req] (swap-vals! pending (constantly nil))]
    (if (nil? req)
      state
      (let [[file snap] req]
        (if (same-snap? snap (:snap state))
          state
          (try
            (let [n (write-snapshot! file snap)]
              (when-not (same-but-time? snap (:snap state))
                (log/info "snapshot: saved" (count (:chunks snap)) "chunks,"
                          (log/human-bytes n) "to" (str file)))
              (-> state (assoc :snap snap :bytes n) (update :writes inc)))
            (catch Throwable t
              (log/info "snapshot: write failed -" (.getMessage t))
              state)))))))

(defn- rel-ticks [world]
  (let [t (long (:tick world 0))]
    (mapv (fn [[k s]] [(- (long k) t) (mapv chunk/id->block-pos s)]) (:block-ticks world))))

(defn request-save! [saver file world]
  (when saver
    (reset! (:pending saver) [file {:chunks      (:chunks world)
                                    :time-of-day (:time-of-day world)
                                    :block-ticks (rel-ticks world)
                                    :profiles    (:profiles world)}])
    (send-off (:agent saver) consume! (:pending saver))
    true))

(defn await-saver!
  ([saver] (await-saver! saver 2000))
  ([saver ms] (if saver (await-for ms (:agent saver)) true)))

(defn stop-saver! [saver file world]
  (request-save! saver file world)
  (await-saver! saver))
