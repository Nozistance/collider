(ns collider.persist.region
  "Append-only region files of chunks, and the manifests of commits."
  (:require [clojure.java.io :as io]
            [collider.persist.atomic-file :as atomic-file])
  (:import (collider.persist Region)
           (java.nio.file Path)))

(set! *warn-on-reflection* true)

(defn- path ^Path [f]
  (.toPath (io/file f)))

(defn of
  "Returns the key of the region that holds chunk id."
  ^long [^long id]
  (Region/of id))

(defn create
  "Returns a new empty region of generation gen in dir. An old one of
  the same key and generation is lost."
  ^Region [dir ^long k ^long gen]
  (Region/create (path dir) k gen))

(defn open-all
  "Returns the regions that the manifest of generation gen in dir
  names, none for generation 0."
  [dir ^long gen]
  (vec (Region/openAll (path dir) gen)))

(defn key-of ^long [^Region r] (.-key r))

(defn gen-of ^long [^Region r] (.-gen r))

(defn copy
  "Returns a region over the same file with an index of its own."
  ^Region [^Region r]
  (.copy r))

(defn append!
  "Writes data to the end of r as the record of chunk id."
  [^Region r ^long id ^bytes data]
  (.append r id data))

(defn chunk-bytes
  "Returns the bytes of chunk id in r, or nil when r lacks it. Throws
  when no record of it is readable."
  [^Region r ^long id]
  (.get r id))

(defn chunks
  "Returns the chunks with a record in r, sorted by z and by x
  within a row."
  [^Region r]
  (.chunks r))

(defn sparse?
  "Returns true when r holds more than twice its live bytes."
  [^Region r]
  (.sparse r))

(defn compact
  "Returns a new region of generation gen with the live records of r.
  A chunk with no readable record is left out."
  ^Region [^Region r ^long gen]
  (.compact r gen))

(defn force!
  "Makes the records written to r durable."
  [^Region r]
  (.force r))

(defn close! [^Region r] (.close r))

(defn write-manifest!
  "Writes the manifest of generation gen in dir naming regions."
  [dir ^long gen regions]
  (atomic-file/put! (Region/manifestPath (path dir) gen)
                    (Region/manifest (into-array Region regions))))

(defn sweep!
  "Deletes the files in dir that neither generation gen nor the one
  before it names."
  [dir ^long gen]
  (Region/sweep (path dir) gen))
