(ns collider.persist.region
  "Append-only region files of chunks, and the manifests of commits."
  (:require [clojure.java.io :as io])
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
  "Returns a new empty region of generation gen in dir."
  ^Region [dir ^long k ^long gen]
  (Region/create (path dir) k gen))

(defn open-all
  "Returns the regions that the manifest of generation gen in dir
  names."
  [dir ^long gen]
  (vec (Region/openAll (path dir) gen)))

(defn key-of ^long [^Region r] (.-key r))

(defn gen-of ^long [^Region r] (.-gen r))

(defn copy
  "Returns a region over the same file with an index of its own."
  ^Region [^Region r]
  (.copy r))

(defn append!
  "Writes the bytes of chunk id to the end of r."
  [^Region r ^long id ^bytes data]
  (.append r id data))

(defn chunk-bytes
  "Returns the bytes of chunk id in r, or nil when r lacks it."
  [^Region r ^long id]
  (.get r id))

(defn chunks
  "Returns the ids of the chunks r holds."
  [^Region r]
  (.chunks r))

(defn sparse?
  "Returns true when r holds more than twice its live bytes."
  [^Region r]
  (.sparse r))

(defn compact
  "Returns a new region of generation gen with the live records of r."
  ^Region [^Region r ^long gen]
  (.compact r gen))

(defn force! [^Region r] (.force r))

(defn close! [^Region r] (.close r))

(defn write-manifest!
  "Writes the manifest of generation gen in dir naming regions."
  [dir ^long gen regions]
  (Region/writeManifest (path dir) gen (into-array Region regions)))

(defn sweep!
  "Deletes the files in dir that neither generation gen nor the one
  before it names."
  [dir ^long gen]
  (Region/sweep (path dir) gen))
