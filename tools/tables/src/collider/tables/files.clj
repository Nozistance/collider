(ns collider.tables.files
  "Files, directories and the entries of a jar."
  (:require [clojure.java.io :as io])
  (:import (java.io File InputStream)
           (java.nio.file Files Path)
           (java.nio.file.attribute FileAttribute)
           (java.util.zip ZipEntry ZipFile)))

(set! *warn-on-reflection* true)

(defn temp-dir ^File [name]
  (let [attrs (make-array FileAttribute 0)]
    (Path/.toFile (Files/createTempDirectory name attrs))))

(defn delete-tree! [^File dir]
  (doseq [f (reverse (file-seq dir))]
    (File/.delete f)))

(defn copy-tree! [^File from ^File to]
  (doseq [f (file-seq from) :when (File/.isFile f)]
    (let [skip (inc (count (File/.getPath from)))
          t (io/file to (subs (File/.getPath f) skip))]
      (io/make-parents t)
      (io/copy f t))))

(defn file-count ^long [^File dir]
  (count (filter File/.isFile (file-seq dir))))

(defn zip-names [^ZipFile zf]
  (map ZipEntry/.getName (enumeration-seq (ZipFile/.entries zf))))

(defn- entry-stream ^InputStream [^ZipFile zf name]
  (ZipFile/.getInputStream zf (ZipFile/.getEntry zf name)))

(defn unzip
  "Copies the entry name of zf into the file to and returns to."
  ^File [^ZipFile zf name ^File to]
  (io/make-parents to)
  (with-open [in (entry-stream zf name)]
    (io/copy in to))
  to)
