(ns collider.tables.fetch
  "Getting the vanilla server jar from Mojang, once."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [collider.tables.progress :refer [measured progress!]]
            [collider.tables.stamp :as stamp])
  (:import (java.io File InputStream OutputStream)
           (java.net URL URLConnection)
           (java.security MessageDigest)
           (java.util HexFormat)))

(set! *warn-on-reflection* true)

(def ^:private manifest-url
  "https://piston-meta.mojang.com/mc/game/version_manifest_v2.json")

(defn sha1 [^File f]
  (let [md (MessageDigest/getInstance "SHA-1")
        buf (byte-array 65536)]
    (with-open [in (io/input-stream f)]
      (loop []
        (let [n (InputStream/.read in buf)]
          (when (pos? n)
            (MessageDigest/.update md buf 0 n)
            (recur)))))
    (HexFormat/.formatHex (HexFormat/of) (MessageDigest/.digest md))))

(def ^:private timeout-ms 15000)

(defn- open-url ^InputStream [url]
  (let [c (URL/.openConnection (URL/new (str url)))]
    (URLConnection/.setConnectTimeout c timeout-ms)
    (URLConnection/.setReadTimeout c timeout-ms)
    (URLConnection/.getInputStream c)))

(defn- unreachable [^Throwable e]
  (let [why (str "The exception was: "
                 (Class/.getSimpleName (class e))
                 ": " (ex-message e))
        cmd (str "Download server.jar " stamp/game " yourself"
                 " and run again with --jar path/to/server.jar")]
    (ex-info "cannot reach Mojang"
             {:what "failed to reach Mojang"
              :why why :command cmd}
             e)))

(defn- fetch-json [url]
  (try (with-open [in (open-url url)]
         (json/read-str (slurp in)))
       (catch Exception e
         (throw (unreachable e)))))

(defn- corrupt [url want got]
  (ex-info "sha1 mismatch"
           {:what "server.jar is corrupt"
            :why (str "Its checksum does not match the one"
                      " Mojang published")
            :command "Run again to download it once more"
            :url url :want want :got got}))

(defn- copy! [^InputStream in ^OutputStream out ^long total]
  (let [buf (byte-array 65536)]
    (loop [done 0 shown -1]
      (let [n (InputStream/.read in buf)
            done (+ done (max n 0))
            pct (quot (* 100 done) (max total 1))]
        (when (not= pct shown)
          (progress! {:event :progress :step :jar
                      :done done :total total}))
        (when (pos? n)
          (OutputStream/.write out buf 0 n)
          (recur done pct))))))

(defn- download! [url ^File to want total]
  (io/make-parents to)
  (with-open [in (open-url url)
              out (io/output-stream to)]
    (copy! in out total))
  (let [got (sha1 to)]
    (when (not= got want)
      (File/.delete to)
      (throw (corrupt url want got)))))

(defn- no-version []
  (ex-info (str "no version " stamp/game)
           {:what "no such version"
            :why (str "Mojang does not list version " stamp/game)
            :command "Update Collider"}))

(defn- server-download []
  (let [versions (get (fetch-json manifest-url) "versions")
        entry (some #(when (= stamp/game (get % "id")) %) versions)]
    (when-not entry
      (throw (no-version)))
    (get-in (fetch-json (get entry "url")) ["downloads" "server"])))

(defn- fetch! [^File jar]
  (let [{:strs [url size sha1]} (server-download)
        part (io/file (File/.getParentFile jar) "server.jar.part")]
    (progress! {:event :begin :step :jar :url url})
    (download! url part sha1 size)
    (File/.renameTo part jar)
    {:source :mojang :bytes size}))

(def ^:private no-jar-command
  "Check --jar, or leave it out to download server.jar from Mojang")

(defn- no-such-jar [local]
  (ex-info (str "no jar at " local)
           {:what "no such jar"
            :why (str "There is no file at " local)
            :command no-jar-command}))

(defn- fetch-into [cache local ^File jar]
  (cond
    local (if (File/.isFile jar)
            {:source :local :path (str local)}
            (throw (no-such-jar local)))
    (File/.isFile jar) {:source :cached :path (str cache)}
    :else (fetch! jar)))

(defn fetch
  "Returns local, or the jar in cache downloaded from Mojang once."
  ^File [cache local]
  (let [jar (io/file (or local (io/file cache "server.jar")))]
    (measured :jar #(fetch-into cache local jar))
    jar))
