(ns build
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b])
  (:import (java.io File)
           (java.lang.reflect Field)
           (java.net URL URLClassLoader)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.zip ZipEntry ZipFile)))

(def class-dir "target/classes")
(def prim-dir  "target/classes")
(def jar-file  "target/collider.jar")
(def basis     (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:src-dirs   ["src"]
            :class-dir  prim-dir
            :basis      basis
            :javac-opts ["-proc:none" "--release" "21"]}))

(declare blocks datapack-names kw packets registries tags-of vanilla-shapes write-edn!)

(defn data [{:keys [dir out] :or {out "resources/mc"}}]
  (let [root    (io/file (or dir (str (System/getProperty "user.home") "/Documents/MC-26.2")))
        reports (io/file root "reports")
        server  (io/file root "jars" "server-plain.jar")]
    (when-not (.isDirectory reports)
      (throw (ex-info (str "no reports in " reports)
                      {:dir (str root)})))
    (println "reading" (str root))
    (let [ps (packets reports)
          {sh :shapes sturdy :sturdy} (vanilla-shapes root)
          bs (blocks reports (into #{} (comp (remove (fn [[_ b]] (contains? sh (get (first (filter #(get % "default") (get b "states"))) "id")))) (map (comp kw key)))
                                   (json/read-str (slurp (io/file reports "blocks.json")))))
          rs (registries reports)
          dp (when (.isFile server) (datapack-names server))
          tg (when (.isFile server)
               (tags-of server (distinct (concat (keys rs) (keys (or dp {}))))))
          path (fn [n] (str out "/" n))]
      (write-edn! (path "packets.edn") ps
                  (format "%d states, %d packets"
                          (count ps) (reduce + (for [[_ d] ps [_ m] d] (count m)))))
      (write-edn! (path "blocks.edn") bs (format "%d blocks" (count bs)))
      (write-edn! (path "registries.edn") rs
                  (format "%d registries, %d entries" (count rs) (reduce + (map count (vals rs)))))
      (if dp
        (write-edn! (path "datapack.edn") dp
                    (format "%d registries, %d names" (count dp) (reduce + (map count (vals dp)))))
        (println "  (no server-plain.jar - datapack names skipped)"))
      (write-edn! (path "shapes.edn") sh
                  (format "%d states that are not a whole cube" (count sh)))
      (write-edn! (path "sturdy.edn") sturdy
                  (format "%d states with a non-sturdy face" (count sturdy)))
      (when tg
        (write-edn! (path "tags.edn") tg
                    (format "%d registries, %d tags" (count tg) (reduce + (map count (vals tg)))))))))

(defn uber [_]
  (clean nil)
  (javac nil)
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  :ns-compile '[collider.server]
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"]})
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis     basis
           :main      'collider.server}))

(defn- kw [s]
  (keyword (str/replace (str/replace (str s) #"^minecraft:" "") "_" "-")))

(defn- write-edn! [path data summary]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (binding [*out* w *print-length* nil *print-level* nil]
      (prn data)))
  (println (format "  %-28s %s" path summary)))

(defn- packets [reports]
  (into {}
        (map (fn [[state dirs]]
               [(kw state)
                (into {} (map (fn [[dir ps]]
                                [(kw dir)
                                 (into (sorted-map)
                                       (map (fn [[n {:strs [protocol_id]}]] [(kw n) protocol_id]))
                                       ps)]))
                      dirs)]))
        (json/read-str (slurp (io/file reports "packets.json")))))

(defn- vanilla-loader ^ClassLoader [root]
  (let [tmp (.toFile (Files/createTempDirectory "mc-libs" (make-array FileAttribute 0)))
        jars (with-open [zf (ZipFile. (io/file root "jars" "server.jar"))]
               (doall (for [^ZipEntry e (enumeration-seq (.entries zf))
                            :let [n (.getName e)]
                            :when (and (str/starts-with? n "META-INF/libraries/") (str/ends-with? n ".jar"))]
                        (let [f (io/file tmp (str/replace n "/" "_"))]
                          (with-open [in (.getInputStream zf e)] (io/copy in f))
                          f))))
        urls (into-array URL (map #(.toURL (.toURI ^File %))
                                  (cons (io/file root "jars" "server-plain.jar") jars)))]
    (URLClassLoader. urls (ClassLoader/getPlatformClassLoader))))

(defn- call-static [^ClassLoader cl cls m]
  (.invoke (.getMethod (Class/forName cls true cl) m (make-array Class 0)) nil (object-array 0)))

(defn- static-field [^ClassLoader cl cls f]
  (.get (.getField (Class/forName cls true cl) f) nil))

(def ^:private full-box [[0 0 0 16 16 16]])

(defn- sixteenth [^double v]
  (let [x (* 16.0 v)] (if (== x (Math/rint x)) (long x) x)))

(defn- vanilla-shapes [root]
  (let [cl (vanilla-loader root)]
    (call-static cl "net.minecraft.SharedConstants" "tryDetectVersion")
    (call-static cl "net.minecraft.server.Bootstrap" "bootStrap")
    (let [registry   (static-field cl "net.minecraft.world.level.block.Block" "BLOCK_STATE_REGISTRY")
          get-id     (.getMethod (class registry) "getId" (into-array Class [Object]))
          empty      (static-field cl "net.minecraft.world.level.EmptyBlockGetter" "INSTANCE")
          zero       (static-field cl "net.minecraft.core.BlockPos" "ZERO")
          getter-cls (Class/forName "net.minecraft.world.level.BlockGetter" true cl)
          pos-cls    (Class/forName "net.minecraft.core.BlockPos" true cl)
          state-cls  (Class/forName "net.minecraft.world.level.block.state.BlockBehaviour$BlockStateBase" true cl)
          shape-m    (.getMethod state-cls "getCollisionShape" (into-array Class [getter-cls pos-cls]))
          aabbs-m    (.getMethod (Class/forName "net.minecraft.world.phys.shapes.VoxelShape" true cl) "toAabbs" (make-array Class 0))
          aabb-cls   (Class/forName "net.minecraft.world.phys.AABB" true cl)
          fields     (mapv #(.getField aabb-cls %) ["minX" "minY" "minZ" "maxX" "maxY" "maxZ"])
          dir-cls    (Class/forName "net.minecraft.core.Direction" true cl)
          dirs       (vec (.invoke (.getMethod dir-cls "values" (make-array Class 0)) nil (object-array 0)))
          sturdy-m   (.getMethod state-cls "isFaceSturdy" (into-array Class [getter-cls pos-cls dir-cls]))
          states     (vec (iterator-seq (.iterator ^Iterable registry)))]
      {:shapes (into (sorted-map)
                     (for [st states
                           :let [id    (.invoke get-id registry (object-array [st]))
                                 shape (.invoke shape-m st (object-array [empty zero]))
                                 boxes (mapv (fn [a] (mapv (fn [^Field f] (sixteenth (double (.get f a)))) fields))
                                             (.invoke aabbs-m shape (object-array 0)))]
                           :when (not= boxes full-box)]
                       [id boxes]))
       :sturdy (into (sorted-map)
                     (for [st states
                           :let [id   (.invoke get-id registry (object-array [st]))
                                 mask (reduce (fn [m [i d]]
                                                (if (.invoke sturdy-m st (object-array [empty zero d]))
                                                  (bit-or (long m) (bit-shift-left 1 (long i)))
                                                  m))
                                              0 (map-indexed vector dirs))]
                           :when (not= mask 63)]
                       [id mask]))})))

(defn- blocks [reports full]
  (into (sorted-map)
        (map (fn [[name {:strs [properties states definition]}]]
               (let [first-id (apply min (map #(get % "id") states))
                     default  (or (some (fn [s] (when (get s "default") (get s "id"))) states)
                                  first-id)
                     props    (into (sorted-map)
                                    (map (fn [[p vs]] [(kw p) (mapv keyword vs)]))
                                    properties)]
                 [(kw name) (cond-> {:first first-id :default default
                                     :type (kw (get definition "type"))}
                              (contains? full (kw name)) (assoc :full-cube? true)
                              (seq props) (assoc :props props))])))
        (json/read-str (slurp (io/file reports "blocks.json")))))

(defn- registries [reports]
  (into (sorted-map)
        (map (fn [[name {:strs [entries]}]]
               [(str/replace (str name) #"^minecraft:" "")
                (into (sorted-map)
                      (map (fn [[n {:strs [protocol_id]}]] [(kw n) protocol_id]))
                      entries)]))
        (json/read-str (slurp (io/file reports "registries.json")))))

(def synchronized-registries
  ["banner_pattern" "worldgen/biome" "cat_sound_variant" "cat_variant" "chat_type"
   "chicken_sound_variant" "chicken_variant" "cow_sound_variant" "cow_variant"
   "damage_type" "dialog" "dimension_type" "enchantment" "frog_variant" "instrument"
   "jukebox_song" "painting_variant" "pig_sound_variant" "pig_variant"
   "sulfur_cube_archetype" "test_environment" "test_instance" "timeline"
   "trim_material" "trim_pattern"
   "wolf_sound_variant" "wolf_variant" "world_clock" "zombie_nautilus_variant"])

(defn- datapack-names [jar]
  (with-open [zf (ZipFile. (io/file jar))]
    (let [names (into [] (map #(.getName ^ZipEntry %))
                      (enumeration-seq (.entries zf)))]
      (into (sorted-map)
            (keep (fn [reg]
                    (let [prefix (str "data/minecraft/" reg "/")
                          es (into (sorted-set)
                                   (comp (filter #(str/starts-with? % prefix))
                                         (filter #(str/ends-with? % ".json"))
                                         (map #(subs % (count prefix)))
                                         (remove #(str/includes? % "/"))
                                         (map #(subs % 0 (- (count %) 5)))
                                         (map kw))
                                   names)]
                      (when (seq es) [reg (vec es)]))))
            synchronized-registries))))

(defn- tag-values [^ZipFile zf entry]
  (let [json (json/read-str (slurp (.getInputStream zf entry)))]
    (mapv (fn [v] (if (map? v) (get v "id") v)) (get json "values"))))

(defn- tags-of [jar registries]
  (with-open [zf (ZipFile. (io/file jar))]
    (into (sorted-map)
          (keep (fn [reg]
                  (let [prefix (str "data/minecraft/tags/" reg "/")
                        found (into {}
                                    (keep (fn [^ZipEntry e]
                                            (let [n (.getName e)]
                                              (when (and (str/starts-with? n prefix)
                                                         (str/ends-with? n ".json"))
                                                [(subs n (count prefix) (- (count n) 5))
                                                 (tag-values zf e)]))))
                                    (enumeration-seq (.entries zf)))
                        resolve* (fn resolve* [vs seen]
                                   (into []
                                         (mapcat (fn [v]
                                                   (if (str/starts-with? v "#")
                                                     (let [t (str/replace (subs v 1) #"^minecraft:" "")]
                                                       (if (seen t) [] (resolve* (get found t []) (conj seen t))))
                                                     [(kw v)])))
                                         vs))]
                    (when (seq found)
                      [reg (into (sorted-map)
                                 (map (fn [[t vs]] [t (resolve* vs #{})]))
                                 found)]))))
          registries)))
