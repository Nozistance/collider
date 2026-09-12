(ns build
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b])
  (:import (java.io File)
           (java.lang.reflect Field Method)
           (java.net URL URLClassLoader)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant)
           (java.util.zip ZipEntry ZipFile)))

(def class-dir "target/classes")
(def prim-dir "target/classes")
(def jar-file "target/collider.jar")
(def basis (b/create-basis {:project "deps.edn"}))
(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:src-dirs   ["src"]
            :class-dir  prim-dir
            :basis      basis
            :javac-opts ["-proc:none" "--release" "21"]}))

(declare block-drops block-props blocks compostables datapack-names fire-odds flt kw light-table packets recipes registries sound-types tags-of vanilla-items vanilla-shapes write-edn!)

(defn data [{:keys [dir out] :or {out "resources/mc"}}]
  (let [root (io/file (or dir (str (System/getProperty "user.home") "/Documents/MC-26.2")))
        reports (io/file root "reports")
        server (io/file root "jars" "server-plain.jar")]
    (when-not (.isDirectory reports)
      (throw (ex-info (str "no reports in " reports)
                      {:dir (str root)})))
    (println "reading" (str root))
    (let [ps (packets reports)
          {sh :shapes ol :outlines sturdy :sturdy center :sturdy-center rigid :sturdy-rigid flags :flags fire :fire lt :light bp :block-props snd :sounds compost :compost} (vanilla-shapes root)
          drops (when (.isFile server) (block-drops server))
          items (merge-with merge (vanilla-items reports) compost)
          bs (blocks reports bp (into #{} (comp (remove (fn [[_ b]] (contains? sh (get (first (filter #(get % "default") (get b "states"))) "id")))) (map (comp kw key)))
                                      (json/read-str (slurp (io/file reports "blocks.json")))))
          rs (registries reports)
          dp (when (.isFile server) (datapack-names server))
          tg (when (.isFile server)
               (tags-of server (distinct (concat (keys rs) (keys (or dp {}))))))
          rec (when (.isFile server) (recipes server tg))
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
      (write-edn! (path "outlines.edn") ol
                  (format "%d states whose outline is not a whole cube" (count ol)))
      (write-edn! (path "sturdy.edn") sturdy
                  (format "%d states with a non-sturdy face" (count sturdy)))
      (write-edn! (path "sturdy-center.edn") center
                  (format "%d states with a face that does not hold a centred block" (count center)))
      (write-edn! (path "sturdy-rigid.edn") rigid
                  (format "%d states with a face that does not hold a rigid block" (count rigid)))
      (write-edn! (path "flags.edn") flags
                  (format "%d states that block motion, ignite by lava, tick randomly, render solid or have a full top collision face"
                          (count flags)))
      (write-edn! (path "light.edn") lt
                  (format "%d dampening runs, %d emission runs, %d occlusion-shape runs over %d shapes"
                          (count (:dampening lt)) (count (:emission lt)) (count (:faces lt)) (count (:kinds lt))))
      (write-edn! (path "fire.edn") fire
                  (format "%d blocks with fire odds" (count fire)))
      (when drops
        (write-edn! (path "drops.edn") drops
                    (format "%d block loot tables, %d of them complex"
                            (count drops) (count (filter #(= :complex (val %)) drops)))))
      (write-edn! (path "sounds.edn") snd (format "%d sound types" (count snd)))
      (write-edn! (path "items.edn") items
                  (format "%d items with a non-default stack, an equipment slot or a compost chance" (count items)))
      (when rec
        (write-edn! (path "recipes.edn") rec
                    (format "%d stonecutting recipes, %d item property sets"
                            (count (:stonecutting rec)) (count (:property-sets rec)))))
      (when tg
        (write-edn! (path "tags.edn") tg
                    (format "%d registries, %d tags" (count tg) (reduce + (map count (vals tg)))))))))

(defn uber [_]
  (clean nil)
  (javac nil)
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir
               :ignores    [".*~$" "^#.*#$" "^\\.#.*" "^\\.DS_Store$" ".*\\.java$" ".*\\.clj$"]})
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  :ns-compile '[collider.core]
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"]})
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis     basis
           :main      'collider.core
           :exclude   [".*\\.java$" ".*\\.cljs$"]}))

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
  (let [cl (vanilla-loader root)
        props (delay (block-props cl))]
    (call-static cl "net.minecraft.SharedConstants" "tryDetectVersion")
    (call-static cl "net.minecraft.server.Bootstrap" "bootStrap")
    (let [registry (static-field cl "net.minecraft.world.level.block.Block" "BLOCK_STATE_REGISTRY")
          get-id (.getMethod (class registry) "getId" (into-array Class [Object]))
          empty (static-field cl "net.minecraft.world.level.EmptyBlockGetter" "INSTANCE")
          zero (static-field cl "net.minecraft.core.BlockPos" "ZERO")
          getter-cls (Class/forName "net.minecraft.world.level.BlockGetter" true cl)
          pos-cls (Class/forName "net.minecraft.core.BlockPos" true cl)
          state-cls (Class/forName "net.minecraft.world.level.block.state.BlockBehaviour$BlockStateBase" true cl)
          shape-m (.getMethod state-cls "getCollisionShape" (into-array Class [getter-cls pos-cls]))
          outline-m (.getMethod state-cls "getShape" (into-array Class [getter-cls pos-cls]))
          aabbs-m (.getMethod (Class/forName "net.minecraft.world.phys.shapes.VoxelShape" true cl) "toAabbs" (make-array Class 0))
          aabb-cls (Class/forName "net.minecraft.world.phys.AABB" true cl)
          fields (mapv #(.getField aabb-cls %) ["minX" "minY" "minZ" "maxX" "maxY" "maxZ"])
          dir-cls (Class/forName "net.minecraft.core.Direction" true cl)
          dirs (vec (.invoke (.getMethod dir-cls "values" (make-array Class 0)) nil (object-array 0)))
          sturdy-m (.getMethod state-cls "isFaceSturdy" (into-array Class [getter-cls pos-cls dir-cls]))
          support-cls (Class/forName "net.minecraft.world.level.block.SupportType" true cl)
          center (static-field cl "net.minecraft.world.level.block.SupportType" "CENTER")
          rigid (static-field cl "net.minecraft.world.level.block.SupportType" "RIGID")
          center-m (.getMethod state-cls "isFaceSturdy" (into-array Class [getter-cls pos-cls dir-cls support-cls]))
          block-cls (Class/forName "net.minecraft.world.level.block.Block" true cl)
          shape-cls (Class/forName "net.minecraft.world.phys.shapes.VoxelShape" true cl)
          face-full-m (.getMethod block-cls "isFaceFull" (into-array Class [shape-cls dir-cls]))
          up (static-field cl "net.minecraft.core.Direction" "UP")
          flag-ms (mapv #(.getMethod state-cls % (make-array Class 0))
                        ["blocksMotion" "ignitedByLava" "isRandomlyTicking" "isSolidRender"])
          states (vec (iterator-seq (.iterator ^Iterable registry)))]
      {:shapes        (into (sorted-map)
                            (for [st states
                                  :let [id (.invoke get-id registry (object-array [st]))
                                        shape (.invoke shape-m st (object-array [empty zero]))
                                        boxes (mapv (fn [a] (mapv (fn [^Field f] (sixteenth (double (.get f a)))) fields))
                                                    (.invoke aabbs-m shape (object-array 0)))]
                                  :when (not= boxes full-box)]
                              [id boxes]))
       :outlines      (into (sorted-map)
                            (for [st states
                                  :let [id (.invoke get-id registry (object-array [st]))
                                        shape (.invoke outline-m st (object-array [empty zero]))
                                        boxes (mapv (fn [a] (mapv (fn [^Field f] (sixteenth (double (.get f a)))) fields))
                                                    (.invoke aabbs-m shape (object-array 0)))]
                                  :when (not= boxes full-box)]
                              [id boxes]))
       :flags         (into (sorted-map)
                            (for [st states
                                  :let [id (.invoke get-id registry (object-array [st]))
                                        bits (reduce (fn [m [i ^Method f]]
                                                       (if (.invoke f st (object-array 0))
                                                         (bit-or (long m) (bit-shift-left 1 (long i)))
                                                         m))
                                                     0 (map-indexed vector flag-ms))
                                        shape (.invoke shape-m st (object-array [empty zero]))
                                        mask (if (.invoke face-full-m nil (object-array [shape up]))
                                               (bit-or (long bits) 16)
                                               bits)]
                                  :when (pos? (long mask))]
                              [id mask]))
       :light         (light-table cl)
       :fire          (fire-odds cl)
       :block-props   (:props @props)
       :sounds        (:sounds @props)
       :compost       (compostables cl)
       :sturdy-center (into (sorted-map)
                            (for [st states
                                  :let [id (.invoke get-id registry (object-array [st]))
                                        mask (reduce (fn [m [i d]]
                                                       (if (.invoke center-m st (object-array [empty zero d center]))
                                                         (bit-or (long m) (bit-shift-left 1 (long i)))
                                                         m))
                                                     0 (map-indexed vector dirs))]
                                  :when (not= mask 63)]
                              [id mask]))
       :sturdy-rigid  (into (sorted-map)
                            (for [st states
                                  :let [id (.invoke get-id registry (object-array [st]))
                                        mask (reduce (fn [m [i d]]
                                                       (if (.invoke center-m st (object-array [empty zero d rigid]))
                                                         (bit-or (long m) (bit-shift-left 1 (long i)))
                                                         m))
                                                     0 (map-indexed vector dirs))]
                                  :when (not= mask 63)]
                              [id mask]))
       :sturdy        (into (sorted-map)
                            (for [st states
                                  :let [id (.invoke get-id registry (object-array [st]))
                                        mask (reduce (fn [m [i d]]
                                                       (if (.invoke sturdy-m st (object-array [empty zero d]))
                                                         (bit-or (long m) (bit-shift-left 1 (long i)))
                                                         m))
                                                     0 (map-indexed vector dirs))]
                                  :when (not= mask 63)]
                              [id mask]))})))


(defn- flt ^double [v]
  (Double/parseDouble (Float/toString (float v))))

(def ^:private sound-parts {:break "Break" :step "Step" :place "Place" :hit "Hit" :fall "Fall"})

(defn- sound-types [^ClassLoader cl]
  (let [cls (Class/forName "net.minecraft.world.level.block.SoundType" true cl)
        loc-m (.getMethod (Class/forName "net.minecraft.sounds.SoundEvent" true cl) "location" (make-array Class 0))
        getters (into (sorted-map)
                      (map (fn [[k n]] [k (.getMethod cls (str "get" n "Sound") (make-array Class 0))]))
                      sound-parts)]
    (into []
          (comp (filter (fn [^Field f] (= cls (.getType f))))
                (map (fn [^Field f]
                       (let [o (.get f nil)]
                         [(kw (str/lower-case (.getName f))) o
                          (into (sorted-map)
                                (map (fn [[k ^Method m]]
                                       [k (kw (str (.invoke loc-m (.invoke m o (object-array 0)) (object-array 0))))]))
                                getters)]))))
          (.getFields cls))))

(defn- field-of ^Field [^Class want ^Class from]
  (when-let [f (->> (take-while some? (iterate (fn [^Class c] (.getSuperclass c)) from))
                    (mapcat (fn [^Class c] (sort-by (fn [^Field f] (.getName f)) (.getDeclaredFields c))))
                    (filter (fn [^Field f] (= want (.getType f))))
                    first)]
    (doto ^Field f (.setAccessible true))))

(defn- block-props [^ClassLoader cl]
  (let [types (sound-types cl)
        by-type (into {} (map (fn [[k o _]] [o k])) types)
        reg (static-field cl "net.minecraft.core.registries.BuiltInRegistries" "BLOCK")
        key-m (.getMethod (class reg) "getKey" (into-array Class [Object]))
        behave (Class/forName "net.minecraft.world.level.block.state.BlockBehaviour" true cl)
        resist-f (doto (.getDeclaredField behave "explosionResistance") (.setAccessible true))
        sound-f (doto (.getDeclaredField behave "soundType") (.setAccessible true))
        set-cls (Class/forName "net.minecraft.world.level.block.state.properties.BlockSetType" true cl)
        wood-cls (Class/forName "net.minecraft.world.level.block.state.properties.WoodType" true cl)
        loc-m (.getMethod (Class/forName "net.minecraft.sounds.SoundEvent" true cl) "location" (make-array Class 0))
        hand-m (.getMethod set-cls "canOpenByHand" (make-array Class 0))
        event (fn [^Class c n o] (kw (str (.invoke loc-m (.invoke (.getMethod c n (make-array Class 0)) o (object-array 0)) (object-array 0)))))
        kinds [[(Class/forName "net.minecraft.world.level.block.DoorBlock" true cl) set-cls "doorOpen" "doorClose" true]
               [(Class/forName "net.minecraft.world.level.block.TrapDoorBlock" true cl) set-cls "trapdoorOpen" "trapdoorClose" true]
               [(Class/forName "net.minecraft.world.level.block.FenceGateBlock" true cl) wood-cls "fenceGateOpen" "fenceGateClose" false]]
        toggle (fn [b] (some (fn [[^Class bc want open close hand?]]
                               (when-let [f (and (.isInstance bc b) (field-of want (class b)))]
                                 (let [t (.get ^Field f b)]
                                   (cond-> {:open (event want open t) :close (event want close t)}
                                           hand? (assoc :hand? (boolean (.invoke hand-m t (object-array 0))))))))
                             kinds))]
    {:props  (into (sorted-map)
                   (for [b (iterator-seq (.iterator ^Iterable reg))]
                     [(kw (str (.invoke key-m reg (object-array [b]))))
                      (merge {:resistance (flt (.get resist-f b)) :sound (get by-type (.get sound-f b))}
                             (toggle b))]))
     :sounds (into (sorted-map) (map (fn [[k _ evs]] [k evs])) types)}))

(defn- compostables [^ClassLoader cl]
  (let [reg (static-field cl "net.minecraft.core.registries.BuiltInRegistries" "ITEM")
        key-m (.getMethod (class reg) "getKey" (into-array Class [Object]))]
    (into (sorted-map)
          (map (fn [e] [(kw (str (.invoke key-m reg (object-array [(key e)])))) {:compost (flt (val e))}]))
          (static-field cl "net.minecraft.world.level.block.ComposterBlock" "COMPOSTABLES"))))

(def ^:private face-axis [1 1 2 2 0 0])
(def ^:private dir-names [:down :up :north :south :west :east])

(defn- project-face [^long axis box]
  (let [[x0 y0 z0 x1 y1 z1] box]
    (case axis
      0 [y0 z0 y1 z1]
      1 [x0 z0 x1 z1]
      2 [x0 y0 x1 y1])))

(defn- shape-boxes [aabbs-m fields shape]
  (mapv (fn [a] (mapv (fn [^Field f] (sixteenth (double (.get f a)))) fields))
        (.invoke aabbs-m shape (object-array 0))))

(defn- face-entry [aabbs-m fields block-shape d shape]
  (let [boxes (shape-boxes aabbs-m fields shape)]
    (cond
      (identical? shape block-shape) :full
      (empty? boxes) nil
      :else (let [ps (mapv #(project-face (long (face-axis d)) %) boxes)]
              (if (= ps [[0 0 16 16]]) :full ps)))))

(defn- runs [pairs]
  (->> (sort-by first pairs)
       (reduce (fn [acc [id v]]
                 (let [[lo hi pv] (peek acc)]
                   (if (and lo (= v pv) (= (long id) (inc (long hi))))
                     (conj (pop acc) [lo id v])
                     (conj acc [id id v]))))
               [])
       vec))

(defn- flag-runs [ids]
  (mapv (fn [[lo hi _]] (if (= lo hi) lo [lo hi])) (runs (map (fn [id] [id true]) ids))))

(defn- value-runs [pairs]
  (mapv (fn [[lo hi v]] (if (= lo hi) [lo v] [lo hi v])) (runs pairs)))

(defn- light-table [^ClassLoader cl]
  (let [registry (static-field cl "net.minecraft.world.level.block.Block" "BLOCK_STATE_REGISTRY")
        get-id (.getMethod (class registry) "getId" (into-array Class [Object]))
        state-cls (Class/forName "net.minecraft.world.level.block.state.BlockBehaviour$BlockStateBase" true cl)
        dir-cls (Class/forName "net.minecraft.core.Direction" true cl)
        dirs (vec (.invoke (.getMethod dir-cls "values" (make-array Class 0)) nil (object-array 0)))
        shape-cls (Class/forName "net.minecraft.world.phys.shapes.VoxelShape" true cl)
        aabbs-m (.getMethod shape-cls "toAabbs" (make-array Class 0))
        aabb-cls (Class/forName "net.minecraft.world.phys.AABB" true cl)
        fields (mapv #(.getField aabb-cls %) ["minX" "minY" "minZ" "maxX" "maxY" "maxZ"])
        block-shape (.invoke (.getMethod (Class/forName "net.minecraft.world.phys.shapes.Shapes" true cl)
                                         "block" (make-array Class 0))
                             nil (object-array 0))
        call (fn [^Method m st] (.invoke m st (object-array 0)))
        emit-m (.getMethod state-cls "getLightEmission" (make-array Class 0))
        damp-m (.getMethod state-cls "getLightDampening" (make-array Class 0))
        occl-m (.getMethod state-cls "canOcclude" (make-array Class 0))
        use-m (.getMethod state-cls "useShapeForLightOcclusion" (make-array Class 0))
        oshape-m (.getMethod state-cls "getOcclusionShape" (make-array Class 0))
        fshape-m (.getMethod state-cls "getFaceOcclusionShape" (into-array Class [dir-cls]))
        states (vec (iterator-seq (.iterator ^Iterable registry)))
        ided (mapv (fn [st] [(.invoke get-id registry (object-array [st])) st]) states)
        faces-of (fn [st]
                   (into (sorted-map)
                         (keep (fn [d]
                                 (when-let [e (face-entry aabbs-m fields block-shape d
                                                          (.invoke fshape-m st (object-array [(dirs d)])))]
                                   [(dir-names d) e])))
                         (range 6)))
        shaped (for [[id st] ided
                     :when (and (call occl-m st) (call use-m st))
                     :let [k {:shape (shape-boxes aabbs-m fields (call oshape-m st))
                              :faces (faces-of st)}]
                     :when (seq (:faces k))]
                 [id k])
        kinds (vec (sort-by pr-str (distinct (map second shaped))))
        index (into {} (map-indexed (fn [i k] [k i])) kinds)]
    {:dampening (value-runs (for [[id st] ided :let [v (call damp-m st)] :when (not= 15 (long v))] [id v]))
     :emission  (value-runs (for [[id st] ided :let [v (call emit-m st)] :when (pos? (long v))] [id v]))
     :occludes  (flag-runs (for [[id st] ided :when (call occl-m st)] id))
     :use-shape (flag-runs (for [[id st] ided :when (call use-m st)] id))
     :kinds     kinds
     :faces     (value-runs (for [[id k] shaped] [id (index k)]))}))

(defn- fire-odds [^ClassLoader cl]
  (let [fire (static-field cl "net.minecraft.world.level.block.Blocks" "FIRE")
        fire-cls (Class/forName "net.minecraft.world.level.block.FireBlock" true cl)
        reg (static-field cl "net.minecraft.core.registries.BuiltInRegistries" "BLOCK")
        key-m (.getMethod (class reg) "getKey" (into-array Class [Object]))
        name-of (fn [b] (let [k (.invoke key-m reg (object-array [b]))]
                          (kw (.invoke (.getMethod (class k) "getPath" (make-array Class 0)) k (object-array 0)))))
        table (fn [n] (let [f (doto (.getDeclaredField fire-cls n) (.setAccessible true))]
                        (into {} (map (fn [[b v]] [(name-of b) v])) (.get f fire))))]
    (merge-with merge
                (into (sorted-map) (map (fn [[k v]] [k {:ignite v}])) (table "igniteOdds"))
                (into (sorted-map) (map (fn [[k v]] [k {:burn v}])) (table "burnOdds")))))

(defn- loot-number [v]
  (cond
    (number? v) [(long v) (long v)]
    (map? v) [(long (get v "min" 1)) (long (get v "max" 1))]
    :else [1 1]))

(defn- loot-condition [c]
  (case (get c "condition")
    "minecraft:survives_explosion" {}
    "minecraft:random_chance" {:chance (double (get c "chance"))}
    "minecraft:table_bonus" {:chance (double (first (get c "chances")))}
    "minecraft:block_state_property" {:props (into {} (map (fn [[k v]] [(kw k) (keyword v)])) (get c "properties"))}
    "minecraft:entity_properties" {:entity? true}
    "minecraft:match_tool" :skip
    "minecraft:inverted" (let [r (loot-condition (get c "term"))] (if (= :skip r) {} :unknown))
    "minecraft:any_of" (if (every? #(= :skip (loot-condition %)) (get c "terms")) :skip :unknown)
    :unknown))

(defn- loot-conditions [cs]
  (reduce (fn [acc c]
            (let [r (loot-condition c)]
              (if (keyword? r) (reduced r) (merge acc r))))
          {} cs))

(defn- loot-entry [e]
  (case (get e "type")
    "minecraft:item"
    (let [cs (loot-conditions (get e "conditions"))]
      (if (keyword? cs)
        cs
        (let [count (some (fn [f] (when (= "minecraft:set_count" (get f "function")) (loot-number (get f "count"))))
                          (get e "functions"))]
          [(cond-> (assoc cs :item (kw (subs (get e "name") 10)))
                   count (assoc :count count))])))
    "minecraft:alternatives"
    (let [cs (loot-conditions (get e "conditions"))]
      (if (keyword? cs)
        cs
        (reduce (fn [acc child]
                  (let [r (loot-entry child)]
                    (cond
                      (= r :skip) acc
                      (keyword? r) (reduced r)
                      :else (into acc (map #(merge cs %)) r))))
                [] (get e "children"))))
    :unknown))

(defn- loot-pool [p]
  (let [cs (loot-conditions (get p "conditions"))
        rolls (loot-number (get p "rolls" 1))]
    (if (keyword? cs)
      cs
      (let [entries (reduce (fn [acc e]
                              (let [r (loot-entry e)]
                                (cond (= r :skip) acc
                                      (keyword? r) (reduced r)
                                      :else (into acc r))))
                            [] (get p "entries"))]
        (if (keyword? entries)
          entries
          (mapv #(cond-> (merge cs %) (not= rolls [1 1]) (assoc :rolls rolls)) entries))))))

(defn- loot-table [json]
  (let [pools (get json "pools")]
    (cond
      (empty? pools) []
      :else (let [rs (map loot-pool pools)]
              (if (some #{:unknown} rs)
                :complex
                (into [] (mapcat #(if (= % :skip) [] %)) rs))))))

(defn- block-drops [jar]
  (with-open [zf (ZipFile. (io/file jar))]
    (let [prefix "data/minecraft/loot_table/blocks/"]
      (into (sorted-map)
            (keep (fn [^ZipEntry e]
                    (let [n (.getName e)]
                      (when (and (str/starts-with? n prefix) (str/ends-with? n ".json"))
                        (let [table (loot-table (json/read-str (slurp (.getInputStream zf e))))]
                          (when (not= table [])
                            [(kw (subs n (count prefix) (- (count n) 5))) table]))))))
            (enumeration-seq (.entries zf))))))

(defn- vanilla-items [reports]
  (into (sorted-map)
        (for [^File f (sort (.listFiles (io/file reports "minecraft" "components" "item")))
              :when (str/ends-with? (.getName f) ".json")
              :let [cs (get (json/read-str (slurp f)) "components")
                    n (get cs "minecraft:max_stack_size" 64)
                    slot (get-in cs ["minecraft:equippable" "slot"])
                    song (get cs "minecraft:jukebox_playable")
                    dye (get cs "minecraft:dye")
                    pat (get cs "minecraft:provides_banner_patterns")
                    m (cond-> (sorted-map) (not= n 64) (assoc :max-stack n) slot (assoc :equip (kw slot))
                              song (assoc :jukebox-song (kw song))
                              dye (assoc :dye (kw dye))
                              (string? pat) (assoc :patterns (str/replace (subs pat 1) #"^minecraft:" "")))]
              :when (seq m)]
          [(kw (str/replace (.getName f) #"\.json$" "")) m])))

(defn- blocks [reports extra full]
  (into (sorted-map)
        (map (fn [[name {:strs [properties states definition]}]]
               (let [first-id (apply min (map #(get % "id") states))
                     default (or (some (fn [s] (when (get s "default") (get s "id"))) states)
                                 first-id)
                     props (into (sorted-map)
                                 (map (fn [[p vs]] [(kw p) (mapv keyword vs)]))
                                 properties)]
                 [(kw name) (cond-> (into (sorted-map)
                                          (merge {:first first-id :default default
                                                  :type  (kw (get definition "type"))}
                                                 (get extra (kw name))))
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

(defn- ingredient [v]
  (cond
    (string? v) (if (str/starts-with? v "#")
                  {:tag (str/replace (subs v 1) #"^minecraft:" "")}
                  [(kw v)])
    (sequential? v) (mapv kw v)
    :else (throw (ex-info "unknown ingredient" {:value v}))))

(def ^:private property-sets
  "RecipeManager.RECIPE_PROPERTY_SETS: recipe type -> the ingredient field the
   client is told about."
  {"furnace_input"       [#{"minecraft:smelting"} "ingredient"]
   "blast_furnace_input" [#{"minecraft:blasting"} "ingredient"]
   "smoker_input"        [#{"minecraft:smoking"} "ingredient"]
   "campfire_input"      [#{"minecraft:campfire_cooking"} "ingredient"]
   "smithing_base"       [#{"minecraft:smithing_transform" "minecraft:smithing_trim"} "base"]
   "smithing_template"   [#{"minecraft:smithing_transform" "minecraft:smithing_trim"} "template"]
   "smithing_addition"   [#{"minecraft:smithing_transform" "minecraft:smithing_trim"} "addition"]})

(defn- recipe-entries [^ZipFile zf]
  (let [prefix "data/minecraft/recipe/"]
    (sort-by key
             (into {}
                   (keep (fn [^ZipEntry e]
                           (let [n (.getName e)]
                             (when (and (str/starts-with? n prefix) (str/ends-with? n ".json")
                                        (not (str/includes? (subs n (count prefix)) "/")))
                               [(subs n (count prefix) (- (count n) 5))
                                (json/read-str (slurp (.getInputStream zf e)))]))))
                   (enumeration-seq (.entries zf))))))

(defn- stonecutting
  [entries]
  (into []
        (keep (fn [[_ json]]
                (when (= "minecraft:stonecutting" (get json "type"))
                  (let [r (get json "result")
                        r (if (string? r) {"id" r} r)
                        n (get r "count" 1)]
                    {:in  (ingredient (get json "ingredient"))
                     :out (cond-> {:item (kw (get r "id"))} (not= 1 n) (assoc :count n))}))))
        entries))

(defn- ingredient-items [tags v]
  (let [i (ingredient v)]
    (if (map? i) (get-in tags ["item" (:tag i)] []) i)))

(defn- property-set [tags entries [types field]]
  (into (sorted-set)
        (mapcat (fn [[_ json]]
                  (when (and (contains? types (get json "type")) (contains? json field))
                    (ingredient-items tags (get json field)))))
        entries))

(defn- recipes
  [jar tags]
  (with-open [zf (ZipFile. (io/file jar))]
    (let [entries (recipe-entries zf)]
      {:stonecutting  (stonecutting entries)
       :property-sets (into (sorted-map)
                            (map (fn [[k spec]] [k (vec (property-set tags entries spec))]))
                            property-sets)})))

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

(defn- pascal [k]
  (apply str (map str/capitalize (str/split (name k) #"-"))))

(defn- hook-status [mark]
  (let [m (str/trim (or mark ""))]
    (cond (str/starts-with? m "x") "done"
          (str/starts-with? m "pending") "pending"
          :else "open")))

(defn- parse-hooks [text]
  (let [sections {"Entities" "entities" "Blocks" "blocks"}]
    (loop [lines (str/split-lines text) cat nil cls nil acc {}]
      (if-let [l (first lines)]
        (cond
          (str/starts-with? l "## ") (recur (rest lines) (get sections (subs l 3)) nil acc)
          (and cat (str/starts-with? l "### "))
          (let [[_ c] (re-matches #"### (\S+) - `([^`]+)`" l)]
            (recur (rest lines) cat c (assoc-in acc [cat c] {:hooks []})))
          (and cat cls (str/starts-with? l "- "))
          (let [[_ h owner mark] (re-matches #"- (\S+) \(([^)]*)\) \|(.*)" l)]
            (recur (rest lines) cat cls
                   (cond-> acc h (update-in [cat cls :hooks] conj {:name h :owner owner :status (hook-status mark)}))))
          :else (recur (rest lines) cat cls acc))
        acc))))

(defn- excluded-counts [text]
  (let [section (fn [head]
                  (->> (str/split-lines text)
                       (drop-while #(not= % (str "## Excluded as " head)))
                       rest
                       (take-while #(not (str/starts-with? % "## ")))
                       (filter #(re-matches #"\| [^|]+ \| [^|]+ \|" %))
                       count
                       (max 0)))]
    {:client-only      (- (section "client-only") 1)
     :developer-compat (- (section "developer compatibility") 1)}))

(def ^:private type-classes
  {:jack-o-lantern "CarvedPumpkinBlock" :enchantment-table "EnchantingTableBlock"})

(defn- letters [s] (str/lower-case (str/replace (str s) #"[^A-Za-z]" "")))
(defn- block-entries [classes]
  (let [blocks (edn/read-string (slurp "resources/mc/blocks.edn"))
        by-letters (into {} (map (fn [c] [(letters c) c])) classes)]
    (reduce (fn [m [b info]]
              (let [t (:type info)
                    c (or (type-classes t)
                          (get by-letters (letters (str (name t) "block")))
                          (get by-letters (letters t))
                          (str (pascal t) "Block"))]
                (update m c (fnil conj []) (name b))))
            {} (sort-by key blocks))))

(defn- with-entries [blocks]
  (let [entries (block-entries (set (keys blocks)))]
    (reduce (fn [m [c es]] (update m c (fn [v] (assoc (or v {:hooks []}) :entries (vec (sort es)))))) blocks entries)))

(defn tracker [{:keys [hooks out] :or {hooks "../exclude/hooks.md" out "parity/implementation.json"}}]
  (let [parsed (parse-hooks (slurp hooks))
        data {:meta     {:generated (str (Instant/now))
                         :commit    (str/trim (b/git-process {:git-args "rev-parse HEAD"}))
                         :target    "26.2"}
              :blocks   (with-entries (get parsed "blocks" {}))
              :entities (get parsed "entities" {})}
        excluded (excluded-counts (slurp hooks))
        total (fn [cat] (let [hs (mapcat :hooks (vals (get data cat)))]
                          [(count (filter #(= "done" (:status %)) hs))
                           (count (filter #(= "pending" (:status %)) hs))
                           (count hs)]))]
    (io/make-parents out)
    (spit out (json/write-str data))
    (let [tally (fn [cat] (let [hs (mapcat :hooks (vals (get data cat)))
                                done (count (filter #(= "done" (:status %)) hs))
                                pending (count (filter #(= "pending" (:status %)) hs))]
                            {:classes (count (get data cat)) :hooks (count hs) :done done
                             :pending pending
                             :pct     (if (seq hs) (Math/round (* 100.0 (/ done (count hs)))) 0)}))
          b (tally :blocks) e (tally :entities)
          sum (fn [k] (+ (long (k b)) (long (k e))))
          all {:classes (sum :classes) :hooks (sum :hooks) :done (sum :done)
               :pending (sum :pending)
               :pct     (if (pos? (sum :hooks)) (Math/round (* 100.0 (/ (sum :done) (sum :hooks)))) 0)}]
      (spit (str (.getParent (io/file out)) "/summary.json")
            (json/write-str (assoc (:meta data) :all all :blocks b :entities e :excluded excluded)))
      (println (format "  %s/summary.json: %d%% overall, %d%% in blocks"
                       (.getParent (io/file out)) (:pct all) (:pct b))))
    (println (format "  %s: blocks %d classes, hooks done/pending/all %s; entities %d classes, %s"
                     out (count (:blocks data)) (total :blocks) (count (:entities data)) (total :entities)))))
