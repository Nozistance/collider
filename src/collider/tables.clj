(ns collider.tables
  "Generating the game data tables from the vanilla server."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [collider.data :as data])
  (:import (clojure.lang ExceptionInfo Reflector)
           (java.io File Writer)
           (java.lang.reflect Field)
           (java.net HttpURLConnection URL URLClassLoader)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.security MessageDigest)
           (java.util HexFormat)
           (java.util.zip ZipEntry ZipFile)))

(set! *warn-on-reflection* true)

(def version data/game)

(def manifest-url
  "https://piston-meta.mojang.com/mc/game/version_manifest_v2.json")

(def ^:dynamic *progress* (fn [_] nil))

(defn- progress! [m]
  (*progress* m))

(defn- timed [step f]
  (progress! {:event :begin :step step})
  (let [t (System/nanoTime)
        v (f)]
    (progress! (assoc v :event :end :step step
                      :took (- (System/nanoTime) t)))
    v))

(defn cache-dir ^File [version]
  (io/file "data" version))

(defn- sha1 [^File f]
  (let [md (MessageDigest/getInstance "SHA-1")
        buf (byte-array 65536)]
    (with-open [in (io/input-stream f)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (.update md buf 0 n)
            (recur)))))
    (.formatHex (HexFormat/of) (.digest md))))

(def ^:private timeout-ms 15000)

(defn- open-url ^java.io.InputStream [url]
  (let [c (.openConnection (URL. (str url)))]
    (.setConnectTimeout c timeout-ms)
    (.setReadTimeout c timeout-ms)
    (.getInputStream c)))

(defn- unreachable [url e]
  (let [why (str "The exception was: "
                 (.getSimpleName (class e)) ": "
                 (.getMessage ^Throwable e))
        cmd (str "Download server.jar " version
                 " yourself and start with"
                 " '{:jar \"path/to/server.jar\"}'")]
    (ex-info (str "cannot reach " (.getHost (URL. (str url))))
             {:step :jar
              :what "failed to reach Mojang"
              :why why :command cmd}
             e)))

(defn- fetch-json [url]
  (try (with-open [in (open-url url)]
         (json/read-str (slurp in)))
       (catch Exception e
         (throw (unreachable url e)))))

(defn- temp-dir ^File [name]
  (let [attrs (make-array FileAttribute 0)]
    (.toFile (Files/createTempDirectory name attrs))))

(defn- delete-tree! [^File dir]
  (doseq [^File f (reverse (file-seq dir))]
    (.delete f)))

(defn- copy-tree! [^File from ^File to]
  (doseq [^File f (file-seq from) :when (.isFile f)]
    (let [rel (subs (.getPath f) (inc (count (.getPath from))))
          t (io/file to rel)]
      (io/make-parents t)
      (io/copy f t))))

(defn- corrupt [url want got]
  (let [why (str "Its checksum does not match the one"
                 " Mojang published")
        cmd (str "Delete " (cache-dir version) " and start again")]
    (ex-info "sha1 mismatch"
             {:step :jar
              :what "server.jar is corrupt"
              :why why :command cmd
              :url url :want want :got got})))

(defn- download! [url ^File to want]
  (io/make-parents to)
  (with-open [in (open-url url)]
    (io/copy in to))
  (let [got (sha1 to)]
    (when (not= got want)
      (.delete to)
      (throw (corrupt url want got)))))

(defn- server-download [version]
  (let [versions (get (fetch-json manifest-url) "versions")
        entry (some #(when (= version (get % "id")) %) versions)]
    (when-not entry
      (throw (ex-info (str "no version " version)
                      {:manifest manifest-url})))
    (get-in (fetch-json (get entry "url")) ["downloads" "server"])))

(defn- fetch! [version ^File jar]
  (let [{:strs [url size sha1]} (server-download version)
        part (io/file (cache-dir version) "server.jar.part")]
    (download! url part sha1)
    (.renameTo part jar)
    {:source :mojang :bytes size}))

(defn- no-such-jar [local]
  (let [cmd (str "Check :jar, or leave it out to download"
                 " server.jar from Mojang")]
    (ex-info (str "no jar at " local)
             {:step :jar
              :what "no such jar"
              :why (str "There is no file at " local)
              :command cmd})))

(defn- copy-local! [local ^File jar]
  (when-not (.isFile (io/file local))
    (throw (no-such-jar local)))
  (io/make-parents jar)
  (io/copy (io/file local) jar)
  {:source :local :path (str local)})

(defn- fetch-into [version local ^File jar]
  (cond
    (.isFile jar) {:source :cached :path (str (.getParentFile jar))}
    local (copy-local! local jar)
    :else (fetch! version jar)))

(defn fetch
  (^File [version] (fetch version nil))
  (^File [version local]
   (let [jar (io/file (cache-dir version) "server.jar")]
     (timed :jar #(fetch-into version local jar))
     jar)))

(defn- zip-names [^ZipFile zf]
  (map #(.getName ^ZipEntry %) (enumeration-seq (.entries zf))))

(defn- unzip ^File [^ZipFile zf name ^File to]
  (io/make-parents to)
  (with-open [in (.getInputStream zf (.getEntry zf name))]
    (io/copy in to))
  to)

(defn- inner-jar ^File [^File bundle]
  (let [out (io/file (.getParentFile bundle) "server-plain.jar")]
    (when-not (.isFile out)
      (with-open [zf (ZipFile. bundle)]
        (->> (zip-names zf)
             (filter #(re-matches #"META-INF/versions/.*\.jar" %))
             first
             (#(unzip zf % out)))))
    out))

(defn- generator-command ^String/1 [^File bundle]
  (let [java (str (System/getProperty "java.home") "/bin/java")]
    (into-array String [java
                        "-DbundlerMainClass=net.minecraft.data.Main"
                        "-jar" (.getAbsolutePath bundle)
                        "--reports"])))

(defn- file-count ^long [^File dir]
  (count (filter #(.isFile ^File %) (file-seq dir))))

(defn- generator-failed [^File bundle ^File log]
  (ex-info "the data generator failed"
           {:step :reports
            :what "data generator failed"
            :why  (str "Its output is in " log)
            :jar  (str bundle)}))

(defn- run-generator! [^File bundle ^File dir]
  (let [work (temp-dir "reports")
        log (io/file (.getParentFile bundle) "reports.log")
        p (-> (ProcessBuilder. (generator-command bundle))
              (.directory work)
              (.redirectErrorStream true)
              (.redirectOutput log)
              (.start))]
    (when-not (zero? (.waitFor p))
      (throw (generator-failed bundle log)))
    (copy-tree! (io/file work "generated" "reports") dir)
    {:files (file-count dir)}))

(defn- reports ^File [^File bundle]
  (let [dir (io/file (.getParentFile bundle) "reports")]
    (if (.isFile (io/file dir "blocks.json"))
      (timed :reports (fn [] {:files (file-count dir)}))
      (timed :reports #(run-generator! bundle dir)))
    dir))

(defn- unpack-libraries [^File bundle ^File tmp]
  (with-open [zf (ZipFile. bundle)]
    (doall
      (for [n (zip-names zf)
            :when (re-matches #"META-INF/libraries/.*\.jar" n)
            :let [f (io/file tmp (str/replace n "/" "_"))]]
        (unzip zf n f)))))

(defn- class-loader ^ClassLoader [jars ^File server]
  (let [urls (map #(.toURL (.toURI ^File %)) (cons server jars))]
    (URLClassLoader. (into-array URL urls)
                     (ClassLoader/getPlatformClassLoader))))

(def ^:dynamic ^ClassLoader *loader*)

(defn- cls ^Class [name]
  (Class/forName (str "net.minecraft." name) true *loader*))

(defn- call [obj m & args]
  (Reflector/invokeInstanceMethod obj m (object-array args)))

(defn- call-static [c m & args]
  (^[Class String Object/1] Reflector/invokeStaticMethod
    (cls c) m (object-array args)))

(defn- static-field [c f]
  (^[Class String] Reflector/getStaticField (cls c) f))

(defn- field-value [obj f]
  (Reflector/getInstanceField obj f))

(defn- field-name [^Field f] (.getName f))

(defn- declared-fields [^Class c]
  (->> (iterate #(.getSuperclass ^Class %) c)
       (take-while some?)
       (mapcat #(sort-by field-name (.getDeclaredFields ^Class %)))))

(defn- hidden-field [^Class c obj want]
  (let [match? (if (string? want)
                 #(= want (.getName ^Field %))
                 #(= want (.getType ^Field %)))]
    (when-let [^Field f (first (filter match? (declared-fields c)))]
      (.get (doto f (.setAccessible true)) obj))))

(defn- kw [s]
  (-> (str s)
      (str/replace #"^minecraft:" "")
      (str/replace "_" "-")
      keyword))

(defn- key-of [reg x] (kw (str (call reg "getKey" x))))

(defn- elements [reg] (iterator-seq (.iterator ^Iterable reg)))

(defn- flt ^double [v]
  (Double/parseDouble (Float/toString (float v))))

(defn- registry [name]
  (static-field "core.registries.BuiltInRegistries" name))

(defn- sixteenth [^double v]
  (let [x (* 16.0 v)]
    (if (== x (Math/rint x)) (long x) x)))

(def ^:private box-fields
  ["minX" "minY" "minZ" "maxX" "maxY" "maxZ"])

(defn- boxes [shape]
  (mapv (fn [a]
          (mapv #(sixteenth (double (field-value a %))) box-fields))
        (call shape "toAabbs")))

(defn- mask ^long [bits]
  (reduce (fn [m [i b]]
            (if b (bit-or (long m) (bit-shift-left 1 (long i))) m))
          0 (map-indexed vector bits)))

(defn- per-state [states f]
  (into (sorted-map)
        (keep (fn [[id st]] (when-some [v (f st)] [id v])))
        states))

(defn- sorted-vals [m f]
  (into (sorted-map) (map (fn [[k v]] [k (f v)])) m))

(defn- unless-default [x v] (when (not= x v) v))

(def ^:private full-box [[0 0 0 16 16 16]])

(def ^:private dir-names [:down :up :north :south :west :east])

(def ^:private face-axis [1 1 2 2 0 0])

(defn- block-states []
  (let [block-cls "world.level.block.Block"
        reg (static-field block-cls "BLOCK_STATE_REGISTRY")]
    (mapv (fn [st] [(call reg "getId" st) st]) (elements reg))))

(def ^:private flag-methods
  ["blocksMotion" "ignitedByLava"
   "isRandomlyTicking" "isSolidRender"])

(defn- shape-env []
  {:air (static-field "world.level.EmptyBlockGetter" "INSTANCE")
   :zero (static-field "core.BlockPos" "ZERO")
   :dirs (vec (call-static "core.Direction" "values"))
   :up (static-field "core.Direction" "UP")
   :center (static-field "world.level.block.SupportType" "CENTER")
   :rigid (static-field "world.level.block.SupportType" "RIGID")})

(defn- collision-shape [{:keys [air zero]} st]
  (call st "getCollisionShape" air zero))

(defn- outline-shape [{:keys [air zero]} st]
  (call st "getShape" air zero))

(defn- partial-box [shape]
  (unless-default full-box (boxes shape)))

(defn- full-top? [{:keys [up] :as env} st]
  (call-static "world.level.block.Block" "isFaceFull"
               (collision-shape env st) up))

(defn- state-flags [env st]
  (let [flags (conj (mapv #(call st %) flag-methods)
                    (full-top? env st))]
    (unless-default 0 (mask flags))))

(defn- state-sturdy [{:keys [air zero dirs]} st & more]
  (let [faces (for [d dirs]
                (apply call st "isFaceSturdy" air zero d more))]
    (unless-default 63 (mask faces))))

(defn- state-shapes [states]
  (let [env (shape-env)
        each (fn [f] (per-state states f))]
    {:shapes (each #(partial-box (collision-shape env %)))
     :outlines (each #(partial-box (outline-shape env %)))
     :flags (each #(state-flags env %))
     :sturdy (each #(state-sturdy env %))
     :sturdy-center (each #(state-sturdy env % (:center env)))
     :sturdy-rigid (each #(state-sturdy env % (:rigid env)))}))

(defn- project-face [^long axis [x0 y0 z0 x1 y1 z1]]
  (case axis
    0 [y0 z0 y1 z1]
    1 [x0 z0 x1 z1]
    2 [x0 y0 x1 y1]))

(defn- face-entry [block-shape d shape]
  (let [bs (boxes shape)
        ps (mapv #(project-face (long (face-axis d)) %) bs)]
    (cond
      (identical? shape block-shape) :full
      (empty? bs) nil
      (= ps [[0 0 16 16]]) :full
      :else ps)))

(defn- runs [pairs]
  (reduce (fn [acc [id v]]
            (let [[lo hi pv] (peek acc)]
              (if (and lo (= v pv) (= (long id) (inc (long hi))))
                (conj (pop acc) [lo id v])
                (conj acc [id id v]))))
          [] (sort-by first pairs)))

(defn- flag-runs [ids]
  (mapv (fn [[lo hi _]] (if (= lo hi) lo [lo hi]))
        (runs (map (fn [id] [id true]) ids))))

(defn- value-runs [pairs]
  (mapv (fn [[lo hi v]] (if (= lo hi) [lo v] [lo hi v]))
        (runs pairs)))

(defn- occludes? [st] (call st "canOcclude"))

(defn- shaped? [st] (call st "useShapeForLightOcclusion"))

(defn- occlusion-faces [block-shape dirs st]
  (into (sorted-map)
        (keep (fn [d]
                (let [s (dirs d)
                      shape (call st "getFaceOcclusionShape" s)]
                  (when-let [e (face-entry block-shape d shape)]
                    [(dir-names d) e]))))
        (range 6)))

(defn- occluding-states [states]
  (let [dirs (vec (call-static "core.Direction" "values"))
        block-shape (call-static "world.phys.shapes.Shapes" "block")]
    (for [[id st] states
          :when (and (occludes? st) (shaped? st))
          :let [k {:shape (boxes (call st "getOcclusionShape"))
                   :faces (occlusion-faces block-shape dirs st)}]
          :when (seq (:faces k))]
      [id k])))

(defn- light-values [states m keep?]
  (value-runs (for [[id st] states
                    :let [v (call st m)]
                    :when (keep? v)]
                [id v])))

(defn- light-flags [states pred]
  (flag-runs (for [[id st] states :when (pred st)] id)))

(defn- light-table [states]
  (let [shaped (occluding-states states)
        kinds (vec (sort-by pr-str (distinct (map second shaped))))
        index (into {} (map-indexed (fn [i k] [k i])) kinds)
        vals-of (fn [m p] (light-values states m p))]
    {:dampening (vals-of "getLightDampening" #(not= 15 (long %)))
     :emission (vals-of "getLightEmission" #(pos? (long %)))
     :occludes  (light-flags states occludes?)
     :use-shape (light-flags states shaped?)
     :kinds     kinds
     :faces     (value-runs (for [[id k] shaped] [id (index k)]))}))

(def ^:private sound-parts
  {:break "Break" :step "Step" :place "Place"
   :hit "Hit" :fall "Fall"})

(defn- sound-events [o]
  (sorted-vals sound-parts
               (fn [part]
                 (let [event (call o (str "get" part "Sound"))]
                   (kw (str (call event "location")))))))

(defn- sound-types []
  (let [c (cls "world.level.block.SoundType")]
    (for [^Field f (.getFields c)
          :when (= c (.getType f))
          :let [o (.get f nil)]]
      [(kw (str/lower-case (.getName f))) o (sound-events o)])))

(defn- simple-name [^Class c]
  (if (str/blank? (.getSimpleName c))
    (recur (.getSuperclass c))
    (.getSimpleName c)))

(defn- block-class [b]
  (-> (simple-name (class b))
      (str/replace #"(?<=.)(?=\p{Upper})" "_")
      str/lower-case
      kw))

(defn- block-pairs [reg c field forward back]
  (apply merge-with merge (sorted-map)
         (for [[a b] (call (static-field c field) "get")]
           {(key-of reg a) {forward (key-of reg b)}
            (key-of reg b) {back (key-of reg a)}})))

(def ^:private ref-fields
  {"deadBlock" :dead "concrete" :concrete "potted" :potted})

(defn- block-ref [reg b [f k]]
  (when-let [r (hidden-field (class b) b f)]
    [k (key-of reg r)]))

(defn- block-refs [reg]
  (into (sorted-map)
        (for [b (elements reg)
              :let [found (keep #(block-ref reg b %) ref-fields)
                    m (into (sorted-map) found)]
              :when (seq m)]
          [(key-of reg b) m])))

(defn- pot-contents [refs]
  (into (sorted-map)
        (for [[pot {:keys [potted]}] refs
              :when (and potted (not= :air potted))]
          [potted {:pot pot}])))

(defn- strippables [reg]
  (into (sorted-map)
        (map (fn [[a b]]
               [(key-of reg a) {:stripped (key-of reg b)}]))
        (hidden-field (cls "world.item.AxeItem") nil "STRIPPABLES")))

(def ^:private toggles
  (let [set-type "world.level.block.state.properties.BlockSetType"
        wood-type "world.level.block.state.properties.WoodType"]
    [["world.level.block.DoorBlock" set-type
      "doorOpen" "doorClose" true]
     ["world.level.block.TrapDoorBlock" set-type
      "trapdoorOpen" "trapdoorClose" true]
     ["world.level.block.FenceGateBlock" wood-type
      "fenceGateOpen" "fenceGateClose" false]]))

(defn- toggle [b]
  (some (fn [[block type open close hand?]]
          (when (.isInstance (cls block) b)
            (let [t (hidden-field (class b) b (cls type))
                  event #(kw (str (call (call t %) "location")))
                  by-hand #(boolean (call t "canOpenByHand"))]
              (cond-> {:open (event open) :close (event close)}
                hand? (assoc :hand? (by-hand))))))
        toggles))

(def ^:private motion-fields
  [[:friction "friction" 0.6]
   [:speed-factor "speedFactor" 1.0]
   [:jump-factor "jumpFactor" 1.0]])

(defn- motion-props
  "The friction, speed and jump factors of a block that moves a
  body differently from plain stone. The ordinary values are left
  out of the table."
  [field]
  (into {}
        (keep (fn [[k n ^double d]]
                (let [v (flt (field n))]
                  (when (not= v d) [k v]))))
        motion-fields))

(defn- own-props [by-type b]
  (let [field #(hidden-field (class b) b %)]
    (merge {:resistance (flt (field "explosionResistance"))
            :sound      (by-type (field "soundType"))
            :class      (block-class b)}
           (motion-props field)
           (toggle b))))

(defn- weathering-pairs [reg]
  (block-pairs reg "world.level.block.WeatheringCopper"
               "NEXT_BY_BLOCK" :next :previous))

(defn- waxable-pairs [reg]
  (block-pairs reg "world.item.HoneycombItem"
               "WAXABLES" :waxed :unwaxed))

(defn- block-table [reg by-type]
  (let [refs (block-refs reg)
        own (into (sorted-map)
                  (for [b (elements reg)]
                    [(key-of reg b) (own-props by-type b)]))]
    (merge-with merge own
                (weathering-pairs reg) (waxable-pairs reg)
                (merge-with merge refs (pot-contents refs))
                (strippables reg))))

(defn- block-props []
  (let [types (sound-types)
        by-type (into {} (map (fn [[k o _]] [o k])) types)]
    {:props  (block-table (registry "BLOCK") by-type)
     :sounds (into (sorted-map)
                   (map (fn [[k _ evs]] [k evs]))
                   types)}))

(defn- wall-items []
  (let [items (registry "ITEM")
        blocks (registry "BLOCK")
        c (cls "world.item.StandingAndWallBlockItem")]
    (into (sorted-map)
          (for [i (elements items)
                :when (.isInstance c i)
                :let [wall (hidden-field (class i) i "wallBlock")]]
            [(key-of items i) {:wall (key-of blocks wall)}]))))

(defn- compostables []
  (let [items (registry "ITEM")]
    (into (sorted-map)
          (map (fn [[i v]] [(key-of items i) {:compost (flt v)}]))
          (static-field "world.level.block.ComposterBlock"
                        "COMPOSTABLES"))))

(defn- fire-odds []
  (let [fire (static-field "world.level.block.Blocks" "FIRE")
        blocks (registry "BLOCK")
        table (fn [f k]
                (into (sorted-map)
                      (map (fn [[b v]] [(key-of blocks b) {k v}]))
                      (hidden-field (class fire) fire f)))]
    (merge-with merge
                (table "igniteOdds" :ignite)
                (table "burnOdds" :burn))))

(defn- placer-features [reg]
  (let [c (cls "world.level.block.BonemealableFeaturePlacerBlock")]
    (into (sorted-map)
          (for [b (elements reg) :when (.isInstance c b)]
            (let [f (hidden-field (class b) b "feature")]
              [(key-of reg b)
               (kw (str (call f "identifier")))])))))

(defn- template [reg t]
  (when t
    (when-not (call (call t "components") "isEmpty")
      (throw (ex-info "remainder with components"
                      {:template (str t)})))
    {:item (key-of reg (call (call t "item") "value"))
     :count (call t "count")}))

(defn- remainders []
  (let [items (registry "ITEM")]
    (into (sorted-map)
          (for [i (elements items)
                :let [t (call i "getCraftingRemainder")]
                :when t]
            [(key-of items i) {:remainder (template items t)}]))))

(defn- banner-colors []
  (let [items (registry "ITEM")
        c (cls "world.item.BannerItem")]
    (into (sorted-map)
          (for [i (elements items) :when (.isInstance c i)]
            (let [dye (call i "getColor")
                  color (call dye "getSerializedName")]
              [(key-of items i) {:banner-color (kw color)}])))))

(defn- dye-colors []
  (into (sorted-map)
        (for [d (call-static "world.item.DyeColor" "values")]
          [(kw (call d "getSerializedName"))
           {:firework (call d "getFireworkColor")
            :diffuse  (call d "getTextureDiffuseColor")}])))

(defn- from-classes [jars server]
  (binding [*loader* (class-loader jars server)]
    (call-static "SharedConstants" "tryDetectVersion")
    (call-static "server.Bootstrap" "bootStrap")
    (let [states (block-states)]
      (merge (state-shapes states)
             (block-props)
             {:light   (light-table states)
              :placers (placer-features (registry "BLOCK"))
              :fire    (fire-odds)
              :compost (compostables)
              :walls   (wall-items)
              :remainders (remainders)
              :banners (banner-colors)
              :dyes    (dye-colors)}))))

(defn- report-json [reports name]
  (json/read-str (slurp (io/file reports name))))

(defn- ids [entries]
  (into (sorted-map)
        (map (fn [[n e]] [(kw n) (get e "protocol_id")]))
        entries))

(defn- packets [reports]
  (into {}
        (map (fn [[state dirs]]
               [(kw state)
                (into {}
                      (map (fn [[dir ps]] [(kw dir) (ids ps)]))
                      dirs)]))
        (report-json reports "packets.json")))

(defn- registries [reports]
  (into (sorted-map)
        (map (fn [[name m]]
               [(str/replace name #"^minecraft:" "")
                (ids (get m "entries"))]))
        (report-json reports "registries.json")))

(defn- block [[name m] extra shaped]
  (let [{:strs [properties states definition]} m
        first-id (apply min (map #(get % "id") states))
        default (some #(when (get % "default") (get % "id")) states)
        props (into (sorted-map)
                    (map (fn [[p vs]] [(kw p) (mapv keyword vs)]))
                    properties)
        base {:first   first-id
              :default (or default first-id)
              :type    (kw (get definition "type"))}
        own (into (sorted-map) (merge base (get extra (kw name))))]
    [(kw name)
     (cond-> own
       (not (contains? shaped default)) (assoc :full-cube? true)
       (seq props) (assoc :props props))]))

(defn- blocks [reports extra shaped]
  (into (sorted-map)
        (map #(block % extra shaped))
        (report-json reports "blocks.json")))

(def ^:private attack-damage-modifier
  ["minecraft:attack_damage" "add_value" "mainhand"])

(defn- attack-damage ^double [components]
  (let [mods (get components "minecraft:attribute_modifiers")
        match? #(= (map % ["type" "operation" "slot"])
                   attack-damage-modifier)]
    (reduce + 0.0 (for [a mods :when (match? a)]
                    (double (get a "amount"))))))

(defn- unmodelled [v]
  (throw (ex-info "default component not modelled" {:value v})))

(defn- empty-or-throw [k v]
  (when (seq v)
    (throw (ex-info "default component not modelled" {k v})))
  v)

(defn- potion-default [v]
  (empty-or-throw "custom_effects" (get v "custom_effects"))
  (when-let [extra (seq (dissoc v "potion" "custom_effects"))]
    (throw (ex-info "default potion not modelled" {:extra extra})))
  {:potion (some-> (get v "potion") kw) :custom-color nil
   :custom-effects [] :custom-name nil})

(defn- pot-default [v]
  (vec (take 4 (concat (map kw v) (repeat :brick)))))

(defn- fireworks-default [v]
  (empty-or-throw "explosions" (get v "explosions"))
  {:flight-duration (get v "flight_duration" 0) :explosions []})

(defn- levels [v] (into (sorted-map) (map (fn [[e n]] [(kw e) n])) v))

(defn- swing-animation [v]
  (sorted-map :type (kw (get v "type" "whack"))
              :duration (get v "duration" 6)))

(def ^:private crafted-components
  {"minecraft:damage"               [:damage identity]
   "minecraft:max_damage"           [:max-damage identity]
   "minecraft:max_stack_size"       [:max-stack-size identity]
   "minecraft:repair_cost"          [:repair-cost identity]
   "minecraft:dye"                  [:dye kw]
   "minecraft:swing_animation"      [:swing-animation swing-animation]
   "minecraft:enchantments"         [:enchantments levels]
   "minecraft:stored_enchantments"  [:stored-enchantments levels]
   "minecraft:potion_contents"      [:potion-contents potion-default]
   "minecraft:banner_patterns"
   [:banner-patterns #(empty-or-throw "banner_patterns" (vec %))]
   "minecraft:pot_decorations"      [:pot-decorations pot-default]
   "minecraft:fireworks"            [:fireworks fireworks-default]
   "minecraft:firework_explosion"   [:firework-explosion unmodelled]
   "minecraft:written_book_content" [:written-book-content unmodelled]
   "minecraft:map_id"               [:map-id unmodelled]
   "minecraft:dyed_color"           [:dyed-color unmodelled]
   "minecraft:base_color"           [:base-color unmodelled]})

(defn- default-components [cs]
  (into (sorted-map)
        (keep (fn [[json [k f]]]
                (when (contains? cs json) [k (f (get cs json))])))
        crafted-components))

(defn- consume-effect [e]
  (cond-> (sorted-map :type (kw (get e "type")))
    (get e "sound") (assoc :sound (kw (get e "sound")))))

(defn- consumable
  "Returns the eating and drinking of an item.
  Fields the recipe leaves out take the game's defaults."
  [v]
  (sorted-map
    :seconds (flt (get v "consume_seconds" 1.6))
    :animation (kw (get v "animation" "eat"))
    :sound (kw (get v "sound" "minecraft:entity.generic.eat"))
    :particles? (get v "has_consume_particles" true)
    :effects (mapv consume-effect (get v "on_consume_effects"))))

(defn- food [v]
  (sorted-map :nutrition (get v "nutrition")
              :saturation (flt (get v "saturation"))
              :always? (get v "can_always_eat" false)))

(defn- use-remainder [v]
  (sorted-map :item (kw (get v "id")) :count (get v "count" 1)))

(defn- use-cooldown [v]
  (cond-> (sorted-map :seconds (flt (get v "seconds")))
    (get v "cooldown_group")
    (assoc :group (kw (get v "cooldown_group")))))

(defn- stack-fields [cs]
  (let [n (get cs "minecraft:max_stack_size" 64)
        slot (get-in cs ["minecraft:equippable" "slot"])
        song (get cs "minecraft:jukebox_playable")
        dye (get cs "minecraft:dye")]
    (cond-> (sorted-map)
      (not= n 64) (assoc :max-stack n)
      slot (assoc :equip (kw slot))
      song (assoc :jukebox-song (kw song))
      dye (assoc :dye (kw dye)))))

(defn- combat-fields [cs]
  (let [egg (get-in cs ["minecraft:entity_data" "id"])
        hit (attack-damage cs)
        resists (get-in cs ["minecraft:damage_resistant" "types"])
        pat (get cs "minecraft:provides_banner_patterns")
        tag #(str/replace (subs % 1) #"^minecraft:" "")]
    (cond-> (sorted-map)
      egg (assoc :spawns (kw egg))
      (pos? hit) (assoc :attack-damage (flt hit))
      (string? resists) (assoc :resists (tag resists))
      (string? pat) (assoc :patterns (tag pat)))))

(defn- consumable-fields [cs]
  (let [eats (get cs "minecraft:consumable")
        left (get cs "minecraft:use_remainder")
        wait (get cs "minecraft:use_cooldown")
        grub (get cs "minecraft:food")]
    (cond-> (sorted-map)
      eats (assoc :consumable (consumable eats))
      left (assoc :use-remainder (use-remainder left))
      wait (assoc :use-cooldown (use-cooldown wait))
      grub (assoc :food (food grub)))))

(defn- item [cs]
  (merge (sorted-map :components (default-components cs))
         (stack-fields cs) (combat-fields cs)
         (consumable-fields cs)))

(defn- item-components [reports]
  (let [dir (io/file reports "minecraft" "components" "item")]
    (for [^File f (sort (.listFiles dir))
          :when (str/ends-with? (.getName f) ".json")]
      [(kw (str/replace (.getName f) #"\.json$" ""))
       (get (json/read-str (slurp f)) "components")])))

(defn- vanilla-items [reports]
  (into (sorted-map)
        (keep (fn [[name cs]]
                (let [m (item cs)]
                  (when (seq m) [name m]))))
        (item-components reports)))

(defn- under [zf prefix]
  (for [n (zip-names zf)
        :when (str/starts-with? n prefix)
        :when (str/ends-with? n ".json")]
    [(subs n (count prefix) (- (count n) 5)) n]))

(defn- read-json [^ZipFile zf path]
  (json/read-str (slurp (.getInputStream zf (.getEntry zf path)))))

(defn- jsons [zf prefix]
  (into (sorted-map)
        (map (fn [[name path]] [name (read-json zf path)]))
        (under zf prefix)))

(defn- loot-number [v]
  (cond
    (number? v) [(long v) (long v)]
    (map? v) [(long (get v "min" 1)) (long (get v "max" 1))]
    :else [1 1]))

(declare loot-condition)

(defn- state-props [c]
  {:props (into {}
                (map (fn [[k v]] [(kw k) (keyword v)]))
                (get c "properties"))})

(defn- nested-condition [c]
  (case (get c "condition")
    "minecraft:inverted"
    (if (= :skip (loot-condition (get c "term"))) {} :unknown)
    "minecraft:any_of"
    (if (every? #(= :skip (loot-condition %)) (get c "terms"))
      :skip
      :unknown)
    :unknown))

(defn- loot-condition [c]
  (case (get c "condition")
    "minecraft:survives_explosion" {:survives-explosion true}
    "minecraft:random_chance" {:chance (double (get c "chance"))}
    "minecraft:table_bonus"
    {:chance (double (first (get c "chances")))}
    "minecraft:block_state_property" (state-props c)
    "minecraft:entity_properties" {:entity? true}
    "minecraft:match_tool" :skip
    (nested-condition c)))

(defn- loot-conditions [cs]
  (reduce (fn [acc c]
            (let [r (loot-condition c)]
              (if (keyword? r) (reduced r) (merge acc r))))
          {} cs))

(defn- collect [f xs]
  (reduce (fn [acc x]
            (let [r (f x)]
              (cond (= r :skip) acc
                    (keyword? r) (reduced r)
                    :else (into acc r))))
          [] xs))

(defn- set-count [e]
  (some #(when (= "minecraft:set_count" (get % "function"))
           (loot-number (get % "count")))
        (get e "functions")))

(defn- item-entry [e cs]
  (let [n (set-count e)]
    [(cond-> (assoc cs :item (kw (get e "name")))
       n (assoc :count n))]))

(defn- loot-entry [e]
  (let [type (get e "type")
        cs (loot-conditions (get e "conditions"))]
    (cond
      (not (#{"minecraft:item" "minecraft:alternatives"} type))
      :unknown
      (keyword? cs) cs
      (= "minecraft:item" type) (item-entry e cs)
      :else
      (let [r (collect loot-entry (get e "children"))]
        (if (keyword? r) r (mapv #(merge cs %) r))))))

(defn- loot-pool [p]
  (let [cs (loot-conditions (get p "conditions"))
        rolls (loot-number (get p "rolls" 1))
        r (when-not (keyword? cs)
            (collect loot-entry (get p "entries")))]
    (cond
      (keyword? cs) cs
      (keyword? r) r
      :else (mapv #(cond-> (merge cs %)
                     (not= rolls [1 1]) (assoc :rolls rolls))
                  r))))

(defn- loot-table [json]
  (let [rs (map loot-pool (get json "pools"))]
    (if (some #{:unknown} rs)
      :complex
      (into [] (mapcat #(if (= % :skip) [] %)) rs))))

(defn- block-drops [zf]
  (into (sorted-map)
        (keep (fn [[name json]]
                (let [t (loot-table json)]
                  (when (not= t []) [(kw name) t]))))
        (jsons zf "data/minecraft/loot_table/blocks/")))

(defn- loot-id [s]
  (kw (str/replace (str s) #"^#" "")))

(declare shear-name)

(defn- table-ref [v]
  (let [s (str/replace (str v) #"^minecraft:" "")]
    (cond
      (str/starts-with? s "entities/") (kw (subs s 9))
      (str/starts-with? s "shearing/") (kw (shear-name (subs s 9)))
      :else s)))

(defn- loot-scalar [v]
  (cond
    (string? v) (loot-id v)
    (not (number? v)) v
    (== (double v) (Math/rint (double v))) (long v)
    :else v))

(declare loot-node)

(defn- loot-provider [m]
  (case (:type m)
    :uniform (select-keys m [:min :max])
    :binomial (select-keys m [:n :p])
    :constant (:value m)
    m))

(defn- loot-map [m]
  (let [r (loot-provider
           (into (sorted-map)
                 (map (fn [[k v]] [(kw k) (loot-node v)]))
                 m))]
    (if (= :loot-table (:type r))
      (assoc r :value (table-ref (get m "value")))
      r)))

(defn- loot-node [v]
  (cond
    (map? v) (loot-map v)
    (sequential? v) (mapv loot-node v)
    :else (loot-scalar v)))

(defn- shear-name [n]
  (if-let [i (str/index-of n "/")]
    (str (subs n 0 i) "-shear" (subs n i))
    (str n "-shear")))

(defn- entity-drops [zf]
  (let [dir "data/minecraft/loot_table/"
        shear (jsons zf (str dir "shearing/"))]
    (into (sorted-map)
          (map (fn [[name json]] [(kw name) (loot-node json)]))
          (concat (jsons zf (str dir "entities/"))
                  (map (fn [[n j]] [(shear-name n) j]) shear)))))

(def synchronized-registries
  ["banner_pattern" "worldgen/biome" "cat_sound_variant" "cat_variant"
   "chat_type" "chicken_sound_variant" "chicken_variant"
   "cow_sound_variant" "cow_variant" "damage_type" "dialog"
   "dimension_type" "enchantment"
   "frog_variant" "instrument" "jukebox_song" "painting_variant"
   "pig_sound_variant" "pig_variant" "sulfur_cube_archetype"
   "test_environment" "test_instance" "timeline" "trim_material"
   "trim_pattern" "wolf_sound_variant" "wolf_variant" "world_clock"
   "zombie_nautilus_variant"])

(def ^:private top-level-names
  (comp (map first) (remove #(str/includes? % "/")) (map kw)))

(defn- datapack-entry [zf reg]
  (let [prefix (str "data/minecraft/" reg "/")
        names (into (sorted-set) top-level-names (under zf prefix))]
    (when (seq names) [reg (vec names)])))

(defn- datapack-names [zf]
  (into (sorted-map)
        (keep #(datapack-entry zf %))
        synchronized-registries))

(defn- plain [s] (str/replace (str s) #"^minecraft:" ""))

(defn- json-name [k] (str/replace (name k) "-" "_"))

(defn- feature-props [props]
  (into (sorted-map) (map (fn [[k v]] [(kw k) (keyword v)])) props))

(defn- state-value [m]
  (let [props (get m "Properties")]
    (cond-> (sorted-map :block (kw (get m "Name")))
      props (assoc :props (feature-props props)))))

(defn- feature-value [v]
  (cond
    (and (map? v) (contains? v "Name")) (state-value v)
    (map? v) (into (sorted-map)
                   (map (fn [[k x]] [(kw k) (feature-value x)]))
                   v)
    (vector? v) (mapv feature-value v)
    (not (string? v)) v
    (str/starts-with? v "#") {:tag (plain (subs v 1))}
    :else (kw v)))

(def ^:private selector-types
  #{"minecraft:random_selector" "minecraft:weighted_random_selector"
    "minecraft:simple_random_selector"
    "minecraft:random_boolean_selector"})

(def ^:private placed-fields
  #{"feature" "default_feature" "vegetation_feature"})

(defn- placed-refs [v]
  (let [ref (fn [[k x]]
              (if (and (string? x) (placed-fields k))
                [(plain x)]
                (placed-refs x)))]
    (cond
      (and (map? v) (contains? v "placement")) [v]
      (map? v) (mapcat ref v)
      (vector? v) (mapcat placed-refs v))))

(declare conf-features)

(defn- placed-of [reg p] (if (map? p) p (get (:placed reg) p)))

(defn- placed-features [reg p]
  (conf-features reg (plain (get (placed-of reg p) "feature"))))

(defn- conf-features [reg nm]
  (let [j (get (:configured reg) nm)]
    (into [nm]
          (when (selector-types (get j "type"))
            (mapcat #(placed-features reg %)
                    (placed-refs (get j "config")))))))

(defn- biome-features [reg tagged j]
  (let [xf (comp cat
                 (mapcat #(placed-features reg (plain %)))
                 (filter tagged))]
    (into [] xf (get j "features"))))

(defn- bone-meal-biomes [reg tagged]
  (into (sorted-map)
        (keep (fn [[nm j]]
                (let [fs (biome-features reg tagged j)]
                  (when (seq fs) [(kw nm) (mapv kw fs)]))))
        (:biomes reg)))

(defn- conf-placed-refs [reg nm]
  (placed-refs (get (get (:configured reg) nm) "config")))

(defn- closure [reg cs ps]
  (let [rs (mapcat #(conf-placed-refs reg %) cs)
        cs' (into cs (map #(plain (get (placed-of reg %) "feature")))
                  (concat ps rs))
        ps' (into ps (filter string?) rs)]
    (if (and (= cs cs') (= ps ps')) [cs' ps'] (recur reg cs' ps'))))

(defn- feature-set [reg kind names]
  (into (sorted-map)
        (map (fn [n] [(kw n) (feature-value (get (kind reg) n))]))
        names))

(def ^:private lang-file "assets/minecraft/lang/en_us.json")

(def ^:private bone-meal-tag
  (str "data/minecraft/tags/worldgen/configured_feature"
       "/can_spawn_from_bone_meal.json"))

(defn- feature-registries [zf]
  (let [dir-of (fn [dir] (str "data/minecraft/worldgen/" dir "/"))]
    (into {}
          (map (fn [[k dir]] [k (jsons zf (dir-of dir))]))
          {:placed "placed_feature"
           :configured "configured_feature"
           :biomes "biome"})))

(defn- features [zf placers]
  (let [reg (feature-registries zf)
        bone-meal (read-json zf bone-meal-tag)
        tagged (into #{} (map plain) (get bone-meal "values"))
        roots (into tagged (map (comp json-name second)) placers)
        [cs ps] (closure reg roots #{"grass_bonemeal"})]
    {:configured (feature-set reg :configured cs)
     :placed     (feature-set reg :placed ps)
     :bone-meal  (bone-meal-biomes reg tagged)
     :placers    placers}))

(defn- ingredient [v]
  (cond
    (string? v) (if (str/starts-with? v "#")
                  {:tag (str/replace (subs v 1) #"^minecraft:" "")}
                  [(kw v)])
    (sequential? v) (mapv kw v)
    :else (throw (ex-info "unknown ingredient" {:value v}))))

(def ^:private property-sets
  (let [smithing #{"minecraft:smithing_transform"
                   "minecraft:smithing_trim"}
        campfire #{"minecraft:campfire_cooking"}]
    {"furnace_input"       [#{"minecraft:smelting"} "ingredient"]
     "blast_furnace_input" [#{"minecraft:blasting"} "ingredient"]
     "smoker_input"        [#{"minecraft:smoking"} "ingredient"]
     "campfire_input"      [campfire "ingredient"]
     "smithing_base"       [smithing "base"]
     "smithing_template"   [smithing "template"]
     "smithing_addition"   [smithing "addition"]}))

(defn- stonecutting-entry [json]
  (when (= "minecraft:stonecutting" (get json "type"))
    (let [r (get json "result")
          r (if (string? r) {"id" r} r)
          n (get r "count" 1)]
      {:in  (ingredient (get json "ingredient"))
       :out (cond-> {:item (kw (get r "id"))}
              (not= 1 n) (assoc :count n))})))

(defn- stonecutting [recipes]
  (into [] (keep stonecutting-entry) recipes))

(defn- property-set [tags recipes [types field]]
  (let [want? (fn [json]
                (and (contains? types (get json "type"))
                     (contains? json field)))
        pick (fn [json]
               (when (want? json)
                 (let [i (ingredient (get json field))]
                   (if (map? i)
                     (get-in tags ["item" (:tag i)] [])
                     i))))]
    (into (sorted-set) (mapcat pick) recipes)))

(def ^:private crafting-types
  {"minecraft:crafting_shaped"                    :shaped
   "minecraft:crafting_shapeless"                 :shapeless
   "minecraft:crafting_transmute"                 :transmute
   "minecraft:crafting_dye"                       :dye
   "minecraft:crafting_imbue"                     :imbue
   "minecraft:crafting_decorated_pot"             :decorated-pot
   "minecraft:crafting_special_bannerduplicate"   :banner-duplicate
   "minecraft:crafting_special_bookcloning"       :book-cloning
   "minecraft:crafting_special_firework_rocket"   :firework-rocket
   "minecraft:crafting_special_firework_star"     :firework-star
   "minecraft:crafting_special_firework_star_fade" :firework-star-fade
   "minecraft:crafting_special_repairitem"        :repair-item
   "minecraft:crafting_special_mapextending"      :map-extending
   "minecraft:crafting_special_shielddecoration"  :shield-decoration})

(defn- item-set [tags v]
  (let [i (ingredient v)
        items (if (map? i)
                (or (get-in tags ["item" (:tag i)])
                    (throw (ex-info "unknown item tag" {:tag i})))
                i)]
    (into (sorted-set) items)))

(defn- shown-name [lang cs]
  (let [v (get cs "minecraft:item_name")]
    (if (map? v) (get lang (get v "translate")) v)))

(defn- station-item [tags lang cs]
  (let [rep (get cs "minecraft:repairable")
        trim (get cs "minecraft:provides_trim_material")
        nm (shown-name lang cs)]
    (cond-> (sorted-map)
      nm (assoc :name nm)
      rep (assoc :repairable (item-set tags (get rep "items")))
      trim (assoc :trim-material (kw trim)))))

(defn- station-items
  "Returns what each item shows as, repairs with and trims as."
  [reports tags lang]
  (into (sorted-map)
        (keep (fn [[name cs]]
                (let [m (station-item tags lang cs)]
                  (when (seq m) [name m]))))
        (item-components reports)))

(defn- cost-of [v]
  (sorted-map :base (get v "base" 0)
              :per-level (get v "per_level_above_first" 0)))

(defn- enchantment-set [tags v]
  (into (sorted-set)
        (if (and (string? v) (str/starts-with? v "#"))
          (get-in tags ["enchantment" (plain (subs v 1))] [])
          (map kw (if (string? v) [v] v)))))

(defn- enchantment [tags json]
  (sorted-map
    :anvil-cost (get json "anvil_cost")
    :max-level (get json "max_level")
    :min-cost (cost-of (get json "min_cost"))
    :supported (item-set tags (get json "supported_items"))
    :exclusive (enchantment-set tags (get json "exclusive_set"))))

(defn- enchantments [zf tags]
  (into (sorted-map)
        (map (fn [[name json]] [(kw name) (enchantment tags json)]))
        (jsons zf "data/minecraft/enchantment/")))

(defn- dimension-attr-value [v]
  (cond
    (map? v)
    (into (sorted-map)
          (map (fn [[k x]] [(kw k) (dimension-attr-value x)]))
          v)
    (vector? v) (mapv dimension-attr-value v)
    :else v))

(defn- dimension-attrs [json]
  (into (sorted-map)
        (map (fn [[k v]] [(kw k) (dimension-attr-value v)]))
        (get json "attributes")))

(defn- monster-spawn-light [v]
  (if (map? v)
    {:min (get v "min_inclusive")
     :max (get v "max_inclusive")}
    v))

(defn- dimension-type [json]
  (cond-> (sorted-map
           :min-y (get json "min_y")
           :height (get json "height")
           :logical-height (get json "logical_height")
           :coordinate-scale (get json "coordinate_scale")
           :has-skylight (get json "has_skylight")
           :has-ceiling (get json "has_ceiling")
           :has-ender-dragon-fight
           (get json "has_ender_dragon_fight" false)
           :has-fixed-time (get json "has_fixed_time" false)
           :ambient-light (get json "ambient_light")
           :infiniburn (kw (subs (get json "infiniburn") 1))
           :monster-spawn-light-level
           (monster-spawn-light
            (get json "monster_spawn_light_level"))
           :monster-spawn-block-light-limit
           (get json "monster_spawn_block_light_limit")
           :skybox (kw (get json "skybox" "overworld"))
           :cardinal-light (kw (get json "cardinal_light" "default"))
           :attributes (dimension-attrs json))
    (contains? json "default_clock")
    (assoc :default-clock (kw (get json "default_clock")))))

(defn- dimension-types [zf]
  (into (sorted-map)
        (map (fn [[name json]]
               [(kw name) (dimension-type json)]))
        (jsons zf "data/minecraft/dimension_type/")))

(defn- raw-ingredient [v]
  (let [i (ingredient v)] (if (map? i) [:tag (:tag i)] (vec i))))

(defn- stew-effects [v]
  (mapv (fn [e]
          {:effect (kw (get e "id"))
           :duration (get e "duration" 160)})
        v))

(defn- result-component [[k v]]
  (if (= k "minecraft:suspicious_stew_effects")
    [:suspicious-stew-effects (stew-effects v)]
    (throw (ex-info "result component not modelled" {k v}))))

(defn- result-components [cs]
  (into (sorted-map) (map result-component) cs))

(defn- result [r]
  (let [r (if (string? r) {"id" r} r)
        cs (get r "components")]
    (cond-> {:item (kw (get r "id")) :count (get r "count" 1)}
      (seq cs) (assoc :components (result-components cs)))))

(def ^:private smithing-types
  {"minecraft:smithing_transform" :transform
   "minecraft:smithing_trim"      :trim})

(defn- smithing-recipe [tags id json type]
  (cond-> (sorted-map :id (kw id) :type type
                      :base (item-set tags (get json "base")))
    (get json "template")
    (assoc :template (item-set tags (get json "template")))
    (get json "addition")
    (assoc :addition (item-set tags (get json "addition")))
    (= :trim type) (assoc :pattern (kw (get json "pattern")))
    (= :transform type) (assoc :result (result (get json "result")))))

(defn- smithing [recipes tags]
  (into []
        (keep (fn [[id json]]
                (when-let [t (smithing-types (get json "type"))]
                  (smithing-recipe tags id json t))))
        recipes))

(defn- bounds [v default]
  (cond (nil? v) default
        (number? v) {:min v :max v}
        :else (cond-> {}
                (contains? v "min") (assoc :min (get v "min"))
                (contains? v "max") (assoc :max (get v "max")))))

(defn- shrink-step [[left right top bottom] [i ^String line]]
  (let [first-non (count (take-while #(= \space %) line))
        trail (count (take-while #(= \space %) (reverse line)))
        last-non (- (count line) 1 trail)]
    [(min left first-non) (max right last-non)
     (if (and (neg? last-non) (= top i)) (inc top) top)
     (if (neg? last-non) (inc bottom) 0)]))

(defn- shrink [pattern]
  (let [[left right top bottom]
        (reduce shrink-step [Integer/MAX_VALUE 0 0 0]
                (map-indexed vector pattern))
        n (count pattern)]
    (if (= n bottom)
      []
      (mapv #(subs (nth pattern (+ % top)) left (inc right))
            (range (- n bottom top))))))

(defn- symmetric? [w h cells]
  (let [cell (fn [x y] (nth cells (+ x (* y w))))
        mirrored? (fn [[x y]] (= (cell x y) (cell (- w 1 x) y)))
        pairs (for [y (range h) x (range (quot w 2))] [x y])]
    (or (= 1 w) (every? mirrored? pairs))))

(defn- cell-key [json ch]
  (when (not= \space ch)
    (or (get-in json ["key" (str ch)])
        (throw (ex-info "undefined symbol" {:symbol ch})))))

(defn- shaped [tags json]
  (let [rows (shrink (get json "pattern"))
        raw (mapv #(cell-key json %) (apply str rows))
        w (count (first rows))
        h (count rows)
        cells (mapv #(some->> % (item-set tags)) raw)
        mirror (mapv #(some-> % raw-ingredient) raw)]
    {:w w :h h :cells cells :symmetric? (symmetric? w h mirror)}))

(def ^:private ingredient-fields
  {:transmute          ["input" "material"]
   :dye                ["target" "dye"]
   :imbue              ["source" "material"]
   :decorated-pot      ["back" "left" "right" "front"]
   :banner-duplicate   ["banner"]
   :book-cloning       ["source" "material"]
   :firework-rocket    ["shell" "fuel" "star"]
   :firework-star      ["trail" "twinkle" "fuel" "dye"]
   :firework-star-fade ["target" "dye"]
   :map-extending      ["map" "material"]
   :shield-decoration  ["banner" "target"]})

(defn- extra-fields [tags type json]
  (case type
    :transmute
    {:material-count
     (bounds (get json "material_count") {:min 1 :max 1})
     :add-material-count?
     (get json "add_material_count_to_result" false)}
    :book-cloning
    {:allowed-generations
     (bounds (get json "allowed_generations") {:min 0 :max 1})}
    :firework-star
    {:shapes (mapv (fn [[k v]] [(kw k) (item-set tags v)])
                   (get json "shapes"))}
    {}))

(defn- fields-of [tags type json]
  (into (extra-fields tags type json)
        (map (fn [f] [(kw f) (item-set tags (get json f))]))
        (ingredient-fields type)))

(defn- shapeless [tags json]
  {:ingredients (mapv #(item-set tags %) (get json "ingredients"))})

(defn- crafting-recipe [tags order [id json]]
  (let [type (crafting-types (get json "type"))
        r (get json "result")]
    (cond-> (merge {:id (kw id) :order order :type type}
                   (case type
                     :shaped (shaped tags json)
                     :shapeless (shapeless tags json)
                     (fields-of tags type json)))
      r (assoc :result (result r)))))

(defn- crafting [recipes tags]
  (into []
        (map-indexed #(crafting-recipe tags %1 %2))
        (filter #(crafting-types (get (val %) "type")) recipes)))

(def ^:private cooking-types
  {"minecraft:smelting"         [:smelting 200]
   "minecraft:blasting"         [:blasting 100]
   "minecraft:smoking"          [:smoking 100]
   "minecraft:campfire_cooking" [:campfire 100]})

(defn- cooking-recipe [id json [type default]]
  (let [r (get json "result")
        r (if (string? r) {"id" r} r)]
    {:id       (kw id)
     :type     type
     :in       (ingredient (get json "ingredient"))
     :out      {:item (kw (get r "id")) :count (get r "count" 1)}
     :time     (get json "cookingtime" default)
     :xp       (flt (get json "experience" 0.0))
     :category (kw (get json "category" "misc"))}))

(defn- cooking [recipes]
  (into []
        (keep (fn [[id json]]
                (when-let [t (cooking-types (get json "type"))]
                  (cooking-recipe id json t))))
        recipes))

(def ^:private fuel-values
  [[:lava-bucket 20000] [:coal-block 16000] [:blaze-rod 2400]
   [:coal 1600] [:charcoal 1600]
   [{:tag "logs"} 300] [{:tag "bamboo_blocks"} 300]
   [{:tag "planks"} 300] [:bamboo-mosaic 300]
   [{:tag "wooden_stairs"} 300] [:bamboo-mosaic-stairs 300]
   [{:tag "wooden_slabs"} 150] [:bamboo-mosaic-slab 150]
   [{:tag "wooden_trapdoors"} 300]
   [{:tag "wooden_pressure_plates"} 300]
   [{:tag "wooden_shelves"} 300] [{:tag "wooden_fences"} 300]
   [{:tag "fence_gates"} 300] [:note-block 300] [:bookshelf 300]
   [:chiseled-bookshelf 300] [:lectern 300] [:jukebox 300]
   [:chest 300] [:trapped-chest 300] [:crafting-table 300]
   [:daylight-detector 300] [{:tag "banners"} 300] [:bow 300]
   [:fishing-rod 300] [:ladder 300]
   [{:tag "signs"} 200] [{:tag "hanging_signs"} 800]
   [:wooden-shovel 200] [:wooden-sword 200] [:wooden-spear 200]
   [:wooden-hoe 200] [:wooden-axe 200] [:wooden-pickaxe 200]
   [{:tag "wooden_doors"} 200] [{:tag "boats"} 1200]
   [{:tag "wool"} 100] [{:tag "wooden_buttons"} 100] [:stick 100]
   [{:tag "saplings"} 100] [:bowl 100]
   [{:tag "wool_carpets"} 67] [:dried-kelp-block 4001]
   [:crossbow 300] [:bamboo 50] [:dead-bush 100]
   [:short-dry-grass 100] [:tall-dry-grass 100] [:scaffolding 50]
   [:loom 300] [:barrel 300] [:cartography-table 300]
   [:fletching-table 300] [:smithing-table 300] [:composter 300]
   [:azalea 100] [:flowering-azalea 100] [:mangrove-roots 300]
   [:leaf-litter 100]])

(defn- fuel-targets [tags target]
  (if (map? target) (get-in tags ["item" (:tag target)] []) [target]))

(defn- fuel [tags items]
  (let [add (fn [m [target time]]
              (into m (comp (filter items) (map #(vector % time)))
                    (fuel-targets tags target)))]
    (apply dissoc (reduce add (sorted-map) fuel-values)
           (get-in tags ["item" "non_flammable_wood"] []))))

(def ^:private effect-colors
  "The colour and the category of every effect."
  {:speed [3402751 :beneficial]
   :slowness [9154528 :harmful]
   :haste [14270531 :beneficial]
   :mining-fatigue [4866583 :harmful]
   :strength [16762624 :beneficial]
   :instant-health [16262179 :beneficial]
   :instant-damage [11101546 :harmful]
   :jump-boost [16646020 :beneficial]
   :nausea [5578058 :harmful]
   :regeneration [13458603 :beneficial]
   :resistance [9520880 :beneficial]
   :fire-resistance [16750848 :beneficial]
   :water-breathing [10017472 :beneficial]
   :invisibility [16185078 :beneficial]
   :blindness [2039587 :harmful]
   :night-vision [12779366 :beneficial]
   :hunger [5797459 :harmful]
   :weakness [4738376 :harmful]
   :poison [8889187 :harmful]
   :wither [7561558 :harmful]
   :health-boost [16284963 :beneficial]
   :absorption [2445989 :beneficial]
   :saturation [16262179 :beneficial]
   :glowing [9740385 :neutral]
   :levitation [13565951 :harmful]
   :luck [5882118 :beneficial]
   :unluck [12624973 :harmful]
   :slow-falling [15978425 :beneficial]
   :conduit-power [1950417 :beneficial]
   :dolphins-grace [8954814 :beneficial]
   :bad-omen [745784 :neutral]
   :hero-of-the-village [4521796 :beneficial]
   :darkness [2696993 :harmful]
   :trial-omen [1484454 :neutral]
   :raid-omen [14565464 :neutral]
   :wind-charged [12438015 :harmful]
   :weaving [7891290 :harmful]
   :oozing [10092451 :harmful]
   :infested [9214860 :harmful]
   :breath-of-the-nautilus [65518 :beneficial]})

(def ^:private instant-effects
  "The effects that apply once rather than over a duration."
  #{:instant-health :instant-damage :saturation})

(def ^:private potion-effects
  "Every potion's effect rows.
  Each row is a triple of effect, duration and amplifier."
  {:water []
   :mundane []
   :thick []
   :awkward []
   :night-vision [[:night-vision 3600 0]]
   :long-night-vision [[:night-vision 9600 0]]
   :invisibility [[:invisibility 3600 0]]
   :long-invisibility [[:invisibility 9600 0]]
   :leaping [[:jump-boost 3600 0]]
   :long-leaping [[:jump-boost 9600 0]]
   :strong-leaping [[:jump-boost 1800 1]]
   :fire-resistance [[:fire-resistance 3600 0]]
   :long-fire-resistance [[:fire-resistance 9600 0]]
   :swiftness [[:speed 3600 0]]
   :long-swiftness [[:speed 9600 0]]
   :strong-swiftness [[:speed 1800 1]]
   :slowness [[:slowness 1800 0]]
   :long-slowness [[:slowness 4800 0]]
   :strong-slowness [[:slowness 400 3]]
   :turtle-master [[:slowness 400 3] [:resistance 400 2]]
   :long-turtle-master [[:slowness 800 3] [:resistance 800 2]]
   :strong-turtle-master [[:slowness 400 5] [:resistance 400 3]]
   :water-breathing [[:water-breathing 3600 0]]
   :long-water-breathing [[:water-breathing 9600 0]]
   :healing [[:instant-health 1 0]]
   :strong-healing [[:instant-health 1 1]]
   :harming [[:instant-damage 1 0]]
   :strong-harming [[:instant-damage 1 1]]
   :poison [[:poison 900 0]]
   :long-poison [[:poison 1800 0]]
   :strong-poison [[:poison 432 1]]
   :regeneration [[:regeneration 900 0]]
   :long-regeneration [[:regeneration 1800 0]]
   :strong-regeneration [[:regeneration 450 1]]
   :strength [[:strength 3600 0]]
   :long-strength [[:strength 9600 0]]
   :strong-strength [[:strength 1800 1]]
   :weakness [[:weakness 1800 0]]
   :long-weakness [[:weakness 4800 0]]
   :luck [[:luck 6000 0]]
   :slow-falling [[:slow-falling 1800 0]]
   :long-slow-falling [[:slow-falling 4800 0]]
   :wind-charged [[:wind-charged 3600 0]]
   :weaving [[:weaving 3600 0]]
   :oozing [[:oozing 3600 0]]
   :infested [[:infested 3600 0]]})

(defn- effect-entry [[k [color category]]]
  (sorted-map :category category :color color
              :instant? (contains? instant-effects k)))

(defn- effect-table [known]
  (into (sorted-map)
        (keep (fn [[k :as e]] (when (known k) [k (effect-entry e)])))
        effect-colors))

(defn- instance [[effect duration amplifier]]
  (sorted-map :amplifier amplifier :duration duration :effect effect))

(defn- potion-table [known]
  (into (sorted-map)
        (keep (fn [[k rows]]
                (when (known k) [k (mapv instance rows)])))
        potion-effects))

(def ^:private brewing-containers
  [:potion :splash-potion :lingering-potion])

(def ^:private container-recipes
  [[:potion :gunpowder :splash-potion]
   [:splash-potion :dragon-breath :lingering-potion]])

(def ^:private potion-mixes
  "Every ingredient mix that turns one potion into another. A :start
  row expands into both its water and its awkward mix."
  [[:water :glowstone-dust :thick]
   [:water :redstone :mundane]
   [:water :nether-wart :awkward]
   [:start :breeze-rod :wind-charged]
   [:start :slime-block :oozing]
   [:start :stone :infested]
   [:start :cobweb :weaving]
   [:awkward :golden-carrot :night-vision]
   [:night-vision :redstone :long-night-vision]
   [:night-vision :fermented-spider-eye :invisibility]
   [:long-night-vision :fermented-spider-eye :long-invisibility]
   [:invisibility :redstone :long-invisibility]
   [:start :magma-cream :fire-resistance]
   [:fire-resistance :redstone :long-fire-resistance]
   [:start :rabbit-foot :leaping]
   [:leaping :redstone :long-leaping]
   [:leaping :glowstone-dust :strong-leaping]
   [:leaping :fermented-spider-eye :slowness]
   [:long-leaping :fermented-spider-eye :long-slowness]
   [:slowness :redstone :long-slowness]
   [:slowness :glowstone-dust :strong-slowness]
   [:awkward :turtle-helmet :turtle-master]
   [:turtle-master :redstone :long-turtle-master]
   [:turtle-master :glowstone-dust :strong-turtle-master]
   [:swiftness :fermented-spider-eye :slowness]
   [:long-swiftness :fermented-spider-eye :long-slowness]
   [:start :sugar :swiftness]
   [:swiftness :redstone :long-swiftness]
   [:swiftness :glowstone-dust :strong-swiftness]
   [:awkward :pufferfish :water-breathing]
   [:water-breathing :redstone :long-water-breathing]
   [:start :glistering-melon-slice :healing]
   [:healing :glowstone-dust :strong-healing]
   [:healing :fermented-spider-eye :harming]
   [:strong-healing :fermented-spider-eye :strong-harming]
   [:harming :glowstone-dust :strong-harming]
   [:poison :fermented-spider-eye :harming]
   [:long-poison :fermented-spider-eye :harming]
   [:strong-poison :fermented-spider-eye :strong-harming]
   [:start :spider-eye :poison]
   [:poison :redstone :long-poison]
   [:poison :glowstone-dust :strong-poison]
   [:start :ghast-tear :regeneration]
   [:regeneration :redstone :long-regeneration]
   [:regeneration :glowstone-dust :strong-regeneration]
   [:start :blaze-powder :strength]
   [:strength :redstone :long-strength]
   [:strength :glowstone-dust :strong-strength]
   [:water :fermented-spider-eye :weakness]
   [:weakness :redstone :long-weakness]
   [:awkward :phantom-membrane :slow-falling]
   [:slow-falling :redstone :long-slow-falling]])

(defn- expand-mix [[from ingredient to]]
  (if (= :start from)
    [[:water ingredient :mundane] [:awkward ingredient to]]
    [[from ingredient to]]))

(defn- mix-entry [[from ingredient to]]
  {:from from :ingredient ingredient :to to})

(defn- mixes [rows known?]
  (into [] (comp (filter #(every? known? %)) (map mix-entry)) rows))

(defn- brewing [tags items potions]
  (let [mix? (fn [[from ingredient to]]
               (and (potions from) (items ingredient) (potions to)))
        xf (comp (filter mix?) (map mix-entry))
        rows (mapcat expand-mix potion-mixes)]
    {:containers      (filterv items brewing-containers)
     :container-mixes (mixes container-recipes items)
     :potion-mixes    (into [] xf rows)
     :fuel            (vec (get-in tags ["item" "brewing_fuel"]))}))

(defn- property-sets-of [tags recipes]
  (sorted-vals property-sets #(vec (property-set tags recipes %))))

(defn- recipes [zf tags dyes items potions]
  (let [named (->> (jsons zf "data/minecraft/recipe/")
                   (remove #(str/includes? (key %) "/")))
        rs (map val named)]
    {:stonecutting  (stonecutting rs)
     :property-sets (property-sets-of tags rs)
     :crafting      (crafting named tags)
     :cooking       (cooking named)
     :smithing      (smithing named tags)
     :fuel          (fuel tags items)
     :brewing       (brewing tags items potions)
     :dyes          dyes}))

(defn- tag-values [json]
  (mapv #(if (map? %) (get % "id") %) (get json "values")))

(defn- resolve-tags [found vs seen]
  (let [tag (fn [t]
              (if (seen t)
                []
                (resolve-tags found (found t []) (conj seen t))))
        one (fn [v]
              (if (str/starts-with? v "#")
                (tag (plain (subs v 1)))
                [(kw v)]))]
    (into [] (mapcat one) vs)))

(defn- tags-of [zf registries]
  (into (sorted-map)
        (keep (fn [reg]
                (let [prefix (str "data/minecraft/tags/" reg "/")
                      found (sorted-vals (jsons zf prefix) tag-values)
                      resolved #(resolve-tags found % #{})]
                  (when (seq found)
                    [reg (sorted-vals found resolved)]))))
        registries))

(defn- write-edn! [dir k data]
  (let [f (io/file dir (str (name k) ".edn"))]
    (io/make-parents f)
    (with-open [w (io/writer f)]
      (binding [*out* w *print-length* nil *print-level* nil]
        (pr data)
        (.write ^Writer w "\n")))))

(defn- tables [zf from-class reports rs]
  (let [{:keys [props shapes compost walls placers remainders
                banners dyes]}
        from-class
        dp (datapack-names zf)
        tags (tags-of zf (distinct (concat (keys rs) (keys dp))))
        item-names (set (keys (get rs "item")))
        potion-names (set (keys (get rs "potion")))
        effect-names (set (keys (get rs "mob_effect")))
        lang (read-json zf lang-file)
        items (merge-with
               merge (vanilla-items reports)
               compost walls remainders banners
               (station-items reports tags lang))]
    (merge (dissoc from-class :props :compost :walls :placers
                   :remainders :banners :dyes)
           {:packets    (packets reports)
            :blocks     (blocks reports props shapes)
            :registries rs
            :datapack   dp
            :drops      (block-drops zf)
            :entity-drops (entity-drops zf)
            :items      items
            :enchantments (enchantments zf tags)
            :dimension-types (dimension-types zf)
            :features   (features zf placers)
            :recipes    (recipes zf tags dyes item-names potion-names)
            :potions    (potion-table potion-names)
            :effects    (effect-table effect-names)
            :tags       tags})))

(defn- write-tables! [^File server ^File dir out from-class]
  (with-open [zf (ZipFile. server)]
    (let [ts (tables zf from-class dir (registries dir))]
      (doseq [[k data] ts]
        (write-edn! out k data))
      (write-edn! out :stamp (data/stamp))
      {:count (count ts) :dir (str out)})))

(defn- generate-tables! [^File bundle ^File server ^File dir out]
  (let [libraries (temp-dir "libraries")]
    (try (let [jars (unpack-libraries bundle libraries)
               from (from-classes jars server)]
           (write-tables! server dir out from))
         (finally (delete-tree! libraries)))))

(defn generate!
  [{:keys [version out jar]
    :or {version version out "target/data"}}]
  (let [bundle (fetch version jar)
        server (inner-jar bundle)
        dir (reports bundle)]
    (timed :tables #(generate-tables! bundle server dir out))))

(defn- emit-edn! [m]
  (println (pr-str m))
  (flush))

(defn -main
  "Generates the tables in this JVM.
  Each event is reported as edn on stdout."
  [opts]
  (binding [*progress* emit-edn!]
    (try (generate! (edn/read-string opts))
         (catch ExceptionInfo e
           (emit-edn! (assoc (ex-data e) :event :error))
           (System/exit 1))
         (catch Exception e
           (emit-edn! {:event :error
                       :what "data generator failed"
                       :why (str "The exception was: " e)})
           (System/exit 1))))
  (System/exit 0))
