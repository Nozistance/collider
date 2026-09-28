(ns collider.tables.classes
  "Reading tables from the classes of the vanilla server."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.tables.files :as files]
            [collider.tables.pack :as pack]
            [collider.tables.reflect
             :refer [*loader* cls call call-static static-field]]
            [collider.tables.value
             :refer [kw flt sorted-vals unknown]])
  (:import (clojure.lang Reflector)
           (java.io File)
           (java.lang.reflect Field InvocationHandler Proxy)
           (java.net URI URL URLClassLoader URLConnection)
           (java.util.zip ZipFile)))

(set! *warn-on-reflection* true)

(defn- unpack-libraries [^File bundle ^File tmp]
  (with-open [zf (ZipFile/new bundle)]
    (doall
      (for [n (files/zip-names zf)
            :when (re-matches #"META-INF/libraries/.*\.jar" n)
            :let [f (io/file tmp (str/replace n "/" "_"))]]
        (files/unzip zf n f)))))

(defn- class-loader ^URLClassLoader [jars ^File server]
  (let [urls (map #(URI/.toURL (File/.toURI %)) (cons server jars))]
    (URLClassLoader/new (into-array URL urls)
                       (ClassLoader/getPlatformClassLoader))))

(defn- field-value [obj f]
  (Reflector/getInstanceField obj f))

(defn- field-name [^Field f] (Field/.getName f))

(defn- declared-fields [^Class c]
  (->> (iterate Class/.getSuperclass c)
       (take-while some?)
       (mapcat #(sort-by field-name (Class/.getDeclaredFields %)))))

(defn- hidden-field [^Class c obj want]
  (let [match? (if (string? want)
                 #(= want (Field/.getName %))
                 #(= want (Field/.getType %)))]
    (when-let [^Field f (first (filter match? (declared-fields c)))]
      (Field/.get (doto f (Field/.setAccessible true)) obj))))

(defn- key-of [reg x] (kw (str (call reg "getKey" x))))

(defn- elements [reg] (iterator-seq (Iterable/.iterator reg)))

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

(defn- palette-runs [pairs]
  (let [rs (runs pairs)
        palette (into [] (comp (map peek) (distinct)) rs)
        index (zipmap palette (range))]
    {:palette palette
     :runs (mapv (fn [[lo hi v]] [lo hi (index v)]) rs)}))

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
  (palette-runs (for [[id st] states
                      :let [v (call st m)]
                      :when (keep? v)]
                  [id v])))

(defn- light-flags [states pred]
  (palette-runs (for [[id st] states :when (pred st)] [id true])))

(defn- light-table [states]
  (let [vals-of (fn [m p] (light-values states m p))]
    {:dampening (vals-of "getLightDampening" #(not= 15 (long %)))
     :emission (vals-of "getLightEmission" #(pos? (long %)))
     :occludes  (light-flags states occludes?)
     :use-shape (light-flags states shaped?)
     :faces     (palette-runs (occluding-states states))}))

(defn- state-shapes [states]
  (let [env (shape-env)
        each (fn [f] (palette-runs (per-state states f)))
        sturdy (fn [& more]
                 (each #(apply state-sturdy env % more)))]
    {:shapes (each #(partial-box (collision-shape env %)))
     :outlines (each #(partial-box (outline-shape env %)))
     :flags (each #(state-flags env %))
     :sturdy {:full (sturdy)
              :center (sturdy (:center env))
              :rigid (sturdy (:rigid env))}}))

(def ^:private sound-parts
  {:break "Break" :step "Step" :place "Place"
   :hit "Hit" :fall "Fall"})

(defn- sound-events [o]
  (-> (sorted-vals sound-parts
                   (fn [part]
                     (let [event (call o (str "get" part "Sound"))]
                       (kw (str (call event "location"))))))
      (assoc :volume (flt (call o "getVolume"))
             :pitch (flt (call o "getPitch")))))

(defn- sound-types []
  (let [c (cls "world.level.block.SoundType")]
    (for [^Field f (Class/.getFields c)
          :when (= c (Field/.getType f))
          :let [o (Field/.get f nil)]]
      [(kw (str/lower-case (Field/.getName f))) o (sound-events o)])))

(defn- simple-name [^Class c]
  (if (str/blank? (Class/.getSimpleName c))
    (recur (Class/.getSuperclass c))
    (Class/.getSimpleName c)))

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
          (when (Class/.isInstance (cls block) b)
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

(defn- motion-props [field]
  (into {}
        (keep (fn [[k n ^double d]]
                (let [v (flt (field n))]
                  (when (not= v d) [k v]))))
        motion-fields))

(defn- instrument [st]
  (let [i (call st "instrument")
        k (keyword (call i "getSerializedName"))]
    (cond-> {}
      (not= :harp k) (assoc :instrument k)
      (call i "worksAboveNoteBlock")
      (assoc :instrument-above? true))))

(defn- level-stub [access]
  (Proxy/newProxyInstance
    *loader* (into-array Class [(cls "world.level.LevelReader")])
    (reify InvocationHandler
      (invoke [_ _ m _]
        (when (= "registryAccess" (call m "getName")) access)))))

(defn- bind-bare-components! [items]
  (let [bare (static-field "core.component.DataComponentMap" "EMPTY")]
    (doseq [i (elements items)]
      (call (call i "builtInRegistryHolder") "bindComponents" bare))))

(defn- clone-env []
  (let [reg (registry "REGISTRY")
        access (call-static "core.RegistryAccess"
                 "fromRegistryOfRegistries" reg)
        block "world.level.block.Block"
        components "core.component.DataComponents"]
    {:level (level-stub access)
     :zero (static-field "core.BlockPos" "ZERO")
     :items (registry "ITEM")
     :ids (static-field block "BLOCK_STATE_REGISTRY")
     :state-kind (static-field components "BLOCK_STATE")}))

(defn- cloned [{:keys [level zero]} st data?]
  (call st "getCloneItemStack" level zero data?))

(defn- clone-item [env st]
  (key-of (:items env) (call (cloned env st false) "getItem")))

(defn- stack-props [env stack]
  (when-let [p (call stack "get" (:state-kind env))]
    (into (sorted-set) (map kw) (keys (call p "properties")))))

(defn- other-clones [env b item]
  (into (sorted-map)
        (keep (fn [st]
                (let [i (clone-item env st)]
                  (when (not= i item)
                    [(call (:ids env) "getId" st) i]))))
        (call (call b "getStateDefinition") "getPossibleStates")))

(defn- clone-props [env reg b]
  (let [st (call b "defaultBlockState")
        item (clone-item env st)
        own (stack-props env (cloned env st false))
        data (reduce disj (stack-props env (cloned env st true)) own)
        others (other-clones env b item)]
    (cond-> {}
      (not= item (key-of reg b)) (assoc :clone item)
      (seq others) (assoc :clones others)
      (seq own) (assoc :clone-props own)
      (seq data) (assoc :data-props data))))

(defn- own-props [by-type env reg b]
  (let [field #(hidden-field (class b) b %)
        st (call b "defaultBlockState")]
    (merge {:resistance (flt (field "explosionResistance"))
            :sound      (by-type (call st "getSoundType"))
            :class      (block-class b)}
           (motion-props field)
           (instrument st)
           (toggle b)
           (clone-props env reg b))))

(defn- weathering-pairs [reg]
  (block-pairs reg "world.level.block.WeatheringCopper"
               "NEXT_BY_BLOCK" :next :previous))

(defn- waxable-pairs [reg]
  (block-pairs reg "world.item.HoneycombItem"
               "WAXABLES" :waxed :unwaxed))

(defn- block-table [reg by-type]
  (let [refs (block-refs reg)
        env (clone-env)
        own (into (sorted-map)
                  (for [b (elements reg)]
                    [(key-of reg b) (own-props by-type env reg b)]))]
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
                :when (Class/.isInstance c i)
                :let [wall (hidden-field (class i) i "wallBlock")]]
            [(key-of items i) {:wall (key-of blocks wall)}]))))

(defn- solid-buckets []
  (let [items (registry "ITEM")
        c (cls "world.item.SolidBucketItem")]
    (into (sorted-map)
          (for [i (elements items)
                :when (Class/.isInstance c i)
                :let [e (hidden-field c i "placeSound")]]
            [(key-of items i)
             {:place-sound (kw (str (call e "location")))}]))))

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
          (for [b (elements reg) :when (Class/.isInstance c b)]
            (let [f (hidden-field (class b) b "feature")]
              [(key-of reg b)
               (kw (str (call f "identifier")))])))))

(defn- template [reg t]
  (when t
    (when-not (call (call t "components") "isEmpty")
      (throw (unknown "remainder with components"
                      {:template (str t)})))
    {:item (key-of reg (call (call t "item") "value"))
     :count (call t "count")}))

(defn- non-breakers []
  (let [items (registry "ITEM")
        stone (call (static-field "world.level.block.Blocks" "STONE")
                    "defaultBlockState")]
    (into (sorted-map)
          (for [i (elements items)
                :let [s (call i "getDefaultInstance")
                      args [s stone nil nil nil]]
                :when (not (apply call i "canDestroyBlock" args))]
            [(key-of items i) {:breaks? false}]))))

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
          (for [i (elements items) :when (Class/.isInstance c i)]
            (let [dye (call i "getColor")
                  color (call dye "getSerializedName")]
              [(key-of items i) {:banner-color (kw color)}])))))

(defn- dye-colors []
  (into (sorted-map)
        (for [d (call-static "world.item.DyeColor" "values")]
          [(kw (call d "getSerializedName"))
           {:firework (call d "getFireworkColor")
            :diffuse  (call d "getTextureDiffuseColor")}])))

(defn- registry-path [data]
  (-> (call data "key") (call "identifier") (call "getPath")))

(defn- synced-registries []
  (mapv registry-path
        (static-field "resources.RegistryDataLoader"
                      "SYNCHRONIZED_REGISTRIES")))

(defn- from-classes [loader]
  (binding [*loader* loader]
    (call-static "SharedConstants" "tryDetectVersion")
    (call-static "server.Bootstrap" "bootStrap")
    (bind-bare-components! (registry "ITEM"))
    (let [states (block-states)]
      (merge (state-shapes states) (block-props)
             {:non-breakers (non-breakers)}
             {:light (light-table states) :fire (fire-odds)
              :placers (placer-features (registry "BLOCK"))
              :compost (compostables)
              :walls (merge (wall-items) (solid-buckets))
              :remainders (remainders) :banners (banner-colors)
              :dyes (dye-colors) :synced (synced-registries)
              :pack (pack/registries)}))))

(def ^:private silent-log4j
  (str "<Configuration status=\"OFF\">"
       "<Loggers><Root level=\"off\"/></Loggers></Configuration>"))

(defn- quiet-log4j!
  "Keeps the log of the vanilla classes out of the working directory."
  [^File dir]
  (let [f (io/file dir "log4j2.xml")]
    (spit f silent-log4j)
    (System/setProperty "log4j2.configurationFile" (str f))))

(defn read-classes
  "Returns what the tables need from the classes of the server."
  [^File bundle ^File server]
  (let [libraries (files/temp-dir "libraries")]
    (quiet-log4j! libraries)
    (URLConnection/setDefaultUseCaches "jar" false)
    (try (with-open [l (class-loader
                        (unpack-libraries bundle libraries) server)]
           (from-classes l))
         (finally (files/delete-tree! libraries)))))
