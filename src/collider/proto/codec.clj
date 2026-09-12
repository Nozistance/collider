(ns collider.proto.codec
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]
            [collider.data :as data])
  (:import (collider.java Buf)
           (java.io ByteArrayInputStream ByteArrayOutputStream DataInputStream DataOutputStream
                    EOFException InputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.util UUID)
           (java.util.zip Deflater Inflater)))

(set! *warn-on-reflection* true)

(def protocol-version 776)
(def game-version "26.2")
(defn write-varint [^Buf buf v]
  (loop [v (bit-and (long v) 0xFFFFFFFF)]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (.writeByte buf (unchecked-int v))
      (do (.writeByte buf (unchecked-int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn read-varint ^long [^Buf buf]
  (loop [n 0 r 0]
    (let [b (long (.readByte buf))
          r (bit-or r (bit-shift-left (bit-and b 0x7F) (* 7 n)))]
      (if (zero? (bit-and b 0x80)) (long (unchecked-int r)) (recur (inc n) r)))))

(defn write-varlong [^Buf buf ^long v]
  (loop [v v]
    (if (zero? (bit-and v (bit-not 0x7F)))
      (.writeByte buf (int v))
      (do (.writeByte buf (int (bit-or (bit-and v 0x7F) 0x80)))
          (recur (unsigned-bit-shift-right v 7))))))

(defn write-string [^Buf buf ^String s]
  (let [bs (.getBytes s StandardCharsets/UTF_8)]
    (write-varint buf (alength bs))
    (.writeBytes buf bs)))

(defn read-string ^String [^Buf buf]
  (let [n (read-varint buf) bs (byte-array n)]
    (.readBytes buf bs)
    (String. bs StandardCharsets/UTF_8)))

(defn write-uuid [^Buf buf ^UUID u]
  (.writeLong buf (.getMostSignificantBits u))
  (.writeLong buf (.getLeastSignificantBits u)))

(defn read-uuid ^UUID [^Buf buf]
  (UUID. (.readLong buf) (.readLong buf)))

(defn write-id [^Buf buf k]
  (write-string buf
                (cond
                  (keyword? k) (data/wire k)
                  (str/includes? (str k) ":") (str k)
                  :else (str "minecraft:" k))))

(defn- write-nbt-string [^DataOutputStream d ^String name ^String v]
  (.writeByte d 8) (.writeUTF d name) (.writeUTF d v))

(declare write-translatable)

(defn- write-argument [^DataOutputStream d a]
  (if (map? a)
    (write-translatable d a)
    (do (write-nbt-string d "text" (str a)) (.writeByte d 0))))

(defn- write-translatable [^DataOutputStream d {:keys [translate with]}]
  (write-nbt-string d "translate" translate)
  (when (seq with)
    (.writeByte d 9) (.writeUTF d "with")
    (cond
      (every? number? with)
      (do (.writeByte d 3) (.writeInt d (count with))
          (doseq [a with] (.writeInt d (int a))))
      (every? string? with)
      (do (.writeByte d 8) (.writeInt d (count with))
          (doseq [a with] (.writeUTF d a)))
      :else
      (do (.writeByte d 10) (.writeInt d (count with))
          (doseq [a with] (write-argument d a)))))
  (.writeByte d 0))

(defn write-component [^Buf buf s]
  (let [bo (ByteArrayOutputStream.)]
    (with-open [d (DataOutputStream. bo)]
      (if (map? s)
        (do (.writeByte buf 10) (write-translatable d s))
        (do (.writeByte buf 8) (.writeUTF d ^String (str s)))))
    (.writeBytes buf (.toByteArray bo))))

(defn- nbt-type ^long [v]
  (cond (map? v) 10 (string? v) 8 (boolean? v) 1
        (instance? Byte v) 1 (instance? Short v) 2 (instance? Long v) 4
        (instance? Float v) 5 (instance? Double v) 6
        (bytes? v) 7 (instance? (Class/forName "[I") v) 11
        (instance? (Class/forName "[J") v) 12
        (integer? v) 3 (vector? v) 9
        :else (throw (ex-info "no NBT type" {:value v}))))

(defn- list-type ^long [v]
  (long (or (:nbt-type (meta v)) (if (empty? v) 0 (nbt-type (first v))))))

(defn- write-nbt-payload [^DataOutputStream d v]
  (cond
    (map? v) (do (doseq [[k x] v :when (some? x)]
                   (.writeByte d (nbt-type x)) (.writeUTF d (name k)) (write-nbt-payload d x))
                 (.writeByte d 0))
    (string? v) (.writeUTF d ^String v)
    (boolean? v) (.writeByte d (if v 1 0))
    (instance? Byte v) (.writeByte d (int ^Byte v))
    (instance? Short v) (.writeShort d (int ^Short v))
    (instance? Long v) (.writeLong d (long v))
    (instance? Float v) (.writeFloat d (float v))
    (instance? Double v) (.writeDouble d (double v))
    (bytes? v) (do (.writeInt d (alength ^bytes v)) (.write d ^bytes v))
    (instance? (Class/forName "[I") v)
    (do (.writeInt d (alength ^ints v)) (dotimes [i (alength ^ints v)] (.writeInt d (aget ^ints v i))))
    (instance? (Class/forName "[J") v)
    (do (.writeInt d (alength ^longs v)) (dotimes [i (alength ^longs v)] (.writeLong d (aget ^longs v i))))
    (integer? v) (.writeInt d (int v))
    (vector? v) (do (.writeByte d (list-type v))
                    (.writeInt d (count v))
                    (doseq [x v] (write-nbt-payload d x)))))

(defn write-nbt [^Buf buf v]
  (let [bo (ByteArrayOutputStream.)]
    (with-open [d (DataOutputStream. bo)]
      (if (nil? v)
        (.writeByte d 0)
        (do (.writeByte d (nbt-type v))
            (write-nbt-payload d v))))
    (.writeBytes buf (.toByteArray bo))))

(defn- read-nbt-payload [^DataInputStream d ^long t]
  (case (int t)
    1 (Byte/valueOf (.readByte d))
    2 (Short/valueOf (.readShort d))
    3 (Integer/valueOf (.readInt d))
    4 (Long/valueOf (.readLong d))
    5 (Float/valueOf (.readFloat d))
    6 (Double/valueOf (.readDouble d))
    7 (let [n (.readInt d) b (byte-array n)] (.readFully d b) b)
    8 (.readUTF d)
    9 (let [et (long (.readByte d)) n (.readInt d)]
        (with-meta (mapv (fn [_] (read-nbt-payload d et)) (range n)) {:nbt-type et}))
    10 (loop [acc []]
         (let [et (long (.readByte d))]
           (if (zero? et)
             (apply array-map (apply concat acc))
             (let [nm (.readUTF d)] (recur (conj acc [(keyword nm) (read-nbt-payload d et)]))))))
    11 (let [n (.readInt d) a (int-array n)] (dotimes [i n] (aset a i (.readInt d))) a)
    12 (let [n (.readInt d) a (long-array n)] (dotimes [i n] (aset a i (.readLong d))) a)
    (throw (ex-info "unknown NBT tag" {:tag t}))))

(defn read-nbt [^Buf buf]
  (let [in (ByteArrayInputStream. (.a buf) (.r buf) (- (.w buf) (.r buf)))
        d (DataInputStream. in)
        t (long (.readByte d))
        v (when-not (zero? t) (read-nbt-payload d t))]
    (set! (.r buf) (- (.w buf) (.available in)))
    v))

(defn write-angle [^Buf buf ^double deg]
  (.writeByte buf (unchecked-int (Math/floor (/ (* deg 256.0) 360.0)))))

(defn write-vec3 [^Buf buf [x y z]]
  (.writeDouble buf (double x)) (.writeDouble buf (double y)) (.writeDouble buf (double z)))

(defn- lp-pack ^long [^double v]
  (Math/round (* (+ (* v 0.5) 0.5) 32766.0)))

(defn write-lp-vec3 [^Buf buf [x y z]]
  (let [x (double x) y (double y) z (double z)
        m (max (Math/abs x) (Math/abs y) (Math/abs z))]
    (if (< m 3.051944088384301E-5)
      (.writeByte buf 0)
      (let [scale (long (Math/ceil m))
            partial? (not= (bit-and scale 3) scale)
            markers (if partial? (bit-or (bit-and scale 3) 4) scale)
            buffer (bit-or markers
                           (bit-shift-left (lp-pack (/ x scale)) 3)
                           (bit-shift-left (lp-pack (/ y scale)) 18)
                           (bit-shift-left (lp-pack (/ z scale)) 33))]
        (.writeByte buf (unchecked-int buffer))
        (.writeByte buf (unchecked-int (bit-shift-right buffer 8)))
        (.writeInt buf (unchecked-int (bit-shift-right buffer 16)))
        (when partial? (write-varint buf (bit-shift-right scale 2)))))))

(defn write-block-pos [^Buf buf ^long x ^long y ^long z]
  (.writeLong buf (bit-or (bit-shift-left (bit-and x 0x3FFFFFF) 38)
                          (bit-shift-left (bit-and z 0x3FFFFFF) 12)
                          (bit-and y 0xFFF))))

(defn read-block-pos [^Buf buf]
  (let [v (.readLong buf)]
    [(bit-shift-right v 38)
     (bit-shift-right (bit-shift-left v 52) 52)
     (bit-shift-right (bit-shift-left v 26) 38)]))

(defn section-pos ^long [^long sx ^long sy ^long sz]
  (bit-or (bit-shift-left (bit-and sx 0x3FFFFF) 42)
          (bit-shift-left (bit-and sz 0x3FFFFF) 20)
          (bit-and sy 0xFFFFF)))

(defn write-list [^Buf buf xs f]
  (write-varint buf (count xs))
  (doseq [x xs] (f buf x)))

(defn write-holder-ref [^Buf buf ^long id]
  (write-varint buf (inc id)))

(defn read-id [^Buf buf]
  (data/kebab (read-string buf)))

(declare components read-patch write-patch)

(defn- codec [r w] {:r r :w w})

(def ^:private c-bool (codec (fn [^Buf b] (.readBoolean b)) (fn [^Buf b v] (.writeBoolean b (boolean v)))))
(def ^:private c-varint (codec (fn [^Buf b] (read-varint b)) (fn [^Buf b v] (write-varint b (long v)))))
(def ^:private c-int (codec (fn [^Buf b] (long (.readInt b))) (fn [^Buf b v] (.writeInt b (int v)))))
(def ^:private c-float (codec (fn [^Buf b] (.readFloat b)) (fn [^Buf b v] (.writeFloat b (float v)))))
(def ^:private c-double (codec (fn [^Buf b] (.readDouble b)) (fn [^Buf b v] (.writeDouble b (double v)))))
(def ^:private c-string (codec (fn [^Buf b] (read-string b)) (fn [^Buf b v] (write-string b (str v)))))
(def ^:private c-ident (codec read-id (fn [^Buf b v] (write-id b v))))
(def ^:private c-uuid (codec (fn [^Buf b] (read-uuid b)) (fn [^Buf b v] (write-uuid b v))))
(def ^:private c-nbt (codec (fn [^Buf b] (read-nbt b)) (fn [^Buf b v] (write-nbt b v))))
(def ^:private c-text c-nbt)
(def ^:private c-unit (codec (fn [^Buf _] true) (fn [^Buf _ _] nil)))
(def ^:private c-block-pos
  (codec (fn [^Buf b] (read-block-pos b))
         (fn [^Buf b [x y z]] (write-block-pos b (long x) (long y) (long z)))))

(defn- c-opt [{:keys [r w]}]
  (codec (fn [^Buf b] (when (.readBoolean b) (r b)))
         (fn [^Buf b v] (.writeBoolean b (some? v)) (when (some? v) (w b v)))))

(defn- c-list [{:keys [r w]}]
  (codec (fn [^Buf b] (let [n (read-varint b)] (mapv (fn [_] (r b)) (range n))))
         (fn [^Buf b v] (write-varint b (count v)) (doseq [x v] (w b x)))))

(defn- c-map [k v]
  (codec (fn [^Buf b]
           (let [n (read-varint b)]
             (apply array-map (mapcat (fn [_] [((:r k) b) ((:r v) b)]) (range n)))))
         (fn [^Buf b m]
           (write-varint b (count m))
           (doseq [[a x] m] ((:w k) b a) ((:w v) b x)))))

(defn- c-either [l r]
  (codec (fn [^Buf b] (if (.readBoolean b) {:left ((:r l) b)} {:right ((:r r) b)}))
         (fn [^Buf b v]
           (if (contains? v :left)
             (do (.writeBoolean b true) ((:w l) b (:left v)))
             (do (.writeBoolean b false) ((:w r) b (:right v)))))))

(defn- record-codec [& kvs]
  (let [fields (mapv vec (partition 2 kvs))
        ks (mapv first fields)]
    (codec (fn [^Buf b]
             (apply array-map (interleave ks (mapv (fn [[_ c]] ((:r c) b)) fields))))
           (fn [^Buf b v] (doseq [[k c] fields] ((:w c) b (get v k)))))))

(defn- c-enum [names]
  (let [by-id (vec names)
        by-name (into {} (map-indexed (fn [i n] [n (long i)])) names)]
    (codec (fn [^Buf b] (let [i (read-varint b)] (get by-id i i)))
           (fn [^Buf b v] (write-varint b (long (if (keyword? v) (get by-name v) v)))))))

(defn- c-reg [registry]
  (codec (fn [^Buf b] (data/entry-name registry (read-varint b)))
         (fn [^Buf b v] (write-varint b (data/entry-id registry v)))))

(defn- c-holder [registry direct]
  (codec (fn [^Buf b]
           (let [i (read-varint b)]
             (if (zero? i) {:direct ((:r direct) b)} (data/entry-name registry (dec i)))))
         (fn [^Buf b v]
           (if (map? v)
             (do (write-varint b 0) ((:w direct) b (:direct v)))
             (write-varint b (inc (data/entry-id registry v)))))))

(defn- c-holder-set [registry]
  (codec (fn [^Buf b]
           (let [n (dec (read-varint b))]
             (if (neg? n)
               {:tag (read-id b)}
               (mapv (fn [_] (data/entry-name registry (read-varint b))) (range n)))))
         (fn [^Buf b v]
           (if (map? v)
             (do (write-varint b 0) (write-id b (:tag v)))
             (do (write-varint b (inc (count v)))
                 (doseq [x v] (write-varint b (data/entry-id registry x))))))))

(defn- c-filterable [inner] (record-codec :raw inner :filtered (c-opt inner)))

(def ^:private dye-colors
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])
(def ^:private c-dye (c-enum dye-colors))

(def ^:private c-sound
  (c-holder "sound_event" (record-codec :sound c-ident :range (c-opt c-float))))

(def ^:private c-effect-details
  (let [self (promise)]
    @(deliver self
              (record-codec :amplifier c-varint :duration c-varint :ambient c-bool
                            :show-particles c-bool :show-icon c-bool
                            :hidden (c-opt (codec (fn [^Buf b] ((:r @self) b))
                                                  (fn [^Buf b v] ((:w @self) b v))))))))

(def ^:private c-effect-instance
  (record-codec :effect (c-reg "mob_effect") :details c-effect-details))

(def ^:private consume-effects
  {:apply-effects     (record-codec :effects (c-list c-effect-instance) :probability c-float)
   :remove-effects    (record-codec :effects (c-holder-set "mob_effect"))
   :clear-all-effects (record-codec)
   :teleport-randomly (record-codec :diameter c-float)
   :play-sound        (record-codec :sound c-sound)})

(def ^:private c-consume-effect
  (codec (fn [^Buf b]
           (let [t (data/entry-name "consume_effect_type" (read-varint b))]
             (assoc ((:r (get consume-effects t)) b) :type t)))
         (fn [^Buf b v]
           (write-varint b (data/entry-id "consume_effect_type" (:type v)))
           ((:w (get consume-effects (:type v))) b v))))

(def ^:private c-template
  (record-codec :item (c-reg "item") :count c-varint
                :patch (codec (fn [^Buf b] (read-patch b)) (fn [^Buf b v] (write-patch b v)))))

(def ^:private c-typed-component
  (codec (fn [^Buf b]
           (let [t (data/entry-name "data_component_type" (read-varint b))]
             [t ((:r (get components t)) b)]))
         (fn [^Buf b [t v]]
           (write-varint b (data/entry-id "data_component_type" t))
           ((:w (get components t)) b v))))

(def ^:private c-no-predicates
  (codec (fn [^Buf b]
           (let [n (read-varint b)]
             (when (pos? n) (throw (ex-info "component predicates not supported" {:count n})))
             []))
         (fn [^Buf b v]
           (when (seq v) (throw (ex-info "component predicates not supported" {})))
           (write-varint b 0))))

(def ^:private c-state-matcher
  (c-either (record-codec :value c-string)
            (record-codec :min (c-opt c-string) :max (c-opt c-string))))

(def ^:private c-block-predicate
  (record-codec :blocks (c-opt (c-holder-set "block"))
                :state (c-opt (c-list (record-codec :name c-string :matcher c-state-matcher)))
                :nbt (c-opt c-nbt)
                :exact (c-list c-typed-component)
                :partial c-no-predicates))

(def ^:private c-adventure (record-codec :predicates (c-list c-block-predicate)))

(def ^:private c-attribute-display
  (let [types {0 (record-codec) 1 (record-codec) 2 (record-codec :text c-text)}]
    (codec (fn [^Buf b] (let [t (read-varint b)] (assoc ((:r (get types t)) b) :display t)))
           (fn [^Buf b v] (let [t (long (:display v 0))]
                            (write-varint b t) ((:w (get types t)) b v))))))

(def ^:private c-attribute-entry
  (record-codec :attribute (c-reg "attribute")
                :modifier (record-codec :id c-ident :amount c-double
                                        :operation (c-enum [:add-value :add-multiplied-base
                                                            :add-multiplied-total]))
                :slot (c-enum [:any :mainhand :offhand :hand :feet :legs :chest :head
                               :armor :body :saddle])
                :display c-attribute-display))

(def ^:private c-tool-rule
  (record-codec :blocks (c-holder-set "block") :speed (c-opt c-float)
                :correct-for-drops (c-opt c-bool)))

(def ^:private c-damage-reduction
  (record-codec :horizontal-blocking-angle c-float
                :type (c-opt (c-holder-set "damage_type"))
                :base c-float :factor c-float))

(def ^:private c-kinetic-condition
  (record-codec :max-duration-ticks c-varint :min-speed c-float :min-relative-speed c-float))

(def ^:private c-firework-explosion
  (record-codec :shape (c-enum [:small-ball :large-ball :star :creeper :burst])
                :colors (c-list c-int) :fade-colors (c-list c-int)
                :trail c-bool :twinkle c-bool))

(def ^:private c-game-profile-properties
  (c-list (record-codec :name c-string :value c-string :signature (c-opt c-string))))

(def ^:private c-profile
  (record-codec
    :profile (c-either (record-codec :id c-uuid :name c-string
                                     :properties c-game-profile-properties)
                       (record-codec :name (c-opt c-string) :id (c-opt c-uuid)
                                     :properties c-game-profile-properties))
    :skin (record-codec :body (c-opt c-ident) :cape (c-opt c-ident) :elytra (c-opt c-ident)
                        :model (c-opt (c-enum [:wide :slim])))))

(def ^:private c-typed-entity-data
  (fn [type-codec] (record-codec :type type-codec :data c-nbt)))

(def ^:private c-instrument
  (c-holder "instrument" (record-codec :sound c-sound :use-duration c-float
                                       :range c-float :description c-text)))

(def ^:private c-material-assets
  (record-codec :base c-string
                :overrides (c-map c-ident c-string)))

(def ^:private c-trim-material
  (c-holder "trim_material" (record-codec :assets c-material-assets :description c-text)))

(def ^:private c-trim-pattern
  (c-holder "trim_pattern" (record-codec :asset c-ident :description c-text :decal c-bool)))

(def ^:private c-banner-pattern
  (c-holder "banner_pattern" (record-codec :asset c-ident :translation-key c-string)))

(def ^:private c-jukebox-song
  (c-holder "jukebox_song" (record-codec :sound c-sound :description c-text
                                         :length c-float :comparator-output c-varint)))

(def ^:private c-painting-variant
  (c-holder "painting_variant" (record-codec :width c-varint :height c-varint :asset c-ident
                                             :title (c-opt c-text) :author (c-opt c-text))))

(def components
  {:custom-data                 c-nbt
   :max-stack-size              c-varint
   :max-damage                  c-varint
   :damage                      c-varint
   :unbreakable                 c-unit
   :use-effects                 (record-codec :can-sprint c-bool :interact-vibrations c-bool
                                              :speed-multiplier c-float)
   :custom-name                 c-text
   :minimum-attack-charge       c-float
   :damage-type                 (c-reg "damage_type")
   :item-name                   c-text
   :item-model                  c-ident
   :lore                        (c-list c-text)
   :rarity                      (c-enum [:common :uncommon :rare :epic])
   :enchantments                (c-map (c-reg "enchantment") c-varint)
   :can-place-on                c-adventure
   :can-break                   c-adventure
   :attribute-modifiers         (c-list c-attribute-entry)
   :custom-model-data           (record-codec :floats (c-list c-float) :flags (c-list c-bool)
                                              :strings (c-list c-string) :colors (c-list c-int))
   :tooltip-display             (record-codec :hide-tooltip c-bool
                                              :hidden (c-list (c-reg "data_component_type")))
   :repair-cost                 c-varint
   :creative-slot-lock          c-unit
   :enchantment-glint-override  c-bool
   :intangible-projectile       c-nbt
   :food                        (record-codec :nutrition c-varint :saturation c-float
                                              :can-always-eat c-bool)
   :consumable                  (record-codec :seconds c-float
                                              :animation (c-enum [:none :eat :drink :block :bow
                                                                  :trident :crossbow :spyglass
                                                                  :toot-horn :brush :bundle :spear])
                                              :sound c-sound :particles c-bool
                                              :on-consume (c-list c-consume-effect))
   :use-remainder               (record-codec :convert-into c-template)
   :use-cooldown                (record-codec :seconds c-float :group (c-opt c-ident))
   :damage-resistant            (record-codec :types (c-holder-set "damage_type"))
   :tool                        (record-codec :rules (c-list c-tool-rule)
                                              :default-mining-speed c-float
                                              :damage-per-block c-varint
                                              :destroy-in-creative c-bool)
   :weapon                      (record-codec :damage-per-attack c-varint
                                              :disable-blocking-seconds c-float)
   :attack-range                (record-codec :min-reach c-float :max-reach c-float
                                              :min-creative-reach c-float :max-creative-reach c-float
                                              :hitbox-margin c-float :mob-factor c-float)
   :enchantable                 c-varint
   :equippable                  (record-codec :slot (c-enum [:mainhand :feet :legs :chest :head
                                                             :offhand :body :saddle])
                                              :equip-sound c-sound
                                              :asset (c-opt c-ident) :camera-overlay (c-opt c-ident)
                                              :allowed-entities (c-opt (c-holder-set "entity_type"))
                                              :dispensable c-bool :swappable c-bool
                                              :damage-on-hurt c-bool :equip-on-interact c-bool
                                              :can-be-sheared c-bool :shearing-sound c-sound)
   :repairable                  (record-codec :items (c-holder-set "item"))
   :glider                      c-unit
   :tooltip-style               c-ident
   :death-protection            (record-codec :death-effects (c-list c-consume-effect))
   :blocks-attacks              (record-codec :block-delay-seconds c-float
                                              :disable-cooldown-scale c-float
                                              :damage-reductions (c-list c-damage-reduction)
                                              :item-damage (record-codec :threshold c-float
                                                                         :base c-float
                                                                         :factor c-float)
                                              :bypassed-by (c-opt (c-holder-set "damage_type"))
                                              :block-sound (c-opt c-sound)
                                              :disable-sound (c-opt c-sound))
   :piercing-weapon             (record-codec :deals-knockback c-bool :dismounts c-bool
                                              :sound (c-opt c-sound) :hit-sound (c-opt c-sound))
   :kinetic-weapon              (record-codec :contact-cooldown-ticks c-varint :delay-ticks c-varint
                                              :dismount (c-opt c-kinetic-condition)
                                              :knockback (c-opt c-kinetic-condition)
                                              :damage (c-opt c-kinetic-condition)
                                              :forward-movement c-float :damage-multiplier c-float
                                              :sound (c-opt c-sound) :hit-sound (c-opt c-sound))
   :swing-animation             (record-codec :type (c-enum [:none :whack :stab])
                                              :duration c-varint)
   :additional-trade-cost       c-varint
   :stored-enchantments         (c-map (c-reg "enchantment") c-varint)
   :dye                         c-dye
   :dyed-color                  c-int
   :map-color                   c-int
   :map-id                      c-varint
   :map-decorations             c-nbt
   :map-post-processing         (c-enum [:lock :scale])
   :charged-projectiles         (c-list c-template)
   :bundle-contents             (c-list c-template)
   :potion-contents             (record-codec :potion (c-opt (c-reg "potion"))
                                              :custom-color (c-opt c-int)
                                              :custom-effects (c-list c-effect-instance)
                                              :custom-name (c-opt c-string))
   :potion-duration-scale       c-float
   :suspicious-stew-effects     (c-list (record-codec :effect (c-reg "mob_effect")
                                                      :duration c-varint))
   :writable-book-content       (c-list (c-filterable c-string))
   :written-book-content        (record-codec :title (c-filterable c-string) :author c-string
                                              :generation c-varint
                                              :pages (c-list (c-filterable c-text))
                                              :resolved c-bool)
   :trim                        (record-codec :material c-trim-material :pattern c-trim-pattern)
   :debug-stick-state           c-nbt
   :entity-data                 (c-typed-entity-data (c-reg "entity_type"))
   :bucket-entity-data          c-nbt
   :block-entity-data           (c-typed-entity-data (c-reg "block_entity_type"))
   :instrument                  c-instrument
   :provides-trim-material      c-trim-material
   :ominous-bottle-amplifier    c-varint
   :jukebox-playable            (record-codec :song c-jukebox-song)
   :provides-banner-patterns    (c-holder-set "banner_pattern")
   :recipes                     c-nbt
   :lodestone-tracker           (record-codec :target (c-opt (record-codec :dimension c-ident
                                                                           :pos c-block-pos))
                                              :tracked c-bool)
   :firework-explosion          c-firework-explosion
   :fireworks                   (record-codec :flight-duration c-varint
                                              :explosions (c-list c-firework-explosion))
   :profile                     c-profile
   :note-block-sound            c-ident
   :banner-patterns             (c-list (record-codec :pattern c-banner-pattern :color c-dye))
   :base-color                  c-dye
   :pot-decorations             (c-list (c-reg "item"))
   :container                   (c-list (c-opt c-template))
   :block-state                 (c-map c-string c-string)
   :bees                        (c-list (record-codec :data (c-typed-entity-data
                                                              (c-reg "entity_type"))
                                                      :ticks-in-hive c-varint
                                                      :min-ticks-in-hive c-varint))
   :sulfur-cube-content         (record-codec :absorbed c-template)
   :lock                        c-nbt
   :container-loot              c-nbt
   :break-sound                 c-sound
   :villager/variant            (c-reg "villager_type")
   :wolf/variant                (c-reg "wolf_variant")
   :wolf/sound-variant          (c-reg "wolf_sound_variant")
   :wolf/collar                 c-dye
   :fox/variant                 c-varint
   :salmon/size                 c-varint
   :parrot/variant              c-varint
   :tropical-fish/pattern       c-varint
   :tropical-fish/base-color    c-dye
   :tropical-fish/pattern-color c-dye
   :mooshroom/variant           c-varint
   :rabbit/variant              c-varint
   :pig/variant                 (c-reg "pig_variant")
   :pig/sound-variant           (c-reg "pig_sound_variant")
   :cow/variant                 (c-reg "cow_variant")
   :cow/sound-variant           (c-reg "cow_sound_variant")
   :chicken/variant             (c-reg "chicken_variant")
   :chicken/sound-variant       (c-reg "chicken_sound_variant")
   :zombie-nautilus/variant     (c-reg "zombie_nautilus_variant")
   :frog/variant                (c-reg "frog_variant")
   :horse/variant               c-varint
   :painting/variant            c-painting-variant
   :llama/variant               c-varint
   :axolotl/variant             c-varint
   :cat/variant                 (c-reg "cat_variant")
   :cat/sound-variant           (c-reg "cat_sound_variant")
   :cat/collar                  c-dye
   :sheep/color                 c-dye
   :shulker/color               c-dye})

(defn- component-codec [kw]
  (or (get components kw)
      (throw (ex-info "no codec for data component" {:component kw}))))

(defn read-patch [^Buf buf]
  (let [added (read-varint buf)
        removed (read-varint buf)]
    (if (and (zero? added) (zero? removed))
      nil
      (let [cs (mapv (fn [_]
                       (let [k (data/entry-name "data_component_type" (read-varint buf))]
                         [k ((:r (component-codec k)) buf)]))
                     (range added))
            rs (mapv (fn [_] (data/entry-name "data_component_type" (read-varint buf)))
                     (range removed))]
        (cond-> {}
                (seq cs) (assoc :components (apply array-map (apply concat cs)))
                (seq rs) (assoc :removed (set rs)))))))

(defn write-patch [^Buf buf patch]
  (let [cs (:components patch)
        rs (sort-by #(data/entry-id "data_component_type" %) (:removed patch))]
    (write-varint buf (count cs))
    (write-varint buf (count rs))
    (doseq [[k v] cs]
      (write-varint buf (data/entry-id "data_component_type" k))
      ((:w (component-codec k)) buf v))
    (doseq [k rs] (write-varint buf (data/entry-id "data_component_type" k)))))

(defn write-item-stack [^Buf buf stack]
  (if (nil? stack)
    (write-varint buf 0)
    (do (write-varint buf (long (:count stack 1)))
        (write-varint buf (data/registry-id "item" (:item stack)))
        (write-patch buf stack))))

(defn read-item-stack [^Buf buf]
  (let [n (read-varint buf)]
    (when (pos? n)
      (let [item (data/entry-name "item" (read-varint buf))]
        (merge {:item item :count n} (read-patch buf))))))

(defn read-hashed-stack [^Buf buf]
  (when (.readBoolean buf)
    (let [item (read-varint buf)
          n (read-varint buf)
          added (read-varint buf)]
      (dotimes [_ added] (read-varint buf) (.readInt buf))
      (let [removed (read-varint buf)]
        (dotimes [_ removed] (read-varint buf))
        (cond-> {:item (data/entry-name "item" item) :count n}
                (or (pos? (long added)) (pos? (long removed))) (assoc :components? true))))))

(def ^:private data-types {:byte 0 :int 1 :float 3 :item 7 :boolean 8 :block-pos 10 :optional-block-pos 11 :block-state 14 :pose 20})
(defn write-entity-data [^Buf buf entries]
  (doseq [[idx type v] entries]
    (.writeByte buf (int idx))
    (write-varint buf (data-types type))
    (case type
      :byte (.writeByte buf (int v))
      :int (write-varint buf (long v))
      :float (.writeFloat buf (float v))
      :item (write-item-stack buf v)
      :boolean (.writeBoolean buf (boolean v))
      :block-pos (let [[x y z] v] (write-block-pos buf (long x) (long y) (long z)))
      :optional-block-pos (do (.writeBoolean buf (some? v))
                              (when v (let [[x y z] v] (write-block-pos buf (long x) (long y) (long z)))))
      :block-state (write-varint buf (long v))
      :pose (write-varint buf (long v))))
  (.writeByte buf 0xFF))

(defn offline-uuid ^UUID [^String name]
  (UUID/nameUUIDFromBytes (.getBytes (str "OfflinePlayer:" name) StandardCharsets/UTF_8)))

(def ^:private ^:const max-uncompressed 8388608)
(defn- read-varint-stream ^long [^InputStream in]
  (loop [n 0 acc 0]
    (let [b (.read in)]
      (when (neg? b) (throw (EOFException. "end of stream")))
      (let [acc (bit-or acc (bit-shift-left (bit-and b 0x7F) (* n 7)))]
        (cond
          (zero? (bit-and b 0x80)) acc
          (>= n 2) (throw (ex-info "frame length varint too long" {}))
          :else (recur (inc n) acc))))))

(def ^:private ^:const frame-keep 8192)
(defn read-frame! ^Buf [^InputStream in ^Buf buf]
  (let [len (read-varint-stream in)]
    (.clear buf frame-keep)
    (.readFrom buf in (int len))
    buf))

(defn decompress! ^Buf [^Buf buf ^long threshold ^Inflater inflater]
  (when-not (neg? threshold)
    (let [n (read-varint buf)]
      (when (pos? n)
        (when (< n threshold)
          (throw (ex-info "badly compressed packet" {:size n :threshold threshold})))
        (when (> n max-uncompressed)
          (throw (ex-info "badly compressed packet" {:size n :max max-uncompressed})))
        (let [src (byte-array (.readableBytes buf))
              dst (byte-array n)]
          (.readBytes buf src)
          (.setInput inflater src)
          (let [got (.inflate inflater dst)]
            (.reset inflater)
            (when (not= got n)
              (throw (ex-info "badly compressed packet" {:got got :expected n}))))
          (.clear buf)
          (.writeBytes buf dst)))))
  buf)

(defn write-frame! [^OutputStream out ^Buf payload ^Buf body ^Buf head threshold ^Deflater deflater ^bytes chunk]
  (.clear body)
  (.clear head)
  (if (neg? (long threshold))
    (.writeBytes body payload)
    (let [n (.readableBytes payload)]
      (if (< n (long threshold))
        (do (write-varint body 0) (.writeBytes body payload))
        (let [src (byte-array n)]
          (.readBytes payload src)
          (write-varint body n)
          (.setInput deflater src)
          (.finish deflater)
          (while (not (.finished deflater))
            (.writeBytes body chunk 0 (.deflate deflater chunk)))
          (.reset deflater)))))
  (write-varint head (.readableBytes body))
  (.writeTo head out)
  (.writeTo body out))
