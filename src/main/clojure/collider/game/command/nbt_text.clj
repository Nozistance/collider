(ns collider.game.command.nbt-text
  "Tags as chat shows them, coloured as /data prints them."
  (:require [collider.hash-order :as hash-order]))

(set! *warn-on-reflection* true)

(def ^:private ^:const max-depth 64)

(def ^:private ^:const max-length 128)

(def ^:private ^:const inline-limit 8)

(def ^:private simple #"[A-Za-z0-9._+-]+")

(defn- lit
  ([s] {:text s})
  ([s color] {:text s :color color}))

(def ^:private folded (lit "<...>" "gray"))

(def ^:private gap (lit " "))

(def ^:private comma (lit ","))

(defn- kind-mark [s] (lit s "red"))

(def ^:private suffixes
  {Byte "b" Short "s" Long "L" Float "f" Double "d"})

(defn- control [c]
  (case c
    \backspace "\\b" \tab "\\t" \newline "\\n" \formfeed "\\f"
    \return "\\r"
    (when (< (int c) 32) (format "\\x%02X" (int c)))))

(defn- quote-step [[q ^StringBuilder sb] c]
  (cond (= c \\) [q (.append sb "\\\\")]
        (or (= c \") (= c \'))
        (let [q (or q (if (= c \") \' \"))]
          (when (= q c) (.append sb \\))
          [q (.append sb (char c))])
        :else [q (.append sb (or (control c) (str c)))]))

(defn- quoted
  "Returns the quote mark and the escaped body of s as a tag string
  writes them."
  [^String s]
  (let [[q sb] (reduce quote-step [nil (StringBuilder.)] s)]
    [(str (or q \")) (str sb)]))

(defn- number [x]
  (cond-> [(lit (str x) "gold")]
    (suffixes (class x)) (conj (kind-mark (suffixes (class x))))))

(defn- string-parts [s]
  (let [[q body] (quoted s)] [(lit q) (lit body "green") (lit q)]))

(defn- key-part [k]
  (let [s (name k)]
    (if (re-matches simple s)
      (lit s "aqua")
      (let [[q body] (quoted s)]
        {:text q :extra [(lit body "aqua") (lit q)]}))))

(declare parts)

(defn- array-item [suffix last? x]
  (cond-> [gap (lit (str x) "gold")]
    suffix (conj (kind-mark suffix))
    (not last?) (conj comma)))

(defn- array-parts [prefix suffix xs]
  (let [n (count xs) last-i (dec n)]
    (concat [(lit "[") (kind-mark prefix) (lit ";")]
            (mapcat #(array-item suffix (= %1 last-i) %2)
                    (range) (take max-length xs))
            (when (> n max-length) [folded])
            [(lit "]")])))

(defn- wraps? [xs]
  (and (< (count xs) inline-limit) (not-every? number? xs)))

(defn- inline-item [depth i x]
  (concat (when (pos? i) [comma gap]) (parts x (inc depth))))

(defn- inline-list [xs depth]
  (concat [(lit "[")]
          (mapcat #(inline-item depth %1 %2) (range) xs)
          [(lit "]")]))

(defn- wrapped-item [depth last-i i x]
  (concat [(lit "")] (parts x (inc depth))
          (when (not= i last-i) [comma gap])))

(defn- wrapped-list [xs depth]
  (let [n (count xs)]
    (concat [(lit "[")]
            (mapcat #(wrapped-item depth (dec n) %1 %2)
                    (range) (take max-length xs))
            (when (> n max-length) [(lit "") folded])
            [(lit "]")])))

(defn- list-parts [xs depth]
  (cond (empty? xs) [(lit "[") (lit "]")]
        (>= depth max-depth) [(lit "[") folded (lit "]")]
        (wraps? xs) (wrapped-list xs depth)
        :else (inline-list xs depth)))

(defn- hash-keys
  "Returns the keys of m in the order a tag gives them back, which
  is the order of their hash codes in a hash table."
  [m]
  (let [ks (vec (keys m))]
    (mapv ks (hash-order/of (map #(.hashCode (name %)) ks)))))

(defn- entry [m k depth]
  (concat [(lit "") (key-part k) (lit ":") gap]
          (parts (get m k) (inc depth))))

(defn- compound-parts [m depth]
  (cond (empty? m) [(lit "{") (lit "}")]
        (>= depth max-depth) [(lit "{") folded (lit "}")]
        :else (let [es (map #(entry m % depth) (hash-keys m))]
                (concat [(lit "{")]
                        (apply concat (interpose [comma gap] es))
                        [(lit "}")]))))

(defn- parts [x depth]
  (cond (map? x) (compound-parts x depth)
        (vector? x) (list-parts x depth)
        (string? x) (string-parts x)
        (boolean? x) (number (byte (if x 1 0)))
        (bytes? x) (array-parts "B" "b" x)
        (instance? (Class/forName "[I") x) (array-parts "I" nil x)
        (instance? (Class/forName "[J") x) (array-parts "L" "L" x)
        (number? x) (number x)
        :else [(lit (str x))]))

(defn pretty
  "Returns tag as a chat component, keys aqua, strings green,
  numbers gold and their kind red."
  [tag]
  {:text "" :extra (vec (parts tag 0))})
