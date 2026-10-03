(ns collider.game.command.selector
  "Entity selectors and player names in commands, their completions
  and the entities they select."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.args :as args]
            [collider.game.command.reader :as r]
            [collider.game.command.snbt :as snbt]
            [collider.game.entity :as entity]
            [collider.game.experience :as xp]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- at? [[s n :as rd] c]
  (and (r/can-read? rd) (= c (nth s n))))

(defn- skip [[s n]] [s (inc n)])

(defn- cursor ^long [p] (second (:rd p)))

(defn- fail [p err] (reduced (assoc p :error err)))

(defn- fail-at [p n k & xs]
  (let [rd [(first (:rd p)) n]]
    (fail (assoc p :rd rd) (apply r/error-at rd k xs))))

(defn- read-with
  [p f]
  (let [res (f (:rd p))]
    (if (r/error? res)
      [nil (fail (assoc p :rd [(first (:rd p)) (:cursor res)]) res)]
      [(first res) (assoc p :rd (second res))])))

(defn- ws [p] (update p :rd r/skip-whitespace))

(defn- bound-char? [[^String s n :as rd]]
  (let [c (.charAt s (int n))]
    (or (<= (int \0) (int c) (int \9)) (= c \-)
        (and (= c \.)
             (not (and (r/can-read? rd 2)
                       (= \. (.charAt s (int (inc (long n)))))))))))

(defn- bound-end [rd]
  (loop [e rd]
    (if (and (r/can-read? e) (bound-char? e)) (recur (skip e)) e)))

(defn- read-bound [[s n :as rd] parse kind]
  (let [[_ i :as end] (bound-end rd)
        t (subs s n i)]
    (if (= "" t)
      [nil end]
      (try [(parse t) end]
           (catch NumberFormatException _
             (r/error-at end (str "parsing." kind ".invalid") t))))))

(defn- bounds-at [rd parse kind]
  (let [lo (read-bound rd parse kind)]
    (if (r/error? lo)
      lo
      (let [[a e] lo
            two? (and (at? e \.) (at? (skip e) \.))
            hi (if two? (read-bound (skip (skip e)) parse kind) lo)]
        (cond (r/error? hi) hi
              (and (nil? a) (nil? (first hi)))
              (r/error-at (second hi) "argument.range.empty")
              :else [[a (first hi)] (second hi)])))))

(defn- swapped? [[a b]] (and a b (> (compare a b) 0)))

(defn read-bounds
  "Returns the range [min max] of the numbers parse reads at rd, with
  an open end nil. With swap? a min above max is an error."
  [[s n :as rd] parse kind swap?]
  (let [res (if (r/can-read? rd)
              (bounds-at rd parse kind)
              (r/error-at rd "argument.range.empty"))]
    (cond (r/error? res) (assoc res :cursor n)
          (and swap? (swapped? (first res)))
          (r/error-at rd "argument.range.swapped")
          :else res)))

(defn- double-range [rd]
  (read-bounds rd #(Double/parseDouble %) "double" true))

(defn- int-range [rd]
  (read-bounds rd #(Integer/parseInt %) "int" true))

(defn- degrees [rd]
  (read-bounds rd #(Float/parseFloat %) "float" false))

(defn- invert? [p]
  (let [p (ws p)]
    (if (at? (:rd p) \!) [true (ws (update p :rd skip))] [false p])))

(defn- tag? [p]
  (let [p (ws p)]
    (if (at? (:rd p) \#) [true (ws (update p :rd skip))] [false p])))

(defn- state [p k] (get-in p [:states k :state] :none))

(defn- element? [p k inv]
  (if inv (not= :single (state p k)) (= :none (state p k))))

(defn- mark [p k inv]
  (assoc-in p [:states k :state] (if inv :multiple :single)))

(defn- pred [p & xs]
  (update-in p [:sel :preds] (fnil conj []) (vec xs)))

(def ^:private options-key "argument.entity.options.")

(def ^:private missing "argument.entity.selector.missing")

(def ^:private unknown "argument.entity.selector.unknown")

(def ^:private not-allowed "argument.entity.selector.not_allowed")

(defn- inapplicable [p start k]
  (fail-at p start "argument.entity.options.inapplicable" k))

(defn- name-option [p]
  (let [start (cursor p) [inv p] (invert? p)
        [v p] (read-with p r/read-string)]
    (cond (reduced? p) p
          (not (element? p :name inv)) (inapplicable p start "name")
          :else (-> (mark p :name inv) (pred :name v inv)))))

(defn- negative? [[a b]]
  (or (and a (neg? a)) (and b (neg? b))))

(defn- distance-option [p]
  (let [start (cursor p) [v p] (read-with p double-range)]
    (cond (reduced? p) p
          (negative? v)
          (fail-at p start (str options-key "distance.negative"))
          :else (-> p (assoc-in [:sel :range] v)
                    (assoc-in [:sel :world?] true)))))

(defn- level-option [p]
  (let [start (cursor p) [v p] (read-with p int-range)]
    (cond (reduced? p) p
          (negative? v)
          (fail-at p start (str options-key "level.negative"))
          :else (-> p (assoc-in [:sel :level] v)
                    (assoc-in [:sel :entities?] false)))))

(defn- double-option [path]
  (fn [p]
    (let [[v p] (read-with p r/read-double)]
      (if (reduced? p)
        p
        (-> p (assoc-in (into [:sel] path) v)
            (assoc-in [:sel :world?] true))))))

(defn- rotation-option [k]
  (fn [p]
    (let [[v p] (read-with p degrees)]
      (if (reduced? p) p (assoc-in p [:sel k] v)))))

(defn- limit-option [p]
  (let [start (cursor p) [v p] (read-with p r/read-int)]
    (cond (reduced? p) p
          (< (long v) 1)
          (fail-at p start (str options-key "limit.toosmall"))
          :else (-> (assoc-in p [:sel :limit] v)
                    (assoc-in [:states :limit] true)))))

(def ^:private orders
  {"nearest" :nearest "furthest" :furthest "random" :random
   "arbitrary" :arbitrary})

(defn- sort-option [p]
  (let [start (cursor p)
        [v p] (read-with (assoc p :sugg :sort) r/read-unquoted)]
    (if-let [o (orders v)]
      (-> (assoc-in p [:sel :order] o)
          (assoc-in [:states :sort] true))
      (fail-at p start (str options-key "sort.irreversible") v))))

(def game-modes ["survival" "creative" "adventure" "spectator"])

(defn- gamemode-option [p]
  (let [p (assoc p :sugg :gamemode)
        start (cursor p) [inv p] (invert? p)]
    (if-not (element? p :gamemode inv)
      (inapplicable p start "gamemode")
      (let [[v p] (read-with p r/read-unquoted)]
        (if (some #{v} game-modes)
          (-> (mark p :gamemode inv) (pred :gamemode (keyword v) inv)
              (assoc-in [:sel :entities?] false))
          (fail-at p start "argument.entity.options.mode.invalid"
                   v))))))

(defn- team-option [p]
  (let [start (cursor p) [inv p] (invert? p)
        [v p] (read-with p r/read-unquoted)]
    (if (element? p :team inv)
      (-> (mark p :team inv) (pred :team v inv))
      (inapplicable p start "team"))))

(defn entity-types []
  (sort (map data/wire (keys (get (data/registries) "entity_type")))))

(defn type-tags []
  (->> (keys (get (data/tags) "entity_type"))
       (map #(str "minecraft:" %))
       sort))

(defn- any-tag? [p] (not= :single (state p :type)))

(defn- tag-ok? [p id]
  (and (any-tag? p)
       (not (contains? (get-in p [:states :type :tags]) id))))

(defn- add-tag [p id]
  (update-in p [:states :type :tags] (fnil conj #{}) id))

(defn- type-tag [p start inv]
  (let [[id p] (read-with p args/read-id)]
    (cond (reduced? p) p
          (not (tag-ok? p id)) (inapplicable p start "type")
          :else (-> (assoc-in p [:states :type :state] :multiple)
                    (add-tag id)
                    (pred :type-tag id inv)))))

(defn- typed [p id inv]
  (let [k (data/kebab id)]
    (cond-> (-> (mark p :type inv) (pred :type k inv))
      (and (= :player k) (not inv)) (assoc-in [:sel :entities?] false)
      (not inv) (assoc-in [:sel :type] k))))

(defn- type-element [p start inv]
  (let [[id p] (read-with p args/read-id)]
    (cond (reduced? p) p
          (not (some #{id} (entity-types)))
          (fail-at p start "argument.entity.options.type.invalid" id)
          :else (typed p id inv))))

(defn- type-option [p]
  (let [p (assoc p :sugg :type)
        start (cursor p) [inv p] (invert? p) [tag p] (tag? p)]
    (cond tag (if (any-tag? p)
                (type-tag p start inv)
                (inapplicable p start "type"))
          (element? p :type inv) (type-element p start inv)
          :else (inapplicable p start "type"))))

(defn- tag-option [p]
  (let [[inv p] (invert? p) [v p] (read-with p r/read-unquoted)]
    (pred p :tag v inv)))

(defn- nbt-option [p]
  (let [[inv p] (invert? p) [v p] (read-with p snbt/read-compound)]
    (if (reduced? p) p (pred p :nbt v inv))))

(defn- expect-at [rd ch]
  (let [e (r/expect rd ch)] (if (r/error? e) e [nil e])))

(defn- expect [p ch] (second (read-with p #(expect-at % ch))))

(defn- entry [p read-key read-value]
  (let [[k p] (read-with (ws p) read-key)
        p (if (reduced? p) p (ws (expect (ws p) \=)))
        [v p] (if (reduced? p) [nil p] (read-value p))
        p (if (reduced? p) p (ws p))]
    [k v (cond-> p (and (not (reduced? p)) (at? (:rd p) \,))
           (update :rd skip))]))

(defn- entries [p read-key read-value]
  (loop [p (ws (expect p \{)) m {}]
    (cond (reduced? p) [m p]
          (and (r/can-read? (:rd p)) (not (at? (:rd p) \})))
          (let [[k v p] (entry p read-key read-value)]
            (recur p (if (reduced? p) m (assoc m k v))))
          :else [m (expect p \})])))

(defn- scores-option [p]
  (let [[m p] (entries p r/read-unquoted #(read-with % int-range))]
    (cond (reduced? p) p
          (seq m) (-> (pred p :scores m)
                      (assoc-in [:states :scores] true))
          :else (assoc-in p [:states :scores] true))))

(defn- criteria [p]
  (let [[m p] (entries p r/read-unquoted
                      #(read-with % r/read-boolean))]
    (if (reduced? p) [nil p] [m (ws p)])))

(defn- progress [p]
  (if (at? (:rd p) \{)
    (criteria p)
    (read-with p r/read-boolean)))

(defn- advancements-option [p]
  (let [[m p] (entries p args/read-id progress)]
    (cond (reduced? p) p
          (seq m) (-> (pred p :advancements m)
                      (assoc-in [:sel :entities?] false)
                      (assoc-in [:states :advancements] true))
          :else (assoc-in p [:states :advancements] true))))

(defn- predicate-option [p]
  (let [[inv p] (invert? p) [id p] (read-with p args/read-id)]
    (if (reduced? p) p (pred p :predicate id inv))))

(defn- unset? [& path] (fn [p] (nil? (get-in p (into [:sel] path)))))

(defn- once? [k] (fn [p] (not (get-in p [:states k]))))

(defn- open? [k] (fn [p] (not= :single (state p k))))

(defn- unless-self [f] (fn [p] (and (not (:self? (:sel p))) (f p))))

(def options
  {"name" [name-option (open? :name)]
   "distance" [distance-option (unset? :range)]
   "level" [level-option (unset? :level)]
   "x" [(double-option [:pos 0]) (unset? :pos 0)]
   "y" [(double-option [:pos 1]) (unset? :pos 1)]
   "z" [(double-option [:pos 2]) (unset? :pos 2)]
   "dx" [(double-option [:delta 0]) (unset? :delta 0)]
   "dy" [(double-option [:delta 1]) (unset? :delta 1)]
   "dz" [(double-option [:delta 2]) (unset? :delta 2)]
   "x_rotation" [(rotation-option :rot-x) (unset? :rot-x)]
   "y_rotation" [(rotation-option :rot-y) (unset? :rot-y)]
   "limit" [limit-option (unless-self (once? :limit))]
   "sort" [sort-option (unless-self (once? :sort))]
   "gamemode" [gamemode-option (open? :gamemode)]
   "team" [team-option (open? :team)]
   "type" [type-option (open? :type)]
   "tag" [tag-option (constantly true)]
   "nbt" [nbt-option (constantly true)]
   "scores" [scores-option (once? :scores)]
   "advancements" [advancements-option (once? :advancements)]
   "predicate" [predicate-option (constantly true)]})

(defn- option-read [p f]
  (let [p (f (ws (assoc (update p :rd skip) :sugg :nothing)))]
    (if (reduced? p) p (ws (assoc p :sugg :next)))))

(defn- option-value [p k start]
  (let [[f ok?] (options k) p (ws p)]
    (cond (nil? f)
          (fail-at p start "argument.entity.options.unknown" k)
          (not (ok? p)) (inapplicable p start k)
          (not (at? (:rd p) \=))
          (fail-at p start "argument.entity.options.valueless" k)
          :else (option-read p f))))

(defn- option [p]
  (let [p (ws p) start (cursor p) [k p] (read-with p r/read-string)]
    (if (reduced? p) p (option-value p k start))))

(defn- unterminated [p]
  (fail p (r/error-at (:rd p) (str options-key "unterminated"))))

(defn- after-option [p]
  (let [rd (:rd p)]
    (cond (not (r/can-read? rd)) p
          (at? rd \,) (assoc p :rd (skip rd) :sugg :key)
          (at? rd \]) (reduced p)
          :else (unterminated p))))

(defn- options-read [p]
  (loop [p (ws (assoc p :sugg :key))]
    (if (and (r/can-read? (:rd p)) (not (at? (:rd p) \])))
      (let [p (option p)
            q (if (reduced? p) p (after-option p))]
        (if (reduced? q) (unreduced q) (recur q)))
      p)))

(defn- close [p]
  (cond (:error p) p
        (r/can-read? (:rd p))
        (assoc p :rd (skip (:rd p)) :sugg :nothing)
        :else (unreduced (unterminated p))))

(def ^:private kinds
  {\a {:limit Integer/MAX_VALUE :entities? false :order :arbitrary
       :type :player}
   \e {:limit Integer/MAX_VALUE :entities? true :order :arbitrary
       :preds [[:alive]]}
   \n {:limit 1 :entities? true :order :nearest :preds [[:alive]]}
   \p {:limit 1 :entities? false :order :nearest :type :player}
   \r {:limit 1 :entities? false :order :random :type :player}
   \s {:limit 1 :entities? true :self? true :order :arbitrary}})

(defn- kind-read [p]
  (let [[s n :as rd] (:rd p) c (nth s n) k (kinds c)]
    (if k
      (assoc p :rd (skip rd) :sel (merge (:sel p) k) :sugg :open)
      (unreduced (fail-at p n unknown (str "@" c))))))

(defn- open-options [p]
  (assoc p :rd (skip (:rd p)) :sugg :key-or-close))

(defn- selector-read [p]
  (let [p (assoc p :sugg :selector :sel {:selector? true})
        p (if (r/can-read? (:rd p))
            (kind-read p)
            (assoc p :error (r/error-at (:rd p) missing)))]
    (if (and (not (:error p)) (at? (:rd p) \[))
      (close (options-read (open-options p)))
      p)))

(defn- uuid-of [^String s]
  (try (UUID/fromString s) (catch IllegalArgumentException _ nil)))

(defn- name-read [p]
  (let [start (cursor p)
        p (cond-> p (r/can-read? (:rd p)) (assoc :sugg :name))
        [v p] (read-with p r/read-string)]
    (cond (reduced? p) (unreduced p)
          (uuid-of v)
          (assoc p :sel {:uuid (uuid-of v) :limit 1 :entities? true})
          (or (= "" v) (> (count v) 16))
          (unreduced (fail-at p start "argument.entity.invalid"))
          :else (assoc p :sel {:name v :limit 1 :entities? false}))))

(def blank
  {:limit 1 :entities? false :self? false :order :arbitrary :type nil
   :name nil :uuid nil :range nil :preds []})

(defn parse
  "Reads a selector or a player name at rd. The result holds the
  reader past it and the selector, or an error, and what the
  completions need. A selector is an error unless allow? is true."
  [rd allow?]
  (let [p {:rd rd :start (second rd) :allow? allow?
           :sugg :name-or-selector}
        p (cond (not (at? rd \@)) (name-read p)
                (not allow?)
                (assoc p :error (r/error-at rd not-allowed))
                :else (selector-read (update p :rd skip)))]
    (update p :sel #(merge blank %))))

(defn- split-at? [c] (#{\. \_ \/ \:} c))

(defn- next-split [s]
  (first (keep-indexed (fn [k c] (when (split-at? c) k)) s)))

(defn matches-sub?
  "Returns true when input starts with pattern, or holds it right
  after a dot, an underscore, a slash or a colon."
  [^String pattern ^String input]
  (loop [i 0]
    (if (.startsWith input pattern (int i))
      true
      (if-let [j (next-split (subs input i))]
        (recur (+ i (long j) 1))
        false))))

(defn- offered [rem texts]
  (filterv #(not= rem %) texts))

(defn suggest-strings
  "Returns the completions among xs of text from start on. Case does
  not count, any word of an x may match, and the text typed in full
  is not offered."
  [^String text start xs]
  (let [rem (subs text start) lo (str/lower-case rem)
        hits (filter #(matches-sub? lo (str/lower-case %)) xs)]
    {:start start :texts (offered rem hits)}))

(defn- id-match? [^String t ^String id]
  (let [[ns path] (str/split id #":" 2)]
    (if (str/includes? t ":")
      (matches-sub? t id)
      (or (matches-sub? t ns) (matches-sub? t path)))))

(defn- prefixed [^String t ids ^String prefix]
  (let [t (subs t (count prefix))]
    (for [id ids :when (id-match? t id)] (str prefix id))))

(defn suggest-ids
  "Returns the completions among ids, each after prefix, of text
  from start on. Text without a colon may match the namespace or the
  path of an id."
  [^String text start ids prefix]
  (let [rem (subs text start) t (str/lower-case rem)]
    {:start start
     :texts (if (str/starts-with? t prefix)
              (offered rem (prefixed t ids prefix))
              [])}))

(def ^:private selector-texts ["@p" "@a" "@r" "@s" "@e" "@n"])

(defn- fixed [text start xs]
  {:start start :texts (offered (subs text start) xs)})

(defn- option-names [p ^String text start]
  (let [lo (str/lower-case (subs text start))]
    (for [[k [_ ok?]] (sort options)
          :when (and (ok? p) (str/starts-with? k lo))]
      (str k "="))))

(defn- mode-texts [p ^String rem]
  (let [lo (str/lower-case rem) bang? (str/starts-with? lo "!")
        pre (if bang? (subs lo 1) lo)
        plain? (and (not bang?) (= :none (state p :gamemode)))
        inv? (and (or bang? (= "" lo))
                  (not= :single (state p :gamemode)))]
    (for [m game-modes :when (str/starts-with? m pre)
          t [(when inv? (str "!" m)) (when plain? m)] :when t]
      t)))

(defn- type-groups [p text start]
  (let [ids (entity-types) tags (filter #(tag-ok? p %) (type-tags))
        f #(suggest-ids text start %1 %2)]
    (cond-> []
      (any-tag? p) (conj (f ids "!"))
      (= :none (state p :type)) (conj (f ids ""))
      (seq tags) (conj (f tags "#") (f tags "!#")))))

(defn- type-suggestions [p text start]
  {:start start
   :texts (vec (mapcat :texts (type-groups p text start)))})

(def ^:private sort-texts ["nearest" "furthest" "random" "arbitrary"])

(defn- name-suggestions [p text names]
  (let [at (cursor p)
        sel (when (:allow? p) selector-texts)
        n (suggest-strings text at names)]
    (update n :texts into (offered (subs text at) sel))))

(defn- option-suggestions [p ^String text at]
  (case (:sugg p)
    :open (fixed text at ["["])
    :key-or-close (fixed text at (cons "]" (option-names p text at)))
    :key (fixed text at (option-names p text at))
    :next (fixed text at ["," "]"])
    :sort (suggest-strings text at sort-texts)
    :gamemode (fixed text at (mode-texts p (subs text at)))
    :type (type-suggestions p text at)
    {:start at :texts []}))

(defn suggestions
  "Returns the completions at the point where parse p stopped. Names
  are the player names to offer."
  [p ^String text names]
  (let [at (cursor p)]
    (case (:sugg p)
      :name-or-selector (name-suggestions p text names)
      :name (suggest-strings text (:start p) names)
      :selector (fixed text (dec at) selector-texts)
      (option-suggestions p text at))))

(defn source-dim
  "Returns the dimension a command runs in."
  [world]
  (get-in world [:source :dim] (:dim world)))

(defn source-pos
  "Returns the position a command runs at."
  [world]
  (get-in world [:source :pos]))

(defn level-view
  "Returns the level of dimension dim seen from world."
  [world dim]
  (let [server (:server world)]
    (if (or (= dim (:dim world)) (nil? server))
      world
      (assoc (level/level server dim) :server server))))

(defn in-level
  "Returns deltas ds for the level of dimension dim, nil when none."
  [world dim ds]
  (cond
    (empty? ds) nil
    (= dim (:dim world)) (vec ds)
    :else [[:level-deltas dim (vec ds)]]))

(defn source-level
  "Returns the level a command runs in."
  [world]
  (level-view world (source-dim world)))

(defn- levels [world]
  (if (:server world)
    (map (fn [d] [d (level-view world d)]) schema/dims)
    [[(:dim world) world]]))

(defn player-entries
  "Returns the players of every level as [eid dim e] by eid."
  [world]
  (sort-by first
           (for [[dim lv] (levels world)
                 [id e] (level/player-entries lv)]
             [id dim e])))

(defn- entity-entries [world]
  (for [[dim lv] (levels world)
        [id e] (sort-by key (:entities lv))]
    [id dim e]))

(defn- alive? [e]
  (not (and (:health e) (<= (double (:health e)) 0.0))))

(defn- dist-sq ^double [from e]
  (let [p (:pos e)
        d #(- (double %1) (double (nth from %2)))
        dx (d (v/x p) 0) dy (d (v/y p) 1) dz (d (v/z p) 2)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- in-distance? [[lo hi] from e]
  (let [d (dist-sq from e) sq #(* (double %) (double %))]
    (and (or (nil? lo) (>= d (double (sq lo))))
         (or (nil? hi) (<= d (double (sq hi)))))))

(defn- in-bounds? [[lo hi] v]
  (and (or (nil? lo) (>= (double v) (double lo)))
       (or (nil? hi) (<= (double v) (double hi)))))

(defn wrapped
  "Returns angle a in degrees as a float from -180 up to 180."
  [a]
  (num/wrap-degrees a))

(defn- turned? [[lo hi] rot]
  (let [a (wrapped (or lo 0.0)) b (wrapped (or hi 359.0))
        r (wrapped rot)]
    (if (> a b) (or (>= r a) (<= r b)) (and (>= r a) (<= r b)))))

(defn- type-tagged? [id t]
  (let [tag (get (data/tags) "entity_type")]
    (some #{t} (get tag (str/replace id #"^minecraft:" "")))))

(defn- tagged? [tag e]
  (if (= "" tag) (empty? (:tags e)) (contains? (:tags e) tag)))

(defn- pred? [[k a inv] e]
  (let [f #(not= (boolean inv) (boolean %))]
    (case k
      :alive (alive? e)
      :name (f (= a (when (= :player (:type e)) (:name e))))
      :gamemode (and (= :player (:type e)) (f (= a (:game-mode e))))
      :team (f (= a ""))
      :type (f (= a (:type e)))
      :type-tag (f (type-tagged? a (:type e)))
      :tag (f (tagged? a e))
      false)))

(defn xp-of
  "Returns the experience of player e in world."
  [world e]
  (xp/account e (- (long (:tick world)) (long (:born e 0)))))

(defn- leveled? [world sel e]
  (or (nil? (:level sel))
      (and (= :player (:type e))
           (in-bounds? (:level sel) (:level (xp-of world e))))))

(defn- entity-box [e]
  (let [[hw h] (entity/box e) p (:pos e)
        x (v/x p) y (v/y p) z (v/z p) hw (double hw)]
    [[(- x hw) (+ x hw)] [y (+ y (double h))] [(- z hw) (+ z hw)]]))

(defn- overlaps? [a b]
  (every? (fn [[[a0 a1] [b0 b1]]]
            (and (< (double a0) (double b1))
                 (> (double a1) (double b0))))
          (map vector a b)))

(defn- sel-pos [world sel]
  (let [p (source-pos world) o (:pos sel)]
    (mapv #(double (get o % (nth p %))) [0 1 2])))

(defn- delta-box [d pos]
  (mapv (fn [i]
          (let [v (double (get d i 0.0)) c (double (pos i))]
            [(+ c (min v 0.0)) (+ c (max v 0.0) 1.0)]))
        [0 1 2]))

(defn- sel-box [sel pos]
  (let [d (:delta sel) hi (second (:range sel))]
    (cond
      (seq d) (delta-box d pos)
      hi (mapv #(vector (- (double %) (double hi))
                        (+ (double %) (double hi) 1.0))
               pos))))

(defn- selects? [world sel pos box [_ _ e]]
  (and (every? #(pred? % e) (:preds sel))
       (or (nil? (:type sel)) (= (:type sel) (:type e)))
       (or (nil? (:rot-x sel)) (turned? (:rot-x sel) (:pitch e 0.0)))
       (or (nil? (:rot-y sel)) (turned? (:rot-y sel) (:yaw e 0.0)))
       (leveled? world sel e)
       (or (nil? box) (overlaps? box (entity-box e)))
       (or (nil? (:range sel)) (in-distance? (:range sel) pos e))))

(defn- random-key [world]
  #(random/of-key [(:tick world) :selector (first %)]))

(defn- ordered [world sel pos xs]
  (case (:order sel)
    :nearest (sort-by #(dist-sq pos (nth % 2)) xs)
    :furthest (sort-by #(- (dist-sq pos (nth % 2))) xs)
    :random (sort-by (random-key world) xs)
    xs))

(defn by-uuid
  "Returns the entity of uuid u as [eid dim e], only among the
  players when players? is true."
  [world players? u]
  (some (fn [[id _ e :as x]]
          (when (= u (entity/uuid-of id e)) x))
        (if players? (player-entries world) (entity-entries world))))

(defn- by-name [world nm]
  (some #(when (= nm (:name (nth % 2))) %) (player-entries world)))

(defn- self-entry [world eid sel ok?]
  (let [x [eid (:dim world) (get-in world [:entities eid])]]
    (when (and (peek x) (ok? x)
               (or (:entities? sel) (= :player (:type (peek x)))))
      [x])))

(defn- candidates [world sel]
  (let [xs (if (:entities? sel)
             (entity-entries world)
             (player-entries world))
        dim (source-dim world)]
    (if (:world? sel) (filter #(= dim (second %)) xs) xs)))

(defn selected
  "Returns the entities that selector sel of player eid selects, each
  as [eid dim e]."
  [world eid sel]
  (let [pos (sel-pos world sel) box (sel-box sel pos)
        ok? #(selects? world sel pos box %)]
    (cond
      (:name sel) (keep identity [(by-name world (:name sel))])
      (:uuid sel) (keep identity [(by-uuid world false (:uuid sel))])
      (:self? sel) (self-entry world eid sel ok?)
      :else (->> (filter ok? (candidates world sel))
                 (ordered world sel pos)
                 (take (:limit sel))))))

(defn player-selected
  "Returns the players that selector sel of player eid selects, each
  as [eid dim e]."
  [world eid sel]
  (if (:uuid sel)
    (keep identity [(by-uuid world true (:uuid sel))])
    (selected world eid sel)))

(defn self
  "Returns entity eid as its own selection."
  [world eid]
  (selected world eid {:self? true :entities? true :limit 1}))
