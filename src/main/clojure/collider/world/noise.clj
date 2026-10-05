(ns collider.world.noise
  "Perlin noise fields of the feature placers.")

(set! *warn-on-reflection* true)

(def ^:private gradient
  (int-array [1 1 0 -1 1 0 1 -1 0 -1 -1 0
              1 0 1 -1 0 1 1 0 -1 -1 0 -1
              0 1 1 0 -1 1 0 1 -1 0 -1 -1
              1 1 0 0 -1 1 -1 1 0 0 -1 -1]))

(defn- bits ^longs [^long seed]
  (long-array [(bit-and (bit-xor seed 25214903917) 0xFFFFFFFFFFFF)]))

(defn- next-bits ^long [^longs s ^long n]
  (let [v (-> (aget s 0)
              (unchecked-multiply 25214903917)
              (unchecked-add 11)
              (bit-and 0xFFFFFFFFFFFF))]
    (aset s 0 v)
    (unchecked-int (bit-shift-right v (- 48 n)))))

(defn- next-int ^long [^longs s ^long bound]
  (if (zero? (bit-and bound (dec bound)))
    (bit-shift-right (* bound (next-bits s 31)) 31)
    (loop []
      (let [v (next-bits s 31)
            m (rem v bound)]
        (if (neg? (unchecked-int (+ (- v m) (dec bound))))
          (recur)
          m)))))

(defn- next-long ^long [^longs s]
  (let [hi (next-bits s 32)
        lo (next-bits s 32)]
    (unchecked-add (bit-shift-left hi 32) lo)))

(defn- next-double ^double [^longs s]
  (let [hi (next-bits s 26)
        lo (next-bits s 27)]
    (* (double (+ (bit-shift-left hi 27) lo))
       (double (float 1.110223E-16)))))

(defn- octave [^longs s]
  (let [xo (* (next-double s) 256.0)
        yo (* (next-double s) 256.0)
        zo (* (next-double s) 256.0)
        perm (int-array (range 256))]
    (dotimes [i 256]
      (let [j (+ i (next-int s (- 256 i)))
            t (aget perm i)]
        (aset perm i (aget perm j))
        (aset perm j t)))
    {:perm perm :xo xo :yo yo :zo zo}))

(defn- hash-of ^long [^ints perm ^long x ^long y ^long z]
  (letfn [(p [^long i] (aget perm (bit-and i 0xFF)))]
    (p (+ (p (+ (p x) y)) z))))

(defn- grad ^double [^long h ^double x ^double y ^double z]
  (let [^ints g gradient
        i (* 3 (bit-and h 15))]
    (+ (* (aget g i) x)
       (* (aget g (+ i 1)) y)
       (* (aget g (+ i 2)) z))))

(defn- smooth ^double [^double t]
  (* t t t (+ (* t (- (* t 6.0) 15.0)) 10.0)))

(defn- lerp ^double [^double t ^double a ^double b]
  (+ a (* t (- b a))))

(defn- corner ^double [perm f r ^long c]
  (let [dx (bit-and c 1)
        dy (bit-and (bit-shift-right c 1) 1)
        dz (bit-shift-right c 2)]
    (grad (hash-of perm (+ (long (f 0)) dx) (+ (long (f 1)) dy)
                   (+ (long (f 2)) dz))
          (- (double (r 0)) dx)
          (- (double (r 1)) dy)
          (- (double (r 2)) dz))))

(defn- floor-int ^long [^double x]
  (unchecked-int (Math/floor x)))

(defn- cell-noise ^double [perm ^double x ^double y ^double z]
  (let [f [(floor-int x) (floor-int y) (floor-int z)]
        r [(- x (long (f 0))) (- y (long (f 1))) (- z (long (f 2)))]
        d #(corner perm f r %)
        xa (smooth (r 0))
        ya (smooth (r 1))]
    (lerp (smooth (r 2))
          (lerp ya (lerp xa (d 0) (d 1)) (lerp xa (d 2) (d 3)))
          (lerp ya (lerp xa (d 4) (d 5)) (lerp xa (d 6) (d 7))))))

(defn- octave-noise ^double [o ^double x ^double y ^double z]
  (cell-noise (:perm o)
              (+ x (double (:xo o)))
              (+ y (double (:yo o)))
              (+ z (double (:zo o)))))

(defn- perlin [^longs s ^long low ^doubles amps]
  (let [fork (next-long s)
        n (alength amps)
        octave-at (fn [^long i]
                    (when-not (zero? (aget amps i))
                      (let [h (.hashCode (str "octave_" (+ low i)))]
                        (octave (bits (bit-xor h fork))))))]
    {:levels (mapv octave-at (range n))
     :amps amps
     :input (Math/pow 2.0 low)
     :value (/ (Math/pow 2.0 (dec n))
               (- (Math/pow 2.0 n) 1.0))}))

(defn- wrap ^double [^double x]
  (let [k (unchecked-long (Math/floor (+ (/ x 3.3554432E7) 0.5)))]
    (- x (* (double k) 3.3554432E7))))

(defn- level ^double [o ^double f p]
  (octave-noise o
                (wrap (* (double (p 0)) f))
                (wrap (* (double (p 1)) f))
                (wrap (* (double (p 2)) f))))

(defn- perlin-value ^double [pl p]
  (let [^doubles amps (:amps pl)
        levels (:levels pl)]
    (loop [i 0
           v 0.0
           f (double (:input pl))
           k (double (:value pl))]
      (if (= i (count levels))
        v
        (let [o (nth levels i)
              n (if o (* (aget amps i) k (level o f p)) 0.0)]
          (recur (inc i) (if o (+ v n) v) (* f 2.0) (/ k 2.0)))))))

(defn- spread ^double [^doubles amps]
  (let [is (keep-indexed #(when-not (zero? %2) %1) amps)]
    (if (empty? is)
      1.0
      (double (- (inc (apply max is)) (apply min is))))))

(defn noise
  "Returns the noise field of seed with the octave amplitudes amps."
  [^long seed ^long low ^doubles amps]
  (let [s (bits seed)
        a (perlin s low amps)
        b (perlin s low amps)]
    {:first a
     :second b
     :factor (/ 0.16666666666666666
                (* 0.1 (+ 1.0 (/ 1.0 (spread amps)))))}))

(defn- value ^double [n ^double x ^double y ^double z]
  (let [f 1.0181268882175227
        a (perlin-value (:first n) [x y z])
        b (perlin-value (:second n) [(* x f) (* y f) (* z f)])]
    (* (+ a b) (double (:factor n)))))

(defn at
  [n x y z scale]
  (let [s (double (float scale))]
    (value n (* (double x) s) (* (double y) s) (* (double z) s))))

(defn- times ^double [x ^double s]
  (double (unchecked-float (* (double (float (int x))) s))))

(defn at-float
  "Returns the noise at block x y z scaled by scale, as a float."
  [n x y z scale]
  (let [s (double (float scale))]
    (value n (times x s) (times y s) (times z s))))
