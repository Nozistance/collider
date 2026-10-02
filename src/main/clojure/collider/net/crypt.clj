(ns collider.net.crypt
  "Connection encryption and the session hash."
  (:import (java.io InputStream OutputStream)
           (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security KeyPair KeyPairGenerator
             MessageDigest SecureRandom)
           (java.util Arrays)
           (javax.crypto Cipher CipherInputStream
             CipherOutputStream SecretKey)
           (javax.crypto.spec IvParameterSpec SecretKeySpec)))

(set! *warn-on-reflection* true)

(defn key-pair
  "Returns a new RSA key pair of 1024 bits."
  ^KeyPair []
  (let [g (KeyPairGenerator/getInstance "RSA")]
    (.initialize g 1024)
    (.generateKeyPair g)))

(defn public-key
  "Returns the public key of kp in DER."
  ^bytes [^KeyPair kp]
  (.getEncoded (.getPublic kp)))

(defn challenge
  "Returns four random bytes for a player to send back encrypted."
  ^bytes []
  (let [a (byte-array 4)]
    (.nextBytes (SecureRandom.) a)
    a))

(defn- decrypted ^bytes [^KeyPair kp ^bytes data]
  (let [c (Cipher/getInstance "RSA")]
    (.init c Cipher/DECRYPT_MODE (.getPrivate kp))
    (.doFinal c data)))

(defn secret
  "Returns the secret a player sent encrypted with the public key of
  kp. Throws unless the token it sent back is the challenge."
  ^SecretKey [kp ^bytes encrypted ^bytes token ^bytes challenge]
  (when-not (Arrays/equals challenge (decrypted kp token))
    (throw (ex-info "protocol error" {:reason :challenge})))
  (SecretKeySpec. (decrypted kp encrypted) "AES"))

(defn digest-hex
  "Returns the SHA-1 of parts as signed hex."
  ^String [& parts]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (run! #(.update md ^bytes %) parts)
    (.toString (BigInteger. (.digest md)) 16)))

(defn server-hash
  "Returns the hash of server-id, the secret s and the key pair kp
  that both a player and the session server compute."
  ^String [^String server-id ^KeyPair kp ^SecretKey s]
  (digest-hex (.getBytes server-id StandardCharsets/ISO_8859_1)
              (.getEncoded s) (public-key kp)))

(defn- cipher ^Cipher [^long mode ^SecretKey s]
  (doto (Cipher/getInstance "AES/CFB8/NoPadding")
    (.init (int mode) s (IvParameterSpec. (.getEncoded s)))))

(defn decrypting
  "Returns in as read through the secret s."
  ^InputStream [^InputStream in s]
  (CipherInputStream. in (cipher Cipher/DECRYPT_MODE s)))

(defn encrypting
  "Returns out as written through the secret s."
  ^OutputStream [^OutputStream out s]
  (CipherOutputStream. out (cipher Cipher/ENCRYPT_MODE s)))
