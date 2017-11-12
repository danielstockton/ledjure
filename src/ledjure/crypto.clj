(ns ledjure.crypto
  (:require [base58.core :as b58])
  (:import [java.security KeyFactory KeyPairGenerator SecureRandom]
           [java.security.spec X509EncodedKeySpec]
           [javax.xml.bind DatatypeConverter]))

(defn generate-keypair []
  (let [random (SecureRandom/getInstance "SHA1PRNG")]
    (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                        (.initialize 256 random)))))

(def keypair (generate-keypair))
(def public-key (.getPublic keypair))
(def private-key (.getPrivate keypair))

(defn decode64 [str]
  (.decode (java.util.Base64/getDecoder) str))

(defn encode64 [bytes]
  (.encodeToString (java.util.Base64/getEncoder) bytes))

(defn bytes->pk [bytes]
  (let [key-spec (X509EncodedKeySpec. bytes)
        key-factory (KeyFactory/getInstance "EC")]
    (.generatePublic key-factory key-spec)))

(defn sign
  "Private key signing of a message. Takes message as string."
  [message private-key]
  (encode64
   (let [msg-data (.getBytes message)
         sig      (doto (java.security.Signature/getInstance "SHA256withECDSA")
                    (.initSign private-key (java.security.SecureRandom.))
                    (.update msg-data))]
     (.sign sig))))

(defn verify
  "Public key verification of a base64-encoded signature and an
   assumed source message."
  [encoded-sig message public-key]
  (let [msg-data  (.getBytes message)
        signature (decode64 encoded-sig)
        sig       (doto (java.security.Signature/getInstance "SHA256withECDSA")
                    (.initVerify public-key)
                    (.update msg-data))]
    (.verify sig signature)))

(defn hash-digest [type data]
  (.digest (java.security.MessageDigest/getInstance type) data))

(defn hash-str [data-bytes]
  (->> data-bytes
       (map #(.substring
              (Integer/toString
               (+ (bit-and % 0xff) 0x100) 16) 1))
       (apply str)))

(defn sha256 [data]
  (hash-digest "SHA-256" data))

(defn sha256*2 [data]
  (-> data (sha256) (sha256)))

(defn address [pk]
  (encode64 (.getEncoded pk)))

(comment
  (def sig (sign "hello world" private-key))

  (verify sig "hello world" public-key)

  )
