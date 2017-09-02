(ns ledjure.core
  (:refer-clojure :exclude [hash])
  (:require [clojure.java.io :as io])
  (:import [java.net ServerSocket]))

(def ^:const version 0.1)
(def ^:const block-difficulty 4)
(def ^:const leading-zeros (apply str (repeat block-difficulty "0")))
(def ^:const max-coins 1e6)
(def ^:const min-tx 1/1000)

(defn now [] (System/currentTimeMillis))

;;;; Crypto

(defn hash-digest [type data]
  (.digest (java.security.MessageDigest/getInstance type)
           (.getBytes data "UTF-8")))

(defn hash-str [data-bytes]
  (->> data-bytes
       (map #(.substring
              (Integer/toString
               (+ (bit-and % 0xff) 0x100) 16) 1))
       (apply str)))

(defn sha256 [data]
  (hash-str (hash-digest "SHA-256" data)))

(defn sha256*2 [data]
  (-> data (sha256) (sha256)))

;;;; Blockchain

(defprotocol Hashable
  (hash [this] [this nonce]))

;; Input -> (txid, amount)
;; Output -> (address, amount)

(defrecord Transaction [ins outs]
  Hashable
  (hash [this]
    (sha256*2 (str (pr-str ins) (pr-str outs)))))

(defprotocol Mineable
  (mine [this]))

(defprotocol Verifiable
  (valid? [this] [this nonce]))

(defrecord Block [idx prev tstamp nonce txs]
  Hashable
  (hash [this]
    (hash this nonce))
  (hash [this nonce]
    (let [root (apply str txs)]
      (sha256*2 (str version idx prev root tstamp block-difficulty nonce))))
  Mineable
  (mine [this]
    (when (valid? this)
      (throw (Exception. "Already mined ;)")))
    (loop [n 0]
      (if (valid? this n)
        (->Block idx prev tstamp n txs)
        (recur (inc n)))))
  Verifiable
  (valid? [this]
    (valid? this nonce))
  (valid? [this nonce]
    (let [h (hash this nonce)
          s (subs h 0 block-difficulty)]
      (= s leading-zeros))))

(def genesis (->Block 0 "GENESIS" (now) 0 [(->Transaction [[]] [["address1" 50]])]))

(defprotocol Ledger
  (balance [this address]
    "Return the balance associated with the address.")
  (index [this]
    "Index txid -> transaction.")
  (valid-tx? [this tx]
    "Validate a new transaction:

     - Correct syntax and data structure.
     - The values must be between 0 and 1 million.
     - The transactions referenced by inputs must exist and by unspent.
     - Input value must be greater than or equal to the output value.
     - Reject if transaction value is below minimum."))

(deftype Blockchain [blocks]
  Ledger
  (balance [this address])
  (index [this]
    (let [index' (transient {})]
      (doseq [b blocks]
        (doseq [tx (:txs b)]
          (doseq [[txid _] (:ins tx)]
            (when txid
              (assoc! index' txid true)))))
      (persistent! index')))
  (valid-tx? [this tx]
    (let [ins       (:ins tx)
          outs      (:outs tx)
          in-vals   (map second ins)
          out-vals  (map second outs)
          total-in  (reduce + in-vals)
          total-out (reduce + out-vals)
          index'    (index this)]
      (when (> (count this) 1)
        (doseq [[txid amount] ins]
          (let [spent? (contains? index' txid)]
            (assert (not spent?) "You spent those already!"))))
      (assert (> total-in total-out) "Inputs less than outputs.")
      (assert (> total-out min-tx) "Transaction too small.")
      tx))
  clojure.lang.Seqable
  (seq [_] (seq blocks))
  clojure.lang.IPersistentVector
  (cons [this txs]
    (let [prev-block (peek blocks)
          new-block  (->Block (count this) (hash prev-block) (now) 0 txs)]
      (Blockchain. (conj blocks new-block))))
  clojure.lang.Counted
  (count [_] (inc (.-idx (peek blocks))))
  clojure.lang.Indexed
  (nth [_ idx] (nth blocks idx)))

(def blockchain (->Blockchain [genesis]))

(comment
  (count blockchain)

  (mine (nth blockchain 0))

  (conj blockchain [1 2 3 4 5])

  (valid-tx? blockchain (->Transaction [[nil 5]] [[nil 1]]))

  (def t1 (->Transaction [["spent" 5]] [["address1" 1]]))

  (def t2 (->Transaction [["spent" 5]] [["address2" 1]]))

  (index (conj blockchain [t2]))

  (valid-tx? (conj blockchain [t1]) t2)

  )

;;;; Server

(defn handler [msg]
  "hi!")

(comment
  (defn receive
    [socket]
    (.readLine (io/reader socket)))

  (defn send
    [socket msg]
    (let [writer (io/writer socket)]
      (.write writer msg)
      (.flush writer)))

  (defn server [port]
    (let [running (atom true)]
      (future
        (with-open [server-sock (ServerSocket. port)]
          (while @running
            (with-open [sock (.accept server-sock)]
              (let [msg-in  (receive sock)
                    msg-out (handler msg-in)]
                (send sock msg-out))))))
      running))

  (defn -main [& args]
    (server 12345)))
