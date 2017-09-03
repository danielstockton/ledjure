(ns ledjure.core
  (:refer-clojure :exclude [hash])
  (:require [clojure.java.io :as io])
  (:import [java.net ServerSocket]))

(def ^:const version 0.1)
(def ^:const block-difficulty 4)
(def ^:const leading-zeros (apply str (repeat block-difficulty "0")))
(def ^:const max-coins 1e6)
(def ^:const min-tx 1/1000)
(def ^:const reward 50)

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

(defrecord Transaction [ins outs lock-time]
  Hashable
  (hash [this]
    (sha256*2 (str (pr-str ins) (pr-str outs) lock-time))))

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

(def genesis (->Block 0 "GENESIS" (now) 0 [(->Transaction [] [] (now))]))

(defprotocol Ledger
  (balance [this address]
    "Return the balance associated with the address.")
  (index [this]
    "Index txid -> transaction.")
  (new-block [this address txs]
    "Produce a new block ready for mining.")
  (mine-block [this block]
    "Complete PoW to produce a valid block.")
  (valid-block? [this tx]
    "Validate a new block. The block is allowed to contain a single transaction
     with no inputs that outputs the agreed reward amount.

     Each transaction must:

     - Have correct syntax and data structure.
     - Have values must be between 0 and 1 million.
     - Reference by inputs that exist and are unspent.
     - Have inputs greater than or equal to the outputs.
     - Be rejected if transaction value is below minimum."))

(def transaction-pool (clojure.lang.PersistentQueue/EMPTY))

(deftype Blockchain [blocks]
  Ledger
  (balance [this address]
    (reduce (fn [bal [txid tx]]
              (reduce (fn [b [a amount]]
                        (if (= a address)
                          (+ b amount)
                          b))
                      bal (:outs tx)))
            0 (index this)))
  (index [this]
    (let [index' (transient {})]
      (doseq [b blocks]
        (when-not (= (:idx b) 0)
          (doseq [tx (:txs b)]
            (let [ins  (:ins tx)
                  outs (:outs tx)]
              (doseq [[txid _ :as in] ins]
                (dissoc! index' txid))
              (assoc! index' (hash tx) tx)))))
      (persistent! index')))
  (valid-block? [this block]
    (let [txs       (:txs block)
          reward-tx (first (filter #(empty? (:ins %)) txs))]
      (doseq [tx txs]
        (let [ins       (:ins tx)
              outs      (:outs tx)
              index     (index this)
              in-vals   (map second ins)
              out-vals  (map second outs)
              total-in  (reduce + in-vals)
              total-out (reduce + out-vals)]
          (assert (or (= tx reward-tx)
                      (every? (fn [[txid amount]]
                                (if-let [tx (get index txid)]
                                  (let [outs (:outs tx)
                                        out  (reduce + (map second outs))]
                                    (>= (or out 0) amount))))
                                ins))
                  "Insufficient funds.")
          (assert (valid? block)
                  "No PoW demonstrated.")
          (assert (or (not= tx reward-tx)
                      (= total-out reward))
                  "Reward is more than agreed.")
          (assert (or (= tx reward-tx)
                      (>= total-in total-out))
                  "Input less than output.")
          (assert (> total-out min-tx)
                  "Transaction too small.")))
      block))
  (new-block [this address txs]
    (let [reward-tx  (->Transaction [] [[address reward]] (now))
          new-txs    (into [reward-tx] txs)
          prev-block (peek blocks)]
      (->Block (count this) (hash prev-block) (now) 0 new-txs)))
  (mine-block [this block]
    (let [mined (mine block)]
      (try
        (valid-block? this mined)
        (catch AssertionError e mined))))
  clojure.lang.Seqable
  (seq [_] (seq blocks))
  clojure.lang.IPersistentVector
  (cons [this block]
    (Blockchain. (conj blocks block)))
  clojure.lang.Counted
  (count [_] (inc (.-idx (peek blocks))))
  clojure.lang.Indexed
  (nth [_ idx] (nth blocks idx)))

(def blockchain (->Blockchain [genesis]))

(comment

  ;;;; Server

  (defn handler [msg]
    (condp = msg
      "new-transaction"
        (conj transaction-pool "new-transaction")))
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
