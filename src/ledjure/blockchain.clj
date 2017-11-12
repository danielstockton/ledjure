(ns ledjure.blockchain
  (:refer-clojure :exclude [hash])
  (:require [ledjure.crypto :as crypto]
            [ledjure.util :as util]
            [cognitect.transit :as transit]
            [io.aviso.ansi :as ansi]))

(def ^:const version 0.1)
(def ^:const block-difficulty 4)
(def ^:const leading-zeros (apply str (repeat block-difficulty "0")))
(def ^:const max-coins 1e6)
(def ^:const min-tx 1/1000)
(def ^:const reward 50)

(defprotocol Hashable
  (hash [this] [this nonce]))

(defprotocol Verifiable
  (valid? [this] [this nonce]))

;; Input -> (txid, signature, public-key)
;; Output -> (address, amount)

(defrecord Transaction [ins outs lock-time]
  Hashable
  (hash [_]
    (crypto/hash-str
     (crypto/sha256*2
      (.getBytes (str (pr-str ins) (pr-str outs) lock-time)))))
  Verifiable
  (valid? [this]
    (every? true?
            (for [[txid sig pk] ins]
              (crypto/verify sig (hash this) pk)))))

(def transaction-write-handler
  (transit/write-handler "Transaction"
                         (fn [o]
                           [(:ins o) (:outs o) (:lock-time o)])))

(def transaction-read-handler
  (transit/read-handler (fn [o] (apply ->Transaction o))))

(defprotocol Mineable
  (mine [this]))

(defrecord Block [idx prev tstamp nonce txs]
  Hashable
  (hash [this]
    (hash this nonce))
  (hash [this nonce]
    (let [root (apply str txs)]
      (crypto/hash-str
       (crypto/sha256*2
        (.getBytes
         (str version idx prev root tstamp block-difficulty nonce))))))
  Mineable
  (mine [this]
    (when (valid? this)
      (throw (Exception. "Already mined ;)")))
    (println (ansi/magenta (str "Mining block " idx)))
    (loop [n 0]
      (if (valid? this n)
        (do
          (println (ansi/magenta (str "Mined " idx "!")))
          (->Block idx prev tstamp n txs))
        (recur (inc n)))))
  Verifiable
  (valid? [this]
    (valid? this nonce))
  (valid? [this nonce]
    (let [h (hash this nonce)
          s (subs h 0 block-difficulty)]
      (= s leading-zeros))))

(def block-write-handler
  (transit/write-handler "Block"
                         (fn [o]
                           [(:idx o) (:prev o) (:tstamp o)
                            (:nonce o) (:txs o)])))

(def block-read-handler
  (transit/read-handler (fn [o] (apply ->Block o))))

(defn genesis [address]
  (->Block 0 "GENESIS" (util/now) 0
           [(->Transaction [] [[address 1e6]] (util/now))]))

(defprotocol Ledger
  (balance [this address]
    "Return the balance associated with the address.")
  (index [this]
    "Index txid -> transaction.")
  (new-block [this address txs]
    "Produce a new block ready for mining.")
  (mine-block [this block]
    "Find nonce (PoW) to produce a valid block.")
  (valid-block? [this tx]
    "Validate a new block. The block is allowed to contain a single transaction
     with no inputs that outputs the agreed reward amount.

     Each transaction must:

     - Have correct syntax and data structure.
     - Reference inputs that exist and are unspent.
     - Have inputs greater than or equal to the outputs.
     - Be rejected if transaction value is below minimum."))

(defrecord Blockchain [blocks]
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
        (doseq [tx (:txs b)]
          (let [ins  (:ins tx)
                outs (:outs tx)]
            (doseq [[txid _ :as in] ins]
              (dissoc! index' txid))
            (assoc! index' (hash tx) tx))))
      (persistent! index')))
  (valid-block? [this block]
    (let [txs       (:txs block)
          reward-tx (first (filter #(empty? (:ins %)) txs))
          index     (index this)]
      (doseq [tx txs]
        (let [ins       (:ins tx)
              outs      (:outs tx)
              total-out (reduce + (map second outs))
              total-in  (reduce
                         (fn [res [txid sig epk]]
                           (let [pk        (crypto/bytes->pk (crypto/decode64 epk))
                                 outs      (:outs (get index txid))
                                 total-out (reduce + (map second outs))]
                             (assert (try
                                       (crypto/verify sig txid pk)
                                       (catch Exception _ false))
                                     "Incorrect signature.")
                             (+ total-out res)))
                         0 ins)]
          (assert (or (= tx reward-tx)
                      (> total-in total-out))
                  (str "Insufficient funds: " (pr-str tx)))
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
    (let [reward-tx  (->Transaction [] [[address reward]] (util/now))
          new-txs    (into [reward-tx] txs)
          prev-block (peek blocks)]
      (->Block (count blocks) (hash prev-block) (util/now) 0 new-txs)))
  (mine-block [this block]
    (let [mined (mine block)]
      (valid-block? this mined))))

(def blockchain-write-handler
  (transit/write-handler "Blockchain" (fn [o] (:blocks o))))

(def blockchain-read-handler
  (transit/read-handler (fn [o] (->Blockchain o))))

(def write-handlers
  {Transaction transaction-write-handler
   Block       block-write-handler
   Blockchain  blockchain-write-handler})

(def read-handlers
  {"Transaction" transaction-read-handler
   "Block"       block-read-handler
   "Blockchain"  blockchain-read-handler})

(defn new
  [address]
  (->Blockchain [(genesis address)]))
