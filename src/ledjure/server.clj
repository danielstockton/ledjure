(ns ledjure.server
  (:require [aleph.tcp :as tcp]
            [clojure.edn :as edn]
            [cognitect.transit :as transit]
            [gloss.core :as gloss]
            [gloss.io :as io]
            [io.aviso.ansi :as ansi]
            [ledjure.blockchain :as blockchain]
            [manifold.deferred :as d]
            [manifold.stream :as s])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn transit-write [msg]
  (let [out    (ByteArrayOutputStream. 4096)
        writer (transit/writer out :json {:handlers blockchain/write-handlers})]
    (transit/write writer msg)
    (.toString out)))

(defn transit-read [msg]
  (let [in     (ByteArrayInputStream. (.getBytes msg))
        reader (transit/reader in :json {:handlers blockchain/read-handlers})]
    (transit/read reader)))

(def protocol
  (gloss/compile-frame
   (gloss/finite-frame :uint32 (gloss/string :utf-8))
   transit-write
   transit-read))

(defn wrap-duplex-stream
  [protocol s]
  (let [out (s/stream)]
    (s/connect
     (s/map #(io/encode protocol %) out)
     s)
    (s/splice
     out
     (io/decode-stream s protocol))))

(defmulti dispatch (fn [msg system] (:k msg)))

(defmethod dispatch :peers/info
  [_ {:keys [wallet]}]
  {:address (ffirst @(:addresses wallet))})

(defmethod dispatch :peers/sync
  [{:keys [payload]} {:keys [blockchain peers server tx-pool wallet]}]
  (let [{:keys [host port]} payload
        blockchain          @blockchain
        address             (ffirst @(-> wallet :addresses))]
    (when (< (count @peers) 100)
      (swap! peers conj payload))
    (println (ansi/blue (str "Connected peers: " (pr-str @peers))))
    (println (ansi/blue (str "Current balance: "
                             (blockchain/balance blockchain address))))
    {:blockchain blockchain
     :peers      @peers
     :txs        (vec tx-pool)
     :address    address}))

(defmethod dispatch :transactions/new
  [{:keys [payload]} {:keys [blockchain tx-pool wallet]}]
  (let [txs (map #(apply blockchain/->Transaction %) payload)]
    (println (ansi/green (str "Incoming txs: " (pr-str txs))))
    ;; (conj tx-pool payload)
    (let [address     (ffirst @(:addresses wallet))
          blocks      (:blocks @blockchain)
          new-block   (blockchain/new-block @blockchain address txs)
          mined-block (blockchain/mine-block @blockchain new-block)
          chain       (blockchain/->Blockchain (conj blocks mined-block))]
      (println (ansi/blue (str "Chain length: " (count (:blocks chain)))))
      (reset! blockchain chain)
      true)))

(defn handler
  [system]
  (fn [s msg]
    (s/connect
     (s/map #(dispatch % system) s)
     s)))

(defn server
  [handler port]
  (tcp/start-server
   (fn [s info]
     (handler (wrap-duplex-stream protocol s) info))
   {:port port}))

(defn client
  [host port]
  (d/chain (tcp/client {:host host :port port})
           #(wrap-duplex-stream protocol %)))

(defn send-msg
  [host port msg]
  (let [c @(client host port)]
    @(s/put! c msg)
    @(s/take! c)))
