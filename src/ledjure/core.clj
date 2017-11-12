(ns ledjure.core
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [io.aviso.ansi :as ansi]
            [ledjure.blockchain :as blockchain]
            [ledjure.server :as server]
            [ledjure.crypto :as crypto]
            [ledjure.util :as util]
            [overtone.at-at :as at]))

(defn host-and-port [uri]
  (let [[host port] (str/split uri #":")]
    [host (Integer. port)]))

(defn config [uri peers]
  (let [[host port] (host-and-port uri)]
    {:blockchain {:wallet (ig/ref :wallet)}
     :handler    {:blockchain (ig/ref :blockchain)
                  :peers      (ig/ref :peers)
                  :tx-pool    (ig/ref :tx-pool)
                  :wallet     (ig/ref :wallet)}
     :peers      {:peers peers}
     :server     {:handler (ig/ref :handler)
                  :host    host
                  :port    port}
     :tx-pool    {}
     :wallet     {}}))

(defmethod ig/init-key :blockchain [_ {:keys [blockchain wallet]}]
  (atom (blockchain/new (-> wallet :address))))

(defmethod ig/init-key :handler [_ opts]
  (server/handler opts))

(defmethod ig/init-key :peers [_ {:keys [peers]}]
  (atom peers))

(defmethod ig/init-key :server [_ {:keys [handler host port] :as opts}]
  {:server (server/server handler port)
   :host   host
   :port   port})

(defmethod ig/init-key :tx-pool [_ _]
  (clojure.lang.PersistentQueue/EMPTY))

(defmethod ig/init-key :wallet [_ _]
  {:address     (crypto/address crypto/public-key)
   :public-key  crypto/public-key
   :private-key crypto/private-key})

(defmethod ig/halt-key! :server [_ server]
  (.close (:server server)))

(defn system [uri peer]
  (let [[host port] (host-and-port uri)]
    (if peer
      (let [[phost pport]   (host-and-port peer)
            {:keys [blockchain
                    peers]} (server/send-msg phost pport
                                             {:k       :peers/sync
                                              :payload {:host host
                                                        :port port}})
            peers           (->> peers
                                 (remove #(and (= (:host %) host)
                                               (= (:port %) port)))
                                 (into #{}))
            system          (ig/init (config uri peers))]
        (reset! (:blockchain system) blockchain)
        system)
      (ig/init (config uri #{})))))

(defn sync-network
  [system]
  (let [{:keys [peers server]} system
        peers                  @peers]
    (if (empty? peers)
      (println (ansi/red "No peers connected."))
      (doseq [{:keys [host port]} peers]
        (let [{:keys [blockchain
                      peers
                      txs
                      address]} (server/send-msg
                                 host port
                                 {:k       :peers/sync
                                  :payload {:host (:host server)
                                            :port (:port server)}})
              chain             (:blockchain system)]
          (when (> (count (:blocks blockchain))
                   (count (:blocks @chain)))
            (println "New chain!" blockchain)
            (reset! chain blockchain)))))))

(defn send-coins
  [system]
  (let [blockchain @(:blockchain system)
        wallet     (:wallet system)
        address    (:address wallet)
        peers      @(:peers system)
        index      (blockchain/index blockchain)
        utxos      (reduce (fn [res [txid tx]]
                             (let [outs  (:outs tx)
                                   total (->> outs
                                              (map (fn [[a i]] (if (= a address) i 0)))
                                              (apply +))]
                               (if (pos? total)
                                 (assoc res txid total)
                                 res)))
                           {} index)]
    (doseq [{:keys [host port]} peers]
      (let [info       (server/send-msg host port {:k :peers/info})
            raddress   (:address info)
            public-key (crypto/encode64 (.getEncoded (:public-key wallet)))
            utxo       (first utxos)
            txid       (first utxo)
            sig        (crypto/sign txid crypto/private-key)
            amount     (inc (rand-int 5))
            change     (dec (- (second utxo) amount))
            ins        [[txid sig public-key]]
            outs       [[raddress amount] [address change]]
            txs        [[ins outs (util/now)]]]
        (server/send-msg host port
                         {:k       :transactions/new
                          :payload txs})))))

(defn -main [& args]
  (println (ansi/yellow "Starting node..."))
  (let [[uri peer] args
        pool        (at/mk-pool)
        system      (system uri peer)]
    (println (ansi/green (-> system :wallet :address)))
    (at/every 5000 #(sync-network system) pool)
    (when (nil? peer)
        (at/every 10000 #(send-coins system) pool))
    (while true
      (Thread/sleep 100))))

(comment
  (def system (ig/init (config 1111 #{} nil)))

  (ig/halt! system)

  (server/send-msg "localhost" 1111 {:k :peers/sync})

  (server/send-msg "localhost" 1111 {:k :transactions/new})

  )
