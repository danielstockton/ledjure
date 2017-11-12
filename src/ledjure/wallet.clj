(ns ledjure.wallet
  (:require [ledjure.crypto :as crypto]))

(defprotocol IWallet
  (new-address [this]
    "Generate a new address."))

(defrecord Wallet [addresses]
  IWallet
  (new-address [this]
    (let [keypair     (crypto/generate-keypair)
          public-key  (.getPublic keypair)
          private-key (.getPrivate keypair)
          address     (crypto/address public-key)]
      (swap! addresses assoc address {:public-key  public-key
                                      :private-key private-key})
      address)))
