(ns ledjure.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [ledjure.core :as l]))

(def address "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy")

(deftest mining
  (let [chain  l/blockchain
        chain  (reduce
                (fn [c i]
                  (let [b (l/mine-block c (l/new-block c address []))]
                    (conj c b)))
                chain (range 3))]
    (testing "Rewards are...rewarded"
      (is (= (l/balance chain address) (* 3 l/reward))))
    (testing "PoW"
      (let [new-block (l/new-block chain address [])
            mined     (l/mine new-block)]
        (is (not (l/valid? new-block)))
        (is (l/valid? mined))))))

(deftest transactions
  (let [chain  l/blockchain
        block  (l/mine-block chain (l/new-block chain address []))
        chain  (conj chain block)
        utxoid (ffirst (seq (l/index chain)))]
    (testing "Double spending prevented"
      (let [tx    [(l/->Transaction [[utxoid 50]] [["someone" 50]] (l/now))]
            block (l/mine-block chain (l/new-block chain "/dev/null" tx))]
        (is (l/valid-block? chain block))
        (let [chain (conj chain block)
              tx    [(l/->Transaction [[utxoid 50]] [["someone" 50]] (l/now))]
              block (l/mine-block chain (l/new-block chain "/dev/null" tx))]
          (is (thrown-with-msg? AssertionError #"Insufficient funds"
                                (l/valid-block? chain block))))))))
