(ns fx-clj.core
  (:require [clj-http.client :as client]
            [clojure.string :as string]
            [clojure.data.json :as json]
            ))

(defn gen-header []
  (let [ auth (str "Bearer " (System/getenv "OANDA_API_ACCESS_TOKEN"))]
    {"Authorization" auth }))

(def rest-api-url
  {:sandbox "http://api-sandbox.oanda.com"
   :practice "https://api-fxpractice.oanda.com"
   :live "https://api-fxtrade.oanda.com"})

(def stream-api-url
  {:sandbox "http://stream-sandbox.oanda.com"
   :practice "https://stream-fxpractice.oanda.com"
   :live "https://stream-fxtrade.oanda.com"})

(defn get-price [rest_url cur]
  (:body (client/get (str rest_url
                          "/v1/prices"
                          "?instruments=" (string/join "%2C" cur) "&"
                          "accountId=" (System/getenv "OANDA_API_ACCOUNT_ID")
                          ) {:as :json :headers (gen-header)} )))

;; (println (get-price (rest-api-url :sandbox) ["EUR_USD"]))

(defrecord tick-event [instrument time bid ask])

;; 2015-08-11T10:47:21.768367Z
;; (f/parse (f/formatters :date-time) "2015-08-11T10:47:21.768367Z")
(defn make-tick-event [resp]
  (let [price (first ((json/read-str resp) "prices"))]
    (->tick-event (price "instrument")
                  (price "time")
                  (price "bid")
                  (price "ask"))))
;; (make-tick-event (get-price (rest-api-url :sandbox) ["EUR_USD"]))
;; (make-tick-event (get-price (stream-api-url :practice) ["EUR_USD"]))

(defn sma-init []
  (def strategy-queue (ref ()))
  (def sma-short-pre (ref 0.0))
  (def sma-long-pre (ref 0.0))
  (def sma-short (ref 0.0))
  (def sma-long (ref 0.0)))

(defn drop-and-conj [val limit]
  (if (> (count @strategy-queue) limit)
    (dosync (alter strategy-queue drop-last)))
  (dosync (alter strategy-queue conj val)))
  
(defn average [lst] (/ (reduce + lst) (count lst)))

(defn string->number [str]
  (let [n (read-string str)]
    (if (number? n) n nil)))

(defn sma [tick]
  ;; キューに 100個の tick-eventを詰める. 100以上は捨てる
;;  (drop-and-conj (string->number (:bid tick)) 10)
  (drop-and-conj (:bid tick) 100)
  ;; キューにある程度tick-eventがたまったら処理
  (println (str @sma-short " " @sma-long))
  (if (= (count @strategy-queue) 101)
    ;; データをセット
      (dosync
       (ref-set sma-short-pre @sma-short)
       (ref-set sma-long-pre @sma-long)
       (ref-set sma-short (average (take 25 @strategy-queue)))
       (ref-set sma-long (average (take 75 @strategy-queue)))
       )
      ))

(defn sma-buy-condition []
  (and (< @sma-short-pre @sma-long-pre) (> @sma-short @sma-long)))

(defn sma-sell-condition []
  (and (> @sma-short-pre @sma-long-pre) (< @sma-short @sma-long)))

(defn sma-judge []
  (if (sma-buy-condition)
    (println "buy"))
  (if (sma-sell-condition)
    (println "sell")))

;; (drop-and-conj (string->number
;;                 (:bid (->tick-event "EUR_USD" "24.07.2015 01:00:00.191" "1.09890" "1.09892"))) 5)


(def pre-bid-value (atom ""))

(defn same-tick-event? [tick]
  (= (:bid tick) @pre-bid-value))

(defn -main [& args]
  (println "== Forex trade start ==")
  (sma-init)
  (while true
    (let [tick (make-tick-event (get-price (rest-api-url :sandbox) ["EUR_USD"]))]
      ;; 前回と同じ bid値の場合は処理をしない
      (when-not (same-tick-event? tick)
        (do
          ;; bid値を更新
          (reset! pre-bid-value (:bid tick))
          
          ;; strategyを実行 
          (sma tick)
          
          ;; 判定
          (sma-judge)
          )))))
