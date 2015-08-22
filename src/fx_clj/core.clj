(ns fx-clj.core
  (:require [clojure-csv.core :as csv])
  (:require [clj-time.core :as t])
  (:require [clj-time.format :as f])
  (:require [clojure.data.json :as json])
  (:require [clj-http.client :as client]))

(defn get-instruments []
  (client/get
   "http://api-sandbox.oanda.com/v1/prices?instruments=EUR_USD"))

(defrecord tick-event [instrument time bid ask])

;; 2015-08-11T10:47:21.768367Z
;; (f/parse (f/formatters :date-time) "2015-08-11T10:47:21.768367Z")

(defn make-tick-event [resp]
  (let [price (first ((json/read-str (resp :body)) "prices"))]
    (->tick-event (price "instrument")
                  (price "time")
                  (price "bid")
                  (price "ask"))))

;; (def stop-process-time (ref (t/now)))

  ;; 初期値は昔にする
(def stop-process-time (ref
                        (f/parse (f/formatter "dd.MM.yyyy HH:mm:ss.SSS") "01.01.2000 00:00:00.000")))

(def strategy-queue (ref ()))

;; (need-sampling? (->tick-event "EUR_USD" "24.07.2015 01:00:00.191" "1.09890" "1.09892") 5 (f/formatter "dd.MM.yyyy HH:mm:ss.SSS"))
;; (f/parse (f/formatter "dd.MM.yyyy HH:mm:ss.SSS") "24.07.2015 01:00:00.191")
;; (t/after? (f/parse (f/formatter "dd.MM.yyyy HH:mm:ss.SSS") "24.07.2015 01:00:00.192") @stop-process-time)

;; 指定された秒数たったか？
(defn need-sampling? [tick sec formatter]
  (let [time (f/parse formatter (:time tick))]
    (if (t/after? time @stop-process-time)
      (dosync (ref-set stop-process-time (t/plus time (t/seconds sec)))
              true)
      false)))

(defn drop-and-conj [val limit]
  (if (> (count @strategy-queue) limit)
    (dosync (alter strategy-queue drop-last)))
  (dosync (alter strategy-queue conj val)))
  
(defn average [lst] (/ (reduce + lst) (count lst)))

(defn string->number [str]
  (let [n (read-string str)]
    (if (number? n) n nil)))

(defn sma-init []
  (def strategy-queue (ref ()))
  (def sma-short-pre (ref 0.0))
  (def sma-long-pre (ref 0.0))
  (def sma-short (ref 0.0))
  (def sma-long (ref 0.0)))

(defn sma [tick]
  (drop-and-conj (string->number (:bid tick)) 100)
  (if (= (count @strategy-queue) 101)
    (do 
    ;; データをセット
    (dosync
     (ref-set sma-short-pre @sma-short)
     (ref-set sma-long-pre @sma-long)
     (ref-set sma-short (average (take 25 @strategy-queue)))
     (ref-set sma-long (average (take 75 @strategy-queue)))
     )
    ;; 判定
    (if (or (and (< @sma-short-pre @sma-long-pre) (> @sma-short @sma-long))
            (and (> @sma-short-pre @sma-long-pre) (< @sma-short @sma-long)))
      true
      false))
    false))

(defn make-tick-event-fm-csv [data]
  (->tick-event "EUR_USD"
                (get data 0)
                (get data 2)
                (get data 1)))

(defn simulation []
  (println "Simulation start")
  (let [data (rest (csv/parse-csv (slurp "data/sample.csv")))
        tick-events (map make-tick-event-fm-csv data)]
    ;; 初期化処理
    (sma-init)
    (def stop-process-time (ref
                            (f/parse (f/formatter "dd.MM.yyyy HH:mm:ss.SSS") "01.01.2000 00:00:00.000")))
    ;; 一つずつ取り出して操作する
    (map (fn [tick-event]
           (if (need-sampling? tick-event 5 (f/formatter "dd.MM.yyyy HH:mm:ss.SSS"))
             (println (sma tick-event))))
         tick-events)))

(defn -main [& args]
  (println "Fx trade start")
  ;; 無限ループ
  (while true
    ;; tickデータをサーバより取得
    (let [tick-event (make-tick-event (get-instruments))]
      ;; 指定された秒数分間引く
      (if (need-sampling? tick-event 5 (f/formatters :date-time))
        (sma tick-event)))))
