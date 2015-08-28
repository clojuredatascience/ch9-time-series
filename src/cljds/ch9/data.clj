(ns cljds.ch9.data
  (:require [incanter.datasets :as d]
            [incanter.core :as i]
            [incanter.stats :as s]
            [clj-time.format :as tf]))

(def time-format
  (tf/formatter "MMM YYYY"))

(defn to-time [month year]
  (tf/parse time-format (str month " " year)))

(defn airline-passengers []
  (->> (d/get-dataset :airline-passengers)
       (i/add-derived-column :time [:month :year] to-time)
       (i/$order :time :asc)
       (i/$ :passengers)))


(defn airline-residuals []
  (let [data (i/log (airline-passengers))
        model (s/linear-model data (range (count data)))]
    (:residuals model)))

(defn re-trend [ys]
  (let [xs    (range (count ys))
        coefs (:coefs (s/linear-model ys xs))]
    (fn [x y]
      (let [y-hat (i/mmult (i/trans coefs)
                           (vector 1.0 x))]
        (+ (first y-hat) y)))))

(defn longley [col]
  (->> (d/get-dataset :longley)
       (i/$ [:x6 col])
       (i/to-matrix)))
