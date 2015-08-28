(ns cljds.ch9.examples
  (:require [cljds.ch9.arima :refer :all]
            [cljds.ch9.data :refer :all]
            [cljds.ch9.optimize :refer :all]
            [incanter.charts :as c]
            [incanter.core :as i ]
            [incanter.datasets :as d ]
            [incanter.stats :as s]
            [incanter.svg :as svg]
            [succession.core :as sc]))

(defn ex-9-1 []
  (-> (d/get-dataset :longley)
      (i/view)))

(defn ex-9-2 []
  (let [data (d/get-dataset :longley)]
    (-> (c/scatter-plot (i/$ :x6 data)
                        (i/$ :x5 data)
                        :x-label "Year"
                        :y-label "Population")
        (i/view))))

(defn ex-9-3 []
  (let [data  (d/get-dataset :longley)
        model (s/linear-model (i/$ :x5 data)
                              (i/$ :x6 data))]
    (println "R-square" (:r-square model))
    (-> (c/scatter-plot (i/$ :x6 data)
                        (i/$ :x5 data)
                        :x-label "Year"
                        :y-label "Population")
        (c/add-lines (i/$ :x6 data)
                     (:fitted model))
        (i/view))))

(defn ex-9-4 []
  (let [data  (d/get-dataset :longley)
        x     (i/$ :x6 data)
        xs    (i/bind-columns x (i/sq x))
        model (s/linear-model (i/$ :x5 data) xs)]
    (println "R-square" (:r-square model))
    (-> (c/scatter-plot (i/$ :x6 data)
                        (i/$ :x5 data)
                        :x-label "Year"
                        :y-label "Population")
        (c/add-lines (i/$ :x6 data)
                     (:fitted model))
        (i/view))))

(defn ex-9-5 []
  (let [data  (d/get-dataset :longley)
        x     (i/$ :x6 data)
        xs    (i/bind-columns x (i/sq x))
        model (s/linear-model (i/$ :x5 data) xs)]
    (-> (c/scatter-plot (i/$ :x6 data)
                        (i/$ :x5 data)
                        :x-label "Year"
                        :y-label "Population")
        (c/add-function (forecast (:coefs model))
                        1947 1970)
        (i/view))))

(defn ex-9-6 []
  (let [data (d/get-dataset :longley)]
    (-> (c/scatter-plot (i/$ :x6 data)
                        (i/$ :x4 data)
                        :x-label "Year"
                        :y-label "Size of Armed Forces")
        (i/view))))

(defn ex-9-7 []
  (let [data (d/get-dataset :longley)
        degree 11
        x  (s/sweep (i/$ :x6 data))
        xs (reduce i/bind-columns
                   (for [i (range (inc degree))]
                     (i/pow x i)))
        model (s/linear-model (i/$ :x4 data) xs
                              :intercept false)]
    (println "R-square" (:r-square model))
    (-> (c/scatter-plot (i/$ 1 xs) (i/$ :x4 data)
                        :x-label "Distance from Mean (Years)"
                        :y-label "Size of Armed Forces")
        (c/add-function (polynomial-forecast (:coefs model)
                                             degree)
                        -7.5 7.5)
        (i/view))))


(defn ex-9-8 []
  (let [data (d/get-dataset :longley)
        degree 10
        x  (s/sweep (i/$ :x6 data))
        xs (reduce i/bind-columns
                   (for [i (range (inc degree))]
                     (i/pow x i)))
        model (s/linear-model (i/$ :x4 data) xs
                              :intercept false)]
    (println "R-square" (:r-square model))
    (-> (c/scatter-plot (i/$ 1 xs) (i/$ :x4 data)
                        :x-label "Distance from Mean (Years)"
                        :y-label "Size of Armed Forces")
        (c/add-function (polynomial-forecast (:coefs model)
                                             degree)
                        -7.5 10)
        (i/view))))

(defn ex-9-9 []
  (-> (d/get-dataset :airline-passengers)
      (i/view)))

(defn ex-9-10 []
  (-> (airline-passengers)
      (timeseries-plot)))

(defn ex-9-11 []
  (-> (airline-passengers)
      (i/log)
      (timeseries-plot)))

(defn ex-9-12 []
  (let [data (i/log (airline-passengers))
        xs   (range (count data))
        model (s/linear-model data xs)]
    (-> (c/xy-plot xs (:residuals model)
                   :x-label "Time"
                   :y-label "Residual")
        (i/view))))

(defn ex-9-13 []
  (-> (airline-passengers)
      (i/log)
      (difference)
      (timeseries-plot)))

(defn ex-9-14 []
  (take 5 (constant-series 42)))

(defn ex-9-15 []
  (->> (random-walk 0)
       (take 50)
       (timeseries-plot)))

(defn ex-9-16 []
  (->> (ar [1] [2] 0)
       (take 10)
       (timeseries-plot)))

(defn ex-9-17 []
  (let [init (s/sample-normal 5)]
    (->> (ar init [0 0 0 0 1] 0)
         (take 30)
         (timeseries-plot))))

(defn ex-9-18 []
  (let [init (s/sample-normal 5)]
    (->> (ar init [0 0 0 0 1] 0.2)
         (take 30)
         (timeseries-plot))))

(defn ex-9-19 []
  (let [init (s/sample-normal 5)
        coefs [0 0 0 0 1]]
    (->> (ar init coefs 0.2)
         (take 100)
         (autocorrelation)
         (take 15)
         (bar-plot))))

(defn ex-9-20 []
  (let [init (s/sample-normal 5)
        coefs [0 0 0 0 1]]
    (->> (ma init coefs 0.5)
         (take 100)
         (timeseries-plot))))

(defn ex-9-21 []
  (let [init (s/sample-normal 5)
        coefs [0 0 0 0 1]]
    (->> (ma init coefs 0.2)
         (take 5000)
         (autocorrelation)
         (take 15)
         (bar-plot))))

(defn ex-9-22 []
  (let [ys (s/sample-normal 10 :sd 1.0)
        es (s/sample-normal 10 :sd 0.2)
        ps   [0 0 0 0.3 0.5]
        qs   [0.2 0.8]]
    (->> (arma ys es ps qs 0.2)
         (take 500)
         (timeseries-plot))))

(defn ex-9-23 []
  (let [ys (s/sample-normal 10 :sd 1.0)
        es (s/sample-normal 10 :sd 0.2)
        ps   [0 0 0 0.3 0.5]
        qs   [0.2 0.8]]
    (->> (arma ys es ps qs 0.2)
         (take 500)
         (autocorrelation)
         (take 15)
         (bar-plot))))

(defn ex-9-24 []
  (let [ys (s/sample-normal 10 :sd 1.0)
        es (s/sample-normal 10 :sd 0.2)
        ps   [0 0 0 0.3 0.5]
        qs   [0.2 0.8]]
    (->> (arma ys es ps qs 0.2)
         (take 500)
         (partial-autocorrelation)
         (take 15)
         (bar-plot))))

(defn ex-9-25 []
  (->> (airline-passengers)
       (difference)
       (autocorrelation)
       (take 15)
       (bar-plot)))

(defn ex-9-26 []
  (->> (airline-passengers)
       (difference)
       (partial-autocorrelation)
       (take 15)
       (bar-plot)))

(defn ex-9-27 []
  (->> (airline-passengers)
       (difference)
       (difference 12)
       (autocorrelation)
       (take 15)
       (bar-plot)))

(defn ex-9-28 []
  (->> (airline-passengers)
       (difference)
       (difference 12)
       (partial-autocorrelation)
       (take 15)
       (bar-plot)))

(defn ex-9-29 []
  (let [data 56
        f (fn [p]
            (s/pdf-binomial data :size 100 :prob p))]
    (-> (c/function-plot f 0.3 0.8
                         :x-label "P"
                         :y-label "Likelihood")
        (i/view))))


(defn ex-9-30 []
  (let [init  (s/sample-normal 2)
        coefs [0 0.5]
        data  (take 100 (ar init coefs 0.2))
        f     (fn [coef]
                (sc/log-likelihood [0 coef] 2 0 data))]
    (-> (c/function-plot f -1 1
                         :x-label "Coefficient"
                         :y-label "Log-Likelihood")
        (i/view))))

(defn ex-9-31 []
  (->> (airline-passengers)
       (i/log)
       (difference)
       (difference 12)
       (arma-model 9 1)))

(def params
  {:ar [-0.23769808471685377 -0.012617164166298971 -0.22328924366924702 -0.11315520136074947 0.07598703443471418 0.05977824480057639 -0.0621015571229344 0.0727399301235127 0.24605963571664013], :ma [-0.14754455658280236], :ll 232.97813750669314}) 

(defn ex-9-32 []
  (let [data (i/log (airline-passengers))
        diff-1  (difference 1 data)
        diff-12 (difference 12 diff-1)
        forecast (->> (arma (take 9 (reverse diff-12))
                       []
                       (:ar params)
                       (:ma params) 0)
                      (take 100)
                      (undifference 12 diff-1)
                      (undifference 1 data))]
    (->> (concat data forecast)
         (i/exp)
         (timeseries-plot))))

(defn ex-9-33 []
  (let [data (i/log (airline-passengers))
        diff-1  (difference 1 data)
        diff-12 (difference 12 diff-1)
        forecast (->> (arma (take 9 (reverse diff-12))
                       [-7.65E-4]
                       (:ar params)
                       (:ma params) 0.0414)
                      (take 100)
                      (undifference 12 diff-1)
                      (undifference 1 data))]
    (->> (concat data forecast)
         (i/exp)
         (timeseries-plot))))

(defn ex-9-34 []
  (let [data    (i/log (airline-passengers))
        diff-1  (difference 1 data)
        diff-12 (difference 12 diff-1)
        init    (take 9 (reverse diff-12))
        forecasts (for [n (range 1000)]
                    (take 100
                          (arma init []
                                (:ar params)
                                (:ma params)
                                0.0005)))
        forecast-mean (map s/mean (i/trans forecasts))
        forecast-sd (-> (map s/sd (i/trans forecasts))
                        (i/div 2)
                        (i/mult 1.96))
        upper (->> (map + forecast-mean forecast-sd)
                   (undifference 12 diff-1)
                   (undifference 1 data)
                   (concat data)
                   (i/exp))
        lower (->> (map - forecast-mean forecast-sd)
                   (undifference 12 diff-1)
                   (undifference 1 data)
                   (concat data)
                   (i/exp))
        n (count upper)]
    (-> (c/xy-plot   (range n) upper
                     :x-label "Time"
                     :y-label "Value"
                     :series-label "Upper Bound"
                     :legend true)
        (c/add-lines (range n) lower
                     :series-label "Lower Bound")
        (i/view))))
