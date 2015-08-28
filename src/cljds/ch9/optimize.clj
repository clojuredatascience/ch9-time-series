(ns cljds.ch9.optimize
  (:require [incanter.core :as i]
            [incanter.optimize :as o]
            [incanter.stats :as s]
            [incanter.datasets :as d]
            [incanter.charts :as c]
            [cljds.ch9.data :refer :all]))

(defn forecast [coefs]
  (fn [x]
    (first
     (i/mmult (i/trans coefs)
              (i/matrix [1.0 x (i/sq x)])))))

(defn polynomial-forecast [coefs degree]
  (fn [x]
    (first
     (i/mmult (i/trans coefs)
              (for [i (range (inc degree))]
                (i/pow x i))))))

(defn prediction-interval [x y xp]
  (let [xtx    (i/mmult (i/trans x) x)
        xtxi   (i/solve xtx)
        xty    (i/mmult (i/trans x) y)
        coefs  (i/mmult xtxi xty)
        fitted (i/mmult x coefs)
        resid  (i/minus y fitted)
        rss    (i/sum-of-squares resid)
        n      (i/nrow y)
        p      (i/ncol x)
        dfe    (- n p)
        mse    (/ rss dfe)
        se-y   (first (i/mmult (i/trans xp) xtxi xp))
        t-stat (i/sqrt (* mse (+ 1 se-y)))
        intl   (* (s/quantile-t 0.975 :df dfe) t-stat)
        yp     (first (i/mmult (i/trans coefs) xp))]
    (vector (- yp intl) (+ yp intl))))

(defn calculate []
  (let [longley (d/get-dataset :longley)
        y       (i/$ :x5 longley)
        x       (i/$ :x6 longley)
        xs      (i/bind-columns x (i/sq x))
        model   (s/linear-model y xs)]
    (println model)
    (-> (c/scatter-plot (i/$ :x6 longley) (i/$ :x5 longley))
        (c/add-function (forecast (:coefs model)) 1945 1970)
        (i/view))))

(defn predict2 [coefs multinomial]
  (fn [x]
    (first (i/mmult (i/trans coefs)
                    (for [i (range multinomial)]
                      (i/pow x i))))))

(defn calculate2 []
  (let [longley (d/get-dataset :longley)
        y       (i/$ :x4 longley)
        x       (-> (i/$ :x6 longley)
                    (s/sweep))
        multinomial 11
        xs (reduce i/bind-columns
                   (for [i (range multinomial)]
                     (i/pow x i)))
        model   (s/linear-model y xs :intercept false)]
    (-> (c/scatter-plot (range (count xs)) (:residuals model))
        (i/view))
    (-> (c/scatter-plot (i/$ 1 xs) y)
        (c/add-function (predict2 (:coefs model) multinomial)
                        -7.5 7.5)
        (i/view))))
