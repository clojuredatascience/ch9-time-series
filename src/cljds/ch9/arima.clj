(ns cljds.ch9.arima
  (:require [clatrix.core :as cla]
            [cljds.ch9.data :refer [airline-passengers]]
            [clj-time.coerce :as tc]
            [clj-time.format :as f]
            [clojure.core.matrix.operators :as mat]
            [clojure.core.matrix :refer [inverse]]
            [incanter.charts :as c :refer [time-series-plot scatter-plot xy-plot bar-chart histogram add-lines]]
            [incanter.core :as i :refer [conj-cols $ $map add-derived-column view $where to-dataset log exp solve toeplitz $order to-vect mmult trans matrix identity-matrix]]
            [incanter.datasets :refer [get-dataset]]
            [incanter.optimize :as o]
            [incanter.io :refer [read-dataset]]
            [incanter.stats :as s :refer [mean auto-correlation sample-normal]]
            [incanter.zoo :as zoo]
            [incanter.svg :as svg]
            [succession.core :as sc])
  (:import [org.apache.commons.math3.analysis MultivariateFunction]
           [org.apache.commons.math3.optim OptimizationData InitialGuess MaxEval]
           [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction GoalType]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv BOBYQAOptimizer SimplexOptimizer NelderMeadSimplex ]))


(defn timeseries-plot [series]
  (-> (c/xy-plot (range (count series)) series
               :x-label "Time"
               :y-label "Value")
      (i/view)))

(defn constant-series [y]
  (cons y (lazy-seq (constant-series y))))

(defn stochastic* [x]
  (let [e (sample-normal 1)]
    (+ x e)))

(defn stochastic [x]
  (iterate stochastic* x))

(defn random-walk [y]
  (let [e (sample-normal 1)
        y (+ y e)]
    (cons y (lazy-seq (random-walk y)))))

(defn ar [ys coefs sigma]
  (let [e (sample-normal 1 :sd sigma)
        y (apply + e (map * ys coefs))]
    (cons y (lazy-seq
             (ar (cons y ys) coefs sigma)))))

(defn ma [es coefs sigma]
  (let [e (sample-normal 1 :sd sigma)
        y (apply + e (map * es coefs))]
    (cons y (lazy-seq
             (ma (cons e es) coefs sigma)))))

(defn arma [ys es ps qs sigma]
  (let [e  (sample-normal 1 :sd sigma)
        ar (apply + (map * ys ps))
        ma (apply + (map * es qs))
        y  (+ ar ma e)]
    (cons y (lazy-seq
                (arma (cons y ys)
                      (cons e es)
                      ps qs sigma)))))

(defn plot-autocorrelation [max-lag seq]
  (let [xs (range (inc max-lag))
        ac (map #(auto-correlation seq %) xs)]
    (-> (bar-chart xs ac :x-label "Lag" :y-label "Autocorrelation")
        (view))))

(defn significance [seq]
  (/ 2 (Math/sqrt (count seq))))

(defn bar-plot [ys]
  (let [xs (range (count ys))]
    (-> (c/bar-chart xs ys
                     :x-label "Lag"
                     :y-label "Value")
        (svg/save-svg "/tmp/9-364.svg"))))

(defn difference
  ([series]
   (difference 1 series))
  ([lags series]
   (map - (drop lags series) series)))

(defn undifference* [seed differences]
  (when-not (empty? differences)
    (let [y (+ (last seed)
               (first differences))]
      (cons y
            (lazy-seq
             (undifference* (cons y (butlast seed))
                            (rest differences)))))))

(defn undifference [lags series differences]
  (let [seed (take lags (reverse series))]
    (undifference* seed differences)))


(defn autocovariance* [series n k]
  (let [lag-product (->> (drop k series)
                         (map * series)
                         (i/sum))]
    (cons (/ lag-product n)
          (lazy-seq
           (autocovariance* series n (inc k))))))

(defn autocovariance [series]
  (autocovariance* (s/sweep series) (count series) 0))


(defn autocorrelation* [series variance n k]
  (let [lag-product (->> (drop k series)
                         (map * series)
                         (i/sum))]
    (cons (/ lag-product variance n)
          (lazy-seq
           (autocorrelation* series variance n (inc k))))))

(defn autocorrelation [series]
  (autocorrelation* (s/sweep series)
                    (s/variance series)
                    (dec (count series)) 0))

(defn pac* [pacs sigma prev next]
  (let [acv (first next)
        sum (i/sum (i/mult pacs (reverse prev)))
        pac (/ (- acv sum) sigma)]
    (cons pac
          (lazy-seq
           (pac* (->> (i/mult pacs pac)
                      (reverse)
                      (i/minus pacs)
                      (cons pac))
                 (* (- 1 (i/pow pac 2)) sigma)
                 (cons acv prev)
                 (rest next))))))

(defn partial-autocorrelation [series]
  (let [acvs (autocovariance series)
        acv1 (first  acvs)
        acv2 (second acvs)
        pac  (/ acv2 acv1)]
    (concat [1.0 pac]
            (pac* (vector pac)
                  (- acv1 (* pac acv2))
                  (vector acv2)
                  (drop 2 acvs)))))

;;; Moved to succession

(defn log-likelihood* [{:keys [vt ft at pt t z h]} y]
  (let [v  (i/minus y (i/mmult z at))
        zt (i/trans z)
        f  (inc (first (i/mmult z pt zt)))
        kt (-> (i/mmult t pt zt)
               (i/plus h)
               (i/div f))
        at (i/plus (i/mmult t at)
                   (i/mmult kt v))
        lt (i/minus t (i/mmult kt z))
        jt (i/minus h kt)
        pt (i/plus (i/mmult t pt (i/trans lt))
                   (i/mmult h (i/trans jt)))]    
    {:vt (cons v vt)
     :ft (cons f ft)
     :at at :pt pt :t t :h h :z z}))

(defn log-likelihood [coefs p q ys]
  (let [m     (max p q)
        phi   (take m (concat (take p coefs) (repeat 0)))
        theta (take m (concat (drop p coefs) (repeat 0)))
        init {:at (i/matrix 0 m 1)
              :pt (i/identity-matrix m)
              :t  (i/bind-columns
                   (i/matrix phi)
                   (i/bind-rows
                    (i/identity-matrix (dec m))
                    (i/matrix 0 1 (dec m))))
              :h (i/plus phi theta)
              :z (first (i/identity-matrix m))}
        ll    (reduce log-likelihood* init ys)
        sigma (-> (i/sq  (:vt ll))
                  (i/div (:ft ll))
                  (s/mean))
        n (count ys)]
    (* -0.5 (+ n
               (* n (i/log (* 2 Math/PI)))
               (* n (i/log sigma))
               (i/sum (i/log (:ft ll)))))))


(defn aic [coefs p q ys]
  (- (* 2 (+ p q 1))
     (* 2 (log-likelihood coefs p q ys)))) 

;;; Simplex

(defn objective-function [f]
  (ObjectiveFunction. (reify MultivariateFunction
                        (value [_ v]
                          (f (vec v))))))

(defn make-simplex-optimizer []
  (SimplexOptimizer. (double 1e-6) (double 1e-6)))

(defn arma-model-2 [ts p q]
  (let [m (+ p q)
        f (fn [coefs]
            (sc/log-likelihood coefs p q ts))]
    (o/maximize f (take m (repeat 0.2)))))

(defn arma-model [p q ys]
  (let [m (+ p q)
        f (fn [params]
            (sc/log-likelihood params p q ys))
        optimal (.optimize (SimplexOptimizer. 1e-10 1e-10)
                           (into-array
                            OptimizationData
                            [(MaxEval. 100000)
                             (objective-function f)
                             GoalType/MAXIMIZE
                             (InitialGuess.
                              (double-array (take m (repeat 0.0))))
                             (NelderMeadSimplex.
                              (double-array (take m (repeat 0.1))))]))
        point (-> optimal .getPoint vec)
        value (-> optimal .getValue)]
    {:ar (take p point)
     :ma (drop p point)
     :ll value}))
