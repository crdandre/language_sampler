;;====================================================
;; CFFT Task Reference
;;====================================================

(ns fft-task-ref.core
  (:require [clojure.core.reducers :as r]))

;; Generate signal with given noise
(defn generate-signal [n freq noise]
  (let [dt (/ 1.0 n)
        time-points (range 0 1 dt)
        sine-wave (map #(Math/sin (* 2 Math/PI freq %)) time-points)]
    (map + (take n sine-wave) (take n noise))))

;; Generate synthetic data with random noise
(defn generate-synthetic-data [n freq noise-amp]
  (let [noise (repeatedly #(- (* 2 noise-amp (rand)) noise-amp))]
    (generate-signal n freq noise)))

;; Moving average filter
(defn moving-average-filter [window-size signal]
  (if (<= window-size 0)
    signal
    (let [sliding-windows (partition window-size 1 signal)]
      (map #(/ (reduce + %) window-size) sliding-windows))))

;; Complex number operations
(defrecord Complex [real imag]
  Object
  (toString [_] (str real " + " imag "i")))

(defn complex [real imag] (->Complex real imag))
(defn magnitude [^Complex c] (Math/sqrt (+ (* (:real c) (:real c)) 
                                         (* (:imag c) (:imag c)))))

(defn complex-mult [^Complex a ^Complex b]
  (complex (- (* (:real a) (:real b)) (* (:imag a) (:imag b)))
          (+ (* (:real a) (:imag b)) (* (:imag a) (:real b)))))

;; Basic DFT implementation
(defn omega [k n total-n]
  (let [theta (/ (* -2 Math/PI k n) total-n)]
    (complex (Math/cos theta) (Math/sin theta))))

(defn basic-dft [signal]
  (let [n (count signal)]
    (for [k (range n)]
      (reduce (fn [acc [x n]]
                (let [w (omega k n n)
                      x-complex (complex x 0.0)]
                  (complex-mult x-complex w)))
              (complex 0.0 0.0)
              (map vector signal (range))))))

;; Find peaks above threshold
(defn find-peaks [threshold spectrum]
  (let [n (count spectrum)]
    (->> (range 1 (dec n))
         (filter (fn [i]
                  (let [prev-mag (magnitude (nth spectrum (dec i)))
                        curr-mag (magnitude (nth spectrum i))
                        next-mag (magnitude (nth spectrum (inc i)))]
                    (and (> curr-mag threshold)
                         (> curr-mag prev-mag)
                         (> curr-mag next-mag))))))))

;; Example usage
(comment
  (let [samples (generate-synthetic-data 10000 10.0 0.1)]
    (println (take 10 samples))))
