(ns pagerank.core (:require [clojure.pprint]))

(def damping-factor 0.85)
(def one-minus-damp 0.15)
(def initial-ranks 1)

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq reader))))

(defn link-map [lines]
  (for [line lines]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)]
      (hash-map :id id, :links links))))

(def link-coll (link-map (read-file "pages.txt")))
(def no-loop (atom (set (range 0 10000))))
(def rank-coll (atom (zipmap (range 0 10000) (repeat initial-ranks))))

(defn update-rank-comp [id]
  (let [count-x (count (get (nth link-coll id) :links))
        curr-rank-x (get @rank-coll id)
        updated-rank (/ curr-rank-x count-x)]
    updated-rank))

(defn line-ranks-comp [id]
  (for [links (get (nth link-coll id) :links)]
    (update-rank-comp (read-string links))))

;this is the method that is taking too long, reduce is the potential problem
(defn sum-ranks-comp [id]
  (reduce + (doall (line-ranks-comp id))))

(defn apply-damp-comp [id]
  (+ one-minus-damp (* damping-factor (sum-ranks-comp id))))

(defn abs [num] (max num (- num)))

(defn rank-step []
    (doseq [x @no-loop]
      (let [damp-comp (apply-damp-comp x)
            curr-rank (get @rank-coll x)]
        (if (< (abs (- damp-comp curr-rank)) 0.01) (swap! no-loop disj x) (swap! rank-coll assoc x damp-comp)))))

(defn number-of-iterations [y]
  (dotimes [_ y]
    (when (> (count @no-loop) 0) (rank-step))))

(defn number-of-threads [t]
  (let [num-it (Math/round (double (/ 1000 t)))
        fs (for [_ (range t)]
             (future (number-of-iterations num-it)))]
    (doseq [f fs]
      @f)))

(defn to-file [time]
  (spit (clojure.java.io/resource "outputs.txt") time))

(defn out-times-to-file []
  (to-file (time (number-of-threads 1)))
  (to-file (time (number-of-threads 2)))
  (to-file (time (number-of-threads 4)))
  (to-file (time (number-of-threads 8)))
  (to-file (time (number-of-threads 16)))
  (to-file (time (number-of-threads 32)))
  (to-file (time (number-of-threads 64)))
  )

;for testing
(defn print-test []
  ;(to-file "test")
  (clojure.pprint/pprint (count @no-loop))
  ;(clojure.pprint/pprint @rank-coll)
  )

;(time (number-of-iterations 1000))
(time (number-of-threads 8))
;(time (rank-step))
;(print-test)