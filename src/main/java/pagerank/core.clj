(ns pagerank.core (:require [clojure.pprint]))

(def damping-factor 0.85)                                   ; specified d in project guidelines
(def one-minus-damp 0.15)
(def initial-ranks 1)                                       ; specified initial rank value in guidelines

(defn read-file [file]                                      ; reads file line by line return lazy seq
  (with-open [reader (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq reader))))

(defn link-map [lines]                                      ; splits lines by white space, makes hash-map and saves to a seq
  (for [line lines]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)]
      (hash-map :id id, :links links))))

(def link-coll (link-map (read-file "pages.txt")))          ; used to access ranks
(def no-loop (atom (set (range 0 10000))))                  ; used to removed converged pages
(def rank-coll (atom (zipmap (range 0 10000) (repeat initial-ranks)))) ; used to update and track computed ranks

(defn update-rank-comp [id]                                 ; PR(T)/C(T) returns a float
  (let [count-x (count (get (nth link-coll id) :links))
        curr-rank-x (get @rank-coll id)
        updated-rank (/ curr-rank-x count-x)]
    updated-rank))

(defn line-ranks-comp [id]                                  ; ∑ PR(T)/C(T) returns a lazy seq of floats
  (for [links (get (nth link-coll id) :links)]
    (update-rank-comp (read-string links))))

(defn sum-ranks-comp [id]                                   ; ∑ PR(T)/C(T) returns summed float
  (reduce + (doall (line-ranks-comp id))))

(defn apply-damp-comp [id]                                  ; (1−d) + d * ∑ PR(T)/C(T) returns a float
  (+ one-minus-damp (* damping-factor (sum-ranks-comp id))))

(defn abs [num] (max num (- num)))                          ; returns absolute value of a number

(defn rank-step []                                          ; checks if computed ranks has converged, if not swaps
    (doseq [x @no-loop]                                     ; 1/100,000 is used to check for convergence
      (let [damp-comp (apply-damp-comp x)
            curr-rank (get @rank-coll x)]
        (if (< (abs (- damp-comp curr-rank)) 0.00001) (swap! no-loop disj x) (swap! rank-coll assoc x damp-comp)))))

(defn number-of-iterations [y]                              ; iterates through rank step
  (dotimes [_ y]
    (when (> (count @no-loop) 0) (rank-step))))

(defn number-of-threads [t]                                 ; specifies number of threads
  (let [num-it (Math/ceil (double (/ 1000 t)))
        fs (for [_ (range t)]
             (future (number-of-iterations num-it)))]
    (doseq [f fs]
      @f)))



(defn to-file [time]                                        ; writes "time" to file outputs.txt
  (spit (clojure.java.io/resource "outputs.txt") time :append true))

(defn out-times-to-file [n]                                 ; used to write timed method to file
  (to-file (with-out-str (time (number-of-threads n))))
  )

;for testing
(defn print-test []                                         ; used for testing
  (clojure.pprint/pprint (count @no-loop))                  ; count of number of converged pages
  (clojure.pprint/pprint @rank-coll)
  )

(out-times-to-file 2)                                       ; fastest computation from multithreading