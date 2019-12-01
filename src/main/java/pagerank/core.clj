(ns pagerank.core (:require clojure.pprint))

(def damping-factor 0.85)
(def one-minus-damp 0.15)
(def initial-ranks 1)

(defn read-lines [file]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq rdr))))

(defn link-map [line-seq]
  (for [line line-seq]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)]
      (hash-map :id id, :links links))))

(def link-coll (link-map (read-lines "pages.txt")))
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

(defn sum-ranks-comp [id]
  (reduce + (line-ranks-comp id)))

(defn apply-damp-comp [id]
  (+ one-minus-damp (* damping-factor (sum-ranks-comp id))))

(defn abs [num] (max num (- num)))

(defn difference [x]
  (let [y (- (apply-damp-comp x) (get @rank-coll x))]
    (abs y) )
  )

(defn rank-step []
  (doseq [x @no-loop]
        (if (< (difference x) 0.00001) (swap! no-loop disj x) (swap! rank-coll assoc x (apply-damp-comp x)))))

(defn number-of-iterations [y]
  (dotimes [_ (Math/round (double y))]
    (rank-step)))

(defn number-of-threads [t]
  (let [num-it (/ 100 t)
        fs (for [_ (range t)]
             (future (number-of-iterations num-it)))]
    (doseq [f fs]
      @f)))


;for testing
(defn print-test []
  ;(clojure.pprint/pprint @no-loop)
  (clojure.pprint/pprint @rank-coll)
  )

;(time (number-of-iterations 100))
(time (number-of-threads 64))
;(time (rank-step))
(print-test)


