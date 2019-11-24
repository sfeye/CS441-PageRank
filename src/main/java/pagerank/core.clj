(ns pagerank.core (:require clojure.pprint))

(def damping-factor 0.85)
(def one-minus-damp 0.15)
;used to apply first step 1/10,000
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

(defn rank-step []
  (loop [x 0]
    (when (< x 10000)
      (let [difference (- (apply-damp-comp x) (get @rank-coll x) )]
      (if (< (abs difference) 0.00001) (println "Page" x "has converged at:" (get @rank-coll x)) (swap! rank-coll assoc x (apply-damp-comp x)))
      (recur (+ x 1))))))

(defn try-to-converge []
  (loop [x 0]
    (when (< x 50)
      (rank-step)
      (recur (+ x 1)))))


;for testing
(defn print-test []
  (clojure.pprint/pprint @rank-coll))

(try-to-converge)
(print-test)

