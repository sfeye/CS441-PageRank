(ns pagerank.core (:require clojure.pprint))

(def damping-factor 0.85)
(def one-minus-damp 0.15)
;used to apply first step 1/10,000
(def initial-ranks 0.0001)

(defn read-lines [file]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq rdr))
    )
  )

(defn rank-map [line-seq]
  (for [line line-seq]
    (let [nums (clojure.string/split line #" ")
          id (first nums)]
      (hash-map (keyword id) initial-ranks)
      )
    )
  )

(defn link-map [line-seq]
  (for [line line-seq]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)]
      (hash-map :id id, :links links)
      )
    )
  )

(def link-coll (link-map (read-lines "pages.txt")))
;(def rank-coll (atom (rank-map (read-lines "pages.txt"))))

(def rank-coll (atom (zipmap (range 0 9999) (repeat initial-ranks))))

(defn update-rank-comp [id]
  (let [count-x (count (get (nth link-coll id) :links))
        ;this is where the error lies...
        curr-rank-x (get @rank-coll id)
        updated-rank (/ curr-rank-x count-x)]
    updated-rank)
  )

(defn line-ranks-comp [id]
  (for [links (get (nth link-coll id) :links)]
    (update-rank-comp (read-string links))))

(defn sum-ranks-comp [id]
  (reduce + (line-ranks-comp id)))

(defn apply-damp-comp [id]
  (+ one-minus-damp (* damping-factor (sum-ranks-comp id))))

(defn rank-step []
  (loop [x 0]
    (when (< x 10000)
      (swap! rank-coll assoc x (apply-damp-comp x))
      (recur (+ x 1))
      )
    )
  ;(clojure.pprint/pprint @rank-coll)
  )

(defn iter-thousand []
  (loop [x 0]
    (when (< x 1)
      (clojure.pprint/pprint @rank-coll)
      )
    )
  )


;for testing
(defn print-test []
  (clojure.pprint/pprint (nth link-coll 300))
  (clojure.pprint/pprint (get (nth link-coll 9999) :id))
  (clojure.pprint/pprint (get (nth link-coll 9999) :links))
  (println "5th:" (nth (get (nth link-coll 9999) :links) 5))
  (clojure.pprint/pprint (get @rank-coll 0))
  (clojure.pprint/pprint @rank-coll)
  )

;(print-test)
(rank-step)
;(iter-thousand)