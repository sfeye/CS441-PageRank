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
      (hash-map :id id, :rank initial-ranks)
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
(def rank-coll (rank-map (read-lines "pages.txt")))

(defn sum-ranks [id]
  (let [count-x (count (get (nth link-coll id) :links))
        curr-rank-x (get (nth rank-coll id) :rank)
        updated-rank (* damping-factor (/ curr-rank-x count-x))]
    (println updated-rank))
  )

(defn assoc-rank []
  (loop [x 1]
    (when (> x 0)
      (loop [y 9999]
        (when (> y 0)
          (println y)
          (recur (- y 1)))))
    (recur (- x 1))
    )
  )

;for testing
(defn print-test []
  (clojure.pprint/pprint (nth link-coll 9999))
  (clojure.pprint/pprint (get (nth link-coll 9999) :id))
  (clojure.pprint/pprint (get (nth link-coll 9999) :links))
  (println "5th:" (nth (get (nth link-coll 9999) :links) 5))
  (clojure.pprint/pprint (nth rank-coll 9999))
  (clojure.pprint/pprint (get (nth rank-coll 9999) :id))
  (clojure.pprint/pprint (get (nth rank-coll 9999) :rank))
  )

(sum-ranks 0)