(ns pagerank.core (:require clojure.pprint))

(def damping-factor 0.85)

(defn read-lines [file]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq rdr))))

(defn rank-map [line-seq]
  (for [line line-seq]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)
          rank (count links)]
      (hash-map :id id, :rank rank)))
  )

(defn link-map [line-seq]
  (for [line line-seq]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)]
      (hash-map :id id, :links links)))
  )

(def link-coll (link-map (read-lines "pages.txt")))
(def rank-coll (rank-map (read-lines "pages.txt")))

(defn compute-rank []
  (loop [x 1000]
    (when (> x 0)
      ;compute the ranks here
      (recur (- x 1))
      )
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

(compute-rank)