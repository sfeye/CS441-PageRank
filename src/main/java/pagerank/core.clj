(ns pagerank.core (:require clojure.pprint))

(def seq-file-lines (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "pages.txt"))]
                      (reduce conj [] (line-seq rdr))))

(def link-map (for [line seq-file-lines]
                (let [nums (clojure.string/split line #" ")
                      id (read-string (first nums))
                      links (rest nums)]
                  (hash-map id links))))

(defn MakeMap []
  (for [line seq-file-lines]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)
          rank (count links)]
      (hash-map (keyword id) rank))
    ))

(defn PrintTest []
  (let [big-map (MakeMap)]
  (clojure.pprint/pprint big-map)))

(PrintTest)