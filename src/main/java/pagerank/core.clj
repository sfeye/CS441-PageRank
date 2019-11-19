(ns pagerank.core (:require clojure.pprint))

(def seq-file-lines (with-open [rdr (clojure.java.io/reader "/Users/samfeye/Desktop/CS441-PageRank/src/main/resources/pages.txt")]
                      (reduce conj [] (line-seq rdr))))

(def link-map (for [line seq-file-lines]
                (let [nums (clojure.string/split line #" ")
                      id (read-string (first nums))
                      links (rest nums)]
                  (hash-map id links))))

(defn MakeMap []
  (for [line seq-file-lines]
    (let [nums (clojure.string/split line #" ")
          id (read-string (first nums))
          links (rest nums)
          rank (count links)]
      (hash-map :id id :rank rank))
    ))
(defn PrintTest []
  (let [bigmap (MakeMap)]
    (clojure.pprint/pprint bigmap)))

(PrintTest)