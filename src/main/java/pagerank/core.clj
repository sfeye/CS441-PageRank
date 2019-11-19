(ns pagerank.core (:require clojure.pprint))

;opens file and returns a sequence of the file by line
(def seq-file-lines (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource "pages.txt"))]
                      (reduce conj [] (line-seq rdr))))

;this returns 10,000 little maps
(def link-map (for [line seq-file-lines]
                (let [nums (clojure.string/split line #" ")
                      id (read-string (first nums))
                      links (rest nums)]
                  (hash-map id links))))

;this is wrong, it currently makes 10,000 little maps
;i want to merge these into one global map, but maps are immutable
(defn MakeMap []
  (for [line seq-file-lines]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)
          rank (count links)]
      (hash-map (keyword id) rank))
    ))

;using this defn to test my merging/assoc attempts
(defn PrintTest []
  ;currently this makes a one element seq of 10,000 maps and prints it
  (let [big-map (MakeMap)]
  (clojure.pprint/pprint big-map)))

(PrintTest)