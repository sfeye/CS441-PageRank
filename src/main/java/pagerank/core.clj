(ns pagerank.core (:require clojure.pprint))

(defn read-lines [file]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq rdr))))

(defn rank-map []
  (for [line (read-lines "pages.txt")]
    (let [nums (clojure.string/split line #" ")
          id (first nums)
          links (rest nums)
          rank (count links)]
      (hash-map (keyword id) rank)))
  )

(defn link-map [line-seq]
  (reduce conj (map hash-map [:id :links] (.split line-seq " " 2))))

(defn PrintTest []
  (clojure.pprint/pprint (nth (map link-map (read-lines "pages.txt")) 9999))
  (clojure.pprint/pprint (nth (rank-map) 9999))
  )

(PrintTest)