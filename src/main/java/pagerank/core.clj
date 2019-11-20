(ns pagerank.core (:require clojure.pprint))

(defn read-lines [file]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file))]
    (doall (line-seq rdr))))

(defn rank-map [line-seq]
  (for [line line-seq]
    (let [nums (clojure.string/split line #" ")
          id (read-string (first nums))
          links (rest nums)
          rank (count links)]
      (hash-map :id id, :rank rank)))
  )

(defn link-map [line-seq]
  (reduce conj (map hash-map [:id :links] (.split line-seq " " 2))))

(def link-coll (map link-map (read-lines "pages.txt")))
(def rank-coll (rank-map (read-lines "pages.txt")))


;for testing
(defn PrintTest []
  (clojure.pprint/pprint (nth link-coll 9999))
  (clojure.pprint/pprint (get (nth link-coll 9999) :id))
  (clojure.pprint/pprint (get (nth link-coll 9999) :links))
  (clojure.pprint/pprint (nth rank-coll 9999))
  (clojure.pprint/pprint (get (nth rank-coll 9999) :id))
  (clojure.pprint/pprint (get (nth rank-coll 9999) :rank))
  )

(PrintTest)