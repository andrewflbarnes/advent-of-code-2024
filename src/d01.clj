(ns d01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn d01 [file]
  (with-open [rdr (io/reader file)]
    (let [raw (->> (line-seq rdr)
                   (map #(->> (str/split % #"\s+")
                              (map Integer/parseInt))))]
      (->> raw
           (reduce #(list
                     (conj (first %1) (first %2))
                     (conj (second %1) (second %2))) [])
           (map sort)
           (apply map #(- %1 %2))
           (map #(Math/abs %))
           (reduce +)
           println)
      (let [data (reduce #(list
                           (conj (first %1) (first %2))
                           (assoc
                            (second %1)
                            (second %2)
                            (inc (get (second %1) (second %2) 0))))
                         [() {}]
                         raw)
            lhs (first data)
            rhs (second data)]
        (println (reduce #(+ %1 (* %2 (get rhs %2 0))) 0 lhs))))))
