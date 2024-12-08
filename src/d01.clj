(ns d01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn d01 [file]
  (with-open [rdr (io/reader file)]
    (let [raw (->> (line-seq rdr)
                   (map #(->> (str/split % #"\s+")
                              (map Integer/parseInt)))
                   (apply map vector))]
      (->> raw
           (map sort)
           (apply map #(- %1 %2))
           (map #(Math/abs %))
           (reduce +)
           println)
      (let [lhs (frequencies (first raw))
            rhs (frequencies (last raw))]
        (->> lhs
             (reduce #(+ %1 (*
                             (first %2)
                             (second %2)
                             (get rhs (first %2) 0))) 0)
             println)))))
