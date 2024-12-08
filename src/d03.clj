(ns d03
  (:require [clojure.string :as str]))

(defn solve [raw]
  (->> (re-seq #"mul\(\d{1,3},\d{1,3}\)" raw)
       (map #(re-seq #"\d{1,3}" %))
       (map #(map Integer/parseInt %))
       (map #(apply * %))
       (reduce +)))

(defn d03 [file]
  (let [raw (slurp file)]
    (println (solve raw))
    (->> (str/split raw #"do\(\)")
         (map #(str/split % #"don't\(\)"))
         (map first)
         str/join
         solve
         println)))
