(ns d02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inc? [[x, n]]
  (or (nil? n) (< x n)))

(defn dec? [[x, n]]
  (or (nil? n) (> x n)))

(defn limited? [[x, n]]
  (or (nil? n) (let [v (abs (- x n))] (and (<= v 3) (>= v 1)))))

(defn safe? [l]
  (and (every? limited? l)
       (or (every? inc? l) (every? dec? l))))

(defn safecount [raw]
  (->> raw
       (map #(vector % (concat (drop 1 %) [nil])))
       (map #(apply map vector %))
       (filter safe?)))

(defn d02 [file]
  (with-open [rdr (io/reader file)]
    (let [raw (->> (line-seq rdr)
                   (map #(->> (str/split % #"\s+")
                              (map Integer/parseInt))))]
      (->> raw
           safecount
           count
           println)
      (->> raw
           (map #(->> (range (count %))
                      (map
                       (fn [idx]
                         (keep-indexed (fn [i v] (when (not= i idx) v)) %)))))
           (map safecount)
           (map count)
           (filter #(> % 0))
           count
           println))))
