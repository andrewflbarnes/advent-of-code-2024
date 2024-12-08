(ns d02
  (:require [clojure.string :as str]))

(defn limited? [x]
  (<= 1 (abs x) 3))

(defn safe? [raw]
  (let [checks (->> (partition 2 1 raw)
                    (map #(apply - %)))]
    (and (every? limited? checks)
         (or (every? pos? checks)
             (every? neg? checks)))))

(defn d02 [file]
  (let [raw (->> (slurp file)
                 str/split-lines
                 (map #(str/split % #"\s+"))
                 (map #(map Integer/parseInt %)))]
    (->> raw
         (filter safe?)
         count
         println)
    (->> raw
         (map #(->> (range (count %))
                    (map
                     (fn [idx]
                       (keep-indexed (fn [i v] (when (not= i idx) v)) %)))))
         (keep #(first (filter safe? %)))
         count
         println)))
