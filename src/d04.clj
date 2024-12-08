(ns d04
  (:require [clojure.string :as str]))

(def word #"XMAS")

(defn wordcount [x]
  (->> x
       (re-seq word)
       count))

(defn rotate [turn mat]
  (case turn
    (:90deg :0.25) (map reverse (apply map list mat))
    (:180deg :0.5) (reverse (map reverse mat))
    (:270deg :0.75) (reverse (apply map list mat))))

(defn xmasrng [x]
  (let [rng (range (- (count x) 2))]
    (->> rng
         (map #(map (fn [r] (list % r)) rng))
         (apply concat)
         (map (fn [[r c]] (list (take 3 (drop c (nth x r)))
                                (take 3 (drop c (nth x (inc r))))
                                (take 3 (drop c (nth x (+ 2 r)))))))
         (map #(apply concat %))
         (map str/join)
         (filter #(re-matches #"[MS].[MS].A.[MS].[MS]" %))
         (filter #(and (not= (nth % 0) (nth % 8))
                       (not= (nth % 2) (nth % 6)))))))

(defn rotations [x]
  (let [sz (count x)
        rsz (+ (* 2 sz) 1)
        deg225tpl (->> (range 2 rsz)
                       (map
                        #(->> (range
                               (max 1 (- % sz))
                               (min % (+ 1 sz)))
                              (map (fn [v] (list (dec v) (- % 1 v)))))))
        deg225 (map #(map (fn [[a b]] (nth (nth x a) b)) %) deg225tpl)
        deg225o (map #(map (fn [[a b]] (nth (nth x (- sz a 1)) b)) %) deg225tpl)]

    (->> (list x
               (rotate :90deg x)
               (rotate :180deg x)
               (rotate :270deg x)
               deg225
               (rotate :180deg deg225)
               deg225o
               (rotate :180deg deg225o))
         (map #(map str/join %)))))

(defn d04 [file]
  (let [raw (slurp file)]
    (->> (str/split-lines raw)
         (map char-array)
         (map chars)
         (map seq)
         rotations
         flatten
         (map wordcount)
         (reduce +)
         println)
    (->> (str/split-lines raw)
         (map char-array)
         (map chars)
         (map seq)
         xmasrng
         count
         println)))
