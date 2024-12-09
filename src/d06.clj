(ns d06
  (:require [clojure.string :as str]))

(defn in-bounds [grid [x y]]
  (and (>= x 0)
       (>= y 0)
       (< x (count grid))
       (< y (count (first grid)))))

(defn rot [dir]
  (case dir
    :north :east
    :east :south
    :south :west
    :west :north))

(defn next-step [pos dir]
  (let [x (pos 0)
        y (pos 1)]
    (case dir
      :north [(dec x) y]
      :east [x (inc y)]
      :south [(inc x) y]
      :west [x (dec y)])))

(defn is-obstacle [grid [x y]]
  (if (in-bounds grid [x y])
    (= (get-in grid [x y]) \#)
    false))

(defn d06 [file]
  (let [grid (->> (slurp file)
                  (str/split-lines))
        start (->> (map #(str/index-of % "^") grid)
                   (map-indexed vector)
                   (filter #(some? (second %)))
                   first)
        pos (atom start)
        dir (atom :north)
        pts (atom #{})]
    (while (in-bounds grid @pos)
      (let [next (next-step @pos @dir)]
        (if (is-obstacle grid next)
          (swap! dir #(rot %))
          (do
            (swap! pts conj @pos)
            (swap! pos (fn [v]
                         (as-> v v
                           (assoc v 0 (next 0))
                           (assoc v 1 (next 1)))))))))
    (println (count @pts))))
