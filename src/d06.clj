(ns d06
  (:require [clojure.string :as str]))

(defn in-bounds [grid [x y]]
  (and (>= x 0)
       (>= y 0)
       (< y (count grid))
       (< x (count (first grid)))))

(defn rot [dir]
  (case dir
    :north :east
    :east :south
    :south :west
    :west :north))

(defn next-step [[x y] dir]
  (case dir
    :north [x (dec y)]
    :east [(inc x) y]
    :south [x (inc y)]
    :west [(dec x) y]))

(defn is-obstacle [grid [x y]]
  (if (in-bounds grid [x y])
    (= (get-in grid [y x]) \#)
    false))

(defn next-obstacle
  "Find the position of the next obstacle in the given direction"
  [grid x y dir]
  (case dir
    :north (let [found (->> (take y grid)
                            (map #(nth % x))
                            (keep-indexed #(when (= %2 \#) %1))
                            last)]
             (when found
               [x found]))
    :south (let [y-start (+ y 1)
                 found (->> (drop y-start grid)
                            (map #(nth % x))
                            (keep-indexed #(when (= %2 \#) %1))
                            (#(do (println %) %))
                            first)]
             (when found
               [x (+ y-start found)]))
    :west (let [found (->> (nth grid y)
                           (take x)
                           (keep-indexed #(when (= %2 \#) %1))
                           last)]
            (when found
              [found y]))
    :east (let [x-start (+ x 1)
                found (->> (nth grid y)
                           (drop x-start)
                           (keep-indexed #(when (= %2 \#) %1))
                           first)]
            (when found
              [(+ x-start found) y]))))

(defn before-next-obstacle
  "Find the position just before the next obstacle in the given direction"
  [grid x y dir]
  (let [obs (next-obstacle grid x y dir)]
    (when obs
      (let [[ox oy] obs]
        (case dir
          :north [ox (inc oy)]
          :south [ox (dec oy)]
          :west [(inc ox) oy]
          :east [(dec ox) oy])))))

(defn dir-op [dir]
  (case dir
    :north :south
    :south :north
    :west :east
    :east :west))

(defn d06 [file]
  (let [grid (->> (slurp file)
                  (str/split-lines))
        start (->> (map #(str/index-of % "^") grid)
                   (map-indexed vector)
                   (filter #(some? (second %)))
                   first
                   (apply #(vector %2 %1)))]
    (let [pos (atom start)
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
      (println (count @pts)))
    (let [pos (atom (conj start :north))
          blocks (atom #{})
          pts (atom #{})]
      (while (in-bounds grid @pos)
        (let [currdir (nth @pos 2)
              next (conj (next-step @pos currdir) currdir)]
          (if (is-obstacle grid next)
            (swap! pos #(conj (vec (take 2 %)) (rot (nth % 2))))
            (do
              (let [check-passed (before-next-obstacle grid (first @pos) (second @pos) (rot (nth @pos 2)))]
                (println @pos next)
                (when (and
                       check-passed
                       (some (vector (first @pos) (second @pos) (dir-op (nth @pos 2))) @pts)
                       (not (contains? @pts @pos)))
                  (println next)
                  (swap! blocks conj ((first next) (second next))))
                (swap! pts conj @pos)
                (swap! pos (fn [_] (vec next))))))))
      (println (count @pts)))))

; part 2 pseudo description because tired - implement tomorrow

; we can generally find loop blocking locations by
; - walking the guard path as normal
; - after each step if there is not an obstacle in front
;   of us and we have not already walked on the tile in
;   front of us, look right
; - if there is a blocking location visible
;   and we have already visited the square in front of it
;   and we visited the square travelling oppoisite to our
;   current direction of travel (not looking direction) then
;   we may place an obstacle directly in front of us (NOT
;   the direction we are looking).
; - this also covers cases where the block we are checking
;   isn't part of and original obstacle on the guard root.
; - this process forces us back onto a previous section of
;   the guard path in a direction we have already travelled
;   which by extension must mean we are now in a loop. We cannot
;   place an obstacle if we have already stepped on that location
;   else it will prevent us from reaching this point in the first
;   place (at least if done as part of initial conditions).

; For example in the below assume we started at point s and are
; now at point x. We are travelling west but looking north we
; can see an obstacle. Placing an obstacle in front of us means we
; - must turn right
; - will eventually meet the visible obstacle and turn right again
; - will mean we are now back on the path we have already travelled
; - will mean we will eventually get back to position x

; ...#....
; ...^>>#.
; ...^.v..
; ...s.v..
; .....v..
; ..Ox<v..
; .....#..
