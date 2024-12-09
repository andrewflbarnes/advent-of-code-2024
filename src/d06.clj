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
