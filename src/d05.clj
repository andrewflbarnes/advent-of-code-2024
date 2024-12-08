(ns d05
  (:require [clojure.string :as str]))

(defn pagesort [sorts pages]
  (sort #(->> (or
               (some->>
                (get sorts %1)
                (some #{%2})
                -)
               (some->>
                (get sorts %2)
                (some #{%1}))
               (- %1 %2)))
        pages))

(defn d05 [file]
  ;; RAWR!
  (let [[rawr rawup] (as-> (slurp file) v
                       (str/split v #"\n\n"))
        rules (->> rawr str/split-lines)
        negrules (->> rules
                      (map #(str/split % #"\|"))
                      (map #(apply (fn [a b] (str "\\b" b "\\b.*\\b" a "\\b")) %))
                      (map re-pattern))
        updates (->> rawup str/split-lines)
        sorts (->> rules
                   (map #(->> (str/split % #"\|")
                              (map Integer/parseInt)))
                   (reduce (fn
                             [acc [a b]]
                             (if (> a b)
                               (update acc a conj b)
                               acc)) {}))]
    (->> updates
         (filter #(not-any? (fn [nr] (re-find nr %)) negrules))
         (map #(->> (str/split % #",")
                    (map Integer/parseInt)))
         (map #(nth % (-> % count (/ 2))))
         (reduce +)
         println)
    (->> updates
         (filter #(some (fn [nr] (re-find nr %)) negrules))
         (map #(->> (str/split % #",")
                    (map Integer/parseInt)))
         (map #(pagesort sorts %))
         (map #(nth % (-> % count (/ 2))))
         (reduce +)
         println)))
