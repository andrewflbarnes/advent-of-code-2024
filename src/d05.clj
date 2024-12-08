(ns d05
  (:require [clojure.string :as str]))

(defn d05 [file]
  ;; RAWR!
  (let [[rawr rawup] (as-> (slurp file) v
                       (str/split v #"\n\n"))
        rules (->> rawr str/split-lines)
        negrules (->> rules
                      (map #(str/split % #"\|"))
                      (map #(apply (fn [a b] (str "\\b" b "\\b.*\\b" a "\\b")) %))
                      (map re-pattern))
        updates (->> rawup str/split-lines)]
    (println negrules)
    (println updates)
    (->> updates
         (filter #(not-any? (fn [nr] (re-find nr %)) negrules))
         (map #(->> (str/split % #",")
                    (map Integer/parseInt)))
         (map #(nth % (-> % count (/ 2))))
         (reduce +)
         println)))
