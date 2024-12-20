(ns aoc 
  (:require
   [d01]
   [d02]
   [d03]
   [d04]
   [d05]
   [d06]))

(defn fname [day suff]
  (let [fsuff (cond (nil? suff) "" :else (str "-" suff))
        dpad (cond (< 1 (count (str day))) "" :else "0")]
    (str "src/input/d" dpad day fsuff ".txt")))

(defn missing [day] (println "No implementation for day" day))

(defn runday [day file]
  (let [dayfunc (case day
                  "1" d01/d01
                  "2" d02/d02
                  "3" d03/d03
                  "4" d04/d04
                  "5" d05/d05
                  "6" d06/d06
                  nil)]
    (cond (nil? dayfunc)
          (missing day)
          :else
          (dayfunc file))))

(defn -main [& opts]
  (let [day (nth opts 0)
        suff (nth opts 1 nil)
        input (fname day suff)]
    (println "AOC day" day ":" input)
    (runday day input)))
