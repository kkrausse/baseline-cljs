(ns kev.advent-of-code-2023.d1.solve
  (:require
   [clojure.string]
   [clojure.java.io :as io]))

(def digit->num
  {"one" 1
   "two" 2
   "three" 3
   "four" 4,
   "five" 5,
   "six" 6, "seven" 7, "eight" 8, "nine" 9
   })

(->> "kev/advent_of_code_2023/d1/input.txt"
     io/resource
     slurp
     (clojure.string/split-lines)
     (map (fn [line]
            (let [[_ first-d last-d] (re-matches #"^.*?(\d|six|three|two|seven|five|eight|one|nine|four).*(\d|six|three|two|seven|five|eight|one|nine|four).*$" line)
                  [_ single-d] (re-matches #"^.*?(\d|six|three|two|seven|five|eight|one|nine|four).*$" line)
                  [f l] (if last-d
                          [first-d last-d]
                          [single-d single-d])]
              (Integer/parseInt (str (get digit->num f f)
                                     (get digit->num l l))))))
     (reduce +))

(->> ["eightwo"]
     (map (fn [line]
            (let [[_ first-d last-d] (re-matches #"^.*?(\d|six|three|two|seven|five|eight|one|nine|four).*(\d|six|three|two|seven|five|eight|one|nine|four).*$" line)
                  [_ single-d] (re-matches #"^.*?(\d|six|three|two|seven|five|eight|one|nine|four).*$" line)
                  [f l] (if last-d
                          [first-d last-d]
                          [single-d single-d])]
              (Integer/parseInt (str (get digit->num f f)
                                     (get digit->num l l)))))))
