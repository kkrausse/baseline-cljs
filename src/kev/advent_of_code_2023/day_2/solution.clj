(ns kev.advent-of-code-2023.day-2.solution
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [malli.core]
   [net.cgrand.xforms :as x]))

(defmacro fn->> [& forms]
  `(fn [x#] (->> x# ~@forms)))

(defn parse-game [line]
  (as-> {:v line} $
    (merge $ (let [[_ game r] (re-matches #"Game (\d+):(.*)" (:v $))]
               {:v r
                :game (Integer/parseInt game)}))
    (merge $ {:bags (->> (str/split (:v $) #";")
                         (map (fn->> (#(str/split % #","))
                                     (map (fn->> (re-matches #"\s?(\d+) ([a-z]+)")
                                                 ((fn [[_ n color]]
                                                    [(keyword color) (Integer/parseInt n)]))))
                                     (into {}))))})
    (merge $ {:max-by-color (->> (:bags $)
                                 (into {}
                                   (comp cat
                                         (x/by-key x/max))))})))

(comment

  ;; part 1
  (let [bag {:red 12 :green 13 :blue 14}]
    (->> "kev/advent_of_code_2023/day_2/input.txt"
         (io/resource)
         (slurp)
         (str/split-lines)
         (map parse-game)
         (filter (fn [{:keys [max-by-color]}]
                   (every? (fn [[k v]]
                             (<= v (get bag k)))
                           max-by-color)))
         (transduce (map :game) +)))
  (comment
    2593)

  ;; part 2
  (->> "kev/advent_of_code_2023/day_2/input.txt"
       (io/resource)
       (slurp)
       (str/split-lines)
       (transduce
        (comp (map parse-game)
              (map (fn compute-power [{:keys [max-by-color]}]
                     (transduce (map second) * max-by-color))))
        +))
  (comment
    54699)
  )
