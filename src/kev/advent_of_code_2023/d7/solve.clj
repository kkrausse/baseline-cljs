(ns kev.advent-of-code-2023.d7.solve
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(comment

  (->> "kev/advent_of_code_2023/d7/input.txt"
       io/resource slurp
       str/split-lines
       (map (fn [l]
              (let [[_ hand bid] (re-matches #"(\S+) (\d+)" l)]
                {:hand hand
                 :bid (Integer/parseInt bid)})))
       (def plays))

  ;; part 1
  (->> plays
       (sort-by (fn [{:keys [hand]}]
                  [(->> hand
                        frequencies
                        (map (fn [[_c n]]
                               (Math/pow n 2)))
                        (reduce +))
                   (str/replace hand #"[A-Z]" {"A" "Z"
                                               "K" "Y"
                                               "Q" "X"
                                               "J" "W"
                                               "T" "V"})]))
       (map-indexed (fn [i {:keys [bid]}]
                      (* (inc i) bid)))
       (reduce +))
  (comment
    253313241)


  ;; part 2
  (->> plays
       (sort-by (fn [{:keys [hand]}]
                  [(let [freq (frequencies (remove #{\J} hand))
                         [k v] (apply max-key second [\J 0] freq)
                         freq' (assoc freq k (+ v
                                                (count (filter #{\J} hand))))]
                     (->> freq'
                          (map (fn [[_c n]]
                                 (Math/pow n 2)))
                          (reduce +)))
                   (str/replace hand #"[A-Z]" {"A" "Z"
                                               "K" "Y"
                                               "Q" "X"
                                               "J" "0"
                                               "T" "V"})]))
       (map-indexed (fn [i {:keys [bid]}]
                      (* (inc i) bid)))
       (reduce +))
  (comment
    253362743)
  )
