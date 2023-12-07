(ns kev.advent-of-code-2023.d4.solve
 (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [net.cgrand.xforms :as x]
   [net.cgrand.xforms.rfs :as x.rfs]))

(comment

  (->> "kev/advent_of_code_2023/d4/input.txt"
       io/resource slurp
       (clojure.string/split-lines)
       (map (fn parse-line [s]
              (let [[winners mine]
                    (-> (str/split s #":")
                        (second)
                        (str/split #"\|")
                        (->> (map #(->> (str/split % #"\s")
                                        (remove empty?)))))]
                {:card (-> #"Card\s*(\d+):.*"
                           (re-matches s)
                           (second))
                 :winners winners
                 :mine mine
                 :s s})))
       (map (fn [{:keys [winners mine card] :as x}]
              (let [wset (into #{} winners)]
                (-> x
                    (assoc :win-count (transduce (comp (filter wset)
                                                       x/count)
                                                 x.rfs/last
                                                 mine)
                           :card-num (Integer/parseInt card))))))
       (def prepped-cards))

  ;; part 1
  (->> prepped-cards
       (map (fn calc-winnings [{:keys [win-count]}]
              (if (< 0 win-count)
                (int (Math/pow 2 (dec win-count)))
                0)))
       (reduce +))
  (comment
    24175)


  ;; part 2
  (->> prepped-cards
       (reduce
        (fn [ccs {:keys [card-num win-count]}]
          (->> (range win-count)
               (map #(+ card-num 1 %))
               (reduce (fn [ccs new-card]
                         (update ccs new-card #(+ % (get ccs card-num))))
                       ccs)))
        ;; make card counts
        (into {}
          (map (fn [{:keys [card-num]}]
                       [card-num 1]))
          prepped-cards))
       (map second)
       (reduce +))
  (comment
    18846301)


  )
