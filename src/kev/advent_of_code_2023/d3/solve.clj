(ns kev.advent-of-code-2023.d3.solve
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [malli.core]
   [net.cgrand.xforms :as x]))

(defn things-around [matrix i j]
  (->> (for [di (map dec (range 3))
             dj (map dec (range 3))]
         (-> matrix
             (get (+ i di))
             (get (+ j dj))))
       (remove nil?)))

(defn engine-symbol? [s]
  (nil? (re-matches #"\.|\d" s)))

(defmacro fn->> [& forms]
  `(fn [x#] (->> x# ~@forms)))

(comment

  ;; part 1
  (->> "kev/advent_of_code_2023/day_3/input.txt"
       io/resource slurp str/split-lines
       (map #(into [] (map str) %))
       (into [])
       ((fn [x]
          (as-> x $
            {:v $}
            (->> (:v $)
                 (map-indexed
                  (fn [i row]
                    (->> row
                         (into []
                           (map-indexed
                            (fn [j car]
                              (cond
                                (re-matches #"\d" car) {:digit car
                                                        :near-symbol?
                                                        (->> (things-around (:v $) i j)
                                                             (some engine-symbol?))}
                                :else car))))
                         ((fn collapse-digits [row]
                            (loop [nrow []
                                   cur nil
                                   [h & t] row]
                              (if h
                                (cond
                                  (and (:digit h)
                                       (:digits cur)) (recur nrow
                                                             {:digits (str (:digits cur) (:digit h))
                                                              :near-symbol? (or (:near-symbol? cur)
                                                                                (:near-symbol? h))}
                                                             t)
                                  (:digit h) (recur nrow
                                                    {:digits (:digit h)
                                                     :near-symbol? (:near-symbol? h)}
                                                    t)
                                  :else (recur (conj (if cur (conj nrow cur) nrow) h)
                                               nil
                                               t))
                                (if cur
                                  (conj nrow cur)
                                  nrow))))))))
                 (into [])))))

       ((fn [x]
          (->> (for [row x
                     item row]
                 item)
               (keep (fn [x]
                       (when (:near-symbol? x)
                         (Integer/parseInt (:digits x))))))))
       (reduce +))
  (comment
    553825)

  ;; part 2
  (->> "kev/advent_of_code_2023/day_3/input.txt"
       io/resource slurp str/split-lines
       (map #(into [] (map str) %))
       (into [])
       ((fn [x]
          (->> x
               (map-indexed
                (fn [i row]
                  (->> row
                       (into []
                         (map-indexed
                          (fn [j car]
                            (cond
                              (re-matches #"\d" car) {:digit car
                                                      :position [i j]
                                                      :near-symbol?
                                                      (->> (things-around x i j)
                                                           (some engine-symbol?))}
                              :else car))))
                       ;; NOTE: modify to preserve positions
                       ((fn collapse-digits [row]
                          (loop [nrow []
                                 cur nil
                                 [h & t] row]
                            (if h
                              (cond
                                (and (:digit h)
                                     (:digits cur))
                                (let [nc {:digits (str (:digits cur) (:digit h))
                                          :start-position (:start-position cur)
                                          :near-symbol? (or (:near-symbol? cur)
                                                            (:near-symbol? h))}]
                                  (recur nrow
                                         nc
                                         t))
                                (:digit h)
                                (let [nc {:digits (:digit h)
                                          :start-position (:position h)
                                          :near-symbol? (:near-symbol? h)}]
                                  (recur nrow
                                         nc
                                         t))
                                :else
                                (recur
                                 (if cur
                                   (conj (into nrow (repeat (count (:digits cur)) cur))
                                         h)
                                   (conj nrow h))
                                 nil
                                 t))
                              (if cur
                                (into nrow (repeat (count (:digits cur)) cur))
                                nrow))))))))
               (into []))))
       ((fn [x]
          (->> x
               (map-indexed
                (fn [i row]
                  (->> row
                       (map-indexed (fn [j item]
                                      (if (and (string? item)
                                               (re-matches #"\*" item))
                                        {:gear? true
                                         :numbers (->> (things-around x i j)
                                                       (group-by :start-position)
                                                       (keep (fn [[k v]]
                                                               (when k
                                                                 (last v)))))}
                                        item)))
                       (into []))))
               (into []))))
       (transduce
        (comp cat
              (filter :gear?)
              (keep (fn [{:keys [numbers]}]
                     (when (= 2 (count numbers))
                       (apply * (map (comp #(Integer/parseInt %) :digits) numbers))))))
        +))
  (comment
    93994191)

  )
