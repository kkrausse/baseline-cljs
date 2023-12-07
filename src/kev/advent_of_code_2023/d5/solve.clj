(ns kev.advent-of-code-2023.d5.solve
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [net.cgrand.xforms :as x]
   [net.cgrand.xforms.rfs :as x.rfs]))


(defn location-ranges [& {:keys [start span thing maps] :as m}]
  (if (= thing "location")
    [(dissoc m :maps)]
    (when-let [{:keys [ranges to]} (some (fn [m]
                                           (when (-> m :from #{thing})
                                             m))
                                         maps)]
      (->> ranges
           (map-indexed vector)
           (keep (fn [[i {mspan :span src :src :keys [dest]}]]
                   (let [nstart (max src start)
                         nend (min (+ src mspan) (+ start span))
                         nspan (- nend nstart)]
                     (when (pos? nspan)
                       {:start (+ nstart (- dest src))
                        :mapped? true
                        :domain-start nstart
                        :i {:i i
                            :domain thing
                            :range to
                            :domain-start nstart
                            :span nspan
                            :range-start (+ dest (- nstart src))}
                        :span nspan}))))
           ((fn add-unmapped-ranges [overlaps]
              (->> overlaps
                   (reduce
                    (fn [unmapped-ranges {mstart :domain-start mspan :span}]
                      (->> unmapped-ranges
                           (mapcat (fn [{ustart :start uspan :span}]
                                     (let [mend (+ mspan mstart)
                                           uend (+ uspan ustart)]
                                       (keep identity
                                             [;; u range left of m
                                              (when (< ustart (min mstart uend))
                                                {:start ustart
                                                 :unmapped? true
                                                 :span (- (min mstart uend) ustart)})
                                              ;; range right of m
                                              (when (< (max mend ustart) uend)
                                                {:start (max mend ustart)
                                                 :unmapped? true
                                                 :span (- uend (max mend ustart))})]))))))
                    [{:start start
                      :span span}])
                   (concat overlaps))))
           (mapcat #(->> %
                         (location-ranges :maps maps
                                          :thing to)))))))
(comment

  (->> "kev/advent_of_code_2023/d5/input.txt"
       io/resource slurp
       ((fn parse-input [in]
          (let [[_ seed-str maps-str] (re-matches #"seeds: (.+)\n\n([\s\S]*)" in)
                seeds (->> (str/split seed-str #" ")
                           (map parse-long))
                maps (->> (str/split maps-str #"\n\n")
                          (map (fn [mstr]
                                 (let [[_ mfrom mto range-str] (re-matches #"([a-z]+)-to-([a-z]+) map:\n([\s\S]*)" mstr)
                                       ranges (->> (str/split-lines range-str)
                                                   (map (fn [rstr]
                                                          (let [[_ dest src span] (re-matches #"(\d+) (\d+) (\d+)" rstr)]
                                                            {:dest (parse-long dest)
                                                             :src (parse-long src)
                                                             :span (parse-long span)}))))]
                                   {:from mfrom
                                    :to mto
                                    :ranges ranges}))))]
            {:seeds seeds
             :maps maps})))
       (def seeds+maps))


  ;; part 1
  (->> seeds+maps
       ((fn [{:keys [maps seeds]}]
          (->> seeds
               (mapcat (fn [seed]
                         (location-ranges :start seed
                                          :span 1
                                          :thing "seed"
                                          :maps maps)))
               (map :start)
               (reduce min)))))
  (comment
    993500720)

  ;; part 2
  (let [{:keys [seeds maps]} seeds+maps]
    (->> seeds
         (partition 2)
         (map (fn [[start span]]
                {:start start
                 :span span}))
         (mapcat (fn [{:keys [start span]}]
                   (location-ranges :start start
                                    :span span
                                    :thing "seed"
                                    :maps maps)))
         (map :start)
         (reduce min)))
  (comment
    4917124)
  )
