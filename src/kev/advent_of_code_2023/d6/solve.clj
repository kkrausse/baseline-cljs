(ns kev.advent-of-code-2023.d6.solve)

(defn binary-find-int
  "assumes (test-fn start) returns truthy and (test-fn end) returns falsy and
  finds the highest int between start and end s.t. test-fn returns truthy."
  [start end test-fn]
  (cond
    (>= 1 (- end start)) start
    (test-fn (int (/ (+ start end) 2))) (binary-find-int (int (/ (+ start end) 2))
                                                         end
                                                         test-fn)
    :else (binary-find-int start
                           (int (/ (+ start end) 2))
                           test-fn)))

(comment
  ;; input:
  "Time:        49     97     94     94
   Distance:   263   1532   1378   1851"

  ;; part 1
  (->> [[49 263]
        [97 1532]
        [94 1378]
        [94 1851]]
       (map (fn [[time dist]]
              (->> (range time)
                   (map (fn [hold-time]
                          (* hold-time (- time hold-time))))
                   (filter #(> % dist))
                   count)))
       (reduce *))
  (comment
    4403592)


  ;; part 2
  (let [time 49979494
        distance 263153213781851
        good? (fn [i] (< distance (* i (- time i))))
        random-good-time (binary-find-int 0
                                          time
                                          good?)
        bad-to-good (binary-find-int 0 random-good-time (complement good?))
        good-to-bad (binary-find-int random-good-time time good?)]
    (- good-to-bad bad-to-good))
  (comment
    38017587)
  )
