(ns succ-incr)

(defn succ [x]
  ({0 1
    1 2
    2 3
    3 4
    4 5
    5 6
    6 7
    7 8
    8 9
    9 0} x))

(defn incr [n]
  (loop [x (reverse n)
         c true
         y nil]
    (cond (empty? x)
          (if c
            (cons 1 y)
            y)
          c
          (let [incr-tmp (succ (first x))]
            (recur (rest x) (= incr-tmp 0) (cons incr-tmp y)))
          :else
          (recur (rest x)
                 nil
                 (cons (first x) y)))))

(incr '(1 2 3))
