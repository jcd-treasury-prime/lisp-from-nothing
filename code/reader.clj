(def input " ABC  (a) ")
(def ^:dynamic *inp-idx* (atom 0))
;;(def ^:dynamic *peeked (atom '()))

(defn peekc
  []
  (get input @*inp-idx*))

(defn readc
  []
  (let [c (peekc)]
    (reset! *inp-idx* (inc @*inp-idx*))
    c))

(defn skipc
  [c]
  (cond (= \space c) (do (readc)
                      (skipc (peekc)))
        ;; newline stuff here; skipping for now
        :else c))

(defn symbolic?
  [c]
  (clojure.string/includes? "ABCDEFGHIJKLMNOPQRSTUVWXYZ*-0123456789"
                            (str c)))

(defn rd-atom
  [c a]
  (if (symbolic? c)
    (do (readc)
        (rd-atom (peekc) (cons c a)))
    (if (= (seq "lin") a)
      "NIL" ; hmm what to return here
      (reverse a))))

(declare rd-obj)

(defn rd-list
  [c a]
  (if (= c \))
    (do (readc)
        (reverse a))
    (let [read-tmp (rd-obj (skipc (peekc)))]
      (rd-list (skipc (peekc))
               (cons read-tmp a)))))

(defn rd-obj
  [c]
  (cond
    ;; missing nil case

    (symbolic? c) (rd-atom c '())

    (= c \() (do (readc)
                 (rd-list (skipc (peekc)) '()))

    :else (do (prn (format "c is %s" c))
              (throw nil))
    ))

(defn my-read
  []
  (rd-obj (skipc (peekc))))
