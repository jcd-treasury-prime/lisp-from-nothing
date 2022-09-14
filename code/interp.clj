;; not meta-circular! but that could be done in the next step
;; missing ATOM
;; I can see why COND is made over IF tho seems harder to write
;; for simplicity I'm doing lists, not raw pairs
;; QUOTE seems hard to implement (I wonder if it's essentially READ?)
;; hard to debug! punchcards...are you kidding me?
;; why did lowercase "nil" not work for me...had to use NIL (or some other symbol)

(defn extend-env
  [env identifiers vals]
  (merge env (zipmap identifiers vals)))

(defn interp
  ([expr]
   (interp expr {}))
  ([expr env]
   (cond
     ;; NIL and VAR LOOKUP
     (symbol? expr)
     (if (= expr 'NIL)
       '()
       (let [lookup (get env expr)]
         (if (nil? lookup)
           (throw (Exception. (format "unbound variable: %s" expr)))
           lookup)))

     ;; QUOTE
     (= (first expr) 'quote)
     (second expr)

     ;; IF
     (= (first expr) 'if)
     (let [conditional (interp (second expr) env)]
       (if conditional
         (interp (-> expr (nth 2)) env)
         (interp (-> expr (nth 3)) env)))

     ;; LET    (let ((x 1) (y 2)) <body>)
     ;;     => ((lambda (x y) <body>) 1 2)
     (= (first expr) 'let)
     (let [bindings (second expr)
           body (nth expr 2)
           let-expr (cons (cons 'lambda (list (map first bindings)
                                              body))
                          (map second bindings))]
       (println (format "let-expr is %s" let-expr))
       (interp let-expr env))

     ;; LAMBDA
     (= (first expr) 'lambda)
     [expr env] ; a closure!

     ;; now can assume it's a list
     :else
     (let [op (first expr)
           args (map #(interp % env) (rest expr))]
       (cond
         (= op 'cons)
         (cons (first args) (second args))

         (= op 'car)
         (first (first args))

         (= op 'cdr)
         (rest (first args))

         (= op 'eq)
         (= (first args) (second args))

         ;; APPLY
         ;; (f 1 2 3)
         :else
         (let [[lambda closed-env] (interp (first expr) env)
               lambda-identifiers (second lambda)
               lambda-body (-> lambda (nth 2))]
           ;; *** which env you extend controls dynamic vs static scope:
           (interp lambda-body (extend-env env lambda-identifiers args))))))))

;; conditional check
; (interp '(if (eq x (quote a)) (quote b) (quote c)) {'x 'b})

;; pg 20 can you write the "label with lambda" example
;; dynamic vs static scope

(def label-with-lambda
  '((lambda (foo)
            (foo (quote (a b c))))
    (lambda (x)
            (if (eq x NIL)
              (quote bar)
              (foo (cdr x))))))

(defn make-super-special-prefixer
  []
  (let [config-val 'x]
    (fn [lst]
      (cons config-val lst))))

;; (let [config-val 'y] ((make-super-special-prefixer) '(a b)))

(def ssp-call
  '(let ((make-super-special-prefixer
          (let ((config-val (quote x)))
            (lambda ()
                    (lambda (lst)
                            (cons config-val lst)))))
         (config-val (quote y)))
     ((make-super-special-prefixer) (quote (a b)))))

