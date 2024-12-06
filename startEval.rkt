#lang racket

(define (startEval expr)
    (evalExpr expr '()))

(define (evalExpr expr env)
    (cond
        ;; Number
        [(number? expr)
            expr]
        ;; Variable
        [(symbol? expr)
            (lookup expr env)]
        ;; Quoted
        [(and (list? expr) (eq? (car expr) 'quote)) 
            (second expr)]
        ;; Arithmetic operators
        [(and (list? expr) (member (car expr) '(+ - * /)))
            (evalArithmetic expr env)]
        ;; Relational operators
        [(and (list? expr) (member (car expr) '(= <= < >= > equal?)))
            (evalRelational expr env)]
        ;; Lists
        [(and (list? expr) (member (car expr) '(car cdr cons pair?)))
            (evalList expr env)]
        ;; Conditional
        [(and (list? expr) (eq? (car expr) 'if))
            (evalIf expr env)]
        ;; Lambda expression
        [(and (list? expr) (eq? (car expr) 'lambda))
            (makeLambda (second expr) (third expr) env)]
        ;; Function application
        [(list? expr)
            (apply (car expr) (cdr expr) env)]
        ;; Local binding
        [(and (list? expr) (member (car expr) '(let letrec)))
            (evalBinding expr env)]
        ;; Else
        [else (error "Unknown expression: " expr)]))

;; Lookup variable
(define (lookup var env)
    (let ((val (assoc var env)))    ;Finds the first pair in env whose car = var. Returns false if it can't find one.
        (if val (cdr val) (error "Unknown variable: " var))))

;; Local binding
(define (evalBinding func args)   ;(evalBinding expr env)]
  ('"Not defined yet")
  ;Add args to env,
  ;then use them to call the func
  
  ;If car of args is a pair and not empty, call addToEnv on it, then call evalBinding on the rest.
  
)

;Takes myPairs (a pair or list of pairs) and adds it to env (an empty list or list of pairs). Works!
(define (addToEnv env myPairs)  ;Adds myPairs to the end of list env
  (if (equal? env '())   ;If env is an empty list...
      (if (list? (car myPairs))
        myPairs          ;If myPairs list of pairs, return the list.
        (list myPairs)   ;If myPairs only one pair, put it in a list and return it.
      )
      
      (if (list? (car myPairs)) ;If env is not empty...
        (append env myPairs)          ;If myPairs is a list of pairs, append it to env.
        (append env (list myPairs))   ;If myPairs is just one pair, put it in a list and append it to env.
      )
  )
)
;TEST VALUES OF addToEnv
  ;(addToEnv '() '(x 3))
  ;(addToEnv '((x 3)) '(y 4))
  ;(addToEnv '((x 3) (z 5)) '((y 4) (w 6)))
  ;(addToEnv '() '((y 4) (w 6)))
 ;  ^All 4 of these currently return () with 1 or more pairs in it. Perfect.

;Makes a brand new environment from a list env
(define (makeEnv env)  
  (addToEnv '() env)
)

;Function to make a name-value pair. Currently unused.
(define (makeVar id val)
    (list id val)
)


;; Arithmetic operators
(define (evalArithmetic expr env)
    (let* ([op (car expr)]
        [left (evalExpr (second expr) env)]
        [right (evalExpr (third expr) env)])
    (case op
        [(+) (+ left right)]
        [(-) (- left right)]
        [(*) (* left right)]
        [(/) (/ left right)]
        [else (error "Unknown operator: " op)])))

;; Relational operators
(define (evalRelational expr env)
    (let* ([op (car expr)]
        [left (evalExpr (second expr) env)]
        [right (evalExpr (third expr) env)])
    (case op
        [(=) (= left right)]
        [(<=) (<= left right)]
        [(<) (< left right)]
        [(>=) (>= left right)]
        [(>) (> left right)]
        [(equal?) (equal? left right)]
        [else (error "Unknown operator: " op)])))

;; Lists
(define (evalList expr env)
    (let* ([op (car expr)]
        [arg (evalExpr (second expr) env)])
    (case op
        [(car) (car arg)]
        [(cdr) (cdr arg)]
        [(cons) (cons arg (evalExpr (third expr) env))]
        [(pair?) (pair? arg)]
        [else (error "Unknown operator: " op)])))

;; Conditional
(define (evalIf expr env)
    (let* ([cond (evalExpr (second expr) env)]
        [then (evalExpr (third expr) env)]
        [else (evalExpr (fourth expr) env)])
    (if cond then else)))

;; Lambda expression
(define (makeLambda params body env)
    (list 'lambda params body env))

;; Function application
(define (apply func args env)
    (let* ([funcVal (evalExpr func env)]
        [argVals (map (lambda (arg) (evalExpr arg env)) args)])
    (applyFunc funcVal argVals)))

(define (applyFunc func args)
    (match func
        [(list 'lambda params body env)
            (let ([newEnv (append (map cons params args) env)])
                (evalExpr body newEnv))]
        [else (error "Unknown function: " func)]))



;; Test arithmetic
;(print
;    (startEval '(+ 1 2))
;)

;; Test relational
;(print
;    (startEval '(= 1 1))
;)

;; Test lists
;(print
;    (startEval '(cons 1 '()))
;)

;; Test if
;(print
;    (startEval '(if (equal? 1 1) 1 2))
;)

;; Test lambda
;(print
   ;(startEval
  ;  '(letrec ((fact
    ;    (lambda (x)
    ;        (if (= x 0) (quote 1)
      ;          (* x (fact (= x 1)))))))
      ;          (fact 10))))

;; Test lambda eRIK'S TEST
(print
   (startEval
    '(
       (lambda (x y) (+ x y)) 10 20))
       )
