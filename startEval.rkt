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
    (let ((val (assoc var env)))
        (if val (cdr val) (error "Unknown variable: " var))))

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

;; Local binding
(define (evalBinding func args)
    ('"Not defined yet"
))

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