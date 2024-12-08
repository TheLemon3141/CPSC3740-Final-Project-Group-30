#lang racket

(define (startEval expr)
    (evalExpr expr '()))

(define (evalExpr expr env)
    (cond
        ;; Number, base case
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
           (evalLambda expr env)]
        ;; Local binding
        [(and (list? expr) (member (car expr) '(let letrec)))
            (evalBinding expr env)]
        ;; Function application
        ;; This is called last and we assume that all other cases have been handled
        ;; Otherwise we can get caar contract violations
        [(and (list? expr) (eq? (caar expr) 'lambda))
            (evalFunction expr env)]
        ;; Else
        [else (error "Unknown expression: " expr)]))

;; Lookup variable
;; Our environment is a list of pairs, for example ((x 1) (y 2))
;; Assoc returns the first pair that matches the key
;; If val is found, return the value, otherwise error
(define (lookup var env)
    (let ((val (assoc var env)))
        (if val (cdr val) (error "Unknown variable: " var))))

;; Arithmetic operators
(define (evalArithmetic expr env)
    (let* ([op (car expr)]                   ; Sets op to the operator found in the expression
        [left (evalExpr (second expr) env)]  ; Sets left to the value of the first argument
        [right (evalExpr (third expr) env)]) ; Sets right to the value of the second argument
    (case op                                 ; Now depending on op, call regular Racket functions
        [(+) (+ left right)]
        [(-) (- left right)]
        [(*) (* left right)]
        [(/) (/ left right)]
        [else (error "Unknown arithmetic operator: " op)])))

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
        [else (error "Unknown relational operator: " op)])))

;; Lists
(define (evalList expr env)
    (let* ([op (car expr)]
        [arg (evalExpr (second expr) env)])
    (case op
        [(car) (car arg)]
        [(cdr) (cdr arg)]
        [(cons) (cons arg (evalExpr (third expr) env))]
        [(pair?) (pair? arg)]
        [else (error "Unknown list operator: " op)])))

;; Conditional
(define (evalIf expr env)
    (let* ([cond (evalExpr (second expr) env)]
        [then (evalExpr (third expr) env)]
        [else (evalExpr (fourth expr) env)])
    (if cond then else)))

;; Lambda expression
;; Discards the lambda keyword, labels a simple list of arguments and a body as a function
;; For example (lambda (x y) (+ x y)) will now be a list that looks like (function (x y) (+ x y) env)
(define (evalLambda expr env)
    (let* ([args (second expr)]
        [body (third expr)])
    (list 'function args body env)))

;; Function application
;; The car of expr is assumed to be a function, evalExpr will call evalLambda to organize the function
;; The rest of expr will be evaluated as arguments
;; For our example above, we can pass arguments 3 and 5, which will evaluate to 3 and 5
(define (evalFunction expr env)
    (let* ([function (evalExpr (car expr) env)]
        [args (map (lambda (arg) (evalExpr arg env)) (cdr expr))])
    (applyFunction function args)))

;; Apply function
;; The function has been organized by evalLambda
;; We extract the parameters, the body and the environment
;; In our example, params = (x y), body = (+ x y) and env = '()
;; We pair each parameter with its argument and append them to env
;; Finally, we call evalExpr to evaluate the body in the new environment
(define (applyFunction function args)
    (if (and (list? function) (eq? (car function) 'function))
        (let* ([params (second function)]
            [body (third function)]
            [env (fourth function)])
            (evalExpr body (append (map cons params args) env)))
        (error "Unknown function: " function)))

;; Local binding
;; calls letHelper or letrecHelper with appropriate arguments
(define (evalBinding expr env)
    (let* ([op (car expr)]   ; let or letrec
        [val (second expr)]  ; bindings, for example ((x 1) (y 2))
        [body (third expr)]) ; body that uses x and y
    (case op
        [(let) (letHelper val env body)]
        [(letrec) (letrecHelper val env body)]
        [else (error "Unknown binding: " op)])))


;; let helper
;; The lambda takes a binding from val and constructs a key-value pair, evaluating the "value"
;; A new environment is constructed by applying that lambda to each binding
;;   in val, then appending it to env
;; The body is then evaluated in the new environment
(define (letHelper val env body)
    (let ([newEnv (append (map (lambda (binding)
        (cons (first binding) (evalExpr (second binding) env))) val) env)])
        (evalExpr body newEnv)))

;; letrec helper, not working
(define (letrecHelper val env body)
    (let* ([newEnv (append (map (lambda (binding)
        (cons (first binding) 'placeholder)) val) env)])
        (evalExpr body newEnv)))
    ;;
    ;(let ([evaluatedBindings (map (lambda (binding)
    ;    (cons (first binding) (evalExpr (second binding) newEnv))) val)])
    ;(let ([finalEnv (append evaluatedBindings env)])
    ;    (evalExpr body finalEnv)))))


;; Test arithmetic
(startEval '(+ 1 2))

;; Test relational
(startEval '(= 1 1))

;; Test lists
(startEval '(cons 1 '()))

;; Test if
(startEval '(if (equal? 1 1) 1 2))

;; Test lambda
; (startEval
;     '(letrec ((fact
;         (lambda (x)
;             (if (= x 0) (quote 1)
;                 (* x (fact (- x 1)))))))
;                 (fact 10)))

;; Test lambda 2
(startEval '((lambda (x) x) 10))

;; Test lambda 3
(startEval '((lambda (x y) (+ x y)) 3 5))

;; Test lambda 4
(startEval '((lambda (x) (if (> x 0) x (- x 2))) -7))

;; Test let
(startEval'(let ([x 10]) (+ x 10)))

;; Test let 2
(startEval
    '(let ([x 5])
        (let ([x 2]
            [y x])
        (+ x y))))

;; Test letrec, not working
(startEval '(letrec ([x 5] [y x]) (+ x y)))

;(define test '((lambda (x y) (+ x y)) 10 20))
;(define test2 (car test))
;(define testenv '())

;(startEval test)
;(list? test)
;(evalFunction test testenv)
;(evalExpr (car test) testenv)