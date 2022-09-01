#lang racket
(require "parenthec.rkt")
;Arthur Hernandez
;Assignment 9

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (catch body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

;i removed letcc from this
;looks like letcc has been changed to catch
(define value-of-cps
  (lambda (eval env-cps k)
    (union-case eval expr
      
      [(const const-expr)
       (apply-k k const-expr)]
      
      [(mult nexp1 nexp2)
       (value-of-cps nexp1 env-cps
                     (multi-k1 nexp2 env-cps k))]
      
      [(sub1 x) (value-of-cps x env-cps (sub-k k))]

      [(zero x) (value-of-cps x env-cps (zero-k k))]
      
      [(if test conseq alt)
       (value-of-cps test env-cps
                     (if-k conseq alt env-cps k))]
      
      [(catch body)
       (value-of-cps body (extend-env k env-cps) k)]
      
      [(throw k-exp v-exp) (value-of-cps k-exp env-cps
                                         (throw-k v-exp env-cps))]
      
      [(let e body) (value-of-cps e env-cps
                                     (let-k body env-cps k))]
      
      [(var x) (apply-env env-cps x k)]
      
      [(lambda body)
       (apply-k k (clos_closure body env-cps))]
      
      [(app rator rand) (value-of-cps rator env-cps (rator-k rand env-cps k))]
      )
    )
  )

;continuation helpers
(define (multi-k1 x2^ env-cps^ k^)
  `(k1-multi, x2^ , env-cps^ , k^))

(define (multi-k2 x1^ k^)
  `(k2-multi, x1^, k^))

(define (sub-k k^)
  `(k-sub, k^))

(define (zero-k k^)
  `(k-zero, k^))

(define (if-k conseq^ alt^ env-cps^ k^)
  `(k-if, conseq^, alt^, env-cps^, k^))

(define (throw-k v-exp^ env-cps^)
  `(k-throw , v-exp^, env-cps^))

(define (let-k body^ env-cps^ k^)
  `(k-let, body^, env-cps^, k^))

(define (rator-k rand^ env-cps^ k^)
  `(k-rator,rand^, env-cps^, k^))

(define (rand-k c-cps^ k^)
  `(k-rand ,c-cps^ ,k^))


;apply-k applier and closer 
(define (apply-k k exp)
  (match k
    [`(k1-multi ,x2^ ,env-cps^ ,k^)
     (value-of-cps x2^ env-cps^
                   (multi-k2 exp k^))]
    
    [`(k2-multi, x1^ , k^)
     (apply-k k^( * x1^ exp))]
    
    [`(k-sub ,k^)
     (apply-k k^(sub1 exp))]
    
    [`(k-zero, k^)
     (apply-k k^(zero? exp))]
    
    [`(k-if, conseq^, alt^, env-cps^ , k^)
     (if exp
         (value-of-cps conseq^ env-cps^ k^)
         (value-of-cps alt^ env-cps^ k^))]
    
    [`(k-throw ,v-exp^ ,env-cps^)
     (value-of-cps v-exp^ env-cps^ exp)]

    [`(k-let ,body^ ,env-cps^ ,k^)
     (value-of-cps body^ (extend-env exp env-cps^) k^)]

    [`(k-rator ,rand^ ,env-cps^ ,k^)
     (value-of-cps rand^ env-cps^
                   (rand-k exp k^))]
    
    [`(k-rand ,c-cps^ ,k^)
     (apply-closure c-cps^ exp k^)]

    [`(k-init) exp]
    
    )
  )
;apply env 
(define (apply-env env y k^)
  (match env
    [`(extend-env ,x^ ,env-cps^)
     (if(zero? y)
         (apply-k k^ x^)
         (apply-env env-cps^ (sub1 y) k^))]
    [`(empty-env) (error "~unbound identifier~")]))

;;;ensure that your constructor invocations are preceded with clos_,
;;;or something other than clos if you use a different name for your
;;;union. Make sure to remove the backquotes and commas in the patterns
;;;in what was your match expression.

(define-union clos
  (closure body env-cps))

;change the match to use union-case
(define (apply-closure c-cps y k^)
  (union-case c-cps clos
    [(closure body env-cps) (value-of-cps body (extend-env y env-cps) k^)]))

;make closure constructor to define-union 
(define (make-closure body env-cps)
  `(closure ,body ,env-cps))

;extend env
(define (extend-env y k^)
  `(extend-env ,y ,k^))

;empty env
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))

;empty k
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_catch
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k)
     )
    )
  )

(main)