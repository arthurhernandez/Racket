#lang racket
;Arthur Hernandez
;Assignment 9
(require "parenthec.rkt")
;looks like letcc has been changed to catch
(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                         (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,y) (env y)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

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
       (apply-k k (make-closure body env-cps))]
      
      [(app rator rand) (value-of-cps rator env-cps (rator-k rand env-cps k))]
      )
    )
  )
 
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
  `(k-operand ,c-cps^ ,k^))

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
         (value-of-cps
          conseq^
          env-cps^
          k^)
         (value-of-cps
          alt^
          env-cps^
          k^))]
    
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

(define (make-closure body env-cps)
  `(closure ,body ,env-cps))

(define (apply-env env y k^)
  (match env
    [`(extend-env ,value^ ,env-cps^)
     (if (zero? y)
         (apply-k k^ value^)
         (apply-env env-cps^ (sub1 y) k^))]
    [`(empty-env) (error 'value-of-cps "unbound identifier")]))

(define (apply-closure c-cps a k^)
  (match c-cps
    [`(closure ,body ,env-cps) (value-of-cps body (extend-env a env-cps) k^)]
    [else (error "Unrecognized closure found in apply-closure:" c-cps "called with" a k^)]))

(define (extend-env value^ env-cps^)
  `(extend-env ,value^ ,env-cps^))


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