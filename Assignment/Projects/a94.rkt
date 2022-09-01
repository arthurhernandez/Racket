#lang racket
(require "parenthec.rkt")
;Arthur Hernandez
;Assignment 9


;Transform your continuation constructors to a define-union,
;change the match in apply-k to instead use union-case, and ensure
;all constructor invocations are preceded with kt_, or something
;other than kt if you use a different name for your union. Make sure
;to remove the backquotes and commas in the patterns in what was your
;match expression.


;original 
;union cases
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
;lots of trouble with correct and non duplicate naming in union kt
(define value-of-cps
  (lambda (eval env-cps k)
    (union-case eval expr
      
      [(const const-expr)
       (apply-k k const-expr)]
      
      [(mult nexp1 nexp2)
       (value-of-cps nexp1 env-cps
                     (kt_k1-multi nexp2 env-cps k))]
      
      [(sub1 x) (value-of-cps x env-cps (kt_k-sub k))]

      [(zero x) (value-of-cps x env-cps (kt_k-zero k))]
      
      [(if test conseq alt)
       (value-of-cps test env-cps
                     (kt_k-if conseq alt env-cps k))]
      
      [(catch body)
       (value-of-cps body (envr_extend-env k env-cps) k)]
      
      [(throw k-exp v-exp) (value-of-cps k-exp env-cps
                                         (kt_k-throw v-exp env-cps))]
      
      [(let e body) (value-of-cps e env-cps
                                     (kt_k-let body env-cps k))]
      
      [(var x) (apply-env env-cps x k)]
      
      [(lambda body)
       (apply-k k (clos_closure body env-cps))]
      
      [(app rator rand) (value-of-cps rator env-cps (kt_k-rator rand env-cps k))]
      )
    )
  )

;continuation helpers

;(define (multi-k1 x2^ env-cps^ k^)
 ; `(k1-multi, x2^ , env-cps^ , k^))

;(define (multi-k2 x1^ k^)
 ;`(k2-multi, x1^, k^))

;(define (sub-k k^)
 ; `(k-sub, k^))

;(define (zero-k k^)
 ; `(k-zero, k^))

;(define (if-k conseq^ alt^ env-cps^ k^)
 ; `(k-if, conseq^, alt^, env-cps^, k^))

;(define (throw-k v-exp^ env-cps^)
 ; `(k-throw , v-exp^, env-cps^))

;(define (let-k body^ env-cps^ k^)
 ; `(k-let, body^, env-cps^, k^))

;(define (rator-k rand^ env-cps^ k^)
 ; `(k-rator,rand^, env-cps^, k^))

;(define (rand-k c-cps^ k^)
 ; `(k-operand ,c-cps^ ,k^))


;apply-k applier and closer 
(define (apply-k k exp)
  (union-case k kt
              
    [(k1-multi x2^ env-cps^ k^)
     (value-of-cps x2^ env-cps^ (kt_k2-multi exp k^))]
    
    [(k2-multi n1^ k^)
     (apply-k k^ (* n1^ exp))]
    
    [(k-sub k^)
     (apply-k k^(sub1 exp))]
    
    [(k-zero k^)
     (apply-k k^(zero? exp))]
    
    [(k-if conseq^ alt^ env-cps^  k^)
     (if exp
         (value-of-cps conseq^ env-cps^ k^)
         (value-of-cps alt^ env-cps^ k^))]
    
    [(k-throw v-exp^ env-cps^)
     (value-of-cps v-exp^ env-cps^ exp)]
    
    [(k-let body^ env-cps^ k^)
     (value-of-cps body^ (envr_extend-env exp env-cps^) k^)] ;;change

    [(k-rator rand^ env-cps^ k^)
     (value-of-cps rand^ env-cps^ (kt_k-rand exp k^))]
    
    [(k-rand c-cps^ k^) (apply-closure c-cps^ exp k^)]

    [(k-init) exp]
    
    )
  )

;apply env ***
(define (apply-env env y k^)
  (union-case env envr
    [(extend-env x^ env-cps^)
     (if(zero? y)
         (apply-k k^ x^)
         (apply-env env-cps^ (sub1 y) k^))]
    [(empty-env) (error "~unbound identifier~")]))

;step 2
(define-union clos
  (closure  body env-cps)
  )

;step 3
(define-union envr 
  (empty-env)
  (extend-env  value^ env-cps^)
  )

;step4 
(define-union kt
  (k-init)
  (k1-multi x2^ env-cps^ k^)
  (k2-multi n1^ k^)
  (k-sub k^)
  (k-zero k^)
  (k-if conseq^ alt^ env-cps^ k^)
  (k-throw v-exp^ env-cps^)
  (k-let body^ env-cps^ k^)
  (k-rand c-cps^ k^)
  (k-rator rand^ env-cps^ k^))


;change the match to use union-case
(define (apply-closure x y k^)
  (union-case x clos
    [(closure body env-cps) (value-of-cps body (envr_extend-env y env-cps) k^)]))

;make closure constructor to define-union no longer necessary??
(define (make-closure body env-cps)
  `(closure ,body ,env-cps))

;extend env
(define (extend-env y k^)
  `(extend-env ,y ,k^))

;empty env
(define empty-env
  (lambda ()
    (envr_empty-env))) 

;empty k
(define empty-k
  (lambda ()
     (kt_k-init))) ;;change

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