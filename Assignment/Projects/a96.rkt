#lang racket
(require "parenthec.rkt")
;Arthur Hernandez
;Assignment 9 pt 6
;got help for this step from multiple classmates

;Registerize the interpreter. Turn each let* expression to a begin block:
;the former let* bindings will become set! expressions, and the body becomes the
;invocation of a function of no arguments. Change all serious functions to be
;functions of no arguments. Define your global registers using define-registers at
;the top of the program.

;original removed
;union cases these are good
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

(define-registers ac-cps ac-x ac-k^ ak-k ak-exp ae-env ae-y ae-k^ vo-evaluate vo-env vo-k )

(define (value-of-cps)
  (union-case vo-evaluate expr
              
    [(const const-expr)
     (begin [set! ak-k vo-k]
            [set! ak-exp const-expr]
            (apply-k))]
    
              [(mult nexp1 nexp2)
               (begin [set! vo-k (kt_k1-multi nexp2 vo-env vo-k)]
                      [set! vo-evaluate nexp1]
                      [set! vo-env vo-env]
                      (value-of-cps))]
    
              [(sub1 x)
               (begin [set! vo-k (kt_k-sub vo-k)]
                       [set! vo-evaluate x]
                       [set! vo-env vo-env]
                      (value-of-cps))]
    
              [(zero x)
               (begin [set! vo-k (kt_k-zero vo-k)]
                      [set! vo-evaluate x]
                      [set! vo-env vo-env]
                      (value-of-cps))]
    
    [(if test conseq alt)
     (begin [set! vo-k (kt_k-if conseq alt vo-env vo-k)]
            [set! vo-evaluate test]
            [set! vo-env vo-env]
            (value-of-cps))]
    
              [(catch body)
               (begin [set! vo-k vo-k]
                      [set! vo-evaluate body]
                      [set! vo-env (envr_extend-env vo-k vo-env)]
                      (value-of-cps))]
    
              [(throw k-exp v-exp)
               (begin [set! vo-k (kt_k-throw v-exp vo-env)]
                      [set! vo-evaluate k-exp]
                      [set! vo-env vo-env]
                      (value-of-cps))]
    
              [(let e body)
               (begin [set! vo-k (kt_k-let body vo-env vo-k)]
                      [set! vo-evaluate e]
                      [set! vo-env vo-env]
                      (value-of-cps))]
    
              [(var y)
               (begin [set! ae-k^ vo-k]
                      [set! ae-env vo-env]
                      [set! ae-y y]
                      (apply-env))]
    
              [(lambda body)
               (begin [set! ak-k vo-k]
                      [set! ak-exp (clos_closure body vo-env)]
                      (apply-k))]
    
              [(app rator rand)
               (begin [set! vo-k (kt_k-rator rand vo-env vo-k)]
                      [set! vo-evaluate rator]
                      [set! vo-env vo-env]
                      (value-of-cps))]
              )
  )

;apply-k applier and closer 
(define (apply-k)
  (union-case ak-k kt 

              [(k1-multi x2^ env-cps^ k^)
               (begin [set! vo-k (kt_k2-multi ak-exp k^)]
                      [set! vo-evaluate x2^]
                      [set! vo-env env-cps^]
                      (value-of-cps))]
    
              [(k2-multi x1^ k^)
               (begin  [set! ak-k k^]
                      [set! ak-exp (* x1^ ak-exp)]
                 (apply-k))]

              [(k-sub k^)
               (begin  [set! ak-k k^]
                      [set! ak-exp (sub1 ak-exp)]
                 (apply-k))]

    [(k-zero k^)
     (begin [set! ak-k k^]
            [set! ak-exp (zero? ak-exp)]
            (apply-k))]

    [(k-if conseq^ alt^ env-cps^ k^)
     (if ak-exp
         (begin [set! vo-k k^]
                [set! vo-evaluate conseq^]
                [set! vo-env env-cps^]
                (value-of-cps))
         (begin [set! vo-k k^]
                [set! vo-evaluate alt^]
                [set! vo-env env-cps^]
                (value-of-cps)))]

    [(k-throw v-exp^ env-cps^)
     (begin [set! vo-k ak-exp]
            [set! vo-evaluate v-exp^]
            [set! vo-env env-cps^]
            (value-of-cps))]

    
              [(k-let body^ env-cps^ k^)
               (begin [set! vo-k k^]
                      [set! vo-evaluate body^]
                      [set! vo-env (envr_extend-env ak-exp env-cps^)]
                      (value-of-cps))]


              [(k-rator rand^ env-cps^ k^)
               (begin [set! vo-k (kt_k-rand ak-exp k^)]
                      [set! vo-evaluate rand^]
                      [set! vo-env env-cps^]
                      (value-of-cps))]


    [(k-rand c-cps^ k^)
     (begin [set! ac-k^ k^]
            [set! ac-cps c-cps^]
            [set! ac-x ak-exp]
            (apply-closure))]


              [(k-init) ak-exp]
    
              )
  )

;apply env ***
(define (apply-env)
  (union-case ae-env envr
              [(extend-env x^ env-cps^)
               (if (zero? ae-y)
                   (begin [set! ak-k ae-k^]
                          [set! ak-exp x^]
                          (apply-k))
                   (begin [set! ae-k^ ae-k^]
                          [set! ae-env env-cps^]
                          [set! ae-y (sub1 ae-y)]
                          (apply-env)))]
              [(empty-env) (error "~unbound identifier~" )]
              )
  )


;step 2 this is good 
(define-union clos
  (closure  body env-cps)
  )

;step 3 this is also good 
(define-union envr 
  (empty-env)
  (extend-env  value^ env-cps^)
  )

;step4 looks good no lets :)
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
  (k-rator rand^ env-cps^ k^)
  )


;change the match to use union-case

(define (apply-closure)
  (union-case ac-cps clos
    [(closure body env-cps)
     (begin [set! vo-k ac-k^]
            [set! vo-evaluate body]
            [set! vo-env (envr_extend-env ac-x env-cps)]
            (value-of-cps))]
    )
  )

;make closure constructor to define-union no longer necessary??
;(define (make-closure body env-cps)
 ;`(closure ,body ,env-cps))

;extend env
;(define (extend-env y k^)
 ; `(extend-env ,y ,k^))


;empty env
(define empty-env
  (lambda ()
    (envr_empty-env))) 

;empty k
(define empty-k
  (lambda ()
     (kt_k-init))) 

(define main 
  (lambda ()
    (begin [set! vo-k (empty-k)]          
           [set! vo-env (empty-env)]
           [set! vo-evaluate
                 (expr_let 
                  (expr_lambda
                   (expr_lambda 
                    (expr_if
                     (expr_zero (expr_var 0))
                     (expr_const 1)
                     (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1))
                                                       (expr_sub1 (expr_var 0)))))))
                  (expr_mult
                   (expr_catch
                    (expr_app
                     (expr_app (expr_var 1) (expr_var 1))
                     (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
                   (expr_const 5)))]
           (value-of-cps))))

(main)

;continuation helpers

;(define (multi-k1 x2^ env-cps^ k^)
 ; `(k1-multi, x2^ , env-cps^ , k^))

;(define (multi-k2 x1^ k^)
 ;`(k2-multi, x1^, k^))

;(define (sub-k k^)
;  `(k-sub, k^))

;(define (zero-k k^)
 ; `(k-zero, k^))

;(define (if-k conseq^ alt^ env-cps^ k^)
 ; `(k-if, conseq^, alt^, env-cps^, k^))

;(define (throw-k v-exp^ env-cps^)
 ; `(k-throw , v-exp^, env-cps^))

;(define (let-k body^ env-cps^ k^)
;  `(k-let, body^, env-cps^, k^))

;(define (rator-k rand^ env-cps^ k^)
;  `(k-rator,rand^, env-cps^, k^))

;(define (rand-k c-cps^ k^)
;  `(k-operand ,c-cps^ ,k^))

