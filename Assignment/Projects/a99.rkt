;#lang racket
;(require "parenthec.rkt")
;Arthur Hernandez
;Assignment 9 pt 8

;Convert all label invocations into assignments to the program counter,
;and then add calls to mount-trampoline and dismount-trampoline. Note this
;lwill require modifying empty-k in your kt union, and the empty-k clause in
;the union-case inside apply-k. Remember to invoke the main label with no
;arguments at the bottom of your file. On the last line of main, print the
;register containing the final value of the program, e.g. (printf “Fact 5: ~s\n” *v*)
;See the parentheC document for notes on these steps.

(define-program-counter programCounter)

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

(define-registers ac-cps ac-x ac-k^ ak-k ak-exp ae-env ae-y ae-k^ vo-evaluate vo-env vo-k  )

(define-label value-of-cps
  (union-case vo-evaluate expr
              
              [(const const-exp)
               (begin [set! ak-k vo-k]
                      [set! ak-exp const-exp]
                      (set! programCounter apply-k))]
    
              [(mult nexp1 nexp2)
               (begin [set! vo-k (kt_k1-multi nexp2 vo-env vo-k)]
                      [set! vo-evaluate nexp1]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
    
              [(sub1 x)
               (begin [set! vo-k (kt_k-sub vo-k)]
                      [set! vo-evaluate x]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
    
              [(zero x)
               (begin [set! vo-k (kt_k-zero vo-k)]
                      [set! vo-evaluate x]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
    
              [(if test conseq alt)
               (begin [set! vo-k (kt_k-if conseq alt vo-env vo-k)]
                      [set! vo-evaluate test]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
    
              [(catch body)
               (begin [set! vo-k vo-k]
                      [set! vo-evaluate body]
                      [set! vo-env (envr_extend-env vo-k vo-env)]
                      (set! programCounter value-of-cps))]
    
              [(throw k-exp v-exp)
               (begin [set! vo-k (kt_k-throw v-exp vo-env)]
                      [set! vo-evaluate k-exp]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
    
              [(let e body)
               (begin [set! vo-k (kt_k-let body vo-env vo-k)]
                      [set! vo-evaluate e]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
    
              [(var y)
               (begin [set! ae-k^ vo-k]
                      [set! ae-env vo-env]
                      [set! ae-y y]
                      (set! programCounter apply-env))]
    
              [(lambda body)
               (begin [set! ak-k vo-k]
                      [set! ak-exp (clos_closure body vo-env)]
                      (set! programCounter apply-k))]
    
              [(app rator rand)
               (begin [set! vo-k (kt_k-rator rand vo-env vo-k)]
                      [set! vo-evaluate rator]
                      [set! vo-env vo-env]
                      (set! programCounter value-of-cps))]
              )
  )

;apply-k applier and closer 
(define-label apply-k
  (union-case ak-k kt 

              [(k1-multi x2^ env-cps^ k^)
               (begin [set! vo-k (kt_k2-multi ak-exp k^)]
                      [set! vo-evaluate x2^]
                      [set! vo-env env-cps^]
                      (set! programCounter value-of-cps))]
    
              [(k2-multi x1^ k^)
               (begin  [set! ak-k k^]
                       [set! ak-exp (* x1^ ak-exp)]
                       (set! programCounter apply-k))]

              [(k-sub k^)
               (begin  [set! ak-k k^]
                       [set! ak-exp (sub1 ak-exp)]
                       (set! programCounter apply-k))]

              [(k-zero k^)
               (begin [set! ak-k k^]
                      [set! ak-exp (zero? ak-exp)]
                      (set! programCounter apply-k))]

              [(k-if conseq^ alt^ env-cps^ k^)
               (if ak-exp
                   (begin [set! vo-k k^]
                          [set! vo-evaluate conseq^]
                          [set! vo-env env-cps^]
                          (value-of-cps))
                   (begin [set! vo-k k^]
                          [set! vo-evaluate alt^]
                          [set! vo-env env-cps^]
                          (set! programCounter value-of-cps)))]

              [(k-throw v-exp^ env-cps^)
               (begin [set! vo-k ak-exp]
                      [set! vo-evaluate v-exp^]
                      [set! vo-env env-cps^]
                      (set! programCounter value-of-cps))]

    
              [(k-let body^ env-cps^ k^)
               (begin [set! vo-k k^]
                      [set! vo-evaluate body^]
                      [set! vo-env (envr_extend-env ak-exp env-cps^)]
                      (set! programCounter value-of-cps))]


              [(k-rator rand^ env-cps^ k^)
               (begin [set! vo-k (kt_k-rand ak-exp k^)]
                      [set! vo-evaluate rand^]
                      [set! vo-env env-cps^]
                      (set! programCounter value-of-cps))]


              [(k-rand c-cps^ k^)
               (begin [set! ac-k^ k^]
                      [set! ac-cps c-cps^]
                      [set! ac-x ak-exp]
                      (set! programCounter apply-closure))]


              [(k-init jumpout) (dismount-trampoline jumpout)]
    
              )
  )

;apply env ***
(define-label apply-env
  (union-case ae-env envr
              [(extend-env x^ env-cps^)
               (if (zero? ae-y)
                   (begin [set! ak-k ae-k^]
                          [set! ak-exp x^]
                          (set! programCounter apply-k))
                   (begin [set! ae-k^ ae-k^]
                          [set! ae-env env-cps^]
                          [set! ae-y (sub1 ae-y)]
                          (set! programCounter apply-env)))]
              [(empty-env) (error "~unbound identifier~" )]
              )
  )


;step 2 this is good 
(define-union clos
  (closure   body env-cps)
  )

;step 3 this is also good 
(define-union envr 
  (empty-env)
  (extend-env  value^ env-cps^)
  )

;step4 looks good no lets :)
(define-union kt
  (k-init jumpout)
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

(define-label apply-closure
  (union-case ac-cps clos
              [(closure body env-cps)
               (begin [set! vo-k ac-k^]
                      [set! vo-evaluate body]
                      [set! vo-env (envr_extend-env ac-x env-cps)]
                      (set! programCounter value-of-cps))]
              )
  )

;make closure constructor to define-union no longer necessary??
;(define-label(make-closure body env-cps)
;`(closure ,body ,env-cps))

;extend env
;(define-label(extend-env y k^)
; `(extend-env ,y ,k^))


;empty env
(define-label empty-env
  (envr_empty-env)
  )

;empty k
(define-label empty-k
  (kt_k-init)
  )

(define-label main 
  (begin [set! vo-env (empty-env)]
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
         (set! programCounter value-of-cps)
         (mount-trampoline kt_k-init vo-k programCounter)
         (printf "Fact 5: ~s\n" ak-exp)
         )
  )


;(main)

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

