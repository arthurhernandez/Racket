#lang racket
(require "parenthec.rkt")
;Arthur Hernandez
;Assignment 9
;got help for this step from multiple classmates

;Rename the formal parameters of serious calls to the same name surrounded
;by asterisks for example, v becomes *v*. Then transform all your serious
;function calls to our A-normal form style, by adding let* above your serious calls. Ensure that the names of the actual parameters to the serious calls are *exactly* the names of the formal parameters in the definition.

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

(define value-of-cps
  (lambda (vo-evaluate vo-env-cps vo-k)
    (union-case vo-evaluate expr
              
    [(const const-expr)
     (let* ([ak-k vo-k]
            [ak-v const-expr])
       (apply-k ak-k ak-v))]
    
    [(mult nexp1 nexp2)
     (let* ([vo-k (kt_k1-multi nexp2 vo-env-cps vo-k)]
            [vo-evaluate nexp1 ]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(sub1 x)
     (let* ([vo-k (kt_k-sub vo-k)]
            [vo-evaluate x]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(zero x)
     (let* ([vo-k (kt_k-zero vo-k)]
            [vo-evaluate x]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(if test conseq alt)
     (let* ([vo-k (kt_k-if conseq alt vo-env-cps vo-k)]
            [vo-evaluate test]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(catch body)
     (let* ([vo-k vo-k]
            [vo-evaluate body]
            [vo-env-cps (envr_extend-env vo-k vo-env-cps)])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(throw k-exp v-exp)
     (let* ([vo-k (kt_k-throw v-exp vo-env-cps)]
            [vo-evaluate k-exp]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(let e body)
     (let* ([vo-k (kt_k-let body vo-env-cps vo-k)]
            [vo-evaluate e]
            [vo-env-cps vo-env-cps])
         (value-of-cps vo-evaluate vo-env-cps vo-k))]
    
    [(var y)
     (let* ([ae-k^ vo-k]
            [ae-env vo-env-cps]
            [ae-y y])
       (apply-env ae-env ae-y ae-k^))]
    
    [(lambda body)
     (let* ([ak-k vo-k]
            [ak-v (clos_closure body vo-env-cps)])
       (apply-k ak-k ak-v))]
    
    [(app rator rand)
     (let* ([vo-k (kt_k-rator rand vo-env-cps vo-k)]
            [vo-evaluate rator]
            [vo-env-cps vo-env-cps])
       (value-of-cps vo-evaluate vo-env-cps vo-k))]
    )
  )
  )

;apply-k applier and closer 
(define (apply-k ak-k ak-exp)
  (union-case ak-k kt 

     [(k1-multi x2^ env-cps^ k^)
     (let* ([vo-x (kt_k2-multi ak-exp k^)]
            [vo-evaluate x2^]
            [vo-env-cps env-cps^])
       (value-of-cps vo-evaluate vo-env-cps vo-x))]
    
    [(k2-multi x1^ k^)
     (let* ([ak-x k^]
            [ak-apply (* x1^ ak-exp)])
       (apply-k ak-x ak-apply))]

    [(k-sub k^)
     (let* ([ak-x k^]
            [ak-apply (sub1 ak-exp)])
       (apply-k ak-x ak-apply))]

    [(k-zero k^)
     (let* ([ak-x k^]
            [ak-apply (zero? ak-exp)])
       (apply-k ak-x ak-apply))]

    [(k-if conseq^ alt^ env-cps^ k^)
     (if ak-exp
         (let* ([vo-x k^]
                [vo-apply conseq^]
                [vo-env-cps env-cps^])
           (value-of-cps vo-apply vo-env-cps vo-x))
         (let* ([vo-x k^]
                [vo-apply alt^]
                [vo-env-cps env-cps^])
           (value-of-cps vo-apply vo-env-cps vo-x)))]

    [(k-throw v-exp^ env-cps^)
     (let* ([vo-x ak-exp]
            [vo-apply v-exp^]
            [vo-env-cps env-cps^])
       (value-of-cps vo-apply vo-env-cps vo-x))]

    
    [(k-let body^ env-cps^ k^)
     (let* ([vo-x k^]
            [vo-apply body^]
            [vo-env-cps (envr_extend-env ak-exp env-cps^)])
       (value-of-cps vo-apply vo-env-cps vo-x))]


    [(k-rator rand^ env-cps^ k^)
     (let* ([vo-x (kt_k-rand ak-exp k^)]
            [vo-apply rand^]
            [vo-env-cps env-cps^])
       (value-of-cps vo-apply vo-env-cps vo-x))]


[(k-rand c-cps^ k^)
 (let* ([ac-k^ k^]
        [ac-x c-cps^]
        [ac-apply ak-exp])
   (apply-closure ac-x ac-apply ac-k^))]


    [(k-init) ak-exp]
    
    )
  )

;apply env ***
(define (apply-env ae-env ae-y ae-k^)
  (union-case ae-env envr
              [(extend-env x^ env-cps^)
               (if (zero? ae-y)
                   (let* ([ak-x ae-k^]
                          [ak-apply x^])
                     (apply-k ak-x ak-apply))
                   (let* ([ae-k^ ae-k^]
                          [ae-x env-cps^]
                          [ae-apply (sub1 ae-y)])
                     (apply-env ae-x ae-apply ae-k^)))]
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

(define (apply-closure ac-x ac-y ac-k^)
  (union-case ac-x clos
    [(closure body env)
     (let* ([vo-k ac-k^]
            [vo-eval body]
            [vo-env (envr_extend-env ac-y env)])
       (value-of-cps vo-eval vo-env vo-k))]))

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
    (let* ([vo-k (empty-k)]          
           [vo-env-cps (empty-env)]
           [vo-eval
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
              (expr_const 5)))])
      (value-of-cps vo-eval vo-env-cps vo-k)
      )
    ))

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


