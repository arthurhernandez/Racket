#lang racket
;Assignment 4
;Arthur Hernandez

;Also make your closures representation independent
;using tagged list data structure. You should write
;two closure helper functions for this interpreter. Write apply-closure-ds and make-closure-ds for value-of-ds.

;(define value-of-ds)

(define value-of-ds
  (lambda (exp env)
    (match exp
      
      [`(+ ,nexp1 ,nexp2)
       (+ (value-of-ds nexp1 env) (value-of-ds nexp2 env))]
      
      [`,y #:when (symbol? y) (apply-env-ds env y)]

      [`(lambda (,x) ,body)
       (make-closure-ds x body env)]
      
      [`(ext-env ,x ,arg ,env)
       (cond
         [(eqv? exp x) arg]
         [else (apply-env-ds env exp)])]
      
      [`(zero? ,rand) (zero? (value-of-ds rand env))]
      
      [`(sub1 ,rand) (sub1 (value-of-ds rand env))]

      [`(* ,n ,m) (* (value-of-ds n env)
                                         (value-of-ds m env))]
      
      [`(if ,pred ,t ,f) (cond
                           ((value-of-ds pred env) (value-of-ds t env))
                                                   (else (value-of-ds f env)))]
      
      [`(let ([,x ,value]) ,body)
       (let ([val (value-of-ds value env)])
         (value-of-ds body (extend-env-ds x val env)))] 
      
      [`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env)(value-of-ds rand env))]

      [`,prim #:when (or (integer? prim) (boolean? prim)) prim]
      
      )
    )
  )

;(define empty-env-ds ...)
(define empty-env-ds
  (λ ()
    (λ (y)
      (error "unbound variable ~y" y))))

;(define extend-env-ds ...)

(define extend-env-ds
  (λ (x arg env)
    (λ (y)
      (cond
        [(eqv? y x) arg]
        [else (env y)]))))

;(define apply-env-ds ...)

(define apply-env-ds
  (lambda (env y)
    (env y)))

;(define make-closure-ds ...)

(define make-closure-ds
  (lambda (x body env)
    `(rator ,x ,body ,env)))


;(define apply-closure-ds ...)

(define apply-closure-ds
  (lambda (end val)
    (match end
            [`(rator ,x ,body ,env)(value-of-ds body (extend-env-ds x val env))])))

;----------------------------Tests-----------------------------;

(println "(value-of-ds '((lambda (x) (if (zero? x)  #t  #f)) 0) (empty-env-ds))")
(value-of-ds
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-ds))

(println "(value-of-ds '((lambda (x) (if (zero? x)  12  47))  0) ")
(value-of-ds
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (empty-env-ds))

(println "(value-of-ds  '(let ([y (* 3 4)])  ((lambda (x) (* x y)) (sub1 6)))  (empty-env-ds))")
(value-of-ds
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-ds))

(println "(value-of-ds '(let ([x (* 2 3)])  (let ([y (sub1 x)])   (* x y)))  (empty-env-ds))")
(value-of-ds
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-ds))

(println "(value-of-ds '(let ([x (* 2 3)])   (let ([x (sub1 x)])   (* x x)))   (empty-env-ds))")
(value-of-ds
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-ds))

(println "(value-of-ds '(((lambda (f) (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))) (lambda (f) (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))) 5) (empty-env-ds))")
(value-of-ds
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-ds))