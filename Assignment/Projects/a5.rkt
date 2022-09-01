#lang racket
;Assignment 5
;Arthur Hernandez

;(define value-of- ...) (given function)
(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      
      [`,n #:when (number? n)  n]
      
      [`(zero? ,n) (zero? (value-of n env))]
      
      [`(sub1 ,n) (sub1 (value-of n env))]
      
      [`(* ,n1 ,n2) (* (value-of n1 env)
                       (value-of n2 env))]
      
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      
      [`(random ,n) (random (value-of n env))]
      
      [`(lambda (,x) ,body) (make-closure x body env)]
      
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))

;(define value-of-cbv ...)
(define val-of-cbv
  (lambda (exp env)
    (match exp

      [`,b #:when (boolean? b) b]

      [`,n #:when (number? n)  n]

      [`(zero? ,n) (zero? (val-of-cbv n env))]

      [`,y #:when (symbol? y) (unbox (env y))]

      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]

      [`(* ,n1 ,n2) (* (val-of-cbv n1 env)
                       (val-of-cbv n2 env))]

      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(set! ,e1 ,e2) (set-box!(env e1) (val-of-cbv e2 env))]

      [`(begin2 ,e1 ,e2) (begin(val-of-cbv e1 env) (val-of-cbv e2 env))]

      [`(,rator ,rand)
       ((val-of-cbv rator env) (box (val-of-cbv rand env)))]
      )
    )
  )

;(define value-of-cbr ...)
(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      
      [`,n #:when (number? n)  n]
      
      [`(zero? ,n) (zero? (val-of-cbr n env))]

      [`,y #:when (symbol? y) (unbox(env y))]

      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env)
                             (val-of-cbr n2 env))]

      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                          (val-of-cbr conseq env)
                                          (val-of-cbr alt env))]

      [`(set! ,e1 ,e2) (set-box!(env e1) (val-of-cbr e2 env))]
      
      [`(begin2 ,exp1 ,exp2) (begin (val-of-cbr exp1 env) (val-of-cbr exp2 env ))]

      [`(,rator ,rand) #:when(symbol? rand) ((val-of-cbr rator env) (env rand) )]
            
      [`(,rator ,rand) #:when (not(symbol? rand)) ((val-of-cbr rator env) (box(val-of-cbr rand env)))]
      )
    )
  )

;(define value-of-cbname ...)
(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]

      [`,n #:when (number? n)  n]

      [`(zero? ,n) (zero? (val-of-cbname n env))]
      
      [`,y #:when (symbol? y) (unbox (env y))]

      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]

      [`(* ,n1 ,n2) (* (val-of-cbname n1 env)
                       (val-of-cbname n2 env))]
      
      [`(,rator ,rand) #:when (symbol? rand) ((val-of-cbname rator env) (env rand) )]
                                                                  
      [`(,rator ,rand) #:when (not (symbol? rand)) ((val-of-cbname rator env) (box (lambda() (val-of-cbname rand env))))]

      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                          (val-of-cbr conseq env)
                                          (val-of-cbr alt env))]

      [`(random ,n) (random (val-of-cbname n env))]

      )
    )
  )

;(define value-of-cbneed ...)
(define val-of-cbneed
  (λ (exp env)
    (match exp
      
      [`,b #:when (boolean? b) b]

      [`,n #:when (number? n)  n]

      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      
      [`,y #:when (symbol? y) (boxout (env y))]
      
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env)
                       (val-of-cbneed n2 env))]
      
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]

      [`(random ,n) (random (val-of-cbneed n env))]

      [`(,rator ,rand) #:when (symbol? rand) ((val-of-cbneed rator env) (env rand) )]
                                                                  
      [`(,rator ,rand) #:when (not (symbol? rand)) ((val-of-cbneed rator env) (box (λ() (val-of-cbneed rand env))))]
      )
    )
  )

;(define empty-env- ...)
(define empty-env
  (λ ()
    (λ (y)
      (error "unbound variable ~y" y))
    )
  )

;(define extend-env- ...)
(define extend-env
  (lambda (x arg env)
    '(extn ,x,arg,env)
    )
  )

;(define apply-env- ...)
(define apply-env-ds
  (lambda (env y)
    (env y)))

;(define make-closure- ...)
(define make-closure
  (lambda (x body env)
    `(rator ,x ,body ,env)))

;(define make-closure-cbv ...)
(define make-closure-cbv
  (lambda (x body env)
    (lambda (val) (val-of-cbv body(lambda (var) (if (eqv? var x) val (env var)))))
    )
  )

;(define make-closure-cbr ...)
(define make-closure-cbr
  (lambda (x body env)
    (lambda (val) (val-of-cbr body(lambda (var) (if (eqv? var x) val (env var)))))
    )
  )

;(define make-closure-cbname ...)
(define make-closure-cbname
  (lambda (x body env)
    (lambda (val) (val-of-cbname body(lambda (var) (if (eqv? var x) val (env var)))))
    )
  )

;(define make-closure-cbneed ...)
(define make-closure-cbneed
  (lambda (x body env)
    (lambda (val) (val-of-cbneed body(lambda (var) (if (eqv? var x) val (env var)))))
    )
  )
;(define apply-closure- ...)
(define apply-closure
  (lambda (end val)
    (match end
            [`(rator ,x ,body ,env)(value-of body (extend-env x val env))])
    )
  )


;(define unbox ...)
(define boxout
  (lambda (box)
    (let ([y ((unbox box))])
      (set-box! box (lambda () y)) y)
    )
  )


;----------------------------Tests-----------------------------;
(val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env))
(val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
 (val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
(val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
(val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
(val-of-cbr
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))
(val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

(val-of-cbname random-sieve (empty-env))

(val-of-cbneed random-sieve (empty-env))

(val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env))
