#lang racket
;Arthur Hernandez
;Assignment 2 CSCI-B 305

;1 refrenced https://stackoverflow.com/questions/28245935/racket-list-ref-implementation-half-working
(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr
          (lambda (n)
            (cond
              ((zero? n) ls)
              (else(cdr(nth-cdr (- n 1))))
              )
            )
          ])
      (car (nth-cdr n))
      )
    )
 )
(println "Problem 1:")
(print "(list-ref '(a b c) 2)")
(list-ref '(a b c) 2)
(print "(list-ref '(a b d e k c) 4)")
(list-ref '(a b d e k c) 4)
(print "(list-ref '(a b c) 0)")
(list-ref '(a b c) 0)
(print "(list-ref '(a b c) 2)")
(list-ref '(a) 0)

;2
(define union
  (lambda (list1 list2)
    (cond(
          (null? list2) list1)
          ((member (car list2) list1) (union list1 (cdr list2)))
          (else (union (cons (car list2) list1) (cdr list2)))
    )
  )
)
(println "Problem 2:")
(print "(union '() '())")
(union '() '())
(print "(union '(x) '())")
(union '(x) '())
(print "(union '(x) '(x))")
(union '(x) '(x))
(print "(union '(x y) '(x z))")
(union '(x y) '(x z))
(print "(union '(x y) '(x z y))")
(union '(x y) '(x z))

;3
(define stretch
  (lambda (n? m)
    (lambda (x)
      (or (eqv? x m)
          (n? x))
      )
    )
  )

(println "Problem 3:")
(print "((stretch even? 1) 0)")
((stretch even? 1) 0)
(print "((stretch even? 1) 1)")
((stretch even? 1) 1)
(print "((stretch even? 1) 2)")
((stretch even? 1) 2)
(print "((stretch even? 1) 3)")
((stretch even? 1) 3)
(print "(filter (stretch even? 1) '(0 1 2 3 4 5))")
(filter (stretch even? 1) '(0 1 2 3 4 5))
(print "(filter (stretch (stretch even? 1) 3) '(0 1 2 3 4 5))")
(filter (stretch (stretch even? 1) 3) '(0 1 2 3 4 5))
(print "(filter (stretch (stretch (stretch even? 1) 3) 7) '(0 1 2 3 4 5))")
(filter (stretch (stretch (stretch even? 1) 3) 7) '(0 1 2 3 4 5))

;4
(define walk-symbol
  (lambda (n list)
    (letrec
        ([ass? (lambda (n)
                      (if (assv n list)
                          (ass? (cdr (assv n list)))
                          n))])
      (ass? n)
      )
    )
  )

(println "Problem 4:")
(print "(walk-symbol 'a '((a . 5)))")
(walk-symbol 'a '((a . 5)))
(print "(walk-symbol 'a '((b . c) (a . b)))")
(walk-symbol 'a '((b . c) (a . b)))
(print "(walk-symbol 'a '((a . 5) (b . 6) (c . a)))")
(walk-symbol 'a '((a . 5) (b . 6) (c . a)))
(print "(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))")
(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))
(print "(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))")
(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
(print "(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))")
(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
(print "(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))")
(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))

;5 need to finish 
(define lambda-exp?
  (λ (E)
    (letrec
      ([p
        (λ (e)
          (match e
            [`,y #:when(symbol? y) #t]
            [`(lambda (,x),body)#:when(symbol? x)(or
                                              (eqv? x body)
                                              (p body))]
            [`(,rator,rand .,more) (and
                                      (p rator)
                                      (p rand))]
            [else #f]))])
      (p E))))

(println "Problem 5:")
(print "(lambda-exp? 'x)")
(lambda-exp? 'x)
(print "(lambda-exp? '(lambda (x) x))")
(lambda-exp? '(lambda (x) x))
(print "(lambda-exp? '(lambda (f) (lambda (x) (f (x x)))))")
(lambda-exp? '(lambda (f) (lambda (x) (f (x x)))))
(print "(lambda-exp? '(lambda (x) (lambda (y) (y x))))")
(lambda-exp? '(lambda (x) (lambda (y) (y x))))
(print "(lambda-exp? '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))")
(lambda-exp? '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
(print "(lambda-exp? '(lambda (lambda) lambda))")
(lambda-exp? '(lambda (lambda) lambda))
(print "(lambda-exp? '((lambda (lambda) lambda) (lambda (y) y)))")
(lambda-exp? '((lambda (lambda) lambda) (lambda (y) y)))
(print "(lambda-exp? '((lambda (x) x) (lambda (x) x)))")
(lambda-exp? '((lambda (x) x) (lambda (x) x)))
(print "(lambda-exp? '((lambda (5) x) (lambda (x) x)))")
(lambda-exp? '((lambda (5) x) (lambda (x) x)))
(print "(lambda-exp? '((lambda (x) x) (lambda (x) x) (lambda (x) x)))")
(lambda-exp? '((lambda (x) x) (lambda (x) x) (lambda (x) x)))
(print "(lambda-exp? '((lambda (lambda (x) x) x)  (lambda (x) x)))")
(lambda-exp? '((lambda (lambda (x) x) x)  (lambda (x) x)))

;6
(define (var-occurs? n e)
    (match e
      [`(lambda
            (,y)#:when ,rest)(cond
                           ((eqv? y n) #f)
                           (var-occurs? n rest))]
            [`(,rator ,rand)(or
                                  (var-occurs? n rator)
                                  (var-occurs? n rand))]
            [`, x (eqv? n x)]
      )
  )
(println "Problem 6:")
(print "(var-occurs? 'x 'x)")
(var-occurs? 'x 'x)
(print "(var-occurs? 'x '(lambda (x) y))")
(var-occurs? 'x '(lambda (x) y))
(print "(var-occurs? 'x '(lambda (y) x))")
(var-occurs? 'x '(lambda (y) x))
(print "(var-occurs? 'x '((z y) x))")
(var-occurs? 'x '((z y) x))

;7
(define vars
  (lambda (other)
    (letrec
      ([n
        (lambda (e)
          (match e
            [`,y #:when(symbol? y)(list y)]
            [`(lambda (,x),rest)#:when(symbol? x) (n rest)]
            [`(,rator ,rand . ,more) (append
                                      (n rator)
                                      (n rand))]
            [else #f]))])
      (n other)
      )
    )
  )

(println "Problem 7:")
(print "(vars 'x)")
(vars 'x)
(print "(vars '(lambda (x) x))")
(vars '(lambda (x) x))
(print "(vars '((lambda (y) (x x)) (x y)))")
(vars '((lambda (y) (x x)) (x y)))
(print "(vars '(lambda (z) ((lambda (y) (a z))(h (lambda (x) (h a))))))")
(vars '(lambda (z) ((lambda (y) (a z))(h (lambda (x) (h a))))))

;8 same as 7 but union
(define unique-vars
  (lambda (other)
    (letrec
      ([n
        (lambda (e)
          (match e
            [`,y #:when(symbol? y)(list y)]
            [`(lambda (,x),rest)#:when(symbol? x) (n rest)]
            [`(,rator ,rand . ,more) (union
                                      (n rator)
                                      (n rand))]
            [else #f]))])
      (n other)
      )
    )
  )
(println "Problem 8:")
(print "(unique-vars '((lambda (y) (x x)) (x y)))")
(unique-vars '((lambda (y) (x x)) (x y)))
(print "(unique-vars '((lambda (z) (lambda (y) (z y))) x))")
(unique-vars '((lambda (z) (lambda (y) (z y))) x))
(print "(unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))")
(unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))

;9
(define var-occurs-free?
  (lambda (n other)
    (letrec
      ([p
        (lambda (e)
          (match e
            [`,y #:when (symbol? y) (eqv? y n)]
            [`(lambda (,x) ,rest) #:when (symbol? x) (and
                                                      (not
                                                       (eqv? x n))
                                                       (var-occurs-free? n rest))]
            [`(,rator ,rand . ,more) (or
                                      (var-occurs-free? n rator)
                                      (var-occurs-free? n rand))]
            [else #f]))])
      (p other)
      )
    )
  )

(println "Problem 9:")
(print "(var-occurs-free? 'x 'x)")
(var-occurs-free? 'x 'x)
(print "(var-occurs-free? 'x '(lambda (y) y))")
(var-occurs-free? 'x '(lambda (y) y))
(print "(var-occurs-free? 'x '(lambda (x) (x y)))")
(var-occurs-free? 'x '(lambda (x) (x y)))
(print "(var-occurs-free? 'x '(lambda (x) (lambda (x) x)))")
(var-occurs-free? 'x '(lambda (x) (lambda (x) x)))
(print "(var-occurs-free? 'y '(lambda (x) (x y)))")
(var-occurs-free? 'y '(lambda (x) (x y)))
(print "(var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))")
(var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))
(print "(var-occurs-free? 'x '((lambda (x) (x x)) (x x)))")
(var-occurs-free? 'x '((lambda (x) (x x)) (x x)))

;10 use 9
(define var-occurs-bound?
  (lambda (n other)
    (letrec
      ([p
        (lambda (e)
          (match e
            [`,y #:when (symbol? y) #f]
            [`(lambda (,x) ,body) #:when (symbol? x) (or
                                                      (and
                                                       (eqv? x n)
                                                       (var-occurs-free? n body))
                                                      (var-occurs-bound? n body))]
            [`(,rator ,rand . ,more) (or
                                      (var-occurs-bound? n rator)
                                      (var-occurs-bound? n rand))]
            [else #f]))])
      (p other)
      )
    )
  )
(println "Problem 10:")
(print "(var-occurs-bound? 'x 'x)")
(var-occurs-bound? 'x 'x)
(print "(var-occurs-bound? 'x '(lambda (x) x))")
(var-occurs-bound? 'x '(lambda (x) x))
(print "(var-occurs-bound? 'y '(lambda (x) x))")
(var-occurs-bound? 'y '(lambda (x) x))
(print "(var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))")
(var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
(print "(var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))")
(var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
(print "(var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))")
(var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
(print "(var-occurs-bound? 'x '(lambda (x) y))")
(var-occurs-bound? 'x '(lambda (x) y))
(print "(var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))")
(var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))

;11
(define unique-free-vars
  (lambda (other)
    (letrec
      ([n
        (lambda(e)
          (match e
            [`,y #:when(symbol? y)(if (var-occurs-free? y other)
                                       (list y) empty)]
            [`(lambda(,x),body)#:when(symbol? x) (n body)]
            [`(,rator,rand .,more) (union
                                    (n rator)
                                    (n rand))]
            [else #f]))])
      (n other)
      )
    )
  )

(println "Problem 11:")
(print "(unique-free-vars 'x)")
(unique-free-vars 'x)
(print "(unique-free-vars '(lambda (x) (x y)))")
(unique-free-vars '(lambda (x) (x y)))
(print "(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))")
(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))

;12 isnt this the same thing as 11?
(define unique-bound-vars
  (lambda (other)
    (letrec
      ([n
        (lambda(e)
          (match e
            [`,y #:when(symbol? y)(if (var-occurs-bound? y other)
                                       (list y) empty)]
            [`(lambda(,x),body)#:when(symbol? x) (n body)]
            [`(,rator,rand .,more) (union
                                    (n rator)
                                    (n rand))]
            [else #f]))])
      (n other)
      )
    )
  )
(println "Problem 12:")
(print "(unique-bound-vars 'x)")
(unique-bound-vars 'x)
(print "(unique-bound-vars '(lambda (x) y))")
(unique-bound-vars '(lambda (x) y))
(print "(unique-bound-vars '(lambda (x) (x y)))")
(unique-bound-vars '(lambda (x) (x y)))
(print "(unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))")
(unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
(print "(unique-bound-vars '(lambda (y) y))")
(unique-bound-vars '(lambda (y) y))
(print "(unique-bound-vars '(lambda (x) (y z)))")
(unique-bound-vars '(lambda (x) (y z)))
(print "(unique-bound-vars '(lambda (x) (lambda (x) x)))")
(unique-bound-vars '(lambda (x) (lambda (x) x)))

;13
(define G
  (λ (p)
    (cond
      [(zero? p) (λ (n m)
                   (cond
                     [(zero? m) n]
                     [else (add1 ((G 0) n (sub1 m)))]))]
      [(zero? (sub1 p)) (λ (n m)
                          (cond
                            [(zero? m) 0]
                            [else ((G 0) n ((G 1) n (sub1 m)))]))]
      [else (λ (n m)
              (cond
                [(zero? m) 1]
                [else ((G (sub1 p)) n ((G p) n (sub1 m)))]))])))
;(define G
 ; (λ (p)
  ;  (λ (n m)
   ; (cond
    ;  [(zero? p) 
     ;  (cond
      ;   [(zero? m) n]
;         [else (add1 n (G (sub1 m)))])]
      ;[(zero? m)
;       (cond
 ;        [(zero? m) 0]
  ;       [else 1])]
   ;   [else 
    ;   (cond
     ;    [(zero? m) 1]
      ;   [else (G (sub1 p) n (G p n (sub1 m)))])]))))

(println "Problem 13:")
(print "(define + (G 0))")
(define + (G 0))
(print "define * (G 1))")
(define * (G 1))
(print "(define ^ (G 2))")
(define ^ (G 2))
(print "(+ 2 3)")
(+ 2 3)
(print "(* 2 3)")
(* 2 3)
(print "(^ 2 3)")
(^ 2 3)
(print "((G 3) 2 3)")
((G 3) 2 3)
(print "((G 3) 3 2)")
((G 3) 3 2)

;14