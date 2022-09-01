#lang racket
;A12
;Arthur Hernandez
(require "monads.rkt")
;
;1
(define (findf-maybe x y)
  (match y
    ['() (Nothing)]
    [`(, a . , d) ( if (x a) `(Just ,a) ( findf-maybe x y))]
    )
  )


(findf-maybe symbol? '(1 2 c))
(findf-maybe boolean? '(#f 1 2 c))
(findf-maybe number? '(a b c))


;2
(define (partition-writer fn als)
  (cond
    [( null? als) (inj-writer '())]
    
    [(not (fn (car als))) (bind-writer (tell (car als)) (lambda (x)
                                                          (partition-writer fn (cdr als))))]
    [else (bind-writer (partition-writer fn (cdr als)) (lambda (x)
                                                         (inj-writer (cons (car als) x))))]
    )
  )

(run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
(run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))

;3
(define power
  (lambda (x y)
    (cond
      [(zero?  y) 1]
      
      [(zero? (sub1 y)) x]
      
      [(odd? y) (* x (power x (sub1 y)))]
      
      [(even? y) (let ((k (/ y 2)))
                   (let ((y (power x k)))
                     (* y y)))]
      )
    )
  )


(define (powerXpartials x y)
  (cond
    [(zero? y)
     (inj-writer 0)]
    
    [(= y 1)
     (inj-writer x)]
    
    [(odd? y) (bind-writer (powerXpartials x (- y 1)) (lambda (k)
                                                        (bind-writer (tell k) (lambda (l)
                                                                                (inj-writer (* x k))))))]
    
    [(even? y) (bind-writer (powerXpartials x (/ y 2)) (lambda (k) (bind-writer  (tell k) (lambda (l)
                                                                                            (inj-writer (* k k))))))]
    )
  )

(run-writer (powerXpartials 2 6))
(run-writer (powerXpartials 3 5))
(run-writer (powerXpartials 5 7))

;5

(define traverse
  (lambda (x bind y)
    (letrec((trav
             (lambda (tree)
               (cond
                 [(pair? tree) (go-on (a <- (trav (car tree)))
                                      (b <- (trav (cdr tree))) (x (cons a b)))]
                 [else (y tree)]
                 )
               )
             )
            ) trav)
    )
  )

(define (reciprocal x)
  (if (zero? x) (Nothing) `(Just ,(/ 1 x))))

(reciprocal 0)
(reciprocal 2)

;\6
(define (halve x)
  (if (zero? (modulo x 2) ) (inj-writer (/ x 2) ) (bind-writer (tell x) (lambda (y) (inj-writer x))
                                                               )
      )
  )

(define traverse-halve
    (traverse inj-writer bind-writer halve))

(run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))


;7

(define (state/sum x)
  (lambda (y)
    ((inj-state y) (+ y x))))
