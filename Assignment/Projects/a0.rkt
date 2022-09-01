#lang racket
;Arthur Hernandez
;C331 Assignment 0
;01/14/22
;1 downloaded dr racket
;2 define pi 
(define pi 3.14)

;3 circle area 
(define (area_circle radius)
  (* pi (sqr radius)))

;4 circle area and circumfrence 
(define (circumfrence-circle radius)
  (* 2 pi radius))

(define (circle_properties radius)
  (list (circumfrence-circle radius)
  (area_circle radius)))

;5 area and circumfrence of a rectangle 
(define (area-rectangle lst)
  (* (apply + lst) (length lst)))

(define (circumfrence-rectangle lst)
  (* 2(+ (apply + lst) (length lst))))

(define (rectangle_properties lst)
  (list (area-rectangle lst)
  (circumfrence-rectangle lst)))

;6 return needle pos in haystack
(define (find-needle lst)
  (find-needles lst 0))

(define (find-needles lst position)
(cond [(equal? 0 (length lst)) -1]
		[(equal? (car lst) "needle") position]
		[else (find-needles (cdr lst) (add1 position))]))

;7 absolute value
(define (abs x)
(cond [(< x 0) (* x -1)]
      [else x]))

;8 increment each thing in a list by 1 using map
(define (increment lst)
  (map add1 lst))

;9
(define (even x)
  (cond [(equal? (modulo x 2) 0) #t]
        [else #f]))

;10
(define add
  (λ (n m)
    (let sum ((m m))
      (cond
        [(zero? m) n]
        [else (add1 (add n (sub1 m)))]))))

;11
(define adds
  (λ (n m)
    (cond
      [(zero? n) m]
      [else (add1 (adds n (sub1 m)))])))
