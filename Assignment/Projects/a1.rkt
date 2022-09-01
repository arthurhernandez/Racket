#lang racket
;Arthur Hernandez
;Assignment CSCI 305


;1 countdown
(define countdown
  (lambda (n)
        (cond
      ((zero? n) '(0))
      (else (cons n (countdown (sub1 n))))
      )
    )
  )

(countdown 5)

;2 insert for y before every x
(define insertL
  (lambda (x y list)
    (cond
      ((empty? list) '())
      ((eqv? x (car list)) (cons y (cons x (insertL x y(cdr list))))) 
      (else (cons (car list) (insertL  x y (cdr list))))
    )
   )
  )

(insertL 'x 'y '(x z z x y x))

;3 remove first instance of x
(define remv-1st
  (lambda (x list)
    (cond
      ((empty? list) '())
      ((eqv? (car list) x) (cdr list)) 
      (else (cons (car list) (insertL x (cdr list)))) 
    )
   )
 )

(remv-1st 'x '(x y z x))

;4 map procedure to all elms in list 

(define map
  (lambda (procedure list)
    (cond
      ((empty? list) '())
      (else (cons (procedure (car list))
            (map procedure (cdr list))))
      )
    )
  )

(map sub1 '(1 2 3 4))

;5 filter using predicate

(define filter
  (lambda (procedure list)
    (cond
      ((empty? list) '())
      ((procedure (car list)) (cons (car list) (filter procedure (cdr list))))
      (else (filter procedure (cdr list)))
      )
    )
  )

(filter even? '(1 2 3 4 5 6))

;6 2 list combination
(define zip
  (lambda (list1 list2)
    (cond
      ((empty? list1) '())
      ((empty? list2) '())
      (else (cons (cons (car list1)(car list2)) (zip (cdr list1) (cdr list2))))
    )
  )
)

(zip '(1 2 3) '(a b e f))

;7 return the index of n 
(define list-index-ofv
  (lambda (n list)
    (cond
      ((empty? list) (println "bad data"))
      ((eqv? (car list) n) 0)
      (else (+ (index-of (cdr list) n) 1))
    )
   )
 )

(list-index-ofv 'n '(y z x x n))

;8 append 2nd list to 1st
(define append
  (lambda (list1 list2)
    (cond
      ((empty? list1) list2)
      (else (cons (car list1) (append (cdr list1) list2)))
    )
  )
)
(append '(42 120) '(1 2 3))

;9 reverse list order

(reverse '(a 3 x))

;10
(define repeat
  (lambda (list n)
    (cond
      ((eqv? n 1) list)
      (else(append list (repeat list (sub1 n))))
     )
    )
  )

(repeat '(4 8 11) 4)

;11 same lists

;(define same-lists*
 ; (lambda (list1 list2)
  ;  (cond
   ;   ((empty? list1) #t)
    ;  ((same-lists* (car list1) (car list2)) (same-lists* (cdr list1) (cdr list2)))
     ; (else #f)
    ;)
  ;)
;)
;(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))


;12 '((w . (x . ())) . (y . (( z . ()) . ())))

;13
;14
(define div
  (lambda (x y)
    (cond
      ((= x y) 1)
      (else (+ 1 (div (- x y ) y)))

      )
    )
  )
(div 25 5)

;15

(define append-map
  (lambda (x list)
    (cond
      ((empty? list) '())
      (else (append (x (car list)) (append-map x (cdr list))))
     )
    )
  )
(append-map countdown (countdown 5))

;16


;17

(define foldr
  (lambda (x y list)
    (cond
      ((empty? list) y)
      (else (foldr x (x y (car list)) (cdr list)))

      )
    )
  )
                  
(foldr cons '() '(1 2 3 4))

