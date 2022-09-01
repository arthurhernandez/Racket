#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;Arthur Hernandez
;Assignemnt 10

;Part 1

(run! 2 (q)
     (== 5 q)
     (conde
      [(conde 
        [(== 5 q)
         (== 6 q)])
       (== 5 q)]
      [(== q 5)]))

;This evaluates to '(5) because == 5 q makes sure that q is substituded to 5 before anything else.
;This means that we will pass into the conditonals of conde ( conde where is q = 6? and it doesnt it equals 5.
;Then we move onto the second case of the outer conde where we just evaluate q to be 5. This means that 5
;is qs final value. 

(run! 1 (q) 
     (fresh (a b) 
            (== `(,a ,b) q)
            (absento 'tag q)
            (symbolo a)))

;this evaluates to
 '((((_0 _1)) (=/= _0 tag) (=/= _1 tag)) (sym _.0) (absento _0 (tag) (absento _1 (tag))))

;even though the answer to this pair formed by "fresh (a b)" is ((_0 _1)). This is because q is tied to the
;`(,a ,b) pair in the third line. In the fourth line we are told that 'tag cannot be a part of q and
;((absento _0 (tag)) (absento _1 (tag))) is added. The last line tells us that a has to be a symbol
; but this is also =/=, therefore (=/= _0 tag) (=/= _1 tag). according to the result request we get our answer.

;;a) ==
;comparison that must be equal
;b) =/=
;comparison of not equal or !=
;c) numbero
;this argument must be a number 
;d) symbolo
;this argument must be a symbol


;Part2

(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))


(define assoco
  (lambda(x ls o)
    (fresh(a d aa dd)
           (== `(,a . ,d)ls)
           (== `(,aa . ,dd)a)
           (conde
            ((== aa x) (== a o))
            ((=/= aa x) (assoco x d o))
            )
           )
    )
  )

(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))
       ))))

(define reverseo
  (lambda(ls o)
    (conde
     ((== `() ls) (== `()o))
     ((fresh(a d res)
             (== `(,a . ,d)ls)
             (reverseo d res)
             (appendo res `(,a)o))
      )
     )
    )
  )

(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))




(define stuttero
  (lambda(ls o)
    (conde
      (( == '()ls) (== '()o))
      ((fresh (a d res ) 
              (== `(,a . ,d)ls)
              (== o`(,a ,a . ,res))
              (stuttero d res)
              )
       )
      )
    )
  )

(println "--------------------Tests-------------------")

(println " " )
(println "(run 1 q (stuttero q '(1 1 2 2 3 3))) " )
(run 1 q (stuttero q '(1 1 2 2 3 3)))
(println "(run* q (stuttero q '(1 1 2 2 3 3))) " )
(run* q (stuttero q '(1 1 2 2 3 3)))
(println "(run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d)))) " )
(run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d))))
(println "(run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d)))) " )
(run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d))))
(println "(run 1 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g))) " )
(run 1 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
(println "(run 2 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g))) " )
(run 2 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))

(println " (run* q (assoco 'x '() q))" )
(run* q (assoco 'x '() q))
(println "(run* q (assoco 'x '((x . 5)) q)) " )
(run* q (assoco 'x '((x . 5)) q))
(println "(run* q (assoco 'x '((y . 6) (x . 5)) q)) " )
(run* q (assoco 'x '((y . 6) (x . 5)) q))
(println "(run* q (assoco 'x '((x . 6) (x . 5)) q)) " )
(run* q (assoco 'x '((x . 6) (x . 5)) q))
(println "(run* q (assoco 'x '((x . 5)) '(x . 5)))" )
(run* q (assoco 'x '((x . 5)) '(x . 5)))
(println "(run* q (assoco 'x '((x . 6) (x . 5)) '(x . 6))) " )
(run* q (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
(println "(run* q (assoco 'x '((x . 6) (x . 5)) '(x . 5)))" )
(run* q (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
(println "(run* q (assoco q '((x . 6) (x . 5)) '(x . 5)))" )
(run* q (assoco q '((x . 6) (x . 5)) '(x . 5)))
(println "(run* q (assoco 'x '((x . 6) . ,q) '(x . 6)))" )
(run* q (assoco 'x '((x . 6) . ,q) '(x . 6)))
(println "(run 5 q (assoco 'x q '(x . 5))) " )
(run 5 q (assoco 'x q '(x . 5)))
(println "(run 5 q (fresh (x y z)
                (assoco x y z)
                (== `(,x ,y ,z) q))) " )
(run 5 q (fresh (x y z)
                (assoco x y z)
                (== `(,x ,y ,z) q)))



(println " (run* q (reverseo '() q))" )
(run* q (reverseo '() q))
(println "(run* q (reverseo '(a) q))" )
(run* q (reverseo '(a) q))
(println "(run* q (reverseo '(a b c d) q)) " )
(run* q (reverseo '(a b c d) q))
(println "(run* q (fresh (x) (reverseo `(a b ,x c d) q))) " )
(run* q (fresh (x) (reverseo `(a b ,x c d) q)))
(println "(run* x (reverseo `(a b ,x d) '(d c b a)) " )
(run* x (reverseo `(a b ,x d) '(d c b a)))
(println "(run* x (reverseo `(a b c d) `(d . ,x)))" )
(run* x (reverseo `(a b c d) `(d . ,x)))
(println "(run* q (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))" )
(run* q (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
(println "(run 10 q (fresh (x y) (reverseo x y) (== `(,x ,y) q))) " )
(run 10 q (fresh (x y) (reverseo x y) (== `(,x ,y) q)))







