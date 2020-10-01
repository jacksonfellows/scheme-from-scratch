(define (mymap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (mymap f (cdr lst)))))

(define (times100 x)
  (fx* x 100))

(mymap times100 '(-3 -2 -1 0 1 2 3))
