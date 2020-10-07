(define (F n)
  (if (fx= n 0) 1
      (fx- n (M (F (fx- n 1))))))

(define (M n)
  (if (fx= n 0) 0
      (fx- n (F (M (fx- n 1))))))

(F 19)
