(define (fib n)
  (if (fx< n 2)
      n
      (fx+ (fib (fx- n 1)) (fib (fx- n 2)))))

(fib 10)
