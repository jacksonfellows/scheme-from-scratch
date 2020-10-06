(define (list? x)
  (cond
   ((null? x) #t)
   ((pair? x) (list? (cdr x)))
   (else #f)))

(list? 123)
