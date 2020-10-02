(let ((v (make-vector 1000)))
  (set! (vector-ref v 999) "hello")
  (cons (vector-length v) (vector-ref v 999)))
