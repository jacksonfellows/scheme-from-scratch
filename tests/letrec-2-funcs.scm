(letrec ((mymap (lambda (f lst)
                  (if (null? lst)
                      '()
                      (cons (f (car lst))
                            (mymap f (cdr lst))))))
         (second (lambda (lst) (car (cdr lst)))))
  (mymap second '((a b) (c d))))
