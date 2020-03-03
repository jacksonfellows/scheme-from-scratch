;; Pairs and lists

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;; doesn't handle lists with cycles
(define (list? x)
  (cond
   ((null? x) #t)
   ((pair? x) (list? (cdr x)))
   (else #f)))

(define (append lst x)
  (if (null? lst)
      x
      (cons (car lst) (append (cdr lst) x))))

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
	      (list (car lst)))))

;; Control features

;; should accept multiple lists
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
	    (map f (cdr lst)))))

;; should accept multiple lists
(define (for-each f lst)
  (if (null? lst)
      'ok
      (begin (f (car lst))
	     (for-each f (cdr lst)))))

;; Output

;; should accept an optional port argument
(define (newline)
  (write-char #\newline))