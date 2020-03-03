(load "stdlib.scm")

(define fxshift 2)
(define t 111) ;; 0x6F, 0b01101111
(define f 47) ;; 0x2F, 0b00101111
(define cshift 8)
(define ctag 15) ;; 0x0F, 0b00001111
(define null 63) ;; 0x3F, 0b00111111

(define emit display)
(define (emitln x)
  (display x)
  (newline))

(define (imm? x)
  (or (number? x) (boolean? x) (char? x) (null? x)))

(define (primcall? x)
  (and (pair? x) (symbol? (car x))))

(define (imm-rep x)
  (cond
   ((number? x) (lsh x fxshift))
   ((boolean? x) (if x t f))
   ((char? x) (+ ctag (lsh x cshift)))
   ((null? x) null)
   ))

(define (emit-wrapped thunk)
  (emit "(")
  (thunk)
  (emit ")"))

(define (emit-wrapped-expr x)
  (emit-wrapped (lambda () (emit-expr x))))

(define primcalls
  (list (cons 'fxadd1 (lambda (args)
			(emit-wrapped-expr (car args))
			(emit "+")
			(emit (imm-rep 1))))
	(cons 'fxsub1 (lambda (args)
			(emit-wrapped-expr (car args))
			(emit "-")
			(emit (imm-rep 1))))
	(cons 'char->fixnum (lambda (args)
			      (emit-wrapped-expr (car args))
			      (emit ">>")
			      (emit (- cshift fxshift))))
	(cons 'fixnum->char (lambda (args)
			      (emit-wrapped (lambda ()
					      (emit-wrapped-expr (car args))
					      (emit "<<")
					      (emit (- cshift fxshift))))
			      (emit "+")
			      (emit ctag)))))

(define (emit-primcall x)
  ((cdr (assq (car x) primcalls)) (cdr x)))

(define (emit-expr x)
  (cond
   ((imm? x) (emit (imm-rep x)))
   ((primcall? x) (emit-primcall x))
   ))

(define (emit-program x)
  (emitln "#include <stdio.h>

#define fxshift 2
#define fxmask 0x03
#define fxtag 0x00

#define t 0x6F
#define f 0x2F

#define cshift 8
#define cmask 0xFF
#define ctag 0x0F

#define null 0x3F

typedef unsigned int scm;")
  (emitln "
scm scheme()
{")
  (emit "return ") (emit-expr x) (emitln ";
}")
  (emitln "
void print_scheme(scm scheme_val)
{
if ((scheme_val & fxmask) == fxtag)
printf(\"%d\", (int)scheme_val >> fxshift);
else if (scheme_val == t)
printf(\"#t\");
else if (scheme_val == f)
printf(\"#f\");
else if ((scheme_val & cmask) == ctag)
printf(\"#\\\\%c\", (char)(scheme_val >> cshift));
else if (scheme_val == null)
printf(\"'()\");
else
printf(\"#<unknown 0x%08x>\", scheme_val);
printf(\"\\n\");
}

int main()
{
print_scheme(scheme());
return 0;
}"))

(emit-program '(fixnum->char (fxadd1 (char->fixnum #\A))))
