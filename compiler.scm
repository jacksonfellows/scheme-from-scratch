(load "stdlib.scm")

(define fxshift 2)
(define fxmask 3) ;; 0x03, 0b00000011
(define fxtag 0)

(define t 111) ;; 0x6F, 0b01101111
(define f 47)  ;; 0x2F, 0b00101111

(define bmask 191) ;; 0xBF, 10111111
(define btag 47)   ;; 0x2F, 00101111

(define cshift 8)
(define cmask 255) ;; 0xFF, 0b11111111
(define ctag 15)   ;; 0x0F, 0b00001111

(define null 63) ;; 0x3F, 0b00111111

(define (imm? x)
  (or (number? x) (boolean? x) (char? x) (null? x)))

(define (compile-imm x)
  (cond
   ((number? x) (lsh x fxshift))
   ((boolean? x) (if x t f))
   ((char? x) (+ ctag (lsh x cshift)))
   ((null? x) null)))

(define (primcall? x)
  (and (pair? x) (symbol? (car x))))

(define *primcalls* '())

(define (make-primcall name expander)
  (set! *primcalls* (cons (cons name expander) *primcalls*)))

;; C constant
(define (cc x)
  (list 'cc x))
(make-primcall 'cc car)

(define (binop r op l) (list (compile-expr r) op (compile-expr l)))

(define (make-unary-primcall name expander)
  (make-primcall name (lambda (args) (expander (car args)))))

(make-unary-primcall 'fxadd1 (lambda (x) (binop x '+ 1)))
(make-unary-primcall 'fxsub1 (lambda (x) (binop x '- 1)))

(make-unary-primcall 'char->fixnum (lambda (x) (binop x '>> (cc (- cshift fxshift)))))
(make-unary-primcall 'fixnum->char (lambda (x) (binop (cc (binop x '<< (cc (- cshift fxshift))))
						      '+
						      (cc ctag))))

(define (to-bool tree) (list tree '? t ': f))

(make-unary-primcall 'fixnum? (lambda (x) (to-bool (binop (cc (binop x '& (cc fxmask)))
							  '==
							  (cc fxtag)))))

(make-unary-primcall 'null? (lambda (x) (to-bool (binop x '== (cc null)))))

(make-unary-primcall 'not (lambda (x) (to-bool (binop x '== (cc f)))))

(make-unary-primcall 'boolean? (lambda (x) (to-bool (binop (cc (binop x '& (cc bmask)))
							   '==
							   (cc btag)))))

(make-unary-primcall 'char? (lambda (x) (to-bool (binop (cc (binop x '& (cc cmask)))
							'==
							(cc ctag)))))

(define (compile-primcall x)
  (let ((primcall-compiler (assq-ref (car x) *primcalls*)))
    (if primcall-compiler
	(primcall-compiler (cdr x))
	(error "cannot compile primcall" x))))

(define (compile-expr x)
  (cond
   ((imm? x) (compile-imm x))
   ((primcall? x) (compile-primcall x))
   (else (error "cannot compile expr" x))))

(define emit display)
(define (emitln x)
  (emit x)
  (newline))

(define (emit-tree tree)
  (cond
   ((list? tree) (emit "(") (for-each emit-tree tree) (emit ")"))
   (else (emit tree))))

(define (emit-expr x)
  (emit-tree (compile-expr x)))

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

(emit-program '(boolean? ()))
