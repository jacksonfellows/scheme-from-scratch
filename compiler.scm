(load "stdlib.scm")

(define fxshift 2)
(define fxmask 3) ;; 0x03, 0b00000011
(define fxtag 0)

(define fxmin (- (lsh 1 29)))
(define fxmax (- (lsh 1 29) 1))

(define t 111) ;; 0x6F, 0b01101111
(define f 47)  ;; 0x2F, 0b00101111

(define bmask 191) ;; 0xBF, 10111111
(define btag 47)   ;; 0x2F, 00101111

(define cshift 8)
(define cmask 255) ;; 0xFF, 0b11111111
(define ctag 15)   ;; 0x0F, 0b00001111

(define null 63) ;; 0x3F, 0b00111111

(define (fixnum? x)
  (and (number? x) (<= fxmin x fxmax)))

(define (imm? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (compile-imm x)
  (cond
   ((fixnum? x) (lsh x fxshift))
   ((boolean? x) (if x t f))
   ((char? x) (+ ctag (lsh x cshift)))
   ((null? x) null)))

(define *primitives* '())

(define (make-primitive name expander)
  (set! *primitives* (cons (cons name expander) *primitives*)))

(define (binop r op l) (list (compile-expr r) op (compile-expr l)))

(define (make-unary-primitive name expander)
  (make-primitive name (lambda (args) (expander (car args)))))

(make-unary-primitive 'fxadd1 (lambda (x) (binop x '+ 1)))
(make-unary-primitive 'fxsub1 (lambda (x) (binop x '- 1)))

(make-unary-primitive 'char->fixnum (lambda (x) (binop x '>> (cc (- cshift fxshift)))))
(make-unary-primitive 'fixnum->char (lambda (x) (binop (cc (binop x '<< (cc (- cshift fxshift))))
						      '+
						      (cc ctag))))

(define (to-bool tree) (list tree '? t ': f))

(make-unary-primitive 'null? (lambda (x) (to-bool (binop x '== (cc null)))))
(make-unary-primitive 'not (lambda (x) (to-bool (binop x '== (cc f)))))
(make-unary-primitive 'fxzero? (lambda (x) (to-bool (binop x '== 0))))

(define (tagged? mask tag) (lambda (x) (to-bool (binop (cc (binop x '& (cc mask)))
						       '==
						       (cc tag)))))

(make-unary-primitive 'fixnum? (tagged? fxmask fxtag))
(make-unary-primitive 'boolean? (tagged? bmask btag))
(make-unary-primitive 'char? (tagged? cmask ctag))

(define (make-binary-primitive name expander)
  (make-primitive name (lambda (args) (expander (car args) (cadr args)))))

(define (pure-binop op) (lambda (x y) (binop x op y)))

(make-binary-primitive 'fx+ (pure-binop '+))
(make-binary-primitive 'fx- (pure-binop '-))

;; 4xy = (4x/4) * 4y
(make-binary-primitive 'fx* (lambda (x y) (binop (cc (binop x '>> (cc fxshift))) '* y)))

;; 4xy = (4x * 4y)/4
;; (make-binary-primitive 'fx* (lambda (x y) (binop (cc (binop x '* y)) '>> (cc fxshift))))

(make-binary-primitive 'fxlogand (pure-binop '&))
(make-binary-primitive 'fxlogor (pure-binop "|"))

(make-binary-primitive 'fx=  (compose to-bool (pure-binop '==)))
(make-binary-primitive 'fx>  (compose to-bool (pure-binop '>)))
(make-binary-primitive 'fx>= (compose to-bool (pure-binop '>=)))
(make-binary-primitive 'fx<  (compose to-bool (pure-binop '<)))
(make-binary-primitive 'fx<= (compose to-bool (pure-binop '<=)))

(make-binary-primitive 'char=  (compose to-bool (pure-binop '==)))
(make-binary-primitive 'char>  (compose to-bool (pure-binop '>)))
(make-binary-primitive 'char>= (compose to-bool (pure-binop '>=)))
(make-binary-primitive 'char<  (compose to-bool (pure-binop '<)))
(make-binary-primitive 'char<= (compose to-bool (pure-binop '<=)))

(define (primitive? x)
  (assq x *primitives*))

(define (primcall? x)
  (and (pair? x) (primitive? (car x))))

(define (compile-primcall x)
  ((assq-ref (car x) *primitives*) (cdr x)))

(define (tagged-pair? tag)
  (lambda (x)
    (and (pair? x) (eq? tag (car x)))))

(define (from-bool x)
  (binop x '!= (cc f)))

(define if? (tagged-pair? 'if))
(define (compile-if x)
  (list (from-bool (cadr x)) '? (compile-expr (caddr x)) ': (compile-expr (cadddr x))))

;; C constant
(define (cc x)
  (list 'cc x))
(define cc? (tagged-pair? 'cc))

(define (compile-expr x)
  (cond
   ((imm? x) (compile-imm x))
   ((if? x) (compile-if x))
   ((cc? x) (cdr x))
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

(emit-program '(fx* (fx+ 1 1) (fx+ 1 1)))
