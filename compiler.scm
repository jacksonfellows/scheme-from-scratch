(load "stdlib.scm")

;; Uses the tagging system outlined in
;; https://wiki.call-cc.org/man/4/Data%20representation

(define wordsize 8) ;; sizeof(size_t)
(define wordsizebits (* 8 wordsize))

;; fixnums

(define fxshift 1)
(define fxmask 1)
(define fxtag 1)

(define fxmin (- (lsh 1 (- wordsizebits 2))))
(define fxmax (- (lsh 1 (- wordsizebits 2)) 1))

(define (fixnum? x)
  (and (number? x) (<= fxmin x fxmax)))

;; chars and booleans

(define cshift 4)
(define cmask 15) ;; 0b1111
(define ctag 10)  ;; 0b1010

(define bshift 4)
(define bmask 15) ;; 0b1111
(define btag 6)   ;; 0b0110

(define t (+ (lsh 1 bshift) btag))
(define f (+ (lsh 0 bshift) btag))

;; other immediate objects

(define specialshift 4)
(define specialtag 14) ;; 0b1110

(define nulltag 0)
(define null (+ (lsh nulltag specialshift) specialtag))

;; compile immediate objects

(define (imm? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (compile-imm x)
  (cond
   ((fixnum? x) (+ fxtag (lsh x fxshift)))
   ((boolean? x) (if x t f))
   ((char? x) (+ ctag (lsh x cshift)))
   ((null? x) null)))

;; define primitive procedures

(define *primitives* '())

(define (make-primitive name expander)
  (set! *primitives* (cons (cons name expander) *primitives*)))

(define (binop r op l) (list (compile-expr r) op (compile-expr l)))

(define (make-unary-primitive name expander)
  (make-primitive name (lambda (args) (expander (car args)))))

(make-unary-primitive 'fxadd1 (lambda (x) (binop x '+ (cc (lsh 1 fxshift)))))
(make-unary-primitive 'fxsub1 (lambda (x) (binop x '- (cc (lsh 1 fxshift)))))

;; only works because the high bit of ctag is 1
(make-unary-primitive 'char->fixnum (lambda (x) (binop x '>> (cc (- cshift fxshift)))))

;; have to subtract 0b1000 because fxtag is already set
(make-unary-primitive 'fixnum->char (lambda (x) (binop (cc (binop x '<< (cc (- cshift fxshift))))
						       '+
						       (cc (- ctag (lsh fxtag 3))))))

(define (to-bool x) (list (list x '<< bshift) '+ btag))

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

(make-binary-primitive 'fx+ (lambda (x y) (binop x '+ (cc (binop y '- (cc fxtag))))))
(make-binary-primitive 'fx- (lambda (x y) (binop (cc (binop x '- y)) '+ (cc fxtag))))

(define (from-fixnum x) (binop x '>> (cc fxshift)))
(define (to-fixnum x) (binop (cc (binop x '<< (cc fxshift))) '+ (cc fxtag)))

(make-binary-primitive 'fx* (lambda (x y) (to-fixnum (cc (binop (cc (from-fixnum x))
								'*
								(cc (from-fixnum y))))
						     '<<
						     (cc fxshift))))

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

;; compile primitive procedures

(define (primitive? x)
  (assq x *primitives*))

(define (primcall? x)
  (and (pair? x) (primitive? (car x))))

(define (compile-primcall x)
  ((assq-ref (car x) *primitives*) (cdr x)))

;; compile if

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

;; compile expressions

(define (compile-expr x)
  (cond
   ((imm? x) (compile-imm x))
   ((if? x) (compile-if x))
   ((cc? x) (cdr x))
   ((primcall? x) (compile-primcall x))
   (else (error "cannot compile expr" x))))

;; emit a program

(define emit display)
(define (emitln x)
  (emit x)
  (newline))

(define (emit-program x)
  (emitln "#include <stdio.h>

#define fxshift 1
#define fxmask 1
#define fxtag 1

#define cshift 4
#define cmask 15
#define ctag 10

#define t 22
#define f 6

#define null 14

typedef size_t scm;")
  (emitln "
scm scheme()
{")
  (emit "return ") (emit x) (emitln ";
}")
  (emitln "
void print_scheme(scm scheme_val)
{
if ((scheme_val & fxmask) == fxtag)
printf(\"%ld\", (long)scheme_val >> fxshift);
else if (scheme_val == t)
printf(\"#t\");
else if (scheme_val == f)
printf(\"#f\");
else if ((scheme_val & cmask) == ctag)
printf(\"#\\\\%c\", (char)(scheme_val >> cshift));
else if (scheme_val == null)
printf(\"'()\");
else
printf(\"#<unknown 0x%016zx>\", scheme_val);
printf(\"\\n\");
}

int main()
{
print_scheme(scheme());
return 0;
}"))

(emit-program (compile-expr '(if (not (null? ())) #\a #\b)))
