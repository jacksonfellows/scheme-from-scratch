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

(define (binop r op l env) (list (compile-expr r env) op (compile-expr l env)))

(define (make-unary-primitive name expander)
  (make-primitive name (lambda (args env) (expander (car args) env))))

(make-unary-primitive 'fxadd1 (lambda (x env) (binop x '+ (cc (lsh 1 fxshift)) env)))
(make-unary-primitive 'fxsub1 (lambda (x env) (binop x '- (cc (lsh 1 fxshift)) env)))

;; only works because the high bit of ctag is 1
(make-unary-primitive 'char->fixnum (lambda (x env) (binop x '>> (cc (- cshift fxshift)) env)))

;; have to subtract 0b1000 because fxtag is already set
(make-unary-primitive 'fixnum->char (lambda (x env)
				      (binop (cc (binop x '<< (cc (- cshift fxshift)) env))
					     '+
					     (cc (- ctag (lsh fxtag 3)))
					     env)))

(define (to-bool x) (list (list x '<< bshift) '+ btag))

(make-unary-primitive 'null? (lambda (x env) (to-bool (binop x '== (cc null) env))))
(make-unary-primitive 'not (lambda (x env) (to-bool (binop x '== (cc f) env))))
(make-unary-primitive 'fxzero? (lambda (x env) (to-bool (binop x '== 0 env))))

(define (tagged? mask tag) (lambda (x env) (to-bool (binop (cc (binop x '& (cc mask) env))
							   '==
							   (cc tag)
							   env))))

(make-unary-primitive 'fixnum? (tagged? fxmask fxtag))
(make-unary-primitive 'boolean? (tagged? bmask btag))
(make-unary-primitive 'char? (tagged? cmask ctag))

(define (make-binary-primitive name expander)
  (make-primitive name (lambda (args env) (expander (car args) (cadr args) env))))

(define (pure-binop op) (lambda (x y env) (binop x op y env)))

(make-binary-primitive 'fx+ (lambda (x y env) (binop x '+ (cc (binop y '- (cc fxtag) env)) env)))
(make-binary-primitive 'fx- (lambda (x y env) (binop (cc (binop x '- y env)) '+ (cc fxtag) env)))

(define (from-fixnum x env) (binop x '>> (cc fxshift) env))
(define (to-fixnum x env) (binop (cc (binop x '<< (cc fxshift) env)) '+ (cc fxtag) env))

(make-binary-primitive 'fx* (lambda (x y env) (to-fixnum (cc (binop (cc (from-fixnum x))
								    '*
								    (cc (from-fixnum y))
								    env)))))

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

(define (compile-primcall x env)
  ((assq-ref (car x) *primitives*) (cdr x) env))

;; compile if

(define (tagged-pair? tag)
  (lambda (x)
    (and (pair? x) (eq? tag (car x)))))

(define (from-bool x env)
  (binop x '!= (cc f) env))

(define if? (tagged-pair? 'if))

(define (compile-if x env)
  (list (from-bool (cadr x) env)
	'? (compile-expr (caddr x) env)
	': (compile-expr (cadddr x) env)))

;; C constant
(define (cc x)
  (list 'cc x))
(define cc? (tagged-pair? 'cc))

;; variables

(define var? symbol?)

(define (empty-env) '(() . ()))

(define (lookup var env)
  (let ((id (assq-ref var env)))
    (if id
	id
	(error var "is unbound"))))

(define extend append)

;; compile lambdas

(define uniq-id
  (let ((n 0))
    (lambda () (set! n (+ 1 n)) n)))

(define (uniq-var prefix)
  (string-append prefix (number->string (uniq-id))))

(define *lambdas* '())

(define lambda? (tagged-pair? 'lambda))

(define (compile-lambda x env)
  (let ((formals (cadr x))
	(body (caddr x)))
    (let ((lambda-name (uniq-var "l"))
	  (formal-pairs (map (lambda (f) (cons f (uniq-var "v"))) formals)))
      (let ((new-env (extend env formal-pairs)))
	(set! *lambdas* (cons
			 (list lambda-name (map cdr formal-pairs) (compile-expr body new-env))
			 *lambdas*))
	(string-append "allocclosure(&" lambda-name ")")))))

;; compile function application

(define app? pair?)

(define (compile-app x env)
  (let ((proc (compile-expr (car x) env))
	(args (map (lambda (arg) (compile-expr arg env)) (cdr x))))
    (list (list (list "scm(*)" (intercalate "," (map (const "scm") args)))
		(list (list "(block*)" (list proc)) "->data[0]"))
	  (intercalate "," args))))

;; compile expressions

(define (compile-expr x env)
  (cond
   ((cc? x) (cdr x))
   ((imm? x) (compile-imm x))
   ((var? x) (lookup x env))
   ((if? x) (compile-if x env))
   ((lambda? x) (compile-lambda x env))
   ((primcall? x) (compile-primcall x env))
   ((app? x) (compile-app x env))
   (else (error "cannot compile expr" x))))

;; emit a program

(define (emit x)
  (if (pair? x)
      (begin (display "(") (for-each emit x) (display ")"))
      (display x)))

(define (emitln x)
  (emit x)
  (newline))

;; TODO: use intercalate
(define (emit-args args)
  (if (not (null? args))
      (begin
	(emit "scm ") (emit (car args))
	(if (not (null? (cdr args)))
	    (for-each (lambda (arg)
			(emit ", scm ") (emit arg))
		      (cdr args))))))

(define (emit-function name args expr)
  (emit "
scm ")
  (emit name)
  (emit "(") (emit-args args) (emitln ")
{")
  (emit "return ") (emit expr) (emitln ";
}"))

(define (emit-program x)
  (emitln "#include \"runtime.h\"")

  (for-each (lambda (l) (apply emit-function l)) *lambdas*)

  (emit-function 'scheme '() x)

  (emitln "
int main()
{
print_scm_val(scheme());
return 0;
}"))

(define (main args)
  (if (not (= (length args) 1))
      (error "wrong # of command line arguments"))
  (let ((o (open-input-file (car args))))
    (emit-program (compile-expr (read o) (empty-env)))
    (close-port o)))
