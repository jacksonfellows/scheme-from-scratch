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
  (or (fixnum? x) (boolean? x) (char? x)))

(define (const? x) (or (imm? x) (null? x) (string? x)))

(define (compile-imm x)
  (cond
   ((fixnum? x) (+ fxtag (lsh x fxshift)))
   ((boolean? x) (if x t f))
   ((char? x) (+ ctag (lsh x cshift)))))

(define (compile-null)
  null)

;; compile strings

(define (compile-string x)
  (list "allocstring(\"" x "\"," (string-length x) ")"))

;; compile symbols

(define (compile-symbol x)
  (let ((s (symbol->string x)))
    (list "allocsymbol(\"" s "\"," (string-length s) ")")))

;; compile pairs

(define (compile-quoted-pair x)
  (cond
   ((null? x) (compile-null))
   ((pair? x)
    (list "cons" (list (compile-quoted (car x))
		       ","
		       (compile-quoted-pair (cdr x)))))
   (else (compile-quoted x))))

;; define primitive procedures
;; TODO: primitive procedures should receive their arguments already evaluated

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

(make-unary-primitive 'null? (lambda (x env) (to-bool (binop x '== ''() env))))
(make-unary-primitive 'not (lambda (x env) (to-bool (binop x '== #f env))))
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

(make-unary-primitive 'string-length
		      (lambda (x env)
			(to-fixnum
			 (cc (list (list "(block*)" (compile-expr x env)) "->header >> headershift"))
			 env)))

(make-binary-primitive 'eq? (compose to-bool (pure-binop '==)))
(make-binary-primitive 'eqv? (compose to-bool (pure-binop '==)))

(define (func f)
  (lambda (args env) (list f (intercalate "," (map (lambda (arg) (compile-expr arg env)) args)))))

(make-primitive 'car (func "CAR"))
(make-primitive 'cdr (func "CDR"))

(make-primitive 'cons (func "cons"))

(make-unary-primitive 'make-vector (lambda (x env)
				     (list "allocvector(" x ")")))

(make-binary-primitive 'vector-ref (lambda (v i env)
				     (list "((block*)" (compile-expr v env) ")->data[" i "]")))

;; TODO: deal with n-ary arguments
;; TODO: should not be a primitive
(make-unary-primitive 'vector (lambda (x env)
				(let ((tmp (new-tmp)))
				  (intercalate
				   ","
				   (list
				    (list tmp "=" (compile-expr (list 'make-vector 1) env))
				    (compile-expr (list 'set! (list 'vector-ref (cc tmp) 0) x) env)
				    tmp)))))

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

;; assignment

(define set!? (tagged-pair? 'set!))

(define (compile-set! x env)
  (binop (cadr x) '= (caddr x) env))

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
	(body (cddr x)))
    (let ((lambda-name (uniq-var "l"))
	  (formal-pairs (map (lambda (f) (cons f (uniq-var "v"))) formals)))
      (let ((new-env (extend env formal-pairs)))
	(set! *lambdas* (cons
			 (list lambda-name (map cdr formal-pairs) (compile-begin-expr body new-env))
			 *lambdas*))
	lambda-name))))

(define closure? (tagged-pair? 'closure))

(define *tmps* '())

(define (new-tmp)
  (let ((tmp (string->symbol (uniq-var "t"))))
    (set! *tmps* (cons tmp *tmps*))
    tmp))

(define (compile-closure x env)
  (let ((l (cadr x))
	(fvs (caddr x)))
    (let ((lambda-name (compile-lambda l env)))
      (let ((alloc-expr (string-append "allocclosure(&" lambda-name "," (number->string (length fvs)) ")"))
	    (tmp (new-tmp)))
	(if (= 0 (length fvs))
	    alloc-expr
	    (intercalate
	     ","
	     (append (list (list tmp "=" alloc-expr))
		     (append (enumerate (lambda (i fv) (binop (list 'env-get (cc tmp) i) '= fv env)) fvs)
			     (list tmp)))))))))

(define env-get? (tagged-pair? 'env-get))

(define (compile-env-get x env)
  (let ((env-var (cadr x))
	(env-i (caddr x)))
    (list (list "(block*)" (compile-expr env-var env)) "->data[" (+ env-i 1) "]")))

;; begin

(define begin? (tagged-pair? 'begin))

(define (compile-begin x env)
  (compile-begin-expr (cdr x) env))

(define (compile-begin-expr x env)
  (intercalate "," (map (lambda (a) (compile-expr a env)) x)))

;; compile function application

(define app? pair?)

(define (compile-app x env)
  (let ((tmp (new-tmp)))
    (let ((proc (compile-expr (car x) env))
	  (args (cons tmp (map (lambda (arg) (compile-expr arg env)) (cdr x)))))
      (intercalate "," (list (list tmp "=" proc)
			     (list (list (list "scm(*)" (intercalate "," (map (const "scm") args)))
					 (list (list "(block*)" (list tmp)) "->data[0]"))
				   (intercalate "," args)))))))

;; quote

(define quote? (tagged-pair? 'quote))

(define (compile-quote x)
  (compile-quoted (cadr x)))

(define (compile-quoted x)
  (cond
   ((imm? x) (compile-imm x))
   ((null? x) (compile-null x))
   ((symbol? x) (compile-symbol x))
   ((string? x) (compile-string x))
   ((pair? x) (compile-quoted-pair x))
   (else (error "cannot quote" x))))

;; compile expressions

(define (compile-expr x env)
  (cond
   ;; constants
   ((cc? x) (cdr x))
   ((imm? x) (compile-imm x))
   ((null? x) (error "missing procedure"))
   ((string? x) (compile-string x))

   ;; variables
   ((var? x) (lookup x env))

   ;; special forms
   ((quote? x) (compile-quote x))
   ((set!? x) (compile-set! x env))
   ((if? x) (compile-if x env))
   ((begin? x) (compile-begin x env))

   ;; closures
   ((closure? x) (compile-closure x env))
   ((env-get? x) (compile-env-get x env))

   ;; primitive calls
   ((primcall? x) (compile-primcall x env))

   ;; function application
   ((app? x) (compile-app x env))

   (else (error "cannot compile expr" x))))

;; Closure conversion, following the approach outlined in
;; http://matt.might.net/articles/compiling-scheme-to-c/

(define (free-vars x)
  (cond
   ;; constants
   ((const? x) '())

   ;; variables
   ((var? x) (list x))

   ;; special forms
   ((quote? x) '())
   ((set!? x) (set-union (free-vars (cadr x)) (free-vars (caddr x))))
   ((if? x) (reduce set-union (map free-vars (cdr x)) '()))
   ((begin? x) (reduce set-union (map free-vars (cdr x)) '()))
   ((lambda? x) (set-difference (reduce set-union (map free-vars (cddr x)) '())
				(list->set (cadr x))))

   ;; closures
   ((closure? x) (set-union (free-vars (cadr x)) (free-vars (caddr x))))
   ((env-get? x) '())

   ;; primitive calls
   ((primcall? x) (reduce set-union (map free-vars (cdr x)) '()))

   ;; function application
   ((app? x) (reduce set-union (map free-vars x) '()))

   (else (error "cannot find free vars of expr" x))))

(define (sub-vars x dict)
  (define (subber e) (sub-vars e dict))
  (cond
   ;; constants
   ((const? x) x)

   ;; variables
   ((var? x) (let ((sub (assq-ref x dict)))
	       (if sub sub x)))

   ;; special forms
   ((quote? x) x)
   ((set!? x) (cons 'set! (map subber (cdr x))))
   ((if? x) (cons 'if (map subber (cdr x))))
   ((begin? x) (cons 'begin (map subber (cdr x))))
   ((lambda? x) (append (list 'lambda (cadr x)) (map subber (cddr x))))

   ;; closures
   ((closure? x) (list 'closure (sub-vars (cadr x) dict) (map subber (caddr x))))
   ((env-get? x) x)

   ;; primitive calls
   ((primcall? x) (cons (car x) (map subber (cdr x))))

   ;; function application
   ((app? x) (map subber x))

   (else (error "cannot substitute variables in expr" x))))

(define (closure-convert x)
  (cond
   ;; constants
   ((const? x) x)

   ;; variables
   ((var? x) x)

   ;; special forms
   ((quote? x) x)
   ((set!? x) (list 'set! (cadr x) (closure-convert (caddr x))))
   ((if? x) (cons 'if (map closure-convert (cdr x))))
   ((begin? x) (cons 'begin (map closure-convert (cdr x))))
   ((lambda? x)
    (let ((formals (cadr x))
	  (body (map closure-convert (cddr x))))
      (let ((fvs (set-difference (free-vars body) (free-vars formals))))
	(let ((closure-env (string->symbol (uniq-var "e"))))
	  (let ((dict (enumerate (lambda (i fv) (cons fv (list 'env-get closure-env i))) fvs)))
	    (list 'closure
		  (append (list 'lambda (cons closure-env formals)) (map (lambda (e) (sub-vars e dict)) body))
		  fvs))))))

   ;; primitive calls
   ((primcall? x) (cons (car x) (map closure-convert (cdr x))))

   ;; function application
   ((app? x) (map closure-convert x))

   (else (error "cannot closure convert expr" x))))

;; Convert mutable variables

(define (mutated-vars x)
  (cond
   ;; constants
   ((const? x) '())

   ;; variables
   ((var? x) '())

   ;; special forms
   ((quote? x) '())
   ((set!? x) (list (cadr x)))
   ((if? x) (reduce set-union (map mutated-vars (cdr x)) '()))
   ((begin? x) (reduce set-union (map mutated-vars (cdr x)) '()))
   ((lambda? x) (set-difference (reduce set-union (map mutated-vars (cddr x)) '())
				(list->set (cadr x))))

   ;; primitive calls
   ((primcall? x) (reduce set-union (map mutated-vars (cdr x)) '()))

   ;; function application
   ((app? x) (reduce set-union (map mutated-vars x) '()))

   (else (error "cannot find mutated vars in expr" x))))

(define (convert-mutable-vars x)
  (cond
   ;; constants
   ((const? x) x)

   ;; variables
   ((var? x) x)

   ;; special forms
   ((quote? x) x)
   ((set!? x) (list 'set! (cadr x) (convert-mutable-vars (caddr x))))
   ((if? x) (cons 'if (map convert-mutable-vars (cdr x))))
   ((begin? x) (cons 'begin (map convert-mutable-vars (cdr x))))
   ((lambda? x)
    (let ((formals (cadr x))
	  (body (map convert-mutable-vars (cddr x))))
      (let ((mvs (set-intersection (mutated-vars body) formals)))
	(cons 'lambda
	      (cons formals
		    (append
		     (map (lambda (mv) (list 'set! mv (list 'vector mv))) mvs)
		     (sub-vars body (map (lambda (mv) (cons mv (list 'vector-ref mv 0))) mvs))))))))

   ;; primitive calls
   ((primcall? x) (cons (car x) (map convert-mutable-vars (cdr x))))

   ;; function application
   ((app? x) (map convert-mutable-vars x))

   (else (error "cannot convert mutable vars in expr" x))))

;; Remove syntactic sugar

(define let? (tagged-pair? 'let))

(define (let->lambda x)
  (let ((vars (map car (cadr x)))
	(vals (map cadr (cadr x)))
	(body (map desugar (cddr x))))
    (append (list (append (list 'lambda vars) body)) vals)))

(define (desugar x)
  (cond
   ;; constants
   ((const? x) x)

   ;; variables
   ((var? x) x)

   ;; special forms
   ((quote? x) x)
   ((set!? x) (list 'set! (cadr x) (desugar (caddr x))))
   ((if? x) (cons 'if (map desugar (cdr x))))
   ((begin? x) (cons 'begin (map desugar (cdr x))))
   ((lambda? x) (append (list 'lambda (cadr x)) (map desugar (cddr x))))

   ;; sugar
   ((let? x) (desugar (let->lambda x)))

   ;; primitive calls
   ((primcall? x) (cons (car x) (map desugar (cdr x))))

   ;; function application
   ((app? x) (map desugar x))

   (else (error "cannot desugar" x))))

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

(define (emit-function-declaration name args)
  (emit "scm ")
  (emit name)
  (emit (intercalate "," (map (const "scm") args)))
  (emitln ";"))

(define (emit-function name args expr)
  (emit "
scm ")
  (emit name)
  (emit "(") (emit-args args) (emitln ")
{")
  (emit "return ") (emit expr) (emitln ";
}"))

(define (emit-program x)
  (emitln "#include \"runtime.h\"\n")

  (for-each (lambda (tmp) (emit "scm ") (emit tmp) (emitln ";")) *tmps*)
  (emitln "")

  (for-each (lambda (l) (emit-function-declaration (car l) (cadr l))) *lambdas*)

  (for-each (lambda (l) (apply emit-function l)) *lambdas*)

  (emit-function 'scheme '() x)

  (emitln "
int main()
{
print_scm_val(scheme());
return 0;
}"))

(define (compile x)
  (emit-program (compile-expr (closure-convert (convert-mutable-vars (desugar x))) (empty-env))))

(define (main args)
  (if (not (= (length args) 1))
      (error "wrong # of command line arguments"))
  (let ((o (open-input-file (car args))))
    (compile (read o))
    (close-port o)))
