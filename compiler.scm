(load "stdlib.scm")

(define nshift 2)
(define t 111) ;; 0x6F, 0b01101111
(define f 47) ;; 0x2F, 0b00101111
(define cshift 8)
(define ctag 15) ;; 0x0F, 0b00001111
(define null 63) ;; 0x3F, 0b00111111

(define emit display)
(define (emitln x)
  (display x)
  (newline))

(define (emit-expr x)
  (cond
   ((number? x) (emit (lsh x nshift)))
   ((boolean? x) (emit (if x t f)))
   ((char? x) (emit (+ ctag (lsh x cshift))))
   ((null? x) (emit null))
   ))

(define (emit-program x)
  (emitln "#include <stdio.h>

#define nshift 2
#define nmask 0x03
#define ntag 0x00

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
if ((scheme_val & nmask) == ntag)
printf(\"%d\", (int)(scheme_val >> nshift));
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

(emit-program '())
