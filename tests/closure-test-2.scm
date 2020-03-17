(((lambda (a b c d e f) (lambda (x) (if x
					 (fx+ a (fx+ b (fx+ c (fx+ d (fx+ e f)))))
					 (fx- a (fx- b (fx- c (fx- d (fx- e f))))))))
  1 2 3 4 5 6)
 #f)
