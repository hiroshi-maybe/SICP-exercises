(load "load-eceval-compiler.scm")

;;; Ex 5.45

;;;;;;;; naiive recursive call ;;;;;;;;

(compile-and-go
 '(define (factorial n)
    (if (= n 1) 1
	(* (factorial (- n 1)) n))))

;;;;;;;; tail recursive ;;;;;;;;

(compile-and-go
 '(define (factorial-t n)
    (define (iter product counter)
      (if (> counter n)
	  product
	  (iter (* counter product)
		(+ counter 1))))
    (iter 1 1)))


