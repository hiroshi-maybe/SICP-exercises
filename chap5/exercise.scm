
;;; Ex 5.2

;(define (factorial n)
;  (define (iter product counter)
;    (if (> counter n)
;	product
;	(iter (* counter product)
;	      (+ counter 1))))
;  (iter 1 1))

(controller
 (assign a (const 1))
 (assign b (const 1))
 iter
 (test (op >) (reg b) (reg n))
 (branch (label fact-done))
 (assign a (op *) (reg b) (reg a))
 (assign b (op +) (reg b) (const 1))
 (goto (label iter))
 fact-done)
