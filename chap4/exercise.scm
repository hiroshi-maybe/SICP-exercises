
(load "ch4.scm")

;;; Ex 4.1

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
	(let ((rest (list-of-values-lr (rest-operands exps) env)))
	  (cons left rest)))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-rl (rest-operands exps) env)))
	(let ((left (eval (first-operand exps) env)))
	  (cons right left)))))
