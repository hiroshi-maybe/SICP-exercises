
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

;;; Ex 4.2

; b
(define (application-call? exp) (tagged-list? exp 'call))
(define (operator-call exp) (cadr exp))
(define (operands-call exp) (cddr exp))

;;; Ex 4.4

(define (and? exp) (tagged-list? exp 'and))
(define (or?  exp) (tagged-list? exp 'or))

; new special forms
(define (eval-4.4-1 exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
	((and? exp) (eval-and exp env))
	((or?   or) (eval-or  exp env))
;; other syntax procedures ;;
	))

(define (eval-and exp env)
  (define (expand-and operands)
    (cond ((null? operands) true)
	  ((true?  (eval (car operands) env)) (expand-and (cdr operands)))
	  (else false)))
  (expand-and (cdr exp) env))

(define (eval-or exp env)
  (define (expand-or operands)
    (cond ((null? operands) false)
	  ((true?  (eval (car operands) env)) true)
	  (else (expand-or (cdr operands)))))
  (expand-or (cdr exp) env))

; derived expressions
(define (eval-4.4-2 exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
	((and? exp) (eval-4.5 (and->if exp) env))
	((or?  exp) (eval-4.5 (or->if  exp) env))
;; other syntax procedures ;;
	))

(define (and-operands exp) (cdr exp))
(define (or-operands  exp) (cdr exp))

(define (and->if exp)
  (expand-and-operands (and-operands exp)))
(define (expand-and-operands operands)
  (if (null? operands)
      true
      (make-if (car operands)
	       (expand-and-operands (cdr operands))
	       false)))

(define (or->if exp)
  (expand-or-operands (or-operands exp)))
(define (expand-or-operands operands)
  (if (null? operands)
      false
      (make-if (car operands)
	       true
	       (expand-or-operands (cdr operands)))))

;;; Ex 4.5
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
		     (if (eq? (car (cond-actions first)) '=>)
			 (list (cadr (cond-actions first)) (cond-predicate first))
			 (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

;;; Ex 4.6
(define (eval-4.6 exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
	((let?  exp) (eval-4.6 (let->combination  exp) env))
;; other syntax procedures ;;
	))

(define (let-bindings exp) (cadr exp))
(define (let-bound-names exp)
  (map car (let-bindings exp)))
(define (let-bound-values exp)
  (map cadr (let-bindings exp)))
(define (let-body exp) (caddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-bound-names exp)
		     (let-body exp))
	(let-bound-values exp)))

;;; Ex 4.7

(define (make-let bindings body)
  (list 'let bindings body))

(define (let*-bindings exp) (cadr exp))
(define (let*-body exp) (caddr exp))
(define (let*->nested-lets exp)
  (let ((bindings (let*-bindings exp)))
    (define (iterate bindings)
      (if (null? bindings)
	  (let*-body exp)
	  (make-let (list (car bindings)) (iterate (cdr bindings)))))
    (iterate (let*-bindings))))

