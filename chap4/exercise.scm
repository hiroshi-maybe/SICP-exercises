
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

;;; Ex 4.8

(define (named-let? exp) (and (let? exp) (not (pair? (cadr exp)))))
(define (named-let-fun exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-params exp) (map car (named-let-bindings exp)))
(define (named-let-vals exp) (map cadr (named-let-bindings exp)))
(define (named-let-body exp) (cadddr exp))

(define (make-define name params body)
  (list 'define (cons name params) body))

(define (let->combination-ex exp)
  (if (named-let? exp)
      (sequence->exp
       (list (make-define (named-let-fun exp) (named-let-params exp) (named-let-body exp))
	     (cons (named-let-fun exp) (named-let-fun-vals exp))))
      (cons (make-lambda (let-bound-names exp)
			 (let-body exp))
	    (let-bound-values exp))))

;;; Ex 4.9

(define (while? exp) (tagged-list? expr 'while))
(define (while-cond exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (while->combination exp)
  (make-define 'while-iterate '()
	       (make-if (while-cond exp)
			(sequence->exp (cons (while-body exp)
					     (list 'while-iterate)))
			'done))
  (list 'while-iterate))

;;; Ex 4.11

(define (make-frame bindings) bindings)
(define (frame-bindings frame) frame)
(define (make-binding var val) (cons var val))
(define (bound-var binding) (car binding))
(define (bound-val binding) (cdr binding))
(define (add-binding-to-frame! var val frame)
  (set-car! (make-binding var val) (frame-bindings frame)))
(define (extend-environment bindings base-env)
  (cons (make-frame bindings) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (let ((binding (car bindings)))
	(cond ((null? bindings))
	      (env-loop (enclosing-environment env))
	      ((eq? var (bound-var binding))
	       (bound-val binding))
	      (else (scan (cdr bindings))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (bound-var (car bindings)))
             (set-car! bindings (make-binding var val)))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings)
             (add-binding-to-frame! var val frame))
            ((eq? var ((bound-var (car bindings)))
             (set-car! bindings (make-binding var val)))
            (else (scan (cdr bindings))))))
    (scan (frame-bindings frame))))

;;; Ex 4.12

(define (scan-frame var frame found-proc not-found-proc)
  (define (scan vars vals)
    (cond ((null? vars)
	   (not-found-proc vars vals frame))
	  ((eq? var (car vars))
	   (found-proc vars vals frame))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
	(frame-values frame)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame var frame
		(lambda (vars vals frame)
		  (set-car! vals val))
		(lambda (vars vals frame)
		  (add-binding-to-frame! var val frame)))))

(define (iterate-env env var found-proc)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
	(scan-frame var (first-frame env)
		found-proc
		(lambda (vars vals frame)
		  (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (lookup-variable-value var env)
  (iterate-env env var
	       (lambda (vars vals frame)
		 (car vals))))

(define (set-variable-value! var val env)
  (iterate-env env var
	       (lambda (vars vals frame)
		 (set-car! vals val))))

;;; Ex 4.13

; should unbind from first frame so that `unbind` should not break outer environment
(define (set-frame! frame env)
  (set-car! frame env))

(define (make-unbind! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals new-vars new-vals)
      (cond ((null? vars)
	     (error "Unbound variable" var)
	    ((eq? var (car vars))
             (set-frame! (make-frame (append new-vars (cdr vars))
				     (append new-vals (cdr vals))) env))
            (else (scan (cdr vars) 
			(cdr vals)
			(cons var new-vars)
			(cons val new-vals))))))
    (scan (frame-variables frame)
          (frame-values frame)
	  '() '())))

;;; Ex 4.15

(define (run-forever) (run-forever))

(define (try p)     ; x
  (if (halts? p p)
      (run-forever) ; y
      'halted))     ; z

; i)  (try try) halts
;    If (try try) is called by x
;       then (halts? try try) should be true
;            However, it runs forever by y
; ii) (try try) does NOT halt
;    If (try try) is called by x
;       then (halts? try try) should be false
;            However, it stops by z
;
; Therefore, inconsistent in any case. We cannot define universal `halts?`

