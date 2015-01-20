(load "ch4-leval.scm")

;;; Ex 4.25
; infinite loop by strict evaluation of recursive call on applicative-order Scheme. It works on normal-order language.

;;; Ex 4.27
; (define count 0)
; (define (id x)
;   (set! count (+ count 1))
;   x)
; (define w (id (id 10)))
;;; L-Eval input:
; count
;;; L-Eval value:
; 1    outer (id ..) is evaluated when (define w ..) is parsed
;;; L-Eval input:
; w
;;; L-Eval value: 
; 10
;;; L-Eval input:
; count
;;; L-Eval value:
; 2
; count is incremented by evaluating `w`

;;; Ex 4.29
;(define (four-times n)
;  (+ n n n n))
; (four-times (fib 30))
; memoized:   2 secs
; no memoize: 9 secs (fib 30) is calculated 4 times

;(define (square x)
;  (* x x))
;;; L-Eval input:
; (square (id 10))
;;; L-Eval value:
; memoized:   100
; no-memoize: 100
;;; L-Eval input:
; count
;;; L-Eval value:
; memoized:   1
; no-memoize: 2

;;; Ex 4.30

; b
; Original:
;   (p1 1) = (1 2)
;   (p2 1) = 1 because `e` in the `p` func is just a thunk: ('thunk '(set! x (cons x '(2))) env)
; Cy's:
;   (p1 1) = (1 2)
;   (p2 1) = (1 2) because `e` in the `p` func is forced

; d
; I prefer to provide options for programmers as below:
;  1: default lazy evaluation with purely functional programming language including strict evalution option
;  2: default strict evaluation including lazy evalution option

;;; Ex 4.31

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameter-names procedure)
           (list-of-delayed-args arguments (procedure-parameter-tags procedure) env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (procedure-parameter-names p)
  (map (lambda (arg-exp)
	 (if (pair? arg-exp) (car arg-exp) arg-exp))
       (cadr p)))

(define (procedure-parameter-tags p)
  (map (lambda (arg-exp)
	 (if (pair? arg-exp) (cadr arg-exp) '()))
       (cadr p)))

(define (list-of-delayed-args exps tags env)
  (if (no-operands? exps)
      '()
      (cons (may-delay-it (first-operand exps) (car tags) env)
	    (list-of-delayed-args (rest-operands exps) (cdr tags) env))))

(define (arg-tag arg-exp)
  (if (pair? arg-exp)
      (cadr arg-exp)
      '()))

(define (may-delay-it arg-exp tag env)
  (cond ((eq? tag 'lazy)      (naive-delay-it arg-exp env))
	((eq? tag 'lazy-memo) (delay-it       arg-exp env))
	(else (eval arg-exp env))))

(define (naive-delay-it exp env)
  (list 'naive-thunk exp env))

(define (force-it obj)
  (cond ((naive-thunk? obj) ; added naive thunk
	 (actual-value (thunk-exp obj) (thunk-env obj)))
	((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (naive-thunk? obj)
  (tagged-list? obj 'naive-thunk))

; Verified following works:
;(define (try a b))
;  (if (= a 0) 1 b))
;(try 0 100)
; 1
;(define (try-lazy a (b lazy))
;  (if (= a 0) 1 b))
;(try 0 (/ 1 0))
; 1
;(define (try-lazy a (b lazy-memo))
;  (if (= a 0) 1 b))
;(try 0 (/ 1 0))
; 1

(define the-global-environment (setup-environment))
(driver-loop)
