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

(define the-global-environment (setup-environment))
(driver-loop)
