
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