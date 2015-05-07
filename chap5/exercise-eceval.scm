(load "load-eceval.scm")

; start repl
(define the-global-environment (setup-environment))
(start eceval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example

;;; EC-Eval input:
;(define (append x y)
;  (if (null? x)
;      y
;      (cons (car x)
;	    (append (cdr x) y))))
;;; EC-Eval value:
;ok
;;; EC-Eval input:
;(append '(a b c) '(d e f))
;;; EC-Eval value:
;(a b c d e f)
