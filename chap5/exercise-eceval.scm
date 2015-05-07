(load "load-eceval.scm")

;;; Ex 5.23
#|
;;; EC-Eval input:
(define (cond-test x)                                                                                                                                                           (cond ((< x 10) 1)                                                                                                                                                                  ((= x 10) 2)                                                                                                                                                                  (else 3)))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(cond-test 5)

(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1

;;; EC-Eval input:
(cond-test 10)

(total-pushes = 27 maximum-depth = 8)
;;; EC-Eval value:
2

;;; EC-Eval input:
(cond-test 15)

(total-pushes = 27 maximum-depth = 8)
;;; EC-Eval value:
3
|#

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
