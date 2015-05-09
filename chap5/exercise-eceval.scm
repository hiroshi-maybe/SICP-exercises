(load "load-eceval.scm")

;;; Ex 5.23
#|
;;; EC-Eval input:
(define (cond-test x)
  (cond ((< x 10) 1)
	((= x 10) 2)
	(else 3)))

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

;;; Ex 5.26

;;;;;;;; naiive recursive call
#|
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;;; EC-Eval input:
(factorial 5)
(total-pushes = 144 maximum-depth = 28)
1: 16
2: 48
3: 80
4: 112
5: 144

; b
<total-pushes> = 32 * <n> - 16

|#

;;;;;;;; tail recursive
#|
(define (factorial-t n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))

;;; EC-Eval input:
(factorial-t 5)
(total-pushes = 204 maximum-depth = 10)
1: 64
2: 99
3: 134
4: 169
5: 204

; b
<total-pushes> = 35 * <n> + 29

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
