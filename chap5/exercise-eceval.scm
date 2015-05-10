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

;;; Ex 5.26, 5.27

#|

;;;;;;;; naiive recursive call ;;;;;;;;

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;;; EC-Eval input:
(factorial 5)
(total-pushes = 144 maximum-depth = 28)

#: total-pushes | maximum-depth
--------------------------------
1: 16  | 8
2: 48  | 13
3: 80  | 18
4: 112 | 23
5: 144 | 28

; b
<total-pushes> = 32 * <n> - 16

;;;;;;;; tail recursive ;;;;;;;;

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

#: total-pushes | maximum-depth
--------------------------------
1: 64  | 10
2: 99  | 10
3: 134 | 10
4: 169 | 10
5: 204 | 10

; b
<total-pushes> = 35 * <n> + 29

;;;;;;; summary ;;;;;;;;

#        : total-pushes  | maximum-depth
----------------------------------------
rec      : 32 * <n> - 16 | 5 * <n> + 3
tail rec : 35 * <n> + 29 | 10 (constant)

|#

;;; Ex 5.33

#|
;;; EC-Eval input:
(define (add a b) (+ a b))

;;; EC-Eval input:
(add 1 2)

;;;;;;; before change ;;;;;;;;
(total-pushes = 16 maximum-depth = 5)

;;;;;;;  after change ;;;;;;;;
(total-pushes = 12 maximum-depth = 5)

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
