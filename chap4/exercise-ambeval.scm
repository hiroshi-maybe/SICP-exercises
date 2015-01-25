;;; How to execute
; (load "exercise-ambeval.scm")

(load "ch4-ambeval.scm")

(define the-global-environment (setup-environment))

(define (my-eval exp)
  (ambeval exp the-global-environment (lambda (x y) 'ok) (lambda () 'ok)))

(define-syntax pre-eval
  (syntax-rules ()
    ((_ exp)
     (my-eval (quote exp)))))

(pre-eval
 (define (require p)
   (if (not p) (amb))))

;;; Ex 4.35
(pre-eval
 (define (an-integer-between low high)
   (require (<= low high))
   (amb low (an-integer-between (+ low 1) high))))

(pre-eval
 (define (a-pythagorean-triple-between low high)
   (let ((i (an-integer-between low high)))
     (let ((j (an-integer-between i high)))
       (let ((k (an-integer-between j high)))
	 (require (= (+ (* i i) (* j j)) (* k k)))
	 (list i j k))))))

;;; Amb-Eval input:
;(a-pythagorean-triple-between 1 20)

;;; Starting a new problem 
;;; Amb-Eval value:
;(3 4 5)
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;(5 12 13)

;;; Ex 4.36

(pre-eval
 (define (an-integer-starting-from n)
   (amb n (an-integer-starting-from (+ n 1)))))

(pre-eval
 (define (a-pythagorean-triple-from low)
   (let ((k (an-integer-starting-from low)))
     (let ((i (an-integer-between low k)))
       (let ((j (an-integer-between i k)))
	 (require (= (+ (* i i) (* j j)) (* k k)))
	 (list i j k))))))

;;; Ex 4.38

(pre-eval
 (define (distinct? items)
   (cond ((null? items) true)
	 ((null? (cdr items)) true)
	 ((member (car items) (cdr items)) false)
	 (else (distinct? (cdr items))))))

(pre-eval
 (define (multiple-dwelling)
   (let ((baker (amb 1 2 3 4 5))
	 (cooper (amb 1 2 3 4 5))
	 (fletcher (amb 1 2 3 4 5))
	 (miller (amb 1 2 3 4 5))
	 (smith (amb 1 2 3 4 5)))
     (require
      (distinct? (list baker cooper fletcher miller smith)))
     (require (not (= baker 5)))
     (require (not (= cooper 1)))
     (require (not (= fletcher 5)))
     (require (not (= fletcher 1)))
     (require (> miller cooper))
;     (require (not (= (abs (- smith fletcher)) 1)))
     (require (not (= (abs (- fletcher cooper)) 1)))
     (list (list 'baker baker)
	   (list 'cooper cooper)
	   (list 'fletcher fletcher)
	   (list 'miller miller)
	   (list 'smith smith)))))

;;; Amb-Eval input:
;(multiple-dwelling)

;;; Starting a new problem 
;;; Amb-Eval value:
;((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
;;; Amb-Eval input:
;try-again
;;; There are no more values of

;;; Ex 4.39

; Put expensive computation (distinct?) at the last condition
(pre-eval
 (define (multiple-dwelling)
   (let ((baker (amb 1 2 3 4 5))
	 (cooper (amb 1 2 3 4 5))
	 (fletcher (amb 1 2 3 4 5))
	 (miller (amb 1 2 3 4 5))
	 (smith (amb 1 2 3 4 5)))
     (require (not (= baker 5)))
     (require (not (= cooper 1)))
     (require (not (= fletcher 5)))
     (require (not (= fletcher 1)))
     (require (> miller cooper))
     (require (not (= (abs (- smith fletcher)) 1)))
     (require (not (= (abs (- fletcher cooper)) 1)))
     (require
      (distinct? (list baker cooper fletcher miller smith)))
     (list (list 'baker baker)
	   (list 'cooper cooper)
	   (list 'fletcher fletcher)
	   (list 'miller miller)
	   (list 'smith smith)))))

;;; START REPL
(driver-loop)