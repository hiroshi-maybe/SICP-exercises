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

;;; Ex 4.36
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



;;; START REPL
(driver-loop)