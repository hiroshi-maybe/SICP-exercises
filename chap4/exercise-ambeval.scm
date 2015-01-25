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

; Expensive computation (distinct?) later
; Restrictive condition (> miller cooper) earlier
(pre-eval
 (define (multiple-dwelling)
   (let ((baker (amb 1 2 3 4 5))
	 (cooper (amb 1 2 3 4 5))
	 (fletcher (amb 1 2 3 4 5))
	 (miller (amb 1 2 3 4 5))
	 (smith (amb 1 2 3 4 5)))
     (require (> miller cooper))
     (require (not (= baker 5)))
     (require (not (= cooper 1)))
     (require (not (= fletcher 5)))
     (require (not (= fletcher 1)))
     (require (not (= (abs (- smith fletcher)) 1)))
     (require (not (= (abs (- fletcher cooper)) 1)))
     (require
      (distinct? (list baker cooper fletcher miller smith)))
     (list (list 'baker baker)
	   (list 'cooper cooper)
	   (list 'fletcher fletcher)
	   (list 'miller miller)
	   (list 'smith smith)))))

;;; Ex 4.40
(pre-eval
 (define (multiple-dwelling)
   (let ((baker (amb 1 2 3 4))
	 (cooper (amb 2 3 4 5))
	 (fletcher (amb 2 3 4))
	 (smith (amb 1 2 3 4 5)))
     (let ((miller (an-integer-between (+ cooper 1) 5)))
       (require (not (= (abs (- smith fletcher)) 1)))
       (require (not (= (abs (- fletcher cooper)) 1)))
       (require
	(distinct? (list baker cooper fletcher miller smith)))
       (list (list 'baker baker)
	     (list 'cooper cooper)
	     (list 'fletcher fletcher)
	     (list 'miller miller)
	     (list 'smith smith))))))

;;; Ex 4.41

(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

(define (between a b)
  (if (> a b)
      '()
      (cons a (between (+ a 1) b))))

(define (flatten x)
  (reduce append '() x))

(define (flatmap proc xs)
  (flatten (map proc xs)))

(define (permutation lists)
  (if (null? lists)
      '(())
      (flatmap (lambda (x)
		 (map (lambda (y)
			(cons x y))
		      (permutation (cdr lists))))
	       (car lists))))

(define (get-baker x) (list-ref x 0))
(define (get-cooper x) (list-ref x 1))
(define (get-miller x) (list-ref x 2))
(define (get-fletcher x) (list-ref x 3))
(define (get-smith x) (list-ref x 4))

(define (pretty-print set)
  (map (lambda (s)
	 (list (list 'baker (get-baker s))
	       (list 'cooper (get-cooper s))
	       (list 'fletcher (get-fletcher s))
	       (list 'miller (get-miller s))
	       (list 'smith (get-smith s))))
	 set))

(define (multiple-dwelling)
  (let ((baker (between 1 4))
	(cooper (between 2 5))
	(miller (between 3 5))
	(fletcher (between 2 4))
	(smith (between 1 5)))
    (filter (lambda (ls)
	      (let ((b (get-baker ls))
		    (c (get-cooper ls))
		    (m (get-miller ls))
		    (f (get-fletcher ls))
		    (s (get-smith ls)))
		(and (> m c)
		     (not (= (abs (- s f)) 1))
		     (not (= (abs (- f c)) 1))
		     (distinct? ls))))
	    (permutation (list baker cooper miller fletcher smith)))))

(pretty-print (multiple-dwelling))

;;; Ex 4.42
(pre-eval
 (define (xor p1 p2)
   (or (and (not p1) p2) (and p1 (not p2)))))

(pre-eval
 (define (liars-puzzle)
   (let ((b (amb 1 2 3 4 5))
	 (e (amb 1 2 3 4 5))
	 (j (amb 1 2 3 4 5))
	 (k (amb 1 2 3 4 5))
	 (m (amb 1 2 3 4 5)))
     (require
      (distinct? (list b e j k m)))
     (require (xor (= k 2) (= b 3)))
     (require (xor (= e 1) (= j 2)))
     (require (xor (= j 3) (= e 5)))
     (require (xor (= k 2) (= m 4)))
     (require (xor (= m 4) (= b 1)))
     (list (list 'Betty b)
	   (list 'Ethel e)
	   (list 'Joan j)
	   (list 'Kitty k)
	   (list 'Mary m)))))

;;; START REPL
(driver-loop)