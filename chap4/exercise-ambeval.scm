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

;;; Ex 4.43
(pre-eval
 (define (yatch-puzzle-1)
   (let ((m (amb 'ma))
	 (c (amb 'l 'r 'g))
	 (h (amb 'l 'g))
	 (b (amb 'me))
	 (p (amb 'l 'r)))
     (require
      (distinct? (list m c h b p)))
     (require (or (and (eq? c 'g) (eq? p 'me))
		  (and (eq? h 'g) (eq? p 'r))))
     (list (list 'Moore m)
	   (list 'Colonel c)
	   (list 'Hall h)
	   (list 'Barnacle b)
	   (list 'Parker p)))))

;;; Starting a new problem 
;;; Amb-Eval value:
;((moore ma) (colonel l) (hall g) (barnacle me) (parker r))
;;; Amb-Eval input:
;try-again
;;; There are no more values of
;(yatch-puzzle-1)
; **** Lorna's father is 'Colonel Downing'

(pre-eval
 (define (yatch-puzzle-2)
   (let ((m (amb 'ma 'r 'g))
	 (c (amb 'l 'r 'g 'ma))
	 (h (amb 'l 'g 'ma))
	 (b (amb 'me))
	 (p (amb 'l 'r)))
     (require
      (distinct? (list m c h b p)))
     (require (or (and (eq? c 'g) (eq? p 'me))
		  (and (eq? h 'g) (eq? p 'r))
		  (and (eq? m 'g) (eq? p 'l))))
     (list (list 'Moore m)
	   (list 'Collad c)
	   (list 'Hall h)
	   (list 'Barnacle b)
	   (list 'Parker p)))))


;;; Starting a new problem 
;;; Amb-Eval value:
;((moore ma) (collad l) (hall g) (barnacle me) (parker r))
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;((moore g) (collad r) (hall ma) (barnacle me) (parker l))
;;; Amb-Eval input:
;try-again
;;; There are no more values of
;(yatch-puzzle-2)
; **** 2 patterns

;;; Ex 4.44

; Improved efficiency by validating sub block and reuse the result
; Validation history in 4-queens: (1)(3 1)(4 1)(2 4 1)(2)(4 2)(1 4 2)(3 1 4 2)
(pre-eval
 (define (replicate make-val num)
   (if (= num 0)
       '()
       (cons (make-val) (replicate make-val (- num 1))))))

(pre-eval
 (define (valid? board-state)
   (define (valid-rest? origin-row offset rows)
     (if (null? rows)
	 true
	 (let ((tested-row (car rows))
	       (invalid-neg (- origin-row offset))
	       (invalid-pos (+ origin-row offset)))
	   (if (and (not (= invalid-neg tested-row))
		    (not (= invalid-pos tested-row))
		    (not (= origin-row tested-row)))
	       (valid-rest? origin-row (+ offset 1) (cdr rows))
	       false))))
   (let ((current-row (car board-state)))
     (valid-rest? current-row 1 (cdr board-state)))))

(pre-eval
 (define (queens board-size)
   (define (iter solution n-left)
     (if (= n-left 0)
	 solution
	 (begin
	   (let ((solution-ex (cons (an-integer-between 1 board-size) solution)))
	     (require (valid? solution-ex))
	     (iter solution-ex (- n-left 1))))))
   (iter '() board-size)))

;;; Amb-Eval input:
;(queens 8)        
;;; Starting a new problem 
;;; Amb-Eval value:
;(4 2 7 3 6 8 5 1)
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;(5 2 4 7 3 8 6 1)
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;(3 5 2 8 6 4 7 1)
;...

;;; Ex 4.51

(pre-eval
 (define count 0))
(pre-eval
 (define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items)))))
(pre-eval
 (define (counter)
   (let ((x (an-element-of '(a b c)))
	 (y (an-element-of '(a b c))))
     (set! count (+ count 1))
     (require (not (eq? x y)))
     (list x y count))))

;**** normal assignment ****;
;;; Amb-Eval input:
;(counter)
;;; Starting a new problem 
;;; Amb-Eval value:
;(a b 1)
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;(a c 1)

(define (perm-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-perm-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
	       (set-variable-value! var val env)
	       (succeed 'ok fail2))
             fail))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((perm-assignment? exp) (analyze-perm-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(pre-eval
 (define (counter-perm)
   (let ((x (an-element-of '(a b c)))
	 (y (an-element-of '(a b c))))
     (permanent-set! count (+ count 1))
     (require (not (eq? x y)))
     (list x y count))))

;;; Amb-Eval input:
;(counter-perm)
;;; Starting a new problem 
;;; Amb-Eval value:
;(a b 2)
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;(a c 3)

;;; Ex 4.52
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-consequent exp)
  (cadr exp))
(define (if-fail-alternative exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((cproc (analyze (if-fail-consequent exp)))
	(aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (cproc env
             (lambda (val fail2)
	       (succeed val fail2))
             (lambda ()
	       (aproc env
		      (lambda (val fail2)
			(succeed val fail2))
		      fail))))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((perm-assignment? exp) (analyze-perm-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(pre-eval
 (define (will-fail)
   (if-fail (let ((x (an-element-of '(1 3 5))))
	      (require (even? x))
	      x)
	    'all-odd)))
;;; Amb-Eval input:
;(will-fail)
;;; Starting a new problem 
;;; Amb-Eval value:
; all-odd

(pre-eval
 (define (wont-fail)
   (if-fail (let ((x (an-element-of '(1 3 5 8))))
	      (require (even? x))
	      x)
	    'all-odd)))
;;; Amb-Eval input:
;(wont-fail)
;;; Starting a new problem 
;;; Amb-Eval value:
; 8

;;; Ex 4.53
(pre-eval
 (define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b))))

;;; Amb-Eval input:
;(let ((pairs '()))
;  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
;             (permanent-set! pairs (cons p pairs))
;             (amb))
;           pairs))
;;; Starting a new problem 
;;; Amb-Eval value:
;((8 35) (3 110) (3 20))

;;; Ex 4.54

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (not (pred-value))
		   (fail2)
		   (succeed 'ok fail2)))
             fail))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((perm-assignment? exp) (analyze-perm-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((require? exp) (analyze-require exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;; Amb-Eval input:
;(an-integer-between 1 2)
;;; Starting a new problem 
;;; Amb-Eval value:
;1
;;; Amb-Eval input:
;try-again
;;; Amb-Eval value:
;2
;;; Amb-Eval input:
;try-again
;;; There are no more values of
;(an-integer-between 1 2)

;;; START REPL
(driver-loop)