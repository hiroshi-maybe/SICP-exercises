(load "ch5-regsim.scm")

;;; Ex 5.2

;(define (factorial n)
;  (define (iter product counter)
;    (if (> counter n)
;	product
;	(iter (* counter product)
;	      (+ counter 1))))
;  (iter 1 1))

(define fact-machine
  (make-machine
   '(a b n)
   (list (list '* *) (list '+ +) (list '> >)) 
   '((assign a (const 1))
     (assign b (const 1))
     iter
     (test (op >) (reg b) (reg n))
     (branch (label fact-done))
     (assign a (op *) (reg b) (reg a))
     (assign b (op +) (reg b) (const 1))
     (goto (label iter))
     fact-done)))

(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(get-register-contents fact-machine 'a)

;;; Ex 5.3

;(define (sqrt x)
;  (sqrt-iter 1.0 x))
;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x)
;		 x)))
;(define (improve guess x)
;  (average guess (/ x guess)))
;(define (average x y)
;  (/ (+ x y) 2))
;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

(define sqrt-machine
  (make-machine
   '(x g t1 t2)
   (list (list '* *) (list '- -) (list '>= >=) (list '< <) (list '/ /) (list '+ +))
   '((assign g (const 1.0))
     iter
     ; good-enough?
     (assign t1 (op *) (reg g) (reg g))
     (assign t2 (op -) (reg t1) (reg x))
     ; abs
     (test (op >=) (reg t2) (const 0))
     (branch (label abs-good))
     (assign t2 (op *) (reg t2) (const -1))
     abs-good
     (test (op <) (reg t2) (const 0.001))
     (branch (label sqrt-done))
     ; improve
     (assign t1 (op /) (reg x) (reg g))
     ; average
     (assign t1 (op +) (reg g) (reg t1))
     (assign g (op /) (reg t1) (const 2))
     (goto (label iter))
     sqrt-done)))
 
(set-register-contents! sqrt-machine 'x 2)
(start sqrt-machine)
(get-register-contents sqrt-machine 'g)

