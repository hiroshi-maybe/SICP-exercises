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
; 120

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
; 1.4142156862745097

;;; Ex 5.4 (and 5.7)

; a
(define exp-rec-machine
  (make-machine
   '(b n val continue)
   (list (list '* *) (list '= =) (list '- -))
   '((assign continue (label exp-done))
     push-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign continue (label pop-loop))
     (assign n (op -) (reg n) (const 1))
     (goto (label push-loop))
     pop-loop
     (assign val (op *) (reg b) (reg val))
     (restore continue)
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     exp-done)))

(set-register-contents! exp-rec-machine 'b 2)
(set-register-contents! exp-rec-machine 'n 3)
(start exp-rec-machine)
(get-register-contents exp-rec-machine 'val)
; 8

; b

(define exp-iter-machine
  (make-machine
   '(b n val)
   (list (list '* *) (list '= =) (list '- -))
   '((assign val (const 1))
     iter-loop
     (test (op =) (reg n) (const 0))
     (branch (label exp-done))
     (assign n (op -) (reg n) (const 1))
     (assign val (op *) (reg b) (reg val))
     (goto (label iter-loop))
     exp-done)))

(set-register-contents! exp-iter-machine 'b 2)
(set-register-contents! exp-iter-machine 'n 3)
(start exp-iter-machine)
(get-register-contents exp-iter-machine 'val)
; 8

;;; Ex 5.8

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (let ((val (assoc next-inst labels)))
				(if val
				    (error "Duplicated label -- ASSEMBLE" next-inst)
				    (receive insts
					     (cons (make-label-entry next-inst
								     insts)
						   labels))))
			      (receive (cons (make-instruction next-inst)
					     insts)
				       labels)))))))

;(define dup-label-machine
;  (make-machine
;   '(a)
;   (list)
;   '(start
;     (goto (label here))
;     here
;     (assign a (const 3))
;     (goto (label there))
;     here
;     (assign a (const 4))
;     (goto (label there))
;     there)))

; (start dup-label-machine)
; (get-register-contents dup-label-machine 'a)
; 3 before modifying `extract-labels`

;;; Ex 5.9

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs
	 (map (lambda (e)
		(if (label-exp? e)
		    (error "Label cannot be put in operation operands -- MAKE-OPERATION-EXP" e)
		    (make-primitive-exp e machine labels)))
	      (operation-exp-operands exp))))
    (lambda ()
            (apply op (map (lambda (p) (p)) aprocs)))))

; Code to test

(define illegal-label-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
; label in operation
     (test (op =) (reg b) (const 0) (label gcd-done))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

