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

; Code to test. Comment out to suppress error to run following exercises
;(define illegal-label-machine
;  (make-machine
;   '(a b t)
;   (list (list 'rem remainder) (list '= =))
;   '(test-b
;;;;;;;; label in operation
;     (test (op =) (reg b) (const 0) (label gcd-done))
;     (branch (label gcd-done))
;     (assign t (op rem) (reg a) (reg b))
;     (assign a (reg b))
;     (assign b (reg t))
;     (goto (label test-b))
;     gcd-done)))

;;; Ex 5.10

; New syntax `add <register A> <register B>`
; value of register A := value of register A + value of register B

(define (make-execution-procedure inst labels machine
				  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
	 (make-assign inst machine labels ops pc))
	((eq? (car inst) 'test)
	 (make-test inst machine labels ops flag pc))
	((eq? (car inst) 'branch)
	 (make-branch inst machine labels flag pc))
	((eq? (car inst) 'goto)
	 (make-goto inst machine labels pc))
	((eq? (car inst) 'save)
	 (make-save inst machine stack pc))
	((eq? (car inst) 'restore)
	 (make-restore inst machine stack pc))
	; New syntax `add`
	((eq? (car inst) 'add)
	 (make-add inst machine pc))
	((eq? (car inst) 'perform)
	 (make-perform inst machine labels ops pc))
	(else (error "Unknown instruction type -- ASSEMBLE"
		                          inst))))

(define (make-add inst machine pc)
  (let ((op1 (get-register machine (op1-reg-name inst)))
	(op2 (get-register machine (op2-reg-name inst))))
    (lambda ()
      (set-contents! op1 (+ (get-contents op1)
			    (get-contents op2)))
      (advance-pc pc))))

(define (op1-reg-name inst) (cadr (cadr inst)))
(define (op2-reg-name inst) (cadr (caddr inst)))

; machine to verify new syntax
(define over-ten-machine
  (make-machine
   '(a b)
   (list (list '> >))
   '(test
     (test (op >) (reg a) (const 10))
     (branch (label over-ten-done))
     (add (reg a) (reg b))
     (goto (label test))
     over-ten-done)))

(set-register-contents! over-ten-machine 'a 1)
(set-register-contents! over-ten-machine 'b 3)
(start over-ten-machine)
(get-register-contents over-ten-machine 'a)
; 13

;;; Ex 5.13

#|
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
;;; @Change START@ ;;;
	      (begin (allocate-register name)
		     (lookup-register name)))))
;;; @Change END@ ;;;
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      (else (error "Unknown request -- MACHINE" message))))
            dispatch)))

; Omit `register-names` and their allocation
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
;    (for-each (lambda (register-name)
;		((machine 'allocate-register) register-name))
;	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; test code
(define over-ten-machine-ex
  (make-machine
   (list (list '> >))
   '(test
     (test (op >) (reg a) (const 10))
     (branch (label over-ten-done))
     (add (reg a) (reg b))
     (goto (label test))
     over-ten-done)))

(set-register-contents! over-ten-machine-ex 'a 1)
(set-register-contents! over-ten-machine-ex 'b 5)
(start over-ten-machine-ex)
(get-register-contents over-ten-machine-ex 'a)
; 11
|#

;;; Ex 5.15, Ex 5.16

(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '())
	(instruction-counter 0) ; 5.15
	(trace false))
    (let ((the-ops
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (begin
		(if trace
		    (print-trace (car insts)))
		((instruction-execution-proc (car insts)))
		(set! instruction-counter (+ instruction-counter 1)) ; 5.15
		(execute)))))
      (define (print-instruction-counter)
	(newline)
	(display (list 'instruction-counter  '= instruction-counter))
	'print-done)
      (define (print-trace inst)
	(newline)
	(display (car inst)))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'instruction-counter) (print-instruction-counter)) ; 5.15
	      ((eq? message 'trace-on)  (set! trace true)) ; 5.16
	      ((eq? message 'trace-off) (set! trace false)) ; 5.16
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

; 5.15
(define (print-instruction-counter machine)
  (machine 'instruction-counter))

; 5.16
(define (enable-trace machine)
  (machine 'trace-on))
(define (disable-trace machine)
  (machine 'trace-off))

; test code
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(enable-trace gcd-machine) ; 5.16

(start gcd-machine)

; 5.16
;(test (op =) (reg b) (const 0))
;(branch (label gcd-done))
;(assign t (op rem) (reg a) (reg b))
;(assign a (reg b))
;(assign b (reg t))
;(goto (label test-b))
;(test (op =) (reg b) (const 0))
;(branch (label gcd-done))
;(assign t (op rem) (reg a) (reg b))
;(assign a (reg b))
;(assign b (reg t))
;(goto (label test-b))
;(test (op =) (reg b) (const 0))
;(branch (label gcd-done))
;(assign t (op rem) (reg a) (reg b))
;(assign a (reg b))
;(assign b (reg t))
;(goto (label test-b))
;(test (op =) (reg b) (const 0))
;(branch (label gcd-done))
;(assign t (op rem) (reg a) (reg b))
;(assign a (reg b))
;(assign b (reg t))
;(goto (label test-b))
;(test (op =) (reg b) (const 0))
;(branch (label gcd-done))

(get-register-contents gcd-machine 'a)
(print-instruction-counter gcd-machine) ; 5.15
; (instruction-counter = 26)



