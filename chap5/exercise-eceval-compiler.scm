(load "load-eceval-compiler.scm")

;;; Ex 5.45

; a

#|
(compile-and-go
 '(define (factorial n)
    (if (= n 1) 1
	(* (factorial (- n 1)) n))))

#: total-pushes | maximum-depth
--------------------------------
2: 13           |  5
3: 19           |  8
4: 25           | 11
5: 31           | 14

#        : total-pushes  | maximum-depth
----------------------------------------
rec      : 6 * <n> + 1   | 3 * <n> - 1

; pull the number-pushes and max-depth of special-purpose factorial machine
; referred from http://community.schemewiki.org/?sicp-ex-5.14
#        : total-pushes  | maximum-depth
----------------------------------------
rec      : 2 * <n> - 2   | 2 * <n> - 2

; pull the number-pushes and max-depth of interpreted factorial machine
; referred from https://github.com/k-ori/SICP-exercises/blob/master/chap5/exercise-eceval.scm#L92
#        : total-pushes  | maximum-depth
----------------------------------------
rec      : 32 * <n> - 16 | 5 * <n> + 3

; For the large n, we can ignore intercept.
* / interpreted : total-pushes  | maximum-depth
--------------------------------------------------------
compiled code   : 6/32 ≈ 0.19   | 3 / 5 = 0.60
special machine : 2/32 ≈ 0.06   | 2 / 5 = 0.40

; Thus special machine is much more efficient.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Got the statistic data for iterative version
;; It's not the part of the exercise though

;;;;;;;; tail recursive ;;;;;;;;

(compile-and-go
 '(define (factorial-t n)
    (define (iter product counter)
      (if (> counter n)
	  product
	  (iter (* counter product)
		(+ counter 1))))
    (iter 1 1)))

#: total-pushes | maximum-depth
--------------------------------
2: 19           |  3
3: 25           |  3
4: 31           |  3
5: 37           |  3

;;;;;;;   comparison    ;;;;;;;;

#        : total-pushes  | maximum-depth
----------------------------------------
rec      : 6 * <n> + 1   | 3(n=1), 3 * <n> -1 (n>=2)
tail rec : 6 * <n> + 7   | 3 (constant)

|#

;;; Ex 5.47

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(compound-branch (make-label 'compound-branch)) ; Ex5.47; New label
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage
	   (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
				  `((test (op primitive-procedure?) (reg proc))
				    (branch (label ,primitive-branch))
				    (test (op compound-procedure?) (reg proc)) ; Ex5.47; New branch
				    (branch (label ,compound-branch))))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 compiled-branch
	 (compile-proc-appl target compiled-linkage))
	(parallel-instruction-sequences
	 (append-instruction-sequences ; Ex5.47; Apply compound-proc by (compound-proc-appl)
	  compound-branch
	  (compound-proc-appl target compiled-linkage))
	 (append-instruction-sequences
	  primitive-branch
	  (end-with-linkage
	   linkage
	   (make-instruction-sequence '(proc argl)
				      (list target)
				      `((assign ,target
						(op apply-primitive-procedure)
						(reg proc)
						(reg argl))))))))
       after-call))))

; Revamp (compile-proc-appl) to redirect to `compound-apply` label in interpreter via `compapp` register.
(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val)
	      (not (eq? linkage 'return)))
	 (make-instruction-sequence '(proc) all-regs
				    `((assign continue (label ,linkage))
				      (save continue)                           ; Save `continue` register
				      (goto (reg compapp)))))                   ; Jump to (reg compapp)
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence '(proc) all-regs
				      `((assign continue (label ,proc-return))
					(save continue)                         ; Save `continue` register
					(goto (reg compapp))                    ; Jump to (reg compapp)
					,proc-return
					(assign ,target (reg val))
					(goto (label ,linkage))))))
	((and (eq? target 'val)
	      (eq? linkage 'return))
	 (make-instruction-sequence '(proc continue) all-regs				    
				    '((save continue)                           ; Save `continue` register
				      (goto (reg compapp)))))                   ; Jump to (reg compapp)
	((and (not (eq? target 'val)) (eq? linkage 'return))
	 (error "return linkage, target not val -- COMPILE"
		target))))


#|

; test

1 ]=> (compile-and-go
'(define (f x)
   (+ 1 (g x))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(define (g x)
  (* x x))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(f 3)

(total-pushes = 16 maximum-depth = 7)
;;; EC-Eval value:
10

;;; EC-Eval input:
(define (g x)
  (+ x x))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(f 3)

(total-pushes = 16 maximum-depth = 7)
;;; EC-Eval value:
7

|#

;;; Ex 5.48

;;;;;;;;;;; New primitives for Ex 5.48 ;;;;;;;;;;;

(define (interpreting-compilation? exp)
  (tagged-list? exp 'compile-and-run)) 

(define (perform-compile expression)
  (assemble
   (statements
    (compile expression
	     'val
	     'return))
   eceval))

(define (interpreting-compile-source exp)
  (cadadr exp)) ; remove `(compile-and-run` and `(quote` in `(compile-and-run (quote (define (factorial n)...`

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read);used by eceval

   ;;used by compiled code
   (list 'list list)
   (list 'cons cons)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'false? false?);for compiled code
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?);for non-tail-recursive machine
   (list 'get-global-environment get-global-environment)

   ;;for compiled code (also in eceval-support.scm)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)

   ; Ex 5.48
   (list 'interpreting-compilation? interpreting-compilation?)
   (list 'perform-compile perform-compile)
   (list 'interpreting-compile-source interpreting-compile-source)

   ))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev
	 compapp;*for compiled to call interpreted
	 )
   eceval-operations
   '(
     ;;SECTION 5.4.4, as modified in 5.5.7
     ;;*for compiled to call interpreted (from exercise 5.47)
     (assign compapp (label compound-apply))
     ;;*next instruction supports entry from compiler (from section 5.5.7)
     (branch (label external-entry))
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform
      (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     ;;**following instruction optional -- if use it, need monitored stack
     (perform (op print-stack-statistics))
     (perform
      (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     ;;*support for entry from compiler (from section 5.5.7)
     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))

     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     ;;SECTION 5.4.1
     eval-dispatch

     ; Ex 5.48
     (test (op interpreting-compilation?) (reg exp))
     (branch (label ev-interpreting-compilation))
     
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
	     (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     ;;*next added to call compiled code from evaluator (section 5.5.7)
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))
     ;;*next added to call compiled code from evaluator (section 5.5.7)
     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))

     primitive-apply
     (assign val (op apply-primitive-procedure)
	     (reg proc)
	     (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
	     (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ;;;SECTION 5.4.2
     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ;;;SECTION 5.4.3

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ;;; Ex 5.48
     ev-interpreting-compilation
     (assign val (op interpreting-compile-source) (reg exp))
     (assign val (op perform-compile) (reg val))
     (goto (label external-entry))
     )))

#|

; test

1 ]=> (compile-and-go
 '(define x 5))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(compile-and-run
 '(define (factorial n) (if (= n 1) 1
        (* (factorial (- n 1)) n))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial x)

(total-pushes = 31 maximum-depth = 14)
;;; EC-Eval value:
120

|#
