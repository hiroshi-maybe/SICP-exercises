(load "ch5-compiler.scm")

;;; Ex 5.33

#|

; left
(compile
 '(define (factorial n)
    (if (= n 1) 1
	(* (factorial (- n 1)) n)))
 'val
 'next)

; right
(compile
 '(define (factorial n)
    (if (= n 1) 1
	(* n (factorial (- n 1)))))
 'val
 'next)

((env) (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
  compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call15
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
  true-branch5
  (assign val (const 1))
  (goto (reg continue))
  false-branch4
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)

  ; (* (factorial (- n 1)) n)                                 ; (* n (factorial-alt (- n 1))) 
  (assign val (op lookup-variable-value) (const n) (reg env)) ; (save env)
  (assign argl (op list) (reg val))                           ; 
  (save argl)                                                 ;
  
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
  compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call6
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
  compiled-branch10
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call9

  ; (* (factorial (- n 1)) n) ; (* n (factorial-alt (- n 1))) 
  (restore argl)              ; (assign argl (op list) (reg val))
                              ; (restore env)
	                      ; (assign val (op lookup-variable-value) (const n) (reg env))
  
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  compiled-branch13
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call12
  after-if3
  after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))
|#

; Efficiency is same. Same count of access to register

;;; Ex 5.34

#|
(compile
 '(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
 'val
 'next)

((env) (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))

  ; entry point of (iter * *)

  entry7
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
  compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call20
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch9))
  true-branch10
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))

  ; call (iter (* counter product) (+ counter 1))

  false-branch9
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)

  ; call (+ counter 1)

  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call14
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)

  ; call (* counter product)

  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val))
  primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call11
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)

  ; call (iter * *) recursively

  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
  compiled-branch18
  (assign val (op compiled-procedure-entry) (reg proc))

  ; just jump to entry point without consuming stack
  ; On the other hand, original version needs to save stuffs in stack before calling (factorial (- n 1)) as below

;  false-branch4
;  ;; compute and return (* (factorial (- n 1)) n)
;  (assign proc (op lookup-variable-value) (const *) (reg env))
;  (save continue)
;  (save proc) ; save * procedure
;  (assign val (op lookup-variable-value) (const n) (reg env)) (assign argl (op list) (reg val))
;  (save argl) ; save partial argument list for *

  (goto (reg val))
  primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call17
  after-if8
  after-lambda6
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val)) (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
  compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc)) (goto (reg val))
  primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call3
  after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))
|#

;;; Ex 5.35

#|
(compile
 '(define (f x)
    (+ x (g (+ x 2))))
 'val
 'next)
|#

;;; Ex 5.36

; Operands are evaluated right-to-left

#|

; evaluate left-to-right

(define (construct-arglist operand-codes)
  (let ((operand-codes operand-codes)) ; remove (reverse *)
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-first-arg ; get first arg instead of last
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-first-arg
              (preserving '(env)
               code-to-get-first-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
	 (preserving
	  '(argl)
	  (car operand-codes)
	  (make-instruction-sequence
	   '(val argl) '(argl)
	   ; `adjoin-arg` instead of `cons` to append arg to tail of `argl`.
	   '((assign argl
		     (op adjoin-arg) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
	code-for-next-arg
	(preserving
	 '(env)
	 code-for-next-arg
	 (code-to-get-rest-args (cdr operand-codes))))))
|#

; less efficient because `append` in `adjoin-arg` needs to go through a list (O(1) to O(n))

;;; Ex 5.37

#|
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
;;; Ignore required or modified registers ;;;
;	(if (and (needs-register? seq2 first-reg)
;		                  (modifies-register? seq1 first-reg))
	(preserving (cdr regs)
		    (make-instruction-sequence
		     (list-union (list first-reg) (registers-needed seq1))
		     (list-difference
		      (registers-modified seq1)
		                   (list first-reg)) 
		     (append `((save ,first-reg))
			     (statements seq1)
			     `((restore ,first-reg))))
		    seq2)
;	(preserving (cdr regs) seq1 seq2)
	)))

(compile
 '(define (f x)
    (+ x 1))
 'val
 'next)

; Bunch of redundant stack uses
((env) (val)
 (

; Redundant stack use
;  (save continue)
;  (save env)
;  (save continue)

  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))

; Redundant stack use
;  (save continue)
;  (save env)
;  (save continue)
  
  (assign proc (op lookup-variable-value) (const +) (reg env))

; Redundant stack use
;  (restore continue)
;  (restore env)
;  (restore continue)
;  (save continue)
;  (save proc)
;  (save env)
;  (save continue)
  
  (assign val (const 1))

; Redundant stack use
;  (restore continue)

  (assign argl (op list) (reg val))

; Redundant stack use
;  (restore env)
;  (save argl)
;  (save continue)
  
  (assign val (op lookup-variable-value) (const x) (reg env))

; Redundant stack use
;  (restore continue)
;  (restore argl)
  
  (assign argl (op cons) (reg val) (reg argl))

; Redundant stack use
;  (restore proc)
;  (restore continue) 

  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
  compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch5

; Redundant stack use
;  (save continue)
  
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

; Redundant stack use
;  (restore continue)

  (goto (reg continue))
  after-call3
  after-lambda1

; Redundant stack use
;  (restore env)

  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))

; Redundant stack use
;  (restore continue)

  ))
|#

;;; Ex 5.38

; a, b

; Just returns seq1 and seq2 to add an appropreate `preserving` with subsequent instructions
(define (spread-arguments a1 a2)
  (let ((seq1 (compile a1 'arg1 'next))
	(seq2 (compile a2 'arg2 'next)))
    (list seq1 seq2)))

(define open-code-primitives '(= * - +))
(define (open-code? exp) (memq (car exp) open-code-primitives))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp)
	 (compile-variable exp target linkage))
	((assignment? exp)
	 (compile-assignment exp target linkage))
	((definition? exp)
	 (compile-definition exp target linkage))
	((if? exp) (compile-if exp target linkage))
	((lambda? exp) (compile-lambda exp target linkage))
	((begin? exp)
	 (compile-sequence (begin-actions exp)
			   target
			   linkage))
	((cond? exp) (compile (cond->if exp) target linkage))
	((open-code? exp) (compile-open-code exp target linkage)) ; added!!!!
	((application? exp)
	 (compile-application exp target linkage))
	(else
	 (error "Unknown expression type -- COMPILE" exp))))

(define (compile-open-code exp target linkage)
  (if (eq? (length exp) 3)
      (let ((op (car exp))
	    (operand-seqs (spread-arguments (cadr exp) (caddr exp))))
	(let ((seq1 (car operand-seqs))
	      (seq2 (cadr operand-seqs)))
	  (end-with-linkage
	   linkage
	   (preserving
	    '(env)
	    seq1
	    (preserving
	     '(env arg1) ; preserve arg1 because it could be modified in `seq2`
	     seq2
	     (make-instruction-sequence '(arg1 arg2) (list target)
					`((assign ,target (op ,op) (reg arg1) (reg arg2)))))))))
      (error "Number of operands should be 3 -- COMPILE" exp)))

#|
(compile
 '(* 2 (+ 1 3))
 'val
 'next)

(() (arg1 arg2 val)
 ((assign arg1 (const 2))
  (save arg1) ; `arg1` is preserved
  (assign arg1 (const 1))
  (assign arg2 (const 3))
  (assign arg2 (op +) (reg arg1) (reg arg2))
  (restore arg1)  ; `arg1` is restored
  (assign val (op *) (reg arg1) (reg arg2))))

; c

(compile
 '(define (factorial n)
    (if (= n 1) 1
	(* (factorial (- n 1)) n)))
 'val
 'next)

; 80 lines -> 43 lines

; left : original 
; right: leverage open-code

((env) (val)
 ((assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

  ;;; diff START ;;;
  
  (save continue)                                               ; (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (save env)                                                    ; (assign arg2 (const 1))
  (assign proc (op lookup-variable-value) (const =) (reg env))  ; (assign val (op =) (reg arg1) (reg arg2))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
  compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call15
  (restore env)
  (restore continue)

  ;;; diff END ;;;
  
  (test (op false?) (reg val))
  (branch (label false-branch4))
  true-branch5
  (assign val (const 1))
  (goto (reg continue))
  
  false-branch4

  ;;; diff START ;;;
  
  (assign proc (op lookup-variable-value) (const *) (reg env))         ; (save continue)
  (save continue)                                                      ; (save env)
  (save proc)                                                          ; (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))          ; (assign arg1 (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))                                    ; (assign arg2 (const 1))
  (save argl)                                                          ; (assign val (op -) (reg arg1) (reg arg2))
  (assign proc (op lookup-variable-value) (const factorial) (reg env)) ; (assign argl (op list) (reg val))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))

  ;;; diff END ;;;

  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
  
  compiled-branch7

  ;;; diff START ;;;
  
  (assign continue (label after-call6))                 ; (assign continue (label proc-return9))
  (assign val (op compiled-procedure-entry) (reg proc)) ; (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))                                      ; (goto (reg val))
                                                        ; proc-return9
                                                        ; (assign arg1 (reg val))
                                                        ; (goto (label after-call6))

  ;;; diff END ;;;
  
  primitive-branch8

  ;;; diff START ;;;
  
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call6                                                       ; after-call6
  (assign argl (op list) (reg val))                                 ; (restore env)
  (restore proc)                                                    ; (assign arg2 (op lookup-variable-value) (const n) (reg env))
  (test (op primitive-procedure?) (reg proc))                       ; (assign val (op *) (reg arg1) (reg arg2))
  (branch (label primitive-branch11))                               ; (restore continue)
  compiled-branch10                                                 ; (goto (reg continue))
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call9
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  compiled-branch13
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call12
  
  ;;; diff END ;;;
  
  after-if3
  after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

|#

;;; Ex 5.39

(define (lookup-frame-by-offset frame-offset env)
  (if (= frame-offset 0)
      (first-frame env)
      (lookup-frame-by-offset (- frame-offset 1) (enclosing-environment env))))

; constructor and selectors
(define (lexical-address addr-frame addr-offset)
  (cons addr-frame addr-offset))
(define (addr-frame address) (car address))
(define (addr-offset address) (cdr address))

(define (lexical-address-lookup env lexical-addr)
  (define (scan-var var-offset vars vals)
    (if (= var-offset 0)
	(let ((value (car vals)))
	  (if (eq? value '*unassigned*)
	      (error "unassigned var" (car var))
	      value))
	(scan-var (- var-offset 1) (cdr vars) (cdr vals))))
  (let ((frame (lookup-frame-by-offset (addr-frame lexical-addr) env)))
    (scan-var (addr-offset lexical-addr)
	      (frame-variables frame)
	      (frame-values frame))))

(define (lexical-address-set! env lexical-addr val)
  (define (set-val! var-offset vars vals)
    (if (= var-offset 0)
	(set-car! vals val)
	(set-val! (- var-offset 1) (cdr vars) (cdr vals))))
  (let ((frame (lookup-frame-by-offset (addr-frame lexical-addr) env)))
    (set-val! (addr-offset lexical-addr)
	      (frame-variables frame)
	      (frame-values frame))))

;;; Ex 5.40

(define (compile exp target linkage compile-time-env)                        ; compile-time-env
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-time-env))      ; compile-time-env
        ((quoted? exp) (compile-quoted exp target linkage compile-time-env)) ; compile-time-env
        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))             ; compile-time-env
        ((assignment? exp)
         (compile-assignment exp target linkage compile-time-env))           ; compile-time-env
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))           ; compile-time-env
        ((if? exp) (compile-if exp target linkage compile-time-env))         ; compile-time-env
        ((lambda? exp) (compile-lambda exp target linkage compile-time-env)) ; compile-time-env
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   compile-time-env))                                ; compile-time-env
        ((cond? exp) (compile (cond->if exp) target linkage compile-time-env)) ; compile-time-env
        ((application? exp)
         (compile-application exp target linkage compile-time-env))            ; compile-time-env
	(else
	 (error "Unknown expression type -- COMPILE" exp))))

(define (compile-lambda exp target linkage compile-time-env) ; compile-time-env
  (let ((proc-entry (make-label 'entry))
	(after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
	   (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
	(end-with-linkage lambda-linkage
			  (make-instruction-sequence '(env) (list target)
						     `((assign ,target
							       (op make-compiled-procedure)
							       (label ,proc-entry)
							       (reg env)))))
	(compile-lambda-body exp proc-entry compile-time-env)) ; compile-time-env
       after-lambda))))

(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
				`(,proc-entry
				  (assign env (op compiled-procedure-env) (reg proc))
				  (assign env
					  (op extend-environment)
					  (const ,formals)
					  (reg argl)
					  (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return
		       (extend-compile-time compile-time-env formals)))) ; extend env

; helper
(define (extend-compile-time-env env frame)
  (cons frame env))

