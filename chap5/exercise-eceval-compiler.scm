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

