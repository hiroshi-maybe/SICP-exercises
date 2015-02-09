(load "ch4-query.scm")

(define my-output-prompt ";;; Query results for ")

; from (query-driver-loop) in ch4-query.scm
(define (run query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          (else
	   (newline)
	   (display my-output-prompt) (display query)
           ;; [extra newline at end] (announce-output output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))))))

(define-syntax run-query
  (syntax-rules ()
    ((_ exp)
     (run (quote exp)))))

(define-syntax run-query-series
  (syntax-rules ()
    ((_ exps)
     (for-each (lambda (exp) (run (quote exp))) exps))))

(initialize-data-base microshaft-data-base)

;;; Ex 4.55

; a
(run-query
 (supervisor ?who (Bitdiddle Ben)))
; b
(run-query
 (job ?who (accounting . ?work)))
; c
(run-query
 (address ?who (Slumerville . ?address)))

;;; Ex 4.56

; a
(run-query
 (and (supervisor ?who (Bitdiddle Ben))
      (address ?who ?where)))

; b
(run-query
 (and (salary (Bitdiddle Ben) ?amount-ben)
      (salary ?who ?amount)
      (lisp-value < ?amount ?amount-ben)))

; c
(run-query
 (and (supervisor ?who ?supervisor)
      (not (job ?supervisor (computer . ?div)))
      (job ?supervisor ?job)))

;;; Ex 4.57

(run-query
 (assert!
  (rule (replace ?person1 ?person2)
	(and (job ?person1 ?job1)
	     (job ?person2 ?job2)
	     (or (same ?job1 ?job2)
		 (can-do-job ?job1 ?job2))
	     (not (same ?person1 ?person2))))))

; a
(run-query
 (replace ?person (Fect Cy D)))

; b
(run-query
 (and (replace ?person1 ?person2)
      (salary ?person1 ?amount1)
      (salary ?person2 ?amount2)
      (lisp-value > ?amount2 ?amount1)))

;;; Ex 4.58

(run-query
 (assert!
  (rule (big-shot ?person)
	(and (job ?person (?division . ?title-p))
	     (or (not (supervisor ?person ?boss))
		 (and (supervisor ?person ?boss)
		      (not (job ?boss (?division . ?title-b)))))))))

;;; Ex 4.59

; a
(run-query
  (meeting ?division (Friday ?time)))

; b
(run-query
 (assert!
  (rule (meeting-time ?person ?day-and-time)
	(or (meeting whole-company ?day-and-time)
	    (and (job ?person (?div . ?title))
		 (meeting ?div ?day-and-time))))))

; c
(run-query
 (meeting-time (Hacker Alyssa P) (Wednesday ?time)))

;;; Ex 4.60

; Need to sort by name to get unique pairs

;;; Ex 4.61

;(1 next-to (2 3) in (1 (2 3) 4))
;((2 3) next-to 4 in (1 (2 3) 4))

;(2 next-to 1 in (2 1 3 1))
;(3 next-to 1 in (2 1 3 1))

;;; Ex 4.62
(run-query
 (assert! (rule (last-pair (?x) (?x)))))
(run-query
 (assert! (rule (last-pair (?u . ?v) (?x))
		(last-pair ?v (?x)))))

(run-query
 (last-pair (1 2 3) ?x))
; No rule which reduces (last-pair ?x (3))

;;; Ex 4.63

(run-query
 (assert!
  (rule (grand-son ?s ?g)
	(and (son ?s ?f)
	     (son ?f ?g)))))
(run-query
 (assert!
  (rule (son ?s ?m)
	(and (wife ?w ?m)
	     (son ?s ?w)))))

;;; Ex 4.68

(run-query
  (assert!
   (rule (append-to-form () ?y ?y))))
(run-query
  (assert!
   (rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))))

(run-query
  (assert!
   (rule (reverse () ()))))
(run-query
  (assert!
   (rule (reverse (?u . ?v) ?y)
	 (and (reverse ?v ?v-rev)
	      (append-to-form ?v-rev (?u) ?y)))))

(run-query
 (reverse (1 2 3) ?x))
