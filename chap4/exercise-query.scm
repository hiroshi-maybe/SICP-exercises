(load "ch4-query.scm")

;;; Ex 4.55

; a
; (supervisor ?who (Bitdiddle Ben))
; b
; (job ?who (accounting . ?work))
; c
; (address ?who (Slumerville . ?address))

;;; Ex 4.56

; a
;(and (supervisor ?who (Bitdiddle Ben))
;     (address ?who ?where))

; b
;(and (salary (Bitdiddle Ben) ?amount-ben)
;     (salary ?who ?amount)
;     (lisp-value < ?amount ?amount-ben))

; c
;(and (supervisor ?who ?supervisor)
;     (not (job ?supervisor (computer . ?div)))
;     (job ?supervisor ?job))

;;; Ex 4.57

;(rule (replace ?person1 ?person2)
;      (and (job ?person1 ?job1)
;	   (job ?person2 ?job2)
;	   (or (same ?job1 ?job2)
;	       (can-do-job ?job1 ?job2))
;	   (not (same ?person1 ?person2))))

; a
;(replace ?person (Fect Cy D))

; b
;(and (replace ?person1 ?person2)
;     (salary ?person1 ?amount1)
;     (salary ?person2 ?amount2)
;     (lisp-value > ?amount1 ?amount2)))

;(query-driver-loop)