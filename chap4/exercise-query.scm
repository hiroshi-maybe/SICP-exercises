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

;;; Ex 4.58

;(rule (big-shot ?person)
;      (and (job ?person (?division . ?title-p))
;	   (or (not (supervisor ?person ?boss))
;	       (and (supervisor ?person ?boss)
;		    (not (job ?boss (?division . ?title-b)))))))

;;; Ex 4.59

; a
; (meeting ?division (Friday ?time))

; b
; (rule (meeting-time ?person ?day-and-time)
;       (or (meeting whole-company ?day-and-time)
; 	  (and (job ?person (?div . ?title))
; 	       (meeting ?div ?day-and-time))))

; c
; (meeting-time (Hacker Alyssa P) (Wednesday ?time))

;;; Ex 4.60

; Need to sort by name to get unique pairs

;(query-driver-loop)