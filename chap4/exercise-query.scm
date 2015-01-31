
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

