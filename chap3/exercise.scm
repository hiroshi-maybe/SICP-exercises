
;;; Ex 3.1

(define (make-accumulator sum)
  (lambda (acc) 
    (begin (set! sum (+ sum acc))
	   sum)))

(define A (make-accumulator 5))
(A 10)
; 15
(A 10)
; 25

;;; Ex 3.2

(define (make-monitored f)
  (let ((counter 0))
    (define (how-many-calls?) counter)
    (define (reset-count) 
      (set! counter 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
	    ((eq? m 'reset-count) (reset-count))
	    (else (set! counter (+ counter 1))
		  (f m))))
    dispatch))

(define s (make-monitored sqrt))
(s 100)
; 10
(s 'how-many-calls?)
; 1
(s 'reset-count)
(s 'how-many-calls?)
; 0


