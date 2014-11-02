
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound) car)
(define (upper-bound) cdr)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (/ (width i) (center i)))

;;; Ex 2.17

(define (last-pair l)
  (let ((rest (cdr l)))
    (if (null? rest)
      l
      (last-pair rest))))

;;; Ex 2.18

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append
			 (cdr list1) list2))))

(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l))
	      (list (car l)))))

;;; Ex 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(cc 100 us-coins)

;;; Ex 2.20

(define (same-parity . args)
  (define (filter pred? list)
    (cond ((null? list) list)
	  ((pred? (car list))
	   (cons (car list) (filter pred? (cdr list))))
	  (else (filter pred? (cdr list)))))
  (cond ((null? args) args)
	((even? (car args)) (filter even? args))
	(else (filter odd? args))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)




