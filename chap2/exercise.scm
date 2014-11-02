
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


