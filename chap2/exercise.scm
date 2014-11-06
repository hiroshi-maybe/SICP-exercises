
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

;;; Ex 2.21

(define (nil) '())

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4 5))

;;; Ex 2.23

(define (for-each proc items)
  (if (null? items) true
      (let ((_ (proc (car items))))
	(for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))

;;; Ex 2.27

(define (deep-reverse items)
  (cond ((null? items) items)
	((not (pair? items)) items)
	(else (append (deep-reverse (cdr items))
		      (list (deep-reverse (car items)))))))

(deep-reverse (list (list 1 2) (list 3 4)))

;;; Ex 2.28

(define (fringe li)
  (cond ((null? li) li)
	((not (pair? li)) (list li))
	(else (append (fringe (car li))
		      (fringe (cdr li))))))

(fringe (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))

;;; Ex 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (is-structure-mobile? structure) 
  (pair? structure)) 

(make-mobile 2 3)
(left-branch (make-mobile 2 3))
(right-branch (make-mobile 2 3))
(branch-length (make-branch 4 5))
(branch-structure (make-branch 4 5))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (is-structure-mobile? structure)
	(total-weight structure)
	structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define level-1-mobile (make-mobile (make-branch 2 1) 
                                    (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2))) 

(total-weight level-1-mobile)
(total-weight level-2-mobile)
(total-weight level-3-mobile)

(define (branch-torque br)
  (* (branch-weight br)
     (branch-length br)))

(branch-torque (make-branch 2 3))

(define (branch-balanced? br)
  (let ((st (branch-structure br)))
    (if (is-structure-mobile? st) (balanced? st) true)))

(branch-balanced? (make-branch 2 3))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (branch-torque left) (branch-torque right))
	 (branch-balanced? left)
	 (branch-balanced? right))))

(balanced? (make-mobile (make-branch 2 3)
			(make-branch 3 2)))

(balanced? level-1-mobile)
(balanced? level-2-mobile)
(balanced? level-3-mobile)
  
(balanced? (make-mobile (make-branch 10 1000)
			(make-branch 1 level-3-mobile)))

;; Ex 2.30

;(define (square-tree tree)
;  (cond ((null? tree) '())
;	((not (pair? tree)) (square tree))
;	(else (cons (square-tree (car tree))
;		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (square subtree))) tree))  

;; Ex 2.31

(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (proc subtree))) tree))

(tree-map square
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Ex 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;; Ex 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (_ len) (+ len 1)) 0 sequence))









