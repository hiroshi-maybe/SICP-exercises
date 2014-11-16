
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

;(define (map proc items)
;  (if (null? items)
;      nil
;      (cons (proc (car items))
;	    (map proc (cdr items)))))

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

;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (_ len) (+ len 1)) 0 sequence))

;; Ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

; 79
(horner-eval 2 (list 1 3 0 5 0 1))

;; Ex 2.35

(define (count-leaves t)
  (accumulate + 0
	      (map (lambda (e)
		     (if (pair? e)
			 (count-leaves e)
			 1)) t)))

(count-leaves (cons (list 1 2) (list 3 4)))

;; Ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; Ex 2.37

(define (dot-product v1 v2) 
  (accumulate + 0 (map * v1 v2)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

;; This clean form is awesome!!!
(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (matrix-*-vector cols row)) m)))

(define v (list 1 3 -5))
(define w (list 4 -2 -1))
(dot-product v w)
; 3
(define m (list (list 1 2 3) (list 4 5 6)))
(define v (list 1 2 3))
(matrix-*-vector m v)
; (14 32)
(define a (list (list 14 9 3) (list 2 11 15) (list 0 12 17) (list 5 2 3)))
(define b (list (list 12 25) (list 9 10) (list 8 5)))
(matrix-*-matrix a b)
; ((273 455) (243 235) (244 205) (102 160))

;; Ex 2.40

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (filter (lambda (pair) (not (= (car pair) (cadr pair))))
	  (flatmap (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 n)))
		   (enumerate-interval 1 n))))

;; Ex 2.41

(define (pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j)) 
		  (enumerate-interval 1 (- i 1)))) 
	   (enumerate-interval 1 n)))

(define (triples n)
  (flatmap (lambda (pair)
	     (map (lambda (j) (append pair (list j))) 
		  (enumerate-interval 1 (cadr pair))))
	   (pairs n)))

(define (sum items)
  (accumulate + 0 items))

(define (triple-sum n s)
  (filter (lambda (triple) (= s (sum triple)))
	  (triples n)))

;; Ex 2.42  

(define (every? pred items)
  (accumulate (lambda (item res) (and res (pred item)))
	      true
	      items))

(define (queens board-size)
  (define empty-board '())
  (define (put-queen row col) (cons row col))
  (define (row queen) (car queen))
  (define (col queen) (cdr queen))

  (define (adjoin-position new-row col board-state)
    (cons (put-queen new-row col) board-state))

  (define (safe? tested-col board-state)
    (define (row-in-board? row)
      (and (> row 0)
	   (<= row board-size)))
    (define (invalid-horizontal-rows rest-queens)
      (map row rest-queens))
    (define (invalid-diagonal-rows rest-queens)
      (filter row-in-board?
	      (flatmap (lambda (queen)
			 (let ((my-row (row queen))
			       (my-col (col queen)))
			   (list (- my-row (- tested-col my-col)) (+ my-row (- tested-col my-col)))))
		       rest-queens)))
    (define (invalid-rows board-state)
      (append (invalid-horizontal-rows (cdr board-state))
	      (invalid-diagonal-rows (cdr board-state))))

    (let ((tested-row (row (car board-state)))
	  (my-invalid-rows (invalid-rows board-state)))
      (every? (lambda (row) (not (= row tested-row))) my-invalid-rows)))
    
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Huffman Encoding Trees

; Symbol -> Weight -> Leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; Tree -> Tree -> Tree
; [left, right, symbols, weight]
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; [Bit] -> Tree -> [Symbol]
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; bit -> Tree -> Tree
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

; Tree -> [(symbol, weight)] -> Tree
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

; [(symbol, weight)] -> Tree
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) ; symbol
			       (cadr pair)) ; frequency
		    (make-leaf-set (cdr pairs))))))

;; Ex 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;; Ex 2.68

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define sample-symbol-message '(A D A B B C A))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left-symbols (symbols (left-branch tree)))
	    (right-symbols (symbols (right-branch tree))))
	(cond ((element-of-set? symbol left-symbols)
	       (cons 0 (encode-symbol symbol (left-branch tree))))
	      ((element-of-set? symbol right-symbols)
	       (cons 1 (encode-symbol symbol (right-branch tree))))
	      (else (error "bad symbol -- " symbol))))))

(encode sample-symbol-message sample-tree)

;; Ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-pairs '((A 3) (B 5) (C 6) (D 6)))

(define (successive-merge trees)
  (if (null? (cdr trees))
      (car trees)
      (let ((left (car trees))
	    (right (cadr trees))
	    (rest (cddr trees)))
	(successive-merge (adjoin-set (make-code-tree left right) rest)))))

(generate-huffman-tree sample-pairs)

(define test-tree (generate-huffman-tree sample-pairs)) 
  
(encode '(A B C D) test-tree)

(define lyrics-tree (generate-huffman-tree '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1))))

;; 84 bits
(encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) lyrics-tree)

;; Ex 2.79

;;;SECTION 2.5.1

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

