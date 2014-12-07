
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

;;; Ex 3.7

; modified solution of Ex 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin 
	  (set! balance (- balance amount)) 
	  balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (authenticate p)
    (eq? p password))
  (define (dispatch p m)
    (let ((auth-valid (authenticate p)))
      (cond ((eq? m 'authenticate) auth-valid)
	    ((not auth-valid) (lambda (x) "Incorrect password"))
	    ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT" m)))))
  dispatch)

(define (make-joint org-acc org-password password)
  (define (dispatch p m)
    (if (not (eq? p password))
	(lambda (x) "Incorrect password in joint account")
	(org-acc org-password m)))
  (if (org-acc org-password 'authenticate)
      dispatch
      (error "Incorrect password (Cannot create joint account)")))

(define peter-acc (make-account 100 'open-sesame))
(peter-acc 'open-sesame 'authenticate)
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 10)

;;; Ex 3.18

(define (contains-cycle list)
  (let ((visited '()))
    (define (mark-visited x)
      (cond ((not (pair? x)) false)
	    ((memq x visited) true)
	    (else (begin (set! visited (cons x visited))
			 false))))
    (define (loop x)
      (cond ((not (pair? x)) false)
	    ((mark-visited (car x)) true)
	    ((mark-visited (cdr x)) true)
	    (else (or (loop (car x)) (loop (cdr x))))))
    (loop list)))

(contains-cycle (list 'a 'b 'c))
; false

(define (last-pair x)
  (if (null? (cdr x)) x(last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z1 (make-cycle (list 'a 'b 'c)))
(contains-cycle z1)
; true
(define z2 (list 'a 'b 'c))
(set-car! z2 (last-pair z2))
(contains-cycle z2)
; true

;;; Ex 3.19

(define (nil? x) (eq? x '()))

(define (contains-cycle-ex list)
  (define (safe-cdr x)
    (if (pair? x)
	(cdr x)
	'()))
  (define (suc x) (safe-cdr x))
  (define (ssuc x) (safe-cdr (safe-cdr x)))
  (define (nil? x) (eq? x '()))
  (define (loop slow fast)
    (cond ((nil? slow) false)
	  ((nil? fast) false)
	  ((eq? slow fast) true)
	  (else (loop (suc slow) (ssuc fast)))))
  (loop (suc list) (ssuc list)))

(contains-cycle-ex (list 'a 'b 'c))
; false
(contains-cycle-ex z1)
; true

;;; Ex 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define print-queue car)

(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;;; Ex 3.23

; same implementation as normal queue
;(define (front-ptr queue) (car queue))
;(define (rear-ptr queue) (cdr queue))
;(define (set-front-ptr! queue item) (set-car! queue item))
;(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-dlink-pair val)
  (cons val '()))
(define (dlink-pair-prev-ptr pair) (cdr pair))
(define (dlink-pair-val pair) (car pair))
(define (set-dlink-pair-prev-ptr! pair item) (set-cdr! pair item))

(define (make-dlink-item val)
  (cons (make-dlink-pair val) '()))
(define dlink-pair car)
(define (next-item item)
  (if (nil? item) item (cdr item)))
(define (prev-item item)
  (if (nil? item) item (dlink-pair-prev-ptr (dlink-pair item))))
(define (val-item item)
  (dlink-pair-val (dlink-pair item)))
(define (set-prev-item! item prev-item)
  (if (not (nil? item))
      (set-dlink-pair-prev-ptr! (dlink-pair item) prev-item)))
(define (set-next-item! item next-item)
  (if (not (nil? item))
      (set-cdr! item next-item)))

(define (empty-deque? queue) (nil? (front-ptr queue)))
(define (make-deque) (cons '() '()))
(define (front-deque queue) (val-item (front-ptr queue)))
(define (rear-deque queue)  (val-item (rear-ptr  queue)))

(define (safe-front-ptr! queue rear-item)  (if (nil? (front-ptr queue)) (set-front-ptr! queue rear-item)))
(define (safe-rear-ptr!  queue front-item) (if (nil? (rear-ptr  queue)) (set-rear-ptr!  queue front-item)))

(define (front-insert-deque! queue val)
  (let ((next-item (front-ptr queue))
	(new-item (make-dlink-item val)))
    (begin (set-prev-item! next-item new-item)
	   (set-next-item! new-item next-item)
	   (set-front-ptr! queue new-item)
	   (safe-rear-ptr! queue new-item))))

(define (rear-insert-deque! queue val)
  (let ((prev-item (rear-ptr queue))
	(new-item (make-dlink-item val)))
    (begin (set-prev-item! new-item prev-item)
	   (set-next-item! prev-item new-item)
	   (set-rear-ptr! queue new-item)
	   (safe-front-ptr! queue new-item))))

(define (front-delete-deque! queue)
  (let ((front-item (front-ptr queue)))
    (if (nil? front-item)
	(error "DELETE! called with an empty deque")
	(let ((my-next (next-item front-item)))
	  (begin (set-front-ptr! queue my-next)
		 (set-prev-item! my-next '())
		 (if (nil? my-next) (safe-rear-ptr! queue '())))))))

(define (rear-delete-deque! queue)
  (let ((rear-item (rear-ptr queue)))
    (if (nil? rear-item)
	(error "DELETE! called with an empty deque")
	(let ((my-prev (prev-item rear-item)))
	  (begin (set-rear-ptr! queue my-prev)
		 (if (nil? my-prev) (safe-front-ptr! queue '())))))))

(define (print-deque queue)
  (define (iter-backword item vals)
    (if (nil? item)
	vals
	(iter-backword (prev-item item) (cons (val-item item) vals))))
  (iter-backword (rear-ptr queue) '()))

(define deq1 (make-deque))
(front-insert-deque! deq1 'b)
(front-insert-deque! deq1 'a)
(rear-insert-deque! deq1 'c)
(print-deque deq1)
(front-delete-deque! deq1)
(rear-delete-deque! deq1)
(print-deque deq1)
(front-insert-deque! deq1 'a)
(rear-insert-deque! deq1 'c)
(print-deque deq1)

;;; Ex 3.25

(define (make-table same-key?)
   (let ((local-table (list '*table*)))
         (define (assoc key records)
           (cond ((null? records) false)
                 ((same-key? key (caar records)) (car records))
                 (else (assoc key (cdr records)))))
         (define (lookup keys table)
	   (let ((key (car keys))
		 (rest-keys (cdr keys)))
	     (if (null? rest-keys)
		 (let ((record (assoc key (cdr table))))
		   (if record
		       (cdr record)
		       false))
		 (let ((subtable (assoc key (cdr table))))
		   (if subtable
		       (lookup rest-keys subtable)
		       false)))))
         (define (insert! keys value table)
	   (let ((key (car keys))
		 (rest-keys (cdr keys)))
	     (if (null? rest-keys)
		 (let ((record (assoc key (cdr table))))
		   (if record
		       (set-cdr! record value)
		       (set-cdr! table
				 (cons (cons key value)
				       (cdr table)))))
		 (let ((subtable (assoc key (cdr table))))
		   (if subtable
		       (insert! rest-keys value subtable)
		       (begin (set-cdr! table
					(cons (list key) (cdr table)))
			      (insert! rest-keys value (cadr table))))))))
         (define (dispatch m)
           (cond ((eq? m 'lookup-proc)  (lambda (keys) (lookup  keys local-table)))
                 ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value local-table)))
                 (else (error "Unkown operation -- TABLE" m))))
         dispatch))
  
(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put (list 'a 'b 'c1) 'val1)
(put (list 'a 'b 'c2) 'val2)
(get (list 'd))
; false
(get (list 'a 'b 'c1))
; val1
(put (list 'a 'b 'c2) 'val2+)
(get (list 'a 'b 'c2))
; val2+
