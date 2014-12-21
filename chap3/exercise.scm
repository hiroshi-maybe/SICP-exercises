
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

;;; Ex 2.38

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logic-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

;;; Ex 3.29

(define (or-gate-ex a1 a2 output)
  (let ((b1 make-wire)
	(b2 make-wire)
	(c make-wire))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))

;;; Ex 3.30

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a b s c-out)
  (let ((c-in (make-wire))
	(first? (null? (cdr a))))
    (if first?
	(set-signal! c-in 0)
	(ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c-out)))

;;; Ex 3.33

(define (averager a b c)
  (let ((u make-connector)
	(v make-connector))
    (adder a b u)
    (multiplier u v c)
    (constant 0.5 v)
    'ok))

;;; Ex 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else 
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;; Ex 3.27

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (cv v)
  (let ((x (make-connector)))
    (constant v x)
    x))

;;; Ex 3.47

; a
(define (make-semaphore n)
  (let ((mutex (make-mutex))
	(counter 0))
    (define (the-semaphore m)
      (mutex 'acquire)
      (cond ((eq? m 'acquire)
	     (if (< locking-count n)
		 (set! counter (+ counter 1))
		 (the-semaphore 'acquire))) ; retry
	    ((eq? m 'release)
	     (set! counter (- counter 1))))
      (the-mutex 'release))
    the-semaphore))

; b
(define (make-semaphore-ex n)
  (let ((cell (list false))
	(counter 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (and (< locking-count n)
		      (test-and-set! cell))
		 (begin (set! counter (+ counter 1))
			(clear! cell))
		 (the-semaphore 'acquire))) ; retry
	    ((eq? m 'release)
	     (if (test-and-set! cell)
		 (begin (set! counter (- counter 1))
			(clear! cell))))))
    the-semaphore))

;;; 3.5

; (cons-stream <a> <b>)
;   is equivalent to
; (cons <a> (delay <b>))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; obtain second number
(stream-car
 (stream-cdr
  (stream-filter (lambda (x) (memq x (list 2 4 6)))
                 (stream-enumerate-interval 0 20))))

;;; Ex 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(stream-car
 (stream-cdr
  (stream-map + (stream-enumerate-interval 1 200000)
	      (stream-enumerate-interval 2 200000)
	      (stream-enumerate-interval 3 200000))))

;;; Ex 3.51

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
; 1 2 3 4 5
(stream-ref x 7)
; 6 7 because stream is consumed sequently

;;; Ex 3.54

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams (add-streams ones integers) factorials)))

(stream-ref factorials 3)

;;; Ex 3.55

(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (partial-sums s) (stream-cdr s))))

(stream-ref (partial-sums integers) 4)
; 15

;;; Ex 3.56

(define (display-stream-until s n)
  (stream-ref (stream-map show s) n))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

;(stream-ref (stream-map show S) 10)
(display-stream-until S 20)

;;; Ex 3.59

; a
(define (integrate-series s)
  (stream-map / s integers))

; b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define (neg-streams s)
  (stream-map (lambda (x) (- x)) s))

(define cosine-series
  (cons-stream 1 (neg-streams (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;; Ex 3.64

(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ((s-next (stream-cdr s)))
    (let ((v1 (stream-car s))
	  (v2 (stream-car s-next)))
      (if (< (abs (- v1 v2)) tolerance)
	  v2
	  (stream-limit s-next tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.00001)

;;; Ex 3.67

(define (interleave s1 s2)
 (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; pairs are not distributed equally
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
		 (stream-cdr s)))))

(display-stream-until (all-pairs integers integers) 30)

