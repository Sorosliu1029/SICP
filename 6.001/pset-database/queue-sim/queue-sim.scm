;;; Copyright (c) 1990 Massachusetts Institute of Technology
;;; 
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy this material, to redistribute
;;; it, and to use it for any non-commercial purpose is granted, subject
;;; to the following restrictions and understandings.
;;; 
;;; 1. Any copy made of this material must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this material agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this material.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; material shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that this material
;;; (including the operation of software contained therein) will be
;;; error-free, and MIT is under no obligation to provide any services, by
;;; way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case. 

;;;This is the code for the queueing simulation

;;define global variables
(define the-agenda nil)
(define available-servers nil)

;;the customer object

(define (make-customer arrival-time time-to-service)
  (define (customer message)
    (cond ((eq? message 'how-long-do-you-need) time-to-service)
          ((eq? message 'done)
	   (let ((time (get-current-time)))
	     (print
	      (list 'enter arrival-time
		    'leave time
		    'service time-to-service 
		    'wait (- time (+ arrival-time
				     time-to-service))))))
          (else (error "unknown message -- customer" message))))
  (request-service (shortest-queue-server available-servers) customer)
  customer)

(define (customer-service-time customer)
  (customer 'how-long-do-you-need))

(define (customer-finished customer)
      (customer 'done))

;;the server object
(define (make-server)
  (let ((my-queue (make-queue)))
    (define (serve customer)
      (after-delay (customer-service-time customer)
                   service-completed))
    (define (process-serve-request customer)
      (let ((queue-was-empty (empty-queue? my-queue)))
        (insert-queue! my-queue customer)
        (if queue-was-empty
            (serve (front my-queue)))))
    (define (service-completed)
      (customer-finished (front my-queue))
      (delete-queue! my-queue)
      (if (not (empty-queue? my-queue))
          (serve (front my-queue))))
    (define (me message)
      (cond ((eq? message 'how-long-is-your-queue)
             (length-queue my-queue))
            ((eq? message 'serve-me) process-serve-request)
            (else (error "unknown message -- server" message))))
    me))

(define (request-service server customer)
  ((server 'serve-me) customer))

(define (server-queue-length server)
  (server 'how-long-is-your-queue))

;;initialization and main loop
(define (simulate number-servers
                  chance-customer-enters
                  request-distribution
                  max-time)
  (define (initialize-simulation)
    (set! the-agenda (make-agenda))
    (set! available-servers
          (make-server-list number-servers))
    (make-customer-source chance-customer-enters
                          request-distribution))
  (define (main-loop)
    (cond ((= (get-current-time) max-time) 'done)
          ((empty-agenda? the-agenda) 'done)
          (else (let ((first-item (first-agenda-item the-agenda)))
                  (first-item)
                  (remove-first-agenda-item! the-agenda)
                  (main-loop)))))
  (initialize-simulation)
  (main-loop))

(define (get-current-time) (current-time the-agenda))

(define (make-customer-source chance-customer-enters
                              request-distribution)
  (define (customer-source-action)
    (if (odds chance-customer-enters)
        (make-customer (get-current-time)
                       (pick-random request-distribution)))
    (after-delay 1 customer-source-action))
  (define (me message)
     (error "unknown message -- customer-source" message))
  (after-delay 1 customer-source-action)
  me)

(define (shortest-queue-server server-list)
  (cdr
   (pair-with-smallest-car
    (mapcar (lambda (server)
              (cons (server-queue-length server)
                    server))
            server-list))))


;;low-level routines

(define (odds percentage)
  (> percentage (/ (random 100) 100)))

(define (pick-random x)
  (nth (random (length x)) x))

(define (make-server-list how-many)
  (cond ((= how-many 0) '())
        (else (cons (make-server)
                    (make-server-list (- how-many 1))))))

(define (pair-with-smallest-car pairs)
  (cond
   ((= (length pairs) 1) (car pairs))
   (else (let ((smallest-in-cdr
                (pair-with-smallest-car (cdr pairs))))
           (cond
            ((< (caar pairs) (car smallest-in-cdr))
             (car pairs))
            (else smallest-in-cdr))))))

;;for statistician exercises
(define (remember-service-time statistician time)
  ((statistician 'service-time) time))

(define (remember-waiting-time statistician time)
  ((statistician 'waiting-time) time))

(define (average-service-time statistician)
  (statistician 'average-service))

(define (average-waiting-time statistician)
  (statistician 'average-wait))
