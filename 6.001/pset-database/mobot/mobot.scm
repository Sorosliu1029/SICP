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

;;; This is the file ps6-adv.scm

;;; A non-moving object is also a procedure

(define (make-thing name)
  (let ((possessor 'no-one))
    (define (thing m)
      (cond ((eq? m 'identity) name)
	    ((eq? m 'type) 'thing)
	    ((eq? m 'change-possessor)
	     (lambda (new-owner)
	       (set! possessor new-owner)))
	    (else (error "I don't know how to do this - make-thing" m))))
    thing))

;;; Abstractions for everyone

(define (make-status-descriptor pos dir) (cons pos dir))
(define position car)
(define orientation cdr)
(define (make-vector x y) (cons x y))
(define xcoord car)
(define ycoord cdr)
(define (add-vectors v1 v2)
  (make-vector (+ (xcoord v1) (xcoord v2))
               (+ (ycoord v1) (ycoord v2))))
(define construct-scene cons)
(define distance car)
(define object cdr)
(define set-position! set-car!)
(define set-orientation! set-cdr!)

;;; Global Procedures for walking and turning

(define (remember-position my-pos)
  (let ((x (xcoord my-pos))
	(y (ycoord my-pos)))
    (make-vector x y)))
    
(define (choose-direction)
  (list-ref (list 'up 'down 'left 'right)
	    (random 4)))

(define (clear? scene)
  (> (distance scene) 1))

(define (walk! status old-pos who)
  (let ((new-pos (add-vectors (position status)
			     (orientation status))))
    (set-position! status new-pos)
    (print who)
    (princ " walked from ")
    (princ old-pos)
    (princ " to ")
    (princ new-pos)))

(define (dir-to-vector direction)
  (cond ((eq? direction 'up) (make-vector 0 1))
	((eq? direction 'down) (make-vector 0 -1))
	((eq? direction 'left) (make-vector -1 0))
	((eq? direction 'right) (make-vector 1 0))
	(else (error "unknown direction - dir-to-vector" direction))))

(define (turn! status direction who)
  (let ((new-orientation (dir-to-vector direction)))
    (set-orientation! status new-orientation)
    (print who)
    (princ " turned ")
    (princ direction)))

(define (announce-scene who scene)
  (print who)
  (princ " sees ")
  (if (or (eq? (object scene) 'empty)
	  (eq? (object scene) 'unknown))
      (princ (object scene))
      (princ ((object scene) 'identity)))
  (princ " at ")
  (princ (distance scene))
  (princ " meters away."))

;;; Updating the world as mobots and foreign objects walk by

(define (update-world! old-pos new-pos object)
  ((world 'store! old-pos) 'empty)
  ((world 'store! new-pos) object))

;;; Look-around procedure for all moving objects

(define (look-around-world status limit-of-distance)
  (let ((my-place (position status))
	(my-orientation (orientation status)))
    (define (loop count place)
      (if (> count limit-of-distance)
	  (construct-scene limit-of-distance 'unknown)
	  (let ((thing (world 'look! place)))
	    (if (not (eq? thing 'empty))
		(construct-scene count thing)
		(loop (+ count 1)
		      (add-vectors place my-orientation))))))
    (loop 1 (add-vectors my-place my-orientation))))

;;;  Each MOBOT's internal visual memory is represented by a map, which shows a
;;;  portion of space that has been explored by the MOBOT.  An 
;;;  instance of the map is a message acceptor that interfaces 
;;;  requests to read and store in the map to the underlying table
;;;  representation.  The world is also represented by such a map (see the
;;;  definition of the world.)

(define (make-map rep min-known-x min-known-y max-known-x max-known-y)
  (define (extend-if-necessary x y)
    (cond ((< x min-known-x)
	   (rep-extend-x rep (- x min-known-x))
	   (set! min-known-x x))
	  ((> x max-known-x)
	   (rep-extend-x rep (- x max-known-x))
	   (set! max-known-x x))
	  ((< y min-known-y)
	   (rep-extend-y rep (- y min-known-y))
	   (set! min-known-y y))
	  ((> y max-known-y)
	   (rep-extend-y rep (- y max-known-y))
	   (set! max-known-y y))))
  (define (map-accessor access-type point)
    (let ((x (xcoord point)) (y (ycoord point)))
      (extend-if-necessary x y)
      (cond ((eq? access-type 'look!)
	     (rep-lookup rep 
			 (- x min-known-x)
			 (- y min-known-y)))
	    ((eq? access-type 'store!)
	     (lambda (thing)
	       (rep-store! rep
			   thing
			   (- x min-known-x)
			   (- y min-known-y))))
	    (else (error "Unknown message" access-type)))))
  map-accessor)

;;; The map representation is as a headed list of headed lists.  
;;;  Each sublist represents a y-direction slice for a particular x.

(define (make-map-rep)			;each map starts out with 1 empty cell.
  (list '*columns*
	(list '*rows* 'empty)))

(define (rep-extend-x rep amount-to-extend)
  (define (make-new-column)
    (cons '*rows*
	  (mapcar (lambda (x) 'unknown)
		  (cdadr rep))))
  (cond ((< amount-to-extend 0)
	 (repeat (- 0 amount-to-extend)
		 (lambda ()
		   (set-cdr! rep
			     (cons (make-new-column)
				   (cdr rep))))))
	((> amount-to-extend 0)
	 (repeat amount-to-extend
		 (lambda ()
		   (set-cdr! (last rep)
			     (cons (make-new-column) '())))))
	(else (error "Bad extend in x" amount-to-extend))))

(define (rep-extend-y rep amount-to-extend)
  (cond ((< amount-to-extend 0)
	 (repeat (- 0 amount-to-extend)
		 (lambda ()
		   (for-each (lambda (column)
			       (set-cdr! column
					 (cons 'unknown (cdr column))))
			   (cdr rep)))))
	((> amount-to-extend 0)
	 (repeat amount-to-extend
		 (lambda ()
		   (for-each (lambda (column)
			       (set-cdr! (last column)
					 (cons 'unknown '())))
			   (cdr rep)))))
	(else (error "Bad extend in x" amount-to-extend))))

(define (rep-lookup map i j)
  (list-ref (cdr (list-ref (cdr map) i)) j))

(define (rep-store! map thing i j)
  (set-car! (list-tail (cdar (list-tail (cdr map) i)) j)
	    thing))

;;; Utilities 

(define (for-each f lst)
  (cond ((null? lst) 0)
	(else (f (car lst))
	      (for-each f (cdr lst)))))

(define (member? name lst)
  (if (eq? name ((car lst) 'identity))
      #!true
      (member? name (cdr lst))))

(define (delete o possessions)
  (let ((answer nil))
    (for-each (lambda (element)
		(if (not (eq? element o))
		    (set! answer (cons element answer))))
	      possessions)))
    
(define (repeat n proc)
  (if (= n 0)
      0
      (sequence (proc)
		(repeat (- n 1)
			proc))))

