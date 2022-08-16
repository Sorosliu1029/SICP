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

;;;This is the code for the square-limit language of problem set 3


(define (for-each proc list)
  (cond ((null? list) "done")
        (else (proc (car list))
              (for-each proc (cdr list)))))

(define (coord-map rect)
  (lambda (point)
    (+vect
     (+vect (scale (xcor point)
		   (horiz rect))
	    (scale (ycor point)
		   (vert rect)))
     (origin rect))))

(define (make-picture seglist)
  (lambda (rect)
    (for-each
     (lambda (segment)
       (drawline ((coord-map rect) (seg-start segment))
                 ((coord-map rect) (seg-end segment))))
     seglist)))


(define empty-picture (make-picture '()))

(define (draw pict)
  (clear-graphics)
  (pict screen))

(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rect
	   (+vect (origin rect)
		  (horiz rect))
	   (vert rect)
	   (scale -1 (horiz rect))))))

(define (repeated function n)
  (lambda (thing)
    (if (= n 0) thing
	((repeated function (-1+ n)) (function thing)))))

(define rotate180 (repeated rotate90 2))
(define rotate270 (repeated rotate90 3))

(define (together pict1 pict2)
  (lambda (rect)
    (pict1 rect)
    (pict2 rect)))

(define (flip pict)
  (lambda (rect)
    (pict (make-rect (+vect (origin rect) (horiz rect))
		     (scale -1 (horiz rect))
		     (vert rect)))))

(define (beside pict1 pict2 a)
  (lambda (rect)
    (pict1 (make-rect
	    (origin rect)
	    (scale a (horiz rect))
	    (vert rect)))
    (pict2 (make-rect
	    (+vect (origin rect)
		   (scale a (horiz rect)))
	    (scale (- 1 a) (horiz rect))
	    (vert rect)))))

(define (above pict1 pict2 a)
  (rotate270 (beside (rotate90 pict1)
		     (rotate90 pict2)
		     a)))

(define (4pict pict1 rot1 pict2 rot2 pict3 rot3 pict4 rot4)
  (beside (above ((repeated rotate90 rot1) pict1)
		 ((repeated rotate90 rot2) pict2)
		 .5)
	  (above ((repeated rotate90 rot3) pict3)
		 ((repeated rotate90 rot4) pict4)
		 .5)
	  .5))

(define (4same pict rot1 rot2 rot3 rot4)
  (4pict pict rot1 pict rot2 pict rot3 pict rot4))

(define (up-push pict n)
  (if (= n 0)
      pict
      (above (up-push pict (-1+ n))
	     pict
	     .25))))

(define (right-push pict n)
  (if (= n 0)
      pict
      (beside pict 
	      (right-push pict (-1+ n))
	      .75)))

(define (corner-push pict n)
  (if (= n 0)
      pict
      (above (beside (up-push pict n)
		     (corner-push pict (-1+ n))
		     .75)
	     (beside pict
		     (right-push pict (-1+ n))
		     .75)
	     .25)))

(define (square-limit pict n)
  (4same (corner-push pict n) 1 2 0 3))

;;; Definitions from the Triangles problem set:

(define make-vector cons)
(define xcor car)
(define ycor cdr)

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(define (+vect v1 v2)
  (make-vector (+ (xcor v1) (xcor v2))
	       (+ (ycor v1) (ycor v2))))

(define (-vect v1 v2)
  (+vect v1 (scale -1 v2)))

(define (scale x v)
  (make-vector (* x (xcor v))
	       (* x (ycor v))))

(define (drawline start end)
  (position-pen (xcor start) (ycor start))
  (draw-line-to (xcor end) (ycor end)))

(define (make-rect origin-vect H V)
  (list 'I-am-a-wreckedangle origin-vect H V))

(define origin second)
(define horiz third)
(define vert fourth)
(define outline-picture (make-picture (list (make-segment (make-vector 0 0)
							  (make-vector 0 1))
					    (make-segment (make-vector 0 1)
							  (make-vector 1 1))
					    (make-segment (make-vector 1 1)
							  (make-vector 1 0))
					    (make-segment (make-vector 1 0)
							  (make-vector 0 0)))))

(define (prim-pict list-of-lines)
  (make-picture (mapcar (lambda (line)
			  (make-segment 
			   (make-vector (car line) (cadr line))
			   (make-vector (caddr line) (cadddr line))))
			list-of-lines)))

(define wedge (prim-pict '((0 0 .5 .5) (.5 .5 1 0))))
(define line (prim-pict '((0 .5 1 .5))))
(define tp (prim-pict '((0 1 1 1) (.5 .5 .5 1))))
(define sh (prim-pict '((0 .4 1 .4) (0 .6 1 .6) (.5 .4 .5 .6))))
(define star (prim-pict '((0 .5 .5 0) (.5 0 1 .5) (1 .5 .5 1) (.5 1 0 .5))))
(define pent (prim-pict '((0 0 1 0) 
			  (1 0 1 .6666666666666666)
			  (1 .6666666666666666 .5 1)
			  (.5 1 0 .6666666666666666)
			  (0 .6666666666666666 0 0))))
(define ppent (together pent (prim-pict '((0 .6666666666666666 .5 0)
					  (.5 0 1 .6666666666666666)))))
(define 2ppent (above ppent (rotate180 ppent) .5))

(define screen (make-rect (make-vector -190 -190) 
			  (make-vector 380 0)
			  (make-vector 0 380)))

