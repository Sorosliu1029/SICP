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

;;;This is the code for problem set 3
;;;It implements a simple drawing language, based on triangles

;;points and segments

(define make-vector cons)
(define xcor car)
(define ycor cdr)

(define zero-vector (make-vector 0 0))

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

;;vector operations

(define (+vect v1 v2)
  (make-vector (+ (xcor v1) (xcor v2))
               (+ (ycor v1) (ycor v2))))

(define (-vect v1 v2)
  (+vect v1 (scale -1 v2)))

(define (scale x v)
  (make-vector (* x (xcor v))
               (* x (ycor v))))

(define (shift v o1 o2)
  (-vect (+vect v o1) o2))

;;drawing lines

(define (drawline start end)
  (position-pen (xcor start) (ycor start))
  (draw-line-to (xcor end) (ycor end)))

;;representing triangles

(define make-triangle list)

(define origin car)
(define side1 cadr)
(define side2 caddr)

;;a triangle defines a map on points

(define (coord-map triangle)
  (lambda (point)
    (+vect
     (+vect (scale (xcor point)
                   (side1 triangle))
            (scale (ycor point)
                   (side2 triangle)))
     (origin triangle))))

;;defining the screen triangle

(define screen-lower-left (make-vector -190 -190))
(define screen-lower-right (make-vector 190 -190))
(define screen-upper-left (make-vector  -190 190))

(define screen-lower-edge
  (-vect screen-lower-right screen-lower-left))

(define screen-left-edge
  (-vect screen-upper-left screen-lower-left))

(define screen-triangle
  (make-triangle screen-lower-left
                 screen-lower-edge
                 (+vect screen-left-edge
                        (scale 0.5 screen-lower-edge))))

;;;making a picture from a list of segments

(define (make-picture seglist)
  (lambda (triangle)
    (for-each
     (lambda (segment)
       (drawline ((coord-map triangle) (seg-start segment))
                 ((coord-map triangle) (seg-end segment))))
     seglist)))

(define (for-each proc list)
  (cond ((null? list) "done")
        (else (proc (car list))
              (for-each proc (cdr list)))))

(define (draw pict)
  (clear-graphics)
  (pict screen-triangle))


;;;repeating an operation

(define (repeated function n)
  (lambda (thing)
    (if (= n 0) thing
	((repeated function (-1+ n)) (function thing)))))


;;;some simple pictures

(define empty-picture (make-picture '()))

(define outline-picture
  (let ((v1 (make-vector 0 0))
	(v2 (make-vector 0 1))
	(v3 (make-vector 1 0)))
    (make-picture (list (make-segment v1 v2)
			(make-segment v2 v3)
			(make-segment v3 v1)))))

(define midpoints
  (let ((center (make-vector (/ 1 3) (/ 1 3)))
	(m1 (make-vector (/ 1 2) 0))
	(m2 (make-vector 0 (/ 1 2)))
	(m3 (make-vector (/ 1 2) (/ 1 2))))
    (make-picture (list (make-segment m1 center)
			(make-segment m2 center)
			(make-segment m3 center)))))

(define band
  (let ((a1 (make-vector .4 0))
        (a2 (make-vector .6 0))
        (b1 (make-vector 0 .4))
        (b2 (make-vector 0 .6)))
    (make-picture (list (make-segment a1 b1)
                        (make-segment a2 b2)))))

(define v-shape
  (let ((m1 (make-vector (/ 2 9) (/ 2 9)))
        (m2 (make-vector (/ 4 9) (/ 4 9)))
        (a1 (make-vector (/ 1 3) 0))
        (a2 (make-vector (/ 2 3) 0))
        (b1 (make-vector 0 (/ 1 3)))
        (b2 (make-vector 0 (/ 2 3))))
    (make-picture (list (make-segment a1 m1)
                        (make-segment m1 b1)
                        (make-segment a2 m2)
                        (make-segment m2 b2)))))


;;one means of combination

(define (split pict1 pict2 ratio)
  (lambda (triangle)
    (define p (scale ratio (side1 triangle)))
    (define oa (origin triangle))
    (define ob (shift p oa zero-vector))
    (pict1 (make-triangle oa p (side2 triangle)))
    (pict2 (make-triangle ob
                          (shift (side1 triangle) oa ob)
                          (shift (side2 triangle) oa ob)))))


