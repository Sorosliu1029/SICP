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

;;; Graphical routines for 9836:  To plot function FOO from -2 to 2 
;;;  every .1 on a graph with a box extending from -3 to 3 in
;;;  the x direction and from 0 to 14 in the y direction, and using
;;;  a line connecting adjacent graph points, say:
;;;      (plot ((box 'line) -3 3 0 14) (graph foo -2 2 .1))

;;; For quick-and-dirty line plotting of the function FOO with 
;;;  auto-ranging for the vertical scale, say:
;;;      (line-plot foo -2 2 .1)

;;; You are not expected to study this code.  Just use
;;; line-plot 


;;; Screen size

(define maxx 255)
(define maxy 194)

(define first-point)

;;; Axis functions

(define ((box plot-type) xlow xhigh ylow yhigh)
  (let ((-maxx (- maxx)) (-maxy (- maxy))
	(xscale (/ (* 2 maxx) (- xhigh xlow)))
	(xoffset (* (/ (+ xhigh xlow) (- xhigh xlow)) maxx))
	(yscale (/ (* 2 maxy) (- yhigh ylow)))
	(yoffset (* (/ (+ yhigh ylow) (- yhigh ylow)) maxy)))
    (define base)
    (position-pen -maxx maxy)
    (draw-line-to -maxx -maxy)
    (draw-line-to maxx -maxy)
    (draw-line-to maxx maxy)
    (draw-line-to -maxx maxy)
    (if (eq? plot-type 'bar)
	(if (< -maxy (- yoffset) maxy)
	    (sequence
	     (set! base (- yoffset))
	     (position-pen -maxx base)
	     (draw-line-to maxx base))
	    (set! base -maxy)))
    (lambda (x y)
      (let ((px (- (* xscale x) xoffset))
	    (py (- (* yscale y) yoffset)))
	(if (and (<= -maxx px maxx) (<= -maxy py maxy))
	    (cond ((eq? plot-type 'point)
		   (draw-point px py))
		  ((eq? plot-type 'bold-point)
		   (draw-bold-point px py))
		  ((eq? plot-type 'bar)
		   (position-pen px base)
		   (draw-line-to px py))
		  ((eq? plot-type 'line)
		   (if first-point
		       (sequence
			(position-pen px py)
			(set! first-point false))
		       (draw-line-to px py)))
		  (else
		   (error "box: unknown plot-type " plot-type))))))))

(define (draw-bold-point px py)
  (draw-point px py)
  (draw-point (1+ px) py)
  (draw-point (-1+ px) py)
  (draw-point px (1+ py))
  (draw-point px (-1+ py)))

;;; Plotting function

(define (plot point-function . curves)
  (set! first-point true)
  (mapc (lambda (curve)
	  (mapc (lambda (point)
		  (apply point-function point))
		curve))
    curves))


;;; Graph generator

(define (graph function low high increment)
  (define real-high (+ high (/ increment 2)))
  (define (iter x ans)
    (if (> x real-high)
	ans
	(iter (+ x increment)
	      (cons (list x (function x))
		    ans)))) 
  (iter low '()))


(define (extent graph)
  (let ((xmin (caar graph)) (xmax (caar graph)) 
	(ymin (cadar graph)) (ymax (cadar graph)))
    (mapc (lambda (point)
	    (if (> xmin (car point)) (set! xmin (car point)))
	    (if (< xmax (car point)) (set! xmax (car point)))
	    (if (> ymin (cadr point)) (set! ymin (cadr point)))
	    (if (< ymax (cadr point)) (set! ymax (cadr point))))
	  (cdr graph))
    (list xmin xmax ymin ymax)))


(define ((tplot type) graph)
  (let ((ext (extent graph)))
    (print ext)
    (plot (apply (box type) ext) graph)))


(define (line-plot function low high increment)
  ((tplot 'line) (graph function low high increment)))

