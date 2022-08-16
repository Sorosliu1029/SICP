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

;;;; 6.001 Fall 1989 PS3

;; Star and spot abstractions

(define make-star
  (lambda (star-name star-position star-magnitude)
    (list star-name star-position star-magnitude)))

(define make-spot
  (lambda (spot-number spot-position spot-brightness)
    (list spot-number spot-position spot-brightness)))

(define make-position
  (lambda (x y) (list x y)))

;; Trial lists of stars and spots

(define trial-stars-1
  (list (make-star 'alpha (make-position 0.0 1.0) 0.0)
	(make-star 'beta  (make-position 0.0 0.0) 0.5)
	(make-star 'gamma (make-position 1.5 1.0) 1.0)))

(define trial-spots-1
  (list (make-spot 1 (make-position 1.03 1.97) 0.2)
        (make-spot 2 (make-position 1.32 3.75) 1.2)
        (make-spot 3 (make-position 0.25 2.75) 0.3)))

;; Initial version of procedure to find all interpretations of an image given a
;; star chart.

(define (all-interpretations image chart)
  (if (null? image)
      (list the-empty-interpretation)
      (let ((spot-to-match (first image))
	    (spots (rest image)))
	(define (match-all stars)
	  (if (null? stars)
	      the-empty-list
	      (append (extend-interpretations
		       (pair-up spot-to-match (car stars))
		       (all-interpretations spots
					    (delete (car stars) chart)))
		      (match-all (cdr stars)))))
	(match-all chart))))

(define (extend-interpretations pair interpretations)
  (define (extend-all interps)
    (if (null? interps)
	the-empty-list
	(cons (augment-interpretation pair (car interps))
	      (extend-all (cdr interps)))))
  (extend-all interpretations))


;; Data abstraction implemementations

(define (delete obj list)
  (cond ((null? list) list)
	((equal? obj (car list)) (cdr list))
	(else
	 (cons (car list)
	       (delete obj (cdr list))))))
	 

(define the-empty-list '())
(define rest cdr)

(define the-empty-interpretation the-empty-list)

(define augment-interpretation cons)

(define pair-up list)
(define pair-spot car)
(define pair-star cadr)

;; Procedures for printing results

(define print-matches
  (lambda (matches)
    (if (null? matches)
	'done
      (sequence (newline)
		(print-match (car matches))
		(print-matches (cdr matches))))))

(define print-match
  (lambda (match)
    (if (null? match)
	(newline)
      (let ((spot (pair-spot (car match)))
	    (star (pair-star (car match))))
	(newline)
	(princ "(star ")
	(princ (star-name star))
	(princ " = spot ")
	(princ (spot-number spot))
	(princ ")")
	(print-match (cdr match))))))

;; Procedure which finds the transformation between the positions of two stars
;; and two points.

(define compute-transformation
  (lambda (xa ya xb yb ua va ub vb)
    (let ((dx (- xb xa))
          (dy (- yb ya))
          (du (- ub ua))
          (dv (- vb va))
          (xm (/ (+ xb xa) 2))
          (ym (/ (+ yb ya) 2))
          (um (/ (+ ub ua) 2))
          (vm (/ (+ vb va) 2)))
      (let ((pr (dot-and-cross dx dy du dv)))
        (let ((c (car pr))
              (s (cdr pr)))
           (let ((x0 (- xm (- (* c um) (* s vm))))
                 (y0 (- ym (+ (* s um) (* c vm)))))
              (list c s x0 y0)))))))

(define dot-and-cross
   (lambda (dx dy du dv)
      (let ((kc (+ (* dx du) (* dy dv)))
            (ks (- (* dy du) (* dx dv))))
        (normalize-c-s kc ks))))

(define normalize-c-s
  (lambda (kc ks)
     (if (and (zero? kc) (zero? ks))
         (error "KC = 0 and KS = 0")
         (let ((k (sqrt (+ (* kc kc) (* ks ks)))))
            (cons (/ kc k) (/ ks k))))))


(define map-uv-to-xy
   (lambda (u v trans)
      (let ((c (first trans))
            (s (second trans))
            (x0 (third trans))
            (y0 (fourth trans)))
         (cons (+ (- (* c u) (* s v)) x0)
               (+ (+ (* s u) (* c v)) y0)))))

;; Measures of the errors we'll tolerate

(define magnitude-tolerance 1.0)

(define distance-tolerance 0.5)