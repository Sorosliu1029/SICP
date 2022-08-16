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

;;; This is the file ps4-3vector.scm
;;;; 3vector arithmetic

(define (add v1 v2)
  (make-3vector (+ (xcoord v1) (xcoord v2))
                (+ (ycoord v1) (ycoord v2))
                (+ (zcoord v1) (zcoord v2))))

(define (sub v1 v2)
  (make-3vector (- (xcoord v1) (xcoord v2))
                (- (ycoord v1) (ycoord v2))
                (- (zcoord v1) (zcoord v2))))

(define (scale c v)
  (make-3vector (* c (xcoord v))
                (* c (ycoord v))
                (* c (zcoord v))))


(define (magnitude v)
  (sqrt (+ (square (xcoord v))
           (square (ycoord v))
           (square (zcoord v)))))


(define (cross v1 v2)
  (make-3vector (- (* (ycoord v1) (zcoord v2))
                   (* (zcoord v1) (ycoord v2)))
                (- (* (zcoord v1) (xcoord v2))
                   (* (xcoord v1) (zcoord v2)))
                (- (* (xcoord v1) (ycoord v2))
                   (* (ycoord v1) (xcoord v2)))))



;;; 3vector representation

(define make-3vector list)
(define xcoord car)
(define ycoord cadr)
(define zcoord caddr)


(define (square x) (* x x))


;;; The zero vector

(define zero (make-3vector 0 0 0))
