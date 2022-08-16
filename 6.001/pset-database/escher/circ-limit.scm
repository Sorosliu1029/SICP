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

;;; This is the code for problem set 3
;;; It implements a simple drawing language, based on circles

;;; circles

(define make-circle list)
(define center car)
(define radius cadr)
(define theta caddr)

;;; vectors

(define make-vector cons)
(define xcor car)
(define ycor cdr)
(define (+vect v1 v2)
  (make-vector (+ (xcor v1) (xcor v2))
               (+ (ycor v1) (ycor v2))))
(define (scale x v)
  (make-vector (* x (xcor v))
               (* x (ycor v))))

;;; chords

(define make-chord cons)
(define chord-start car)
(define chord-end cdr)

;;; drawing lines

(define (drawline v1 v2)
  (position-pen (xcor v1) (ycor v1))
  (draw-line-to (xcor v2) (ycor v2)))

;;; a circle defines a map on points

(define (coord-map circle)
  (lambda (point)
    (let ((angle (theta circle))
          (r (radius circle)))
      (+vect
        (center circle)
        (make-vector (* r (cos (+ angle point)))
                     (* r (sin (+ angle point))))))))

;;; making a screen circle

(define screen-circle (make-circle (make-vector 0 0) 190 0))

;;; making a picture from a list of chords

(define (make-picture chordlist)
  (lambda (circle)
    (for-each
      (drawline ((coord-map circle) (chord-start chord))
                ((coord-map circle) (chord-end chord)))
      chordlist)))

(define (for-each proc lst)
  (cond ((null? lst) "done")
        (else (proc (car lst))
              (for-each proc (cdr lst)))))

(define (draw pict)
  (clear-graphics)
  (pict screen-circle))

;; some simple pictures

(define pi 3.415927)
(define 2pi(* 2 pi))
(define quarter-pi (/ pi 4))
(define p1 quarter-pi)
(define p2 (* 3 quarter-pi))
(define p3 (* 5 quarter-pi))
(define p4 (* 7 quarter-pi))
(define p5 (* 6 quarter-pi))
(define p6 (* 2 quarter-pi))

(define cross (make-picture (list (make-chord p1 p3)
                                  (make-chord p2 p4))))

(define square (make-picture (list (make-chord p1 p2)
                                   (make-chord p2 p3)
                                   (make-chord p3 p4)
                                   (make-chord p4 p1))))

(define triangle (make-picture (list (make-chord p1 p2)
                                     (make-chord p1 p5)
                                     (make-chord p2 p5))))

(define arrow (make-picture (list (make-chord p1 pi)
                                  (make-chord p4 pi))))

;;; repeating an operation

(define(repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (compose f g) (lambda (x) (f (g x))))

;;; one mean of combination

(define (split pict1 pict2 ratio)
  (lambda (circle)
    (let ((r (radius circle))
          (t (theta circle))
          (o (center circle)))
      (define r1 (* ratio r))
      (define r2 (- r r1))
      (define c1 (+vect o (make-vector (* r (- 1 ratio) (cos t))
                                       (* r (- 1 ratio) (sin t)))))
      (define c2 (+vect o (make-vector (* r (- ratio) (cos t))
                                       (* r (- ratio) (sin t)))))
      (pict1 (make-circle c1 r1 t))
      (pict2 (make-circle c2 r2 t)))))

