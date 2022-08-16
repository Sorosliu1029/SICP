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

;;; This is the file PS7-CODE.SCM
;;; Useful stream utility functions

(define the-empty-stream '())
(define empty-stream? null?)

(define (singleton x)
  (cons-stream x the-empty-stream))


(define (nth-stream n s)
  (cond ((empty-stream? s)
	 (error "empty stream -- nth-stream" n))
	((= n 0) (head s))
	(else (nth-stream (- n 1) (tail s)))))

(define (accumulate combiner initial-value stream)
  (cond ((empty-stream? stream) initial-value)
	(else
	 (combiner (head stream)
	      (accumulate combiner
                          initial-value
                          (tail stream))))))

(define (filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
	((pred (head stream))
	 (cons-stream (head stream)
		      (filter pred (tail stream))))
	(else (filter pred (tail stream)))))


;;; Mapping functions

(define (map proc stream)
  (cond ((empty-stream? stream) the-empty-stream)
	(else (cons-stream (proc (head stream))
			   (map proc (tail stream))))))

(define (map-2 f s1 s2)
  (cond ((or (empty-stream? s1) (empty-stream? s2))
	 the-empty-stream)
	(else
	 (cons-stream (f (head s1) (head s2))
		      (map-2 f (tail s1) (tail s2))))))



;;; Iterating down a stream.

(define (for-each proc stream)
  (if (empty-stream? stream)
      'done
      (sequence (proc (head stream))
		(for-each proc (tail stream)))))

;;; Enumerators

(define (enumerate-fringe tree)
  (cond ((atom? tree)
	 (cons-stream tree the-empty-stream))
	(else
	 (append-streams (enumerate-fringe (car tree))
			 (enumerate-fringe (cdr tree))))))

(define (enumerate-interval n1 n2)
  (cond ((> n1 n2)
	 the-empty-stream)
	(else
	 (cons-stream n1
		      (enumerate-interval (1+ n1) n2)))))


(define (append-streams s1 s2)
  (cond ((empty-stream? s1) s2)
	(else
	 (cons-stream (head s1)
		      (append-streams (tail s1) s2)))))




;;; Streams of numbers

(define (add-streams s1 s2)
  (map-2 + s1 s2))

(define (mul-streams s1 s2)
  (map-2 * s1 s2))
							
(define (scale-stream c s)
  (map (lambda (x) (* c x)) s))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream dt integrand) int)))
  int)


(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
	 (let ((h1 (head s1))
	       (h2 (head s2)))
	   (cond ((< h1 h2)
		  (cons-stream h1
			       (merge (tail s1)
				      s2)))
		 ((> h1 h2)
		  (cons-stream h2
			       (merge s1
				      (tail s2))))
		 (else
		  (cons-stream h1
			       (merge (tail s1)
				      (tail s2)))))))))

;;; Stream I/O. 

(define print-stream
  (let ()
    (define (iter s)
      (cond ((empty-stream? s)
	     (princ "}"))
	    (else
	     (princ (head s))
	     (princ " ")
	     (iter (tail s)))))
    (lambda (s)
      (princ "{")
      (iter s))))

;;; You may wonder why PRINT-STREAM has been written in such an obscure
;;; way, When Ben Bitdiddle asked Alyssa P. Hacker about this, she thought
;;; for a few hours and then said that, for printing a very long stream,
;;; the more obvious implementation would result in large amounts of
;;; storage being wasted due to the memoizing of the stream being printed.
;;; Unfortunately, Ben has not yet learned about garbage collection, so
;;; her comment doesn't do him much good.  Assign the explanation what is
;;; going on as an extra credit exercise for your TA or recitation
;;; instructor.



(define (plot-stream s max-y num-vals)
  (define (sign x) (if (< x 0) -1 1))
  (define hp-screen-width 200)
  (define hp-screen-height 180)
  (define x-scale (* 2 (/ hp-screen-width num-vals)))
  (define y-scale (/ hp-screen-height max-y))
  (define (screen-x-point x)
    (round (- (* x x-scale)
              hp-screen-width)))
  (define (screen-y-point y)
    (let ((intended-y (round (* y-scale y))))
       (if (> (abs intended-y) hp-screen-height)
          (* (sign intended-y) hp-screen-height)
          intended-y)))
  (define (iter s count)
    (if (> count num-vals)
        'done
        (sequence (draw-line-to (screen-x-point count)
                                (screen-y-point (head s)))
                  (iter (tail s) (1+ count)))))
  (clear-graphics)
  (position-pen (screen-x-point 0)
                (screen-y-point (head s)))
  (iter (tail s) 1))


(define (tty-stream) (cons-stream (read) (tty-stream)))

;;;; Power series arithmetic module.

;;; A power series SIGMA(a[i]*x^i, i=0, +inf) is represented by a
;;; stream of coefficients, {a0 a1 a2 ...}.


(define (add-series s1 s2)
  (map-2 + s1 s2))

(define (sub-series s1 s2)
  (map-2 - s1 s2))

(define ((function series) x)
  (define (next power-x last-approximation terms)
    (let ((approximation (+ last-approximation (* power-x (head terms))))) 
      (cons-stream approximation
		   (next (* x power-x)
			 approximation
			 (tail terms)))))
  (next 1 0 series))


;;; An interesting power series -- must have integers to run it.


(define factorials
  (cons-stream 1 (mul-streams factorials (tail (tail integers)))))

(define (reciprocals stream)
  (map (lambda (x) (/ 1 x)) stream))

(define expt-series
  (cons-stream 1 (reciprocals factorials)))

(define expt-fun (function expt-series))
