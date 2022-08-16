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

;;; This is the code for Problem Set 7 

(define print-stream
  (let ()
    (define (loop rest)
      (if (empty-stream? rest)
	  (princ ")")
	  (sequence
	   (princ (head rest))
	   (princ " ")
	   (loop (tail rest)))))
    (lambda (s)
      (print "([STREAM] ")
      (loop s))))

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
	((empty-stream? s2) s1)
	(else
	 (cons-stream (+ (head s1) (head s2))
		      (add-streams (tail s1) (tail s2))))))

(define (scale-stream constant s)
  (map (lambda (x) (* constant x)) s))

(define (filter pred s)
  (if (empty-stream? s)
      the-empty-stream
      (if (pred (head s))
	  (cons-stream (head s) (filter pred (tail s)))
	  (filter pred (tail s)))))

(define (map proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (head s))
		   (map proc (tail s)))))

(define (random-bounded lower upper)
  (let ((x (random upper)))
    (if (< x lower)
	(random-bounded lower upper)
	x)))

(define (flip-a-coin)
 (= (random  2) 0))

(define (select-one-of-four)
  (random 4))

(define (get-patient-heart-signal)
  (make-signal (random-bounded 7 14) (random-bounded 10 100)))

(define (make-signal n noise-bound)
  (let ((interval (random-bounded 7 20))
	(speed-variation 0))
    (let ((tolerance (random-bounded 1 interval))
	  (make-random (select-one-of-four)))
      (define (loop count)
	(let ((noise (/ (random noise-bound) 100)))
          (if (= make-random 0)
	      (if (not (= count interval))
		  (cond ((= (select-one-of-four) 0)
			 (set! count 0))
			((= (select-one-of-four) 1)
			 (set! count (+ 7 count))))))
	  (cond ((= count 0)
		 (cons-stream (if (flip-a-coin)
				  (- (+ n 1) noise)
				  (+ (+ n 1) noise))
			      (cond ((= interval 0)(loop interval))
				    (else
				     (set! interval (+ interval
						       speed-variation))
				     (loop interval)))))
		((= count tolerance)
		 (if (flip-a-coin)
		     (loop (- count 1))
		     (cons-stream (if (flip-a-coin)
				      (- 0 noise)
				      noise)
				  (loop (- count 1)))))
		(else
		 (cons-stream (if (flip-a-coin)
				  (- 0 noise)
				  noise)
			      (loop (- count 1)))))))
      (if (not (= make-random 0))
	  (cond ((> make-random 1)          ;;; speed up/slow down OR normal
		 (cond ((= make-random 2)
			(set! interval 7)      ;;; Speeding up
			(set! speed-variation (random-bounded 1 4)))
		       (else                   ;;; Slowing down
			(set! interval 21)
			(set! speed-variation (- (random-bounded 1 4))))))))
      (loop interval))))

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