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

;; For problem 6b, you will not be able to use PLOT-STREAM to plot the elements
;; of the PATIENT-STREAM sequences as you test them for regularity. The reason
;; is that when we generate the PEAK-POSITIONS stream from the BINARIZEd input
;; stream, we effectively compress the stream. Thus, groveling over the first N
;; elements of the stream input to REGULARIZE will not correspond to groveling
;; the first N elements of the original input stream.

;; What you will need to do is rely on the lazy evaluation in streams to create
;; a stream identical to the PATIENT-STREAM except that the first time that an
;; element of this stream of sequences is accessed, it is plotted on the
;; screen. Since each element will be accessed only once, this will suffice for
;; plotting the parts of the PATIENT-STREAM correponding to those parts which
;; get forced by applying REGULARIZE to N elements of the
;; BINARIZEd/PEAK-POSITIONS-ized/etc stream.

;; To that end, we provide the following plotting code. An example of its use
;; follows too, so if this looks perplexing, skip ahead to the discussion.
;; (In fact, you do not need to understand how this works or even really why
;; you need it rather than PLOT-STREAM; all you need to understand is how to
;; use it, which is presented at the end.)

(define (plotizing-stream stream max-y max-no-of-xs)
  (map-stream (make-element-plotter-for-frame max-y max-no-of-xs)
	      stream))

(define (make-element-plotter-for-frame max-y max-no-of-xs)
  (let ((x-scale (* 2 (/ hp-screen-width  max-no-of-xs)))
        (y-scale      (/ hp-screen-height max-y)) )
    (define (screen-y-point y)
      (let ((intended-y (round (* y-scale y))))
        (if (> (abs  intended-y) hp-screen-height)
            (* (sign intended-y) hp-screen-height)
            intended-y)))
    (define (next-x-point)
      (print *screen-x-position*)
      (round (- (* (next-screen-x-position max-no-of-xs) x-scale)
                hp-screen-width)))
    (lambda (y)
      (draw-line-to (next-x-point)
                    (screen-y-point y)))))

(define *screen-x-position* 0)
(define (reset-screen-x-position)
  (set! *screen-x-position* 0)
  (position-pen (- hp-screen-width) 0))

(define (next-screen-x-position max-no-of-xs)
  (let ((current-position *screen-x-position*))
    (set! *screen-x-position* (1+ *screen-x-position*))
    current-position))

(define (reset-plotting)
  (clear-graphics)
  (reset-screen-x-position))

(define (sign x) (if (< x 0) -1 1))

(define hp-screen-width  200)
(define hp-screen-height 180)

;; To use this, say, to create a stream of integers which will plot the
;; integers once each in order as they are forced to evaluate, we would do:

(define plotizing-integers (plotizing-stream integers 10 10))

;; So, for instance, in problem 6b, you will very likely want to do something
;; like:

(define *monitor-max-y*         10)
(define *monitor-max-no-of-xs* 100)

(define (monitor patient-stream tolerance-margin N)
  (let ((plotizing-patient-stream
	 (map-stream (lambda (patient)
		       (plotizing-stream patient
					 *monitor-max-y*
					 *monitor-max-no-of-xs*))
		     patient-stream)))
    ;; TO DO: your monitor
    code that will call (reset-plotting) each time a
    ;; patient is groveled another N CHECK-REGULARITY steps.
    'todo
    ))

;; Thus, whenever you call REGULARIZE on the first N elements of a
;; BINARIZED/PEAK-POSITIONS-ized/etc PLOTIZING-PATIENT sequence, the
;; corresponding elements of the PLOTIZING-PATIENT sequence will (magically) be
;; plotted (well, except for an annoying boundary case with the first element
;; of the input stream...which you should not worry about).