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

;;;This is the second code file for the series-parallel circuit problem set
;;;It contains the improved version of the series-parallel cuircuit
;;;program, which handles capacitors and inductors

;;; Primitive electrical elements.

(define (make-resistor resistance)
  (let ((r (make-complex resistance 0)))
    (attach-type 'resistor
                 (list (list 'resistance resistance)    ;for documentation
                       (lambda (s) r)))))

(define (resistor? ckt)
  (eq? (type ckt) 'resistor))

(define (resistance-resistor resistor)
  (cadr resistor))

(define (conductance-resistor resistor)
  (lambda (s) (recip ((resistance-resistor resistor) s))))


(define (make-capacitor capacitance)
  (let ((c (make-complex capacitance 0)))
    (attach-type 'resistor
                 (list (list 'capacitance capacitance)
                       (lambda (s)
                         (recip (*c c s)))))))

(define (make-inductor inductance)
  (let ((l (make-complex inductance 0)))
    (attach-type 'resistor
                 (list (list 'inductance inductance)
                       (lambda (s) (*c l s))))))


;;; Compound electrical elements
;;; The constructors and selectors are the same as in ps5-res

(define (make-series ckt1 ckt2)
  (attach-type 'series-combination (list ckt1 ckt2)))

(define (series-combination? ckt)
  (eq? (type ckt) 'series-combination))

(define (make-parallel ckt1 ckt2)
  (attach-type 'parallel-combination (list ckt1 ckt2)))

(define (parallel-combination? ckt)
  (eq? (type ckt) 'parallel-combination))

(define (left-branch ckt)
  (car ckt))

(define (right-branch ckt)
  (cadr ckt))


;;These are similar to ps5-res, except for the use of complex-number
;;arithmetic 

(define (conductance-parallel ckt)
  (lambda (s)
    (+c ((conductance (left-branch ckt)) s)
        ((conductance (right-branch ckt)) s))))

(define (conductance-series ckt)
  (lambda (s)
    (recip ((resistance-series ckt) s))))

(define (resistance-series ckt)
  (lambda (s)
    (+c ((resistance (left-branch ckt)) s)
        ((resistance (right-branch ckt)) s))))

(define (resistance-parallel ckt)
  (lambda (s)
    (recip ((conductance-parallel ckt) s))))


;;; Generic operations.
;;; same as ps5-res

(define (resistance ckt)
  (cond ((resistor? ckt)
	 (resistance-resistor (contents ckt)))
	((parallel-combination? ckt)
	 (resistance-parallel (contents ckt)))
	((series-combination? ckt)
	 (resistance-series (contents ckt)))
	(else
	 (error "Unknown circuit type -- RESISTANCE" ckt))))

(define (conductance ckt)
  (cond ((resistor? ckt)
	 (conductance-resistor (contents ckt)))
	((parallel-combination? ckt)
	 (conductance-parallel (contents ckt)))
	((series-combination? ckt)
	 (conductance-series (contents ckt)))
	(else
	 (error "Unknown circuit type -- CONDUCTANCE" ckt))))


;;;complex number arithmetic

(define (+c z1 z2)
  (make-complex (+ (real-part z1) (real-part z2))
                (+ (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-complex (- (* (real-part z1) (real-part z2))
                   (* (imag-part z1) (imag-part z2)))
                (+ (* (real-part z1) (imag-part z2))
                   (* (imag-part z1) (real-part z2)))))

(define (recip z)
  (define very-small-number 1.0e-30)   ;e indicates exponential notation
  (define very-big-number 1.0e+30)
  (let ((magsq (+ (square (real-part z)) (square (imag-part z)))))
    (if (< magsq very-small-number)     ;To prevent division by zero.
        (make-complex very-big-number 0)
        (make-complex (/ (real-part z) magsq)
                      (/ (- (imag-part z)) magsq)))))


(define (make-complex r i) (cons r i))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))


(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))


(define (angle z)
  (atan (imag-part z) (real-part z)))


(define (square x) (* x x))



;;; Type system for analysis tools
;;; same as in ps5-res

(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (if (not (atom? datum))
      (car datum)
      (error "Bad typed datum -- TYPE" datum)))

(define (contents datum)
  (if (not (atom? datum))
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))
