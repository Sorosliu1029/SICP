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

;;; This is the first code file for the series-parallel circuit problem set
;;; Modified by WELG to be consistent with proper dispatching

(define (make-resistor resistance)
  (attach-type 'resistor resistance))

(define (resistor? ckt)
  (eq? (type ckt) 'resistor))

(define (make-series ckt1 ckt2)
  (attach-type 'series-combination (list ckt1 ckt2)))

(define (series-combination? ckt)
  (eq? (type ckt) 'series-combination))

(define (make-parallel ckt1 ckt2)
  (attach-type 'parallel-combination (list ckt1 ckt2)))

(define (parallel-combination? ckt)
  (eq? (type ckt) 'parallel-combination))

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

(define (resistance-resistor resistor)
  resistor)

(define (conductance-resistor resistor)
  (/ 1 (resistance-resistor resistor)))

(define (conductance-parallel ckt)
  (+ (conductance (left-branch ckt))
     (conductance (right-branch ckt))))

(define (conductance-series ckt)
  (/ 1 (resistance-series ckt)))

(define (resistance-series ckt)
  (+ (resistance (left-branch ckt))
     (resistance (right-branch ckt))))

(define (resistance-parallel ckt)
  (/ 1 (conductance-parallel ckt)))

(define (left-branch ckt)
  (car ckt))

(define (right-branch ckt)
  (cadr ckt))

;;;Type stuff the same as before
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
