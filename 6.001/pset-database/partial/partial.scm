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

;
; 6.001 PS#4, Partial Evaluator for Numeric Expressions
; 3/01/84 17:09:38 DKG
;


(define (partial-eval exp)
  (cond ((self-evaluating? exp)
	 exp)
	((variable? exp)
	 (let ((val (system-eval exp)))
	   (cond ((unknown? val)
		  exp)
		 (else val))))
	((if? exp)
	 (let ((p (partial-eval (if-predicate exp))))
	   (cond ((known? p)
		  (cond (p (partial-eval (if-consequent exp)))
			(else (partial-eval (if-alternate exp)))))
		 (else (make-if
			     p
			     (partial-eval (if-consequent exp))
			     (partial-eval (if-alternate exp)))))))
	((application? exp)
	 (partial-apply (operator exp)
			(partial-eval-list (operands exp))))
	(else (error "Unknown expression type -- PARTIAL-EVAL" exp))))

(define (partial-apply procedure arguments)
	(cond ((list-known? arguments)
	       (system-eval (cons procedure arguments)))
	      (else (cons procedure arguments))))


(define (self-evaluating? exp) (number? exp))

(define (if? exp) (eq? (car exp) 'if))
(define (if-predicate if-exp) (second if-exp))
(define (if-consequent if-exp) (third if-exp))
(define (if-alternate if-exp) (fourth if-exp))
(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (variable? exp) (atom? exp))

(define (application? exp) (list? exp))
(define (operator application-exp) (car application-exp))
(define (operands application-exp) (cdr application-exp))

(define (system-eval exp) (eval exp user-initial-environment)) 

(define (unknown) 'unknown)
(define (known? exp) (or (eq? T exp) (null? exp) (number? exp)))
(define (unknown? exp) (not (known? exp)))

; LIST-KNOWN? returns T if all of the elements of LISTc-EXP are known
; NIL otherwise
(define (list-known? list-exp)
	(cond ((null? list-exp) T)
	      (else (and (known? (car list-exp))
			 (list-known? (cdr list-exp))))))

; PARTIAL-EVAL-LIST applies PARTIAL-EVAL to all elements
; of the list EXP-LIST and returns the resulting list
(define (partial-eval-list exp-list)
	(cond ((null? exp-list) exp-list)
	      (else (cons (partial-eval (car exp-list))
			  (partial-eval-list (cdr exp-list))))))

;
; COND defintions
;
(define (cond? expression)
	(if (list? expression) (eq? (car expression) 'cond) NIL))
(define (make-cond list-of-clauses) (cons 'cond list-of-clauses))
(define (cond-clauses cond-expression) (cdr cond-expression))

(define (make-clause predicate consequent) (list predicate consequent))
(define (clause-predicate clause) (first clause))
(define (clause-consequent clause) (second clause))

(define (check-first-clause-true list-of-clauses)
  (if (null? list-of-clauses)
      nil
      (let ((first-clause (car list-of-clauses)))
        (if (and (known? (clause-predicate first-clause))
                 (clause-predicate first-clause))
            (clause-consequent first-clause)
            (make-cond list-of-clauses)))))

;
; Partial Data Base of the number of arguments that
;   primitive procedures require
;

(define (make-proc-def operator count) (list operator count))
(define (proc-def-op proc-def) (first proc-def))
(define (proc-def-args proc-def) (second proc-def))

(define PROCEDURE-DEFS (list 
			(make-proc-def '+ 2)
			(make-proc-def '- 2)
			(make-proc-def '* 2)
			(make-proc-def '/ 2)
			(make-proc-def '= 2)
			(make-proc-def 'and 2)
			(make-proc-def 'not 1)
			(make-proc-def 'or 2)
			(make-proc-def '-1+ 1)
			(make-proc-def 'quotient 2)
			(make-proc-def 'remainder 2)
			(make-proc-def 'integer-divide 2)
			(make-proc-def 'abs 1)
			(make-proc-def 'max 2)
			(make-proc-def 'print 1)
			(make-proc-def 'min 2)
			(make-proc-def 'sqrt 1)
			))


; need this so ELSE clause of COND is taken

(DEFINE ELSE T)