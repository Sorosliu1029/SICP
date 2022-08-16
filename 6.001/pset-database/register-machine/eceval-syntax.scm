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

;;;; Syntax for simulation of explicit-control evaluator

;;; represent expressions -- from 4.1.2

(define (self-evaluating? exp) (number? exp))

(define (quoted? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'quote)))

(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'set!)))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'define)))

(define (definition-variable exp)
  (if (variable? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp) 
  (if (variable? (cadr exp))
      (caddr exp)
      (cons 'lambda
	    (cons (cdadr exp)    
		  (cddr exp)))))

(define (lambda? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'lambda)))

(define (conditional? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'cond)))

(define (clauses exp) (cdr exp))

(define (no-clauses? clauses) (null? clauses))

(define (first-clause clauses) (car clauses))

(define (rest-clauses clauses) (cdr clauses))

(define (predicate clause) (car clause))

(define (actions clause) (cdr clause))

(define (true? x) (not (null? x)))

(define (else-clause? clause)
  (eq? (predicate clause) 'else))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))


;;;no-args? added and application? changed (from 5.2.1)

(define (no-args? exp)
  (if (atom? exp)
      nil
      (null? (cdr exp))))

(define (application? exp)
  (if (atom? exp)
      nil
      (not (null? (cdr exp)))))

(define (operator app) (car app))

(define (operands app) (cdr app))


;;;last-operand? added -- from 5.2.1

(define (last-operand? args)
  (null? (cdr args)))

(define (no-operands? args) (null? args))

(define (first-operand args) (car args))

(define (rest-operands args) (cdr args))

(define (make-procedure lambda-exp env)
  (list 'procedure lambda-exp env))

(define (compound-procedure? proc)
  (if (atom? proc)
      nil
      (eq? (car proc) 'procedure)))

(define (parameters proc) (cadr (cadr proc)))

(define (procedure-body proc) (cddr (cadr proc)))

(define (procedure-environment proc) (caddr proc))


;;;operations on environments -- from 4.1.3

(define (lookup-variable-value var env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (binding-value b)
        (error "Unbound variable" var))))

(define (binding-in-env var env)
  (if (no-more-frames? env)
      no-binding
      (let ((b (binding-in-frame var (first-frame env))))
        (if (found-binding? b)
            b
            (binding-in-env var (rest-frames env))))))

(define (extend-environment variables values base-env)
  (adjoin-frame (make-frame variables values) base-env))

(define (set-variable-value! var val env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (set-binding-value! b val)
        (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((b (binding-in-frame var (first-frame env))))
    (if (found-binding? b)
        (set-binding-value! b val)
	(set-first-frame!
	 env
	 (adjoin-binding (make-binding var val)
			 (first-frame env))))))


;;;representing environments -- from 4.1.3

(define (first-frame env) (car env))

(define (rest-frames env) (cdr env))

(define (no-more-frames? env) (null? env))

(define (adjoin-frame frame env) (cons frame env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))


;;;representing frames -- from 4.1.3

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values)) '())
        ((null? variables)
         (error "Too many values supplied" values))
        ((null? values)
         (error "Too few values supplied" variables))
        (else
         (cons (make-binding (car variables)
                             (car values))
               (make-frame (cdr variables)
                           (cdr values))))))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (assq key bindings)
  (cond ((null? bindings) no-binding)
        ((eq? key (binding-variable (car bindings))) (car bindings))
        (else (assq key (cdr bindings)))))

(define (binding-in-frame var frame)
  (assq var frame))

(define (found-binding? b)
  (not (eq? b no-binding)))

(define no-binding nil)


;;;representing bindings -- from 4.1.3

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))
