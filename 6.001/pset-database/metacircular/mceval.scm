;;; -*- Scheme -*-
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



;;; This is the file PS8-EVAL.SCM

;;; It contains the metacircular evaluator, as described in section 4.1 of the
;;; text, with a few minor modifications.

;;; You should just load this file into Scheme without editing it.  The new
;;; procedures that you will need to modify in order to do the problem set have
;;; been copied into in a separate file for your convenience.

;;; SETTING UP THE ENVIRONMENT

;;; We initialize the global environment by snarfing a few primitives from the
;;; underlying scheme system.  This is different from the treatment of
;;; primitives in the book.  (See the comments below under "applying primitive
;;; procedures".)  If you want to add more primitives to your evaluator, just
;;; modify the list PRIMITIVE-PROCEDURE-NAMES to include more Scheme
;;; primitives.

;;; The actual structure of the environment is determined by the constructor
;;; EXTEND-ENVIRONMENT which is listed below together with the code that
;;; manipulates environments.

(define primitive-procedure-names
  '(+ - * / = < > 1+ -1+ cons car cdr atom? eq? null? not))

(define (setup-environment)
  (let ((initial-env
	 (extend-environment primitive-procedure-names
			     (mapcar (lambda (pname)
				       (list 'primitive
					     (eval pname
						   user-initial-environment)))
				     primitive-procedure-names)
			     '())))
    (define-variable! 'false false initial-env)
    (define-variable! 'true true initial-env)
    initial-env))

(define the-global-environment '())


;;; INITIALIZATION AND DRIVER LOOP

;;; To start the metacircular evaluator, call INITIALIZE-EVALUATOR.  This
;;; initializes the global environment, and starts the DRIVER-LOOP.  Use
;;; INITIALIZE-EVALUATOR instead of DRIVER-LOOP if you want to erase any
;;; definitions you have accumulated and start fresh with a clean global
;;; environment


(define (initialize-evaluator)
  (set! the-global-environment (setup-environment))
  (driver-loop))

;;; The driver loop reads an expression, evaluates it in the global
;;; environment, and prints the result.  Note that the driver uses a prompt of
;;; "MC-EVAL==> " to help you avoid confusing typing to the metacircular
;;; evaluator with typing to the underlying SCHEME interpreter.  (Also, we have
;;; called our evaluator procedures MINI-EVAL and MINI-APPLY to help you avoid
;;; confusing them with Scheme's EVAL and APPLY.)

;;; When/If your interaction with the evaluator bombs out in an error, you
;;; should restart it by calling DRIVER-LOOP again.

(define (driver-loop)
  (newline)
  (princ "MC-EVAL==> ")
  (user-print (mini-eval (read-from-keyboard) the-global-environment))
  (driver-loop))

;;; We use a special PRINT here, which avoids printing the environment part of
;;; a compound procedure, since the latter is a very long (or even circular)
;;; list.

(define (user-print object)
  (cond ((compound-procedure? object)
         (print (list 'compound-procedure
                      (parameters object)
                      (procedure-body object)
                      'procedure-env)))
        (else (print object))))

;;; THE CORE OF THE EVALUATOR -- from section 4.1.1

(define (mini-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((variable? exp) (lookup-variable-value exp env))
        ((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((lambda? exp) (make-procedure exp env))
        ((conditional? exp) (eval-cond (clauses exp) env))
        ((application? exp)
         (mini-apply (mini-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (mini-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (parameters procedure)
                         arguments
                         (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (mini-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps)
                                    env)))))

(define (eval-cond clist env)
  (cond ((no-clauses? clist) false)
        ((else-clause? (first-clause clist))
         (eval-sequence (actions (first-clause clist))
                        env))
        ((true? (mini-eval (predicate (first-clause clist)) env))
         (eval-sequence (actions (first-clause clist))
                        env))
        (else (eval-cond (rest-clauses clist) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mini-eval (first-exp exps) env))
        (else (mini-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (let ((new-value (mini-eval (assignment-value exp) env)))
    (set-variable-value! (assignment-variable exp)
                         new-value
                         env)
    new-value))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mini-eval (definition-value exp) env)
                    env)
  (definition-variable exp))

;;; Syntax of the language -- from section 4.1.2

(define (self-evaluating? exp) (number? exp))

(define (quoted? exp)
  (if (atom? exp)
      false
      (eq? (car exp) 'quote)))

(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (if (atom? exp)
      false
      (eq? (car exp) 'set!)))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (if (atom? exp)
      false
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
      false
      (eq? (car exp) 'lambda)))


(define (conditional? exp)
  (if (atom? exp)
      false
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

(define (application? exp) (not (atom? exp)))

(define (operator app) (car app))

(define (operands app) (cdr app))

(define (no-operands? args) (null? args))

(define (first-operand args) (car args))

(define (rest-operands args) (cdr args))

(define (make-procedure lambda-exp env)
  (list 'procedure lambda-exp env))

(define (compound-procedure? proc)
  (if (atom? proc)
      false
      (eq? (car proc) 'procedure)))


(define (parameters proc) (cadr (cadr proc)))

(define (procedure-body proc) (cddr (cadr proc)))

(define (procedure-environment proc) (caddr proc))


;;; APPLYING PRIMITIVE PROCEDURES

;;; The mechanism for applying primitive procedures is somewhat different from
;;; the one given in section 4.1.4 of the text.  The modification is as
;;; suggested in exercise 4.8 of the text.  Instead of representing a primitive
;;; as a list
;;;  (primitive <name-of-primitive>)
;;; we represent it as
;;;  (primitive <Scheme procedure to apply>)

(define (primitive-procedure? proc)
  (if (atom? proc)
      false
      (eq? (car proc) 'primitive)))

(define (primitive-id proc) (cadr proc))

;;; To apply a primitive procedure, we ask the underlying Scheme system to
;;; perform the application.  (Of course, an implementation on a low-level
;;; machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply (primitive-id p) args))



;;; ENVIRONMENTS -- from section 4.1.3

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



(define (first-frame env) (car env))

(define (rest-frames env) (cdr env))

(define (no-more-frames? env) (null? env))

(define (adjoin-frame frame env) (cons frame env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))

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

(define no-binding false)

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))