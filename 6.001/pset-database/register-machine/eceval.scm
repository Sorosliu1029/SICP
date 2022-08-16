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

;;;; 6.001 Explicit-Control Register-Machine Evaluator

;;;from 4.1.4, with expanded set of primitive procedures

(define (setup-environment)
  (let ((initial-env
    (extend-environment primitive-procedure-names
                        primitive-procedure-objects
                        '())))
  (define-variable! 'nil nil initial-env)
  (define-variable! 't (not nil) initial-env)
  initial-env))

(define (primitive-procedure? proc)
  (if (atom? proc)
      nil
      (eq? (car proc) 'primitive)))

(define (primitive-id proc) (cadr proc))


(define primitive-procedure-names
  '(car cdr cons eq? atom? null? + - * / < > =))

(define primitive-procedure-objects
  '((primitive car)
    (primitive cdr)
    (primitive cons)
    (primitive eq?)
    (primitive atom?)
    (primitive null?)
    (primitive +)
    (primitive -)
    (primitive *)
    (primitive /)
    (primitive <)
    (primitive >)
    (primitive =)
    ))

(define the-global-environment (setup-environment))

;;;from 5.2.1
(define (apply-primitive-procedure p args)
  (apply (eval (primitive-id p) user-initial-environment)
         (reverse args)))

(define (make-bindings proc args)
  (extend-binding-environment (parameters proc)
                              args
                              (procedure-environment proc)))

(define (extend-binding-environment vars args env)
  (extend-environment vars (reverse args) env))

;;;from 5.3.6

(define (user-print object)
  (cond ((compound-procedure? object)
         (print (list 'compound-procedure
                      (parameters object)
                      (procedure-body object)
                      '[procedure-env])))
        ((compiled-procedure? object)
         (print '[compiled-procedure]))
        (else (print object))))

;; with stuff from 5.3.6 for compiler-eceval interface

(define-machine explicit-control-evaluator
  (registers exp env val continue fun argl unev)
  (controller
read-eval-print-loop
  (perform (initialize-stack))
  (perform (newline))
  (perform (princ 'EC-EVAL==>))
  (assign exp (read))
  (assign env the-global-environment)
  (assign continue print-result)
  (goto eval-dispatch)
print-result
  (perform (user-print (fetch val)))
  (goto read-eval-print-loop)
unknown-procedure-type-error
  (assign val 'unknown-procedure-type-error)
  (goto signal-error)

unknown-expression-type-error
  (assign val 'unknown-expression-type-error)
  (goto signal-error)

signal-error
  (perform (user-print (fetch val)))
  (goto read-eval-print-loop)

external-entry
   (perform (initialize-stack))
   (assign env the-global-environment)
   (assign continue print-result)
   (save continue)
   (goto (fetch val))

eval-dispatch
  (branch (self-evaluating? (fetch exp)) ev-self-eval)
  (branch (quoted? (fetch exp)) ev-quote)
  (branch (variable? (fetch exp)) ev-variable)
  (branch (definition? (fetch exp)) ev-definition)
  (branch (assignment? (fetch exp)) ev-assignment)
  (branch (lambda? (fetch exp)) ev-lambda)
  (branch (conditional? (fetch exp)) ev-cond)
  (branch (no-args? (fetch exp)) ev-no-args)
  (branch (application? (fetch exp)) ev-application)
  (goto unknown-expression-type-error)
ev-self-eval
  (assign val (fetch exp))
  (goto (fetch continue))
ev-quote
  (assign val (text-of-quotation (fetch exp)))
  (goto (fetch continue))
ev-variable
  (assign val
          (lookup-variable-value (fetch exp) (fetch env)))
  (goto (fetch continue))
ev-lambda
  (assign val (make-procedure (fetch exp) (fetch env)))
  (goto (fetch continue))
ev-no-args
  (assign exp (operator (fetch exp)))
  (save continue)
  (assign continue setup-no-arg-apply)
  (goto eval-dispatch)
setup-no-arg-apply
  (assign fun (fetch val))
  (assign argl '())
  (goto apply-dispatch)
ev-application
  (assign unev (operands (fetch exp)))
  (assign exp (operator (fetch exp)))
  (save continue)
  (save env)
  (save unev)
  (assign continue eval-args)
  (goto eval-dispatch)
eval-args
  (restore unev)
  (restore env)
  (assign fun (fetch val))
  (save fun)
  (assign argl '())
  (goto eval-arg-loop)

eval-arg-loop
  (save argl)
  (assign exp (first-operand (fetch unev)))
  (branch (last-operand? (fetch unev)) eval-last-arg)
  (save env)
  (save unev)
  (assign continue accumulate-arg)
  (goto eval-dispatch)
accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (assign unev (rest-operands (fetch unev)))
  (goto eval-arg-loop)
eval-last-arg
  (assign continue accumulate-last-arg)
  (goto eval-dispatch)
accumulate-last-arg
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (restore fun)
  (goto apply-dispatch)

apply-dispatch
  (branch (primitive-procedure? (fetch fun)) primitive-apply)
  (branch (compound-procedure? (fetch fun)) compound-apply)
  (branch (compiled-procedure? (fetch fun)) compiled-apply)
  (goto unknown-procedure-type-error)
compiled-apply
   (assign val (compiled-procedure-entry (fetch fun)))
   (goto (fetch val))
primitive-apply
  (assign val
          (apply-primitive-procedure (fetch fun)
                                     (fetch argl)))
  (restore continue)
  (goto (fetch continue))
compound-apply
  (assign env (make-bindings (fetch fun) (fetch argl)))
  (assign unev (procedure-body (fetch fun)))
  (goto eval-sequence)
eval-sequence
  (assign exp (first-exp (fetch unev)))
  (branch (last-exp? (fetch unev)) last-exp)
  (save unev)
  (save env)
  (assign continue eval-sequence-continue)
  (goto eval-dispatch)
eval-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (rest-exps (fetch unev)))
  (goto eval-sequence)
last-exp
  (restore continue)
  (goto eval-dispatch)

ev-cond
  (save continue)
  (assign continue evcond-decide)
  (assign unev (clauses (fetch exp)))
evcond-pred
  (branch (no-clauses? (fetch unev)) evcond-return-nil)
  (assign exp (first-clause (fetch unev)))
  (branch (else-clause? (fetch exp)) evcond-else-clause)
  (save env)
  (save unev)
  (assign exp (predicate (fetch exp)))
  (goto eval-dispatch)

evcond-return-nil
  (restore continue)
  (assign val nil)
  (goto (fetch continue))
evcond-decide
  (restore unev)
  (restore env)
  (branch (true? (fetch val)) evcond-true-predicate)
  (assign unev (rest-clauses (fetch unev)))
  (goto evcond-pred)
evcond-true-predicate
  (assign exp (first-clause (fetch unev)))
evcond-else-clause
  (assign unev (actions (fetch exp)))
  (goto eval-sequence)
ev-assignment
  (assign unev (assignment-variable (fetch exp)))
  (save unev)
  (assign exp (assignment-value (fetch exp)))
  (save env)
  (save continue)
  (assign continue ev-assignment-1)
  (goto eval-dispatch)
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (set-variable-value! (fetch unev) (fetch val) (fetch env)))
  (goto (fetch continue))
ev-definition
  (assign unev (definition-variable (fetch exp)))
  (save unev)
  (assign exp (definition-value (fetch exp)))
  (save env)
  (save continue)
  (assign continue ev-definition-1)
  (goto eval-dispatch)
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (define-variable! (fetch unev) (fetch val) (fetch env)))
  (assign val (fetch unev)) 
  (goto (fetch continue))

  ;;end of controller of explicit-control-evaluator
  ))

;;;a short procedure, for convenience

(define (go)
  (start explicit-control-evaluator))

;;;from 5.3.6 for compiler interface

(define (compile-and-go expression)
  (remote-assign
   explicit-control-evaluator
   'val
   (build-instruction-list explicit-control-evaluator
                           (compile expression)))
  (eval '(goto external-entry)
        explicit-control-evaluator))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (if (atom? proc)
      nil
      (eq? (car proc) 'compiled-procedure)))

(define (compiled-procedure-entry proc)
  (cadr proc))

(define (compiled-procedure-env proc)
  (caddr proc))
