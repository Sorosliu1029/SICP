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

;;;; 6.001 Compiler

;;;section 5.3.6
(define (compile expression)
  (statements (compile-expression expression
                                  initial-c-t-env
                                  'val
                                  'return)))

(define initial-c-t-env '())

;;;sections 5.3.1-5.3.5

(define (compile-expression exp c-t-env target cont)
  (cond ((self-evaluating? exp)
         (compile-constant exp c-t-env target cont))
        ((quoted? exp)
         (compile-constant (text-of-quotation exp)
                           c-t-env target cont))
        ((variable? exp)
         (compile-variable-access exp c-t-env target cont))
        ((assignment? exp)
         (compile-assignment exp c-t-env target cont))
        ((definition? exp)
         (compile-definition exp c-t-env target cont))
        ((lambda? exp)
         (compile-lambda exp c-t-env target cont))
        ((conditional? exp)
         (compile-cond (clauses exp) c-t-env target cont))
        ((no-args? exp)
         (compile-no-args exp c-t-env target cont))
        ((application? exp)
         (compile-application exp c-t-env target cont))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (preserving reg seq1 seq2)
  (if (and (needs-register seq2 reg)
           (modifies-register seq1 reg))
      (append-instruction-sequences
       (wrap-save-restore seq1 reg)
       seq2)
      (append-instruction-sequences seq1 seq2)))

(define (compile-continuation continuation)
  (cond ((eq? continuation 'return) (compile-return))
        ((eq? continuation 'next)
         (empty-instruction-sequence))
        (else (make-jump continuation))))

(define (compile-constant constant c-t-env target cont)
  (append-instruction-sequences
   (make-register-assignment target (make-constant constant))
   (compile-continuation cont)))

(define (compile-variable-access var c-t-env target cont)
  (append-instruction-sequences
   (make-register-assignment target
                             (make-variable-access var
                                                   c-t-env))
   (compile-continuation cont)))

(define (compile-application app c-t-env target cont)
  (preserving
   'env
   (compile-expression (operator app) c-t-env 'fun 'next)
   (preserving 'fun
               (compile-operands (operands app) c-t-env)
               (compile-call target cont))))

(define (compile-operands rands c-t-env)
  (let ((first-operand-code
         (compile-first-operand rands c-t-env)))
    (if (last-operand? rands)
        first-operand-code
        (preserving
         'env
         first-operand-code
         (compile-rest-operands (rest-operands rands)
                                c-t-env)))))

(define (compile-first-operand rands c-t-env)
  (append-instruction-sequences
   (compile-expression (first-operand rands)
                       c-t-env 'val 'next)
   (make-register-assignment
    'argl
    (make-singleton-arglist (make-fetch 'val)))))

(define (compile-rest-operands rands c-t-env)
  (let ((next-operand-code
         (compile-next-operand rands c-t-env)))
    (if (last-operand? rands)
        next-operand-code
        (preserving
         'env
         next-operand-code
         (compile-rest-operands (rest-operands rands)
                                c-t-env)))))

(define (compile-next-operand rands c-t-env)
  (preserving 
   'argl
   (compile-expression (first-operand rands)
                       c-t-env 'val 'next)
   (make-register-assignment
    'argl
    (make-add-to-arglist (make-fetch 'val)
                         (make-fetch 'argl)))))

(define (compile-no-args app c-t-env target cont)
  (append-instruction-sequences
   (compile-expression (operator app) c-t-env 'fun 'next)
   (make-register-assignment 'argl (make-empty-arglist))
   (compile-call target cont)))

(define (compile-call target cont)
  (if (eq? target 'val)
      (compile-call-result-in-val cont)
      (append-instruction-sequences
       (compile-call-result-in-val 'next)
       (make-register-assignment target (make-fetch 'val))
       (compile-continuation cont))))

(define (compile-call-result-in-val cont)
  (cond ((eq? cont 'return)
         (compile-call-return-to nil))
        ((eq? cont 'next)
         (let ((after-call (make-new-label 'after-call)))
           (append-instruction-sequences
            (compile-call-return-to after-call)
            (make-entry-point-designator after-call))))
        (else
         (compile-call-return-to cont))))

(define (compile-return)
  (append-instruction-sequences
   (make-restore 'continue)
   (make-return-from-procedure)))

(define (compile-call-return-to return-entry)
  (if (null? return-entry)
      (make-transfer-to-procedure)
      (append-instruction-sequences
       (make-register-assignment 'continue return-entry)
       (make-save 'continue)
       (make-transfer-to-procedure))))

(define (compile-cond clauses c-t-env target cont)
  (if (eq? cont 'next)
      (let ((end-of-cond (make-new-label 'cond-end)))
        (append-instruction-sequences
         (compile-clauses clauses c-t-env target end-of-cond)
         (make-entry-point-designator end-of-cond)))
      (compile-clauses clauses c-t-env target cont)))

(define (compile-clauses clauses c-t-env target cont)
  (if (no-clauses? clauses)
      (compile-constant nil c-t-env target cont)
      (compile-a-clause (first-clause clauses)
                        (rest-clauses clauses)
                        c-t-env target cont)))

(define (compile-a-clause clause rest c-t-env target cont)
  (let ((consequent (compile-sequence (actions clause)
                                      c-t-env target cont)))
    (if (else-clause? clause)
        consequent
        (let
         ((alternative (compile-clauses rest
                                        c-t-env target cont))
          (pred (compile-expression (predicate clause)
                                    c-t-env 'val 'next))
          (true-branch (make-new-label 'true-branch)))
         (let ((alternative-and-consequent
                (parallel-instruction-sequences
                 alternative
                 (append-instruction-sequences
                  (make-entry-point-designator true-branch)
                  consequent))))
           (preserving
            'env
            pred
            (append-instruction-sequences
             (make-branch (make-test 'val) true-branch)
             alternative-and-consequent)))))))

(define (compile-sequence seq c-t-env target cont)
  (if (last-exp? seq)
      (compile-expression (first-exp seq) c-t-env target cont)
      (preserving
       'env
       (compile-expression (first-exp seq) c-t-env nil 'next)
       (compile-sequence (rest-exps seq) c-t-env target cont)
       )))

(define (compile-assignment exp c-t-env target cont)
  (let ((hold-value (if (null? target) 'val target)))
    (preserving
     'env
     (compile-expression (assignment-value exp)
                         c-t-env hold-value 'next)
     (append-instruction-sequences
      (make-variable-assignment (assignment-variable exp)
                                c-t-env
                                (make-fetch hold-value))
      (compile-continuation cont)))))

(define (compile-definition exp c-t-env target cont)
  (let ((hold-value (if (null? target) 'val target))
        (var (definition-variable exp)))
    (preserving
     'env
     (compile-expression (definition-value exp)
                         c-t-env hold-value 'next)
     (append-instruction-sequences
      (make-variable-definition var
                                c-t-env
                                (make-fetch hold-value))
      (make-register-assignment target (make-constant var))
      (compile-continuation cont)))))

(define (compile-lambda exp c-t-env target cont)
  (if (eq? cont 'next)
      (let ((after-lambda (make-new-label 'after-lambda)))
        (append-instruction-sequences
         (compile-lambda-2 exp c-t-env target after-lambda)
         (make-entry-point-designator after-lambda)))
      (compile-lambda-2 exp c-t-env target cont)))

(define (compile-lambda-2 exp c-t-env target cont)
  (let ((proc-entry (make-new-label 'entry)))
    (tack-on-instruction-sequence
     (append-instruction-sequences
      (make-register-assignment
       target
       (make-procedure-constructor proc-entry))
      (compile-continuation cont))
     (compile-lambda-body exp c-t-env proc-entry))))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (compile-lambda-body exp c-t-env proc-entry)
  (append-instruction-sequences
   (make-entry-point-designator proc-entry)
   (make-environment-switch (lambda-parameters exp))
   (compile-sequence
    (lambda-body exp)
    (extend-compile-time-env (lambda-parameters exp) c-t-env)
    'val
    'return)))


;;;section 5.3.7
(define (extend-compile-time-env params c-t-env)
  (cons params c-t-env))


(define (make-instruction-sequence needed modified statements)
  (list needed modified statements))

(define (registers-needed s) (car s))

(define (registers-modified s) (cadr s))

(define (statements s) (caddr s))

(define (needs-register seq reg)
  (element-of-set? reg (registers-needed seq)))

(define (modifies-register seq reg)
  (element-of-set? reg (registers-modified seq)))

(define (make-instruction needed modified statement)
  (make-instruction-sequence needed modified (list statement)))

(define (empty-instruction-sequence)
  (make-instruction-sequence empty-set empty-set '()))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (union-set (registers-needed seq1)
                (difference-set (registers-needed seq2)
                                (registers-modified seq1)))
     (union-set (registers-modified seq1)
                (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))


(define (tack-on-instruction-sequence seq body-seq)
  (append-instruction-sequences
   seq
   (make-instruction-sequence empty-set
                              empty-set
                              (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (union-set (registers-needed seq1) (registers-needed seq2))
   (union-set (registers-modified seq1) (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

(define (difference-set s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (difference-set (cdr s1) s2))
        (else (cons (car s1) (difference-set (cdr s1) s2)))))

(define (element-of-set? x s) (memq x s))

(define (singleton x) (list x))

(define (make-set list-of-elements) list-of-elements)

(define empty-set '())

(define (make-val-spec registers-needed expression)
  (list registers-needed expression))

(define (val-spec-registers-needed value)
  (car value))

(define (val-spec-expression value)
  (cadr value))

(define (make-constant c)
  (make-val-spec empty-set (list 'quote c)))

(define (make-label symbol)
  (make-val-spec empty-set symbol))

(define (make-new-label name)
  (make-label (make-new-symbol name)))

(define make-new-symbol generate-uninterned-symbol)

(define (make-fetch reg)
  (make-val-spec (singleton reg) (list 'fetch reg)))

(define (make-operation operation . inputs)
  (make-val-spec
   (union-all-sets (mapcar val-spec-registers-needed inputs))
   (cons operation (mapcar val-spec-expression inputs))))

(define (union-all-sets sets)
  (if (null? sets)
      empty-set
      (union-set (car sets) (union-all-sets (cdr sets)))))

(define (make-register-assignment reg val-spec)
  (if (null? reg)
      (empty-instruction-sequence)
      (make-instruction
       (val-spec-registers-needed val-spec)
       (singleton reg)
       (list 'assign reg (val-spec-expression val-spec)))))

(define (make-nonlocal-goto continuation cont-needs)
  (make-goto continuation (make-set cont-needs) all))

(define all (make-set '(fun env val argl continue)))

(define (make-jump continuation)
  (make-goto continuation empty-set empty-set))

(define (make-goto cont cont-needs cont-modifies)
  (make-instruction
   (union-set (val-spec-registers-needed cont) cont-needs)
   cont-modifies
   (list 'goto (val-spec-expression cont))))

(define (make-branch predicate true-branch)
  (make-instruction
   (union-set (val-spec-registers-needed predicate)
              (val-spec-registers-needed true-branch))
   empty-set
   (list 'branch
         (val-spec-expression predicate)
         (val-spec-expression true-branch))))

(define (make-save reg)
  (make-instruction (singleton reg)
                    empty-set
                    (list 'save reg)))

(define (make-restore reg)
  (make-instruction empty-set
                    (singleton reg)
                    (list 'restore reg)))

(define (make-perform action)
  (make-instruction
   (val-spec-registers-needed action)
   empty-set
   (list 'perform (val-spec-expression action))))

(define (make-entry-point-designator label-val-spec)
  (make-instruction empty-set
                    empty-set
                    (val-spec-expression label-val-spec)))

(define (wrap-save-restore seq reg)
  (make-instruction-sequence
   (registers-needed seq)
   (difference-set (registers-modified seq) (singleton reg))
   (append (statements (make-save reg))
           (statements seq)
           (statements (make-restore reg)))))

(define (make-variable-access var c-t-env)
  (make-operation 'lookup-variable-value
                  (make-constant var)
                  (make-fetch 'env)))

(define (make-test reg)
  (make-operation 'true? (make-fetch reg)))

(define (make-variable-assignment var c-t-env value)
  (make-perform
   (make-operation 'set-variable-value!
                   (make-constant var)
                   value
                   (make-fetch 'env))))

(define (make-variable-definition var c-t-env value)
  (make-perform
   (make-operation 'define-variable!
                   (make-constant var)
                   value
                   (make-fetch 'env))))

(define (make-procedure-constructor entry)
  (make-operation 'make-compiled-procedure
                  entry
                  (make-fetch 'env)))

(define (make-environment-switch formals)
  (append-instruction-sequences
   (make-register-assignment
    'env
    (make-operation 'compiled-procedure-env
                    (make-fetch 'fun)))
   (make-register-assignment
    'env
    (make-operation 'extend-binding-environment
                    (make-constant formals)
                    (make-fetch 'argl)
                    (make-fetch 'env)))))

(define (make-singleton-arglist first-arg-spec)
  (make-operation 'cons first-arg-spec (make-constant '())))

(define (make-add-to-arglist next-arg-spec rest-args-spec)
  (make-operation 'cons next-arg-spec rest-args-spec))

(define (make-empty-arglist)
  (make-constant '()))

(define (make-transfer-to-procedure)
  (make-nonlocal-goto (make-label 'apply-dispatch)
		      '(fun argl)))

(define (make-return-from-procedure)
  (make-nonlocal-goto (make-fetch 'continue)
		      '(val)))
