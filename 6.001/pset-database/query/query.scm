;;;;        -*-scheme-*- Query language interpreter

;;; This file contains everything that is needed to run the query system
;;;  from chapter 4 of the textbook.  Except where noted, the code is
;;;  from the book.

;;; History  of modifications follows:

;;; 3/25/89 - HAL
;;; wrote explicit procedures INITIALIZE-QEVAL-OPERATIONS and 
;;; PROCESS-SYNTAXED-QUERY to make it
;;; easier for students to add new special forms and change the 
;;; driver loop

;;; 2/11/89 - HAL
;;; Modify Extend-if-possible and add Reduce-from-frame so that
;;; (?x ?y) and (?y ?x) unify without failure due to the freefor test.

;;; 9/5/86 - GJS
;;;  Cleanup of this file.

;;; 12/1/85 - Julie & Jerry (GJS)
;;;  Modified Freefor? and removed Lookup-in-frame.
;;;  Lookup-in-frame took (cdr nil) when there was no binding.
;;;  Also removed Unbound?, which isn't in use.

;;; 7/6/85 - GJS
;;;  Disjoin changed to use interleave-delayed.  This imposes an order
;;;  on the clauses, so that an OR rule is equivalent to two separate rules.
;;;  Asserted? changed to use append-delayed and code for append-delayed
;;;  added.
;;;  Contract-question mark fixed to preserve the numbers on variables.

;;; 7/85 - Julie
;;;  I commented out the compiler declaration, since that
;;;  prevents this source file from loading into Scheme on the Chipmunk.

;;; 4/18/85 - Julie
;;;  (Bug fixed in contract-question-mark.  Blew up if variable
;;;  contained a rule-application-id.  Also fixed in 3rd printing of book.)


;;; Utilities come first.  In particular, the interpreter has a
;;;  PUT/GET dispatch mechanism.  It also uses streams extensively.

;;; First, we implement PUT and GET as in section 3.3.3

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            '()
            (let ((pair (assq key-2 (cdr subtable))))
              (if (null? pair)
                  '()
                  (cdr pair))))))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            (set-cdr! local-table
                      (cons (cons key-1
                                  (cons (cons key-2 value) '()))
                            (cdr local-table)))
            (let ((pair (assq key-2 (cdr subtable))))
              (if (null? pair)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))
                  (set-cdr! pair value))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; These will be SET! in INITIALIZE-DATA-BASE.

(define get '())
(define put '())

;;; We need some stream stuff from chapter 3.

;;; To be compatible with RRRS we renamed MAP to MAP-STREAM.

(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (head s))
                   (map-stream proc (tail s)))))


(define (flatmap proc stream)
  (flatten (map-stream proc stream)))

(define (flatten stream)
  (accumulate-delayed interleave-delayed the-empty-stream stream))

(define (singleton s) (cons-stream s the-empty-stream))
(define (interleave s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (interleave s2
                               (tail s1)))))
(define (interleave-delayed s1 delayed-s2)
  (if (empty-stream? s1)
      (force delayed-s2)
      (cons-stream (head s1)
                   (interleave-delayed (force delayed-s2)
				       (delay (tail s1))))))

(define (accumulate-delayed combiner initial-value stream)
  (if (empty-stream? stream)
      initial-value
      (combiner (head stream)
                (delay
                 (accumulate-delayed combiner initial-value (tail stream))))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (append-delayed s1 delayed-s2)
  (if (empty-stream? s1)
      (force delayed-s2)
      (cons-stream (head s1)
                   (append-delayed (tail s1) delayed-s2))))


;;; This is not from the book.  Used by INITIALIZE-DATA-BASE, below.

(define (list-to-stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list-to-stream (cdr l)))))

;;; This is not from the book.  It is used to conveniently set up a
;;;  database from a list of assertions and rules.

(define (initialize-data-base big-list)
  (define (deal-out statements rules assertions)
    (if (null? statements)
        (sequence (set! THE-ASSERTIONS (list-to-stream assertions))
                  (set! THE-RULES (list-to-stream rules))
                  'done)
        (let ((s (query-syntax-process (car statements))))
          (if (rule? s)
              (sequence (store-rule-in-index s)
                        (deal-out (cdr statements)
                                  (cons s rules)
                                  assertions))
              (sequence
               (store-assertion-in-index s)
               (deal-out (cdr statements)
                         rules
                         (cons s assertions)))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!))
    (initialize-qeval-operations)
    (deal-out big-list '() '())))


(define (initialize-qeval-operations)
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true))


;;; This is the query language interpreter from section 4.5 of the book,
;;;  with a few changes
;;; -The PUTs are commented out, because they are being done
;;;  in initialize-data-base
;;; -PP responses instead of PRINTing them
;;; -Driver loop reorganized to use the editor interface

;;; Query-driver-loop changed from book

(define (query-driver-loop)
  (newline)
  (princ "query==> ")
  (let ((q (read-from-keyboard)))
    (if (equal? q '(exit))
        'done
	(begin (if q (process-query q))
	       (newline)
	       (query-driver-loop)))))

;;; Pulled out of query-driver-loop

(define (process-query query)
  (newline)
  (princ "Responses to query:")
  (let ((q (query-syntax-process query)))
    (if (assertion-to-be-added? q)
        (sequence (add-rule-or-assertion! (add-assertion-body q))
                  (print "assertion added to data base")
                  (query-driver-loop))
        (process-syntaxed-query q))))


(define (process-syntaxed-query q)
  (print-stream-elements-on-separate-lines
       (map-stream (lambda (frame)
			(instantiate q
				     frame
				     (lambda (v f) 
				       (contract-question-mark v))))
               (qeval q (singleton '())))))


(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((constant? exp) exp)
          ((var? exp)
           (let ((vcell (binding-in-frame exp frame)))
             (if (null? vcell)             
                 (unbound-var-handler exp frame)
                 (copy (binding-value vcell)))))
          (else (cons (copy (car exp))
                      (copy (cdr exp))))))
  (copy exp))

;;; Essential center of interpreter -- from SECTION 4.5.2

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if (not (null? qproc))
        (qproc (contents query) frame-stream)
        (asserted? query frame-stream))))


;;; The default case of the interpreter -- primitive query

;;; Unhappily, it is possible for this to be right recursive
;;;  unless we prevent it by using APPEND-DELAYED here.  GJS

(define (asserted? query-pattern frame-stream)
  (flatmap (lambda (frame)                              ;***GJS***
             (append-delayed (find-assertions query-pattern frame)
                             (delay (apply-rules query-pattern frame))))
           frame-stream))

(define (find-assertions pattern frame)
  (flatmap (lambda (datum)
             (pattern-match pattern datum frame))
           (fetch-assertions pattern frame)))


;;; Query language special forms below

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;;; (put 'and 'qeval conjoin)


;;; Sadly, this needs an INTERLEAVE-DELAYED to prevent infinite recursion
;;;  down the second disjunct in the following:      ***GJS***

;;;    (rule (noun-phrase (?x . ?y))
;;;      (or (and (noun ?x) (null ?y))
;;;          (and (adjective ?x) (noun-phrase ?y))))

;;; Inverted order of disjuncts won't work even with I-D because the
;;;  first disjunct will be infinitely recursive. 

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed (qeval (first-disjunct disjuncts)
                                 frame-stream)
                          (delay (disjoin (rest-disjuncts disjuncts)
                                          frame-stream)))))

;;; (put 'or 'qeval disjoin)

(define (negate a frame-stream)
  (flatmap
   (lambda (frame)
     (if (empty-stream? (qeval (negated-query a)
                               (singleton frame)))
         (singleton frame)
         the-empty-stream))
   frame-stream))

;;; (put 'not 'qeval negate)


(define (lisp-value call frame-stream)
  (flatmap
   (lambda (frame)
     (if (execute
          (instantiate call
                       frame
                       (lambda (v f)
                         (error "Unknown pat var -- LISP-VALUE"
                                v))))
         (singleton frame)
         the-empty-stream))
   frame-stream))

;;; (put 'lisp-value 'qeval lisp-value)


(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))



(define (always-true ignore frame-stream)
  frame-stream)
                                        
;;; (put 'always-true 'qeval always-true)

;;; One-sided assertion matcher.

(define (pattern-match pat dat frame)
  (let ((result (internal-match pat dat frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton result))))

(define (internal-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? pat)
         (extend-if-consistent pat dat frame))
        ((constant? pat)
         (if (constant? dat)
             (if (same-constant? pat dat)
                 frame
                 'failed)
             'failed))
        ((constant? dat) 'failed)
        (else (internal-match (cdr pat)
                              (cdr dat)
                              (internal-match (car pat)
                                              (car dat)
                                              frame)))))

(define (extend-if-consistent var dat frame)
  (let ((value (binding-in-frame var frame)))
    (if (null? value)
        (extend var dat frame)
        (internal-match (binding-value value) dat frame))))



;;; Rule applicator uses unify matcher (next page)

(define (apply-rules pattern frame)
  (flatmap (lambda (rule)
             (apply-a-rule rule pattern frame))
           (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))            
      (if (empty-stream? unify-result)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 unify-result)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((constant? exp) exp)
            ((var? exp)
             (make-new-variable exp rule-application-id))
            (else (cons (tree-walk (car exp))
                        (tree-walk (cdr exp))))))
  (tree-walk rule)))

;;; A traditional unification matcher
;;;  invented by J.A.Robinson (JACM Jan. 1965)

(define (unify-match p1 p2 frame)
  (let ((result (internal-unify p1 p2 frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton result))))

(define (internal-unify p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))   ; ***
        ((constant? p1)
         (if (constant? p2)
             (if (same-constant? p1 p2)
                 frame
                 'failed)
             'failed))
        ((constant? p2) 'failed)
        (else (internal-unify (cdr p1)
                              (cdr p2)
                              (internal-unify (car p1)
                                              (car p2)
                                              frame)))))

(define (reduce-from-frame var frame)
  (let ((value-cell (binding-in-frame var frame)))
    (if (null? value-cell)
        var
        (binding-value value-cell))))

(define (extend-if-possible var val frame)
  (let ((val (reduce-from-frame val frame)))      ;***
    (if (equal? var val)                          ;***
        frame
        (let ((value-cell (binding-in-frame var frame)))
          (if (null? value-cell)
              (if (freefor? var val frame)        ;***
                  (extend var val frame)
                  'failed)
              (internal-unify (binding-value value-cell)
                              val
                              frame))))))

(define (freefor? var exp frame)
  (define (freewalk e)
    (cond ((constant? e) true)
          ((var? e)
           (if (equal? var e)
	       false
               (let ((b (binding-in-frame e frame)))
                 (if (null? b)
		     true
                     (freewalk (binding-value b))))))
          ((freewalk (car e)) (freewalk (cdr e)))
          (else false)))
  (freewalk exp))

;;; Simple database system

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if (null? s) the-empty-stream s)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (append-streams
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))


(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;;; Query expression syntax

(define (type exp)
  (if (atom? exp) 
      (error "Unknown expression TYPE" exp)
      (if (symbol? (car exp))
          (car exp)
          "Untyped-query-object")))

(define (contents exp)
  (if (atom? exp) 
      (error "Unknown expression CONTENTS" exp)
      (cdr exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp) (car (contents exp)))

(define empty-conjunction? null?)
(define first-conjunct car)
(define rest-conjuncts cdr)

(define empty-disjunction? null?)
(define first-disjunct car)
(define rest-disjuncts cdr)

(define negated-query car)

(define predicate car)
(define args cdr)

(define (rule? statement)
  (if (atom? statement)
      false
      (eq? (car statement) 'rule)))

(define conclusion cadr)
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

;;; Primitive query syntax

(define (var? exp)
  (if (atom? exp)
      false
      (eq? (car exp) '?)))

(define constant? atom?)
(define constant-symbol? symbol?)
(define same-constant? equal?)

;;; Binding frame abstraction

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (1+ rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))


;;The PP here is PRINT in the book
(define (print-stream-elements-on-separate-lines s)
  (if (empty-stream? s)
      (print "done")
      (sequence (pp (head s))
                (print-stream-elements-on-separate-lines
                 (tail s)))))


;;; Input-output hack to allow one to type ?x for (? x)

(define (query-syntax-process exp)
  (map-over-atoms expand-question-mark exp))

(define (map-over-atoms proc exp)
  (if (atom? exp)
      (proc exp)
      (cons (map-over-atoms proc (car exp))
            (map-over-atoms proc (cdr exp)))))

(define (expand-question-mark atom)
  (if (symbol? atom)
      (let ((characters (explode atom)))
        (if (eq? (car characters) '?)
            (list '? (implode (cdr characters)))
            atom))
      atom))

;**Bug fixed -- this is what it should be
;(define (contract-question-mark variable)
;  (if (number? (cadr variable)) ;rule-app-id
;      (implode (append '(?)
;                       (explode (caddr variable))
;                       '(-)
;                       (explode (cadr variable))))
;      (implode (append '(?) (explode (cadr variable))))))

;;before we can use this, we need to fix the system so that explode
;works on numbers.  for now, we will use

(define (contract-question-mark variable)
  (if (number? (cadr variable)) ;rule-app-id
      variable
      (implode (append '(?) (explode (cadr variable))))))

