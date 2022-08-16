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



;;; ps7.scm

;;; we start by instrumenting cons, set-car! and set-cdr! to keep a count
;;; of how often they are used.  (reset-counters) resets everything to zero,
;;; and (report-counters) gives a report on how many times they have been
;;; used since the last reset.  the cons count tells how many new cons cells
;;; have been used.  the set count lumps together the number of calls to
;;; set-car! and set-cdr!.
;;;
;;; the instrumented versions are called ins-cons, ins-set-car!, ins-set-cdr!.

(define cons-count 0)
(define set-count 0)
(define assq-count 0)

(define (reset-counters)
  (set! cons-count 0)
  (set! set-count 0)
  (set! assq-count 0))

(define (report-counters)
  (newline)
  (princ "Cons: ") (princ cons-count)
  (newline)
  (princ "Set:  ") (princ set-count)
  (newline)
  (princ "Assq: ") (princ assq-count)
  '())

(define (ins-set-car! cell new)
  (set! set-count (+ 1 set-count))
  (set-car! cell new))

(define (ins-set-cdr! cell new)
  (set! set-count (+ 1 set-count))
  (set-cdr! cell new))

(define (ins-cons x y)
  (set! cons-count (+ 1 cons-count))
  (cons x y))

;;; standard definitions of get and put

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            nil
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  nil
                  (cdr record))))))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))
                  (set-cdr! record value)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


