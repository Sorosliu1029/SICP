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



;;; Ben Bitdiddle's Paranoid Programming System
;;; This is the file PS8-PPS.SCM

(define (careful-version suspect-procedure)
  (define (me x)                      ;ME will be the careful version
    (check-all (preconditions me) (lambda (pred) (pred x)))
    (let ((v (suspect-procedure x)))
      (check-all (postconditions me) (lambda (pred) (pred v)))
      (check-all (transfer-conditions me) (lambda (pred) (pred v x)))
      v))
  (set-kernel-procedure! me suspect-procedure)
  me)

(define (check-all set-of-predicates predicate-application-form)
  (define (loop s)
    (cond ((null? s) t)
          ((predicate-application-form (car s)) (loop (cdr s)))
          (else (error "Failed consistency check" (car s)))))
  (loop set-of-predicates))

(define (preconditions proc)       
  (get proc preconditions))        

(define (postconditions proc)
  (get proc postconditions))

(define (transfer-conditions proc)
  (get proc transfer-conditions))

(define (kernel-procedure f)
  (get f kernel-procedure))

(define (declare declaration-type proc predicate-procedure)
  (let ((set (get proc declaration-type)))
    (if (not (memq predicate-procedure set))
        (put proc declaration-type (cons predicate-procedure set)))
    'OK))

(define (set-kernel-procedure! f1 f2)
  (put f1 kernel-procedure f2))

(define (careful-compose f g)
  (define (kernel x)
    (let ((vg ((kernel-procedure g) x)))
      (check-all (postconditions g) (lambda (p) (p vg)))
      (check-all (transfer-conditions g) (lambda (p) (p vg x)))
      (check-all (set-difference (preconditions f) (postconditions g))
                 (lambda (p) (p vg)))
      (let ((vf ((kernel-procedure f) vg)))     ;Compute value of F
        (check-all (transfer-conditions f) (lambda (p) (p vf vg)))
        vf)))
  (define (me x)
    (check-all (preconditions g) (lambda (p) (p x)))
    (let ((vme (kernel x)))
      (check-all (postconditions f) (lambda (p) (p vme)))
      vme))
  (set-kernel-procedure! me kernel)
  (put me preconditions (preconditions g))
  (put me postconditions (postconditions f))
  me)

(define (set-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (set-difference (cdr s1) s2))
        (else (cons (car s1) (set-difference (cdr s1) s2)))))


;;; This is the represention for tables, taken from pages 216-217
;;; of the book.

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
