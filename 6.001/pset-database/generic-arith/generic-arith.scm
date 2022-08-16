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

; generic arithmetic operations

(define (add x y) (operate-2 'add x y))
(define (sub x y) (operate-2 'sub x y))
(define (mul x y) (operate-2 'mul x y))
(define (div x y) (operate-2 'div x y))
(define (=zero? x) (operate '=zero? x))
(define (negate x) (operate 'negate x))

; a sample compound generic operation

(define (square x) (mul x x))

; the operate mechanism.  Note that we don't deal with coercion here.

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))   ;operator is defined on type
        (proc (contents obj))
        (error "Operator undefined on this type -- OPERATE"
               (list op obj)))))

(define (operate-2 op arg1 arg2)
  (let ((t1 (type arg1)))
    (if (eq? t1 (type arg2))
        (let ((proc (get t1 op)))
          (if (not (null? proc)) 
              (proc (contents arg1) (contents arg2))
              (error
               "Operator undefined on this type -- OPERATE-2"
               (list op arg1 arg2))))
        (error "Operands not of same type -- OPERATE-2"
               (list op arg1 arg2)))))

; code for creating the table, you don't need to worry about this.

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

; procedures for dealing with ordinary numbers.

(define (+number x y) (make-number (+ x y)))
(define (-number x y) (make-number (- x y)))
(define (*number x y) (make-number (* x y)))
(define (/number x y) (make-number (/ x y)))
(define (negate-number x) (make-number (- x)))
(define (=zero-number? x) (= x 0))

(define (make-number x) (attach-type 'number x))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)
(put 'number 'negate negate-number)
(put 'number '=zero? =zero-number?)

; the bottom level typing system

(define (attach-type type contents)
  (if (and (eq? type 'number) (number? contents))
      contents
      (cons type contents)))

(define (type datum)
  (cond ((number? datum) 'number)
        ((not (atom? datum)) (car datum))
        (else (error "Bad typed datum -- TYPE" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((not (atom? datum)) (cdr datum))
        (else (error "Bad typed datum -- CONTENTS" datum))))

; procedures for dealing with polynomials

(define (+poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-polynomial (variable p1)
                       (+terms (term-list p1)
                               (term-list p2)))
      (error "Polys not in same var -- +POLY" (list p1 p2))))

(define (*poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-polynomial (variable p1)
                       (*terms (term-list p1)
                               (term-list p2)))
      (error "Polys not in same var -- *POLY" (list p1 p2))))

(define (=zero-poly? p)
  (empty-termlist? (term-list p)))

(put 'polynomial 'add +poly)
(put 'polynomial 'mul *poly)
(put 'polynomial '=zero? =zero-poly?)


; procedures for dealing with term lists

(define (+terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (+terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2
                               (+terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term (make-term (order t1)
                                          (add (coeff t1)
                                               (coeff t2)))
                               (+terms (rest-terms L1)
                                       (rest-terms L2)))))))))

(define (*terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (+terms (*-term-by-all-terms (first-term L1) L2)
              (*terms (rest-terms L1) L2))))

(define (*-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                     (*-term-by-all-terms t1 (rest-terms L))))))

; procedures for representing term lists.

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))                ;slight simplification
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial variable term-list)
  (attach-type 'polynomial (cons variable term-list)))

(define (variable p) (car p))
(define (term-list p) (cdr p))
(define (same-variable? v1 v2) (eq? v1 v2))


; the rational arithmetic package
;
; these functions manipulate rationals within the package

(define (+rat x y)
  (make-rat (add (mul (numer x) (denom y))
                 (mul (denom x) (numer y)))
            (mul (denom x) (denom y))))

(define (-rat x y)
  (make-rat (sub (mul (numer x) (denom y))
                 (mul (denom x) (numer y)))
            (mul (denom x) (denom y))))

(define (*rat x y)
  (make-rat (mul (numer x) (numer y))
            (mul (denom x) (denom y))))

(define (/rat x y)
  (make-rat (mul (numer x) (denom y))
            (mul (denom x) (numer y))))

(define (negate-rat x)
  (make-rat (negate (numer x))
            (denom x)))

(define (=zero-rat? x)
  (=zero? (numer x)))

; procedures for representing rationals

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

; procedures to interface rationals to other parts of the generic system

(define (make-rational x)
   (attach-type 'rational x))

(define (+rational x y)
   (make-rational (+rat x y)))

(define (-rational x y)
   (make-rational (-rat x y)))

(define (*rational x y)
   (make-rational (*rat x y)))

(define (/rational x y)
   (make-rational (/rat x y)))

(define (negate-rational x)
   (make-rational (negate-rat x)))


(put 'rational 'add +rational)
(put 'rational 'sub -rational)
(put 'rational 'mul *rational)
(put 'rational 'div /rational)
(put 'rational 'negate negate-rational)
(put 'rational '=zero? =zero-rat?)


; procedure for building a generic rational

(define (create-rational x y)
   (make-rational (make-rat x y)))
