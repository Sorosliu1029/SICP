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

;;;This is the file STREAM-PAIRS.SCM

(define (divisible? x y) (= (remainder x y) 0))

;;; Useful stream utility functions

(define (filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (head stream))
         (cons-stream (head stream)
                      (filter pred (tail stream))))
        (else (filter pred (tail stream)))))


;;; Mapping functions

(define (map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (head stream))
                   (map proc (tail stream)))))


;;; Iterating along a stream.

(define (for-each proc stream)
  (if (empty-stream? stream)
      'done
      (sequence (proc (head stream))
                (for-each proc (tail stream)))))

;;; Streams of numbers

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (head s1) (head s2))
                           (add-streams (tail s1) (tail s2))))))


(define (scale-stream c stream)
  (map (lambda (x) (* x c)) stream))

;;; Differs from book by not checking for empty streams
(define (interleave s1 s2)
  (cons-stream (head s1)
	       (interleave s2
			   (tail s1))))


(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((h1 (head s1))
               (h2 (head s2)))
           (cond ((< h1 h2) (cons-stream h1 (merge (tail s1) s2)))
                 ((> h1 h2) (cons-stream h2 (merge s1 (tail s2))))
                 (else (cons-stream h1 (merge (tail s1) (tail s2)))))))))

;;;This next procedure is to be used in forming streams of pairs,
;;;once you have defined the procedure MERGE-WEIGHTED
(define (weighted-pairs s t pair-weight)
  (cons-stream (list (head s) (head t))
               (merge-weighted
                (map (lambda (x) (list (head s) x))
                     (tail t))
                (weighted-pairs (tail s)
                                (tail t)
                                pair-weight)
                (lambda (p)
                  (pair-weight (car p) (cadr p))))))

;;;This procedure forms streams of weighted pairs, where pairs of the
;;;same weight have been combined.  In order to use it, you must
;;;define an appropriate procedure COMBINE-SAME-WEIGHTS
(define (same-weight-pairs s t pair-weight)
  (combine-same-weights (weighted-pairs s t pair-weight)
                        pair-weight))

;;; Stream I/O. 

(define print-stream
  (let ()
    (define (iter s)
      (if (empty-stream? s)
	  (princ "}")
	  (sequence (princ (head s))
		    (princ " ")
		    (iter (tail s)))))
    (lambda (s)
      (princ "{")
      (iter s))))
;; You may wonder why PRINT-STREAM has been written in such an obscure
;; way, when
;; (define (print-stream s)
;;   (princ "{")
;;   (for-each (lambda (x) (princ x) (princ " ")) s)
;;   (princ "}"))
;; would have the same effect.
;;  When Ben Bitdiddle asked Alyssa P. Hacker about this, she thought
;; for a few hours and then said that, for printing a very long stream,
;; the more obvious implementation would result in large amounts of
;; storage being consumed due to the memoizing of the stream being printed.
;; Unfortunately, Ben has not yet learned about garbage collection, so
;; her comment doesn't do him much good.  Assign the explanation of what is
;; going on as an extra credit exercise for your TA or recitation
;; instructor.


;;; For exercise 3.43
(define (show x)
  (print x)
  x)

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (-1+ n) (tail s))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))
