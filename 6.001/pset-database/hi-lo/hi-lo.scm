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


;; this file contains code for use in problem set 2 on strategies for guessing numbers

;; procedure for choosing nearest integer
(define flip
   (lambda (n)
     (cond ((integer? n) n)
           ((= (random 2) 1) (floor n))
           (else (ceiling n)))))

;; Louis' first attempt

(define strategy-random
  (lambda ()
    (define helper                             ; internal helper procedure
      (lambda (n)                              ; argument is no. of trials
        (if (=-hidden (+ low-limit
                         (random (+ (- high-limit low-limit)
                                 1))))         ; to get random number in range
                                               ; have found right number
            n                                  ; return no. of trials
            (helper (+ n 1)))))                ; else do again with new guess
    (helper 1)))                               ; start off with first try

;; you need to complete the procedure below for general strategies

(define general-strategy
  (lambda (next-guess next-lub next-glb trial guess lub glb)
    (if (=-hidden guess)
        trial
        (general-strategy
        ;; need to complete this
              ))))


(define tester
  (lambda (no-of-trials strategy type)
    (define final-stats (make-statistic type))
    (define updater (make-updater type))
    (define run-it
      (lambda (n stat)
        (if (= n no-of-trials)
            ;; fill in here
            (sequence (setup 1 1000)
                      ;; fill in here
                  ))))
    (run-it 0 0)))
