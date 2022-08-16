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

;; code for food problem set 

;; constructor for dishes

(define make-dish
  (lambda (name ingredients cals cost)
    (list name ingredients cals cost)))


;; constructor for ingredients

(define make-ingredient
  (lambda ( name cals-unit cost-unit)
     (list name cals-unit cost-unit)))

;; sample list of dishes
(define dishes
   (list (make-dish 'hamburger '((beef .25) (bun 1)) 0 0)
         (make-dish 'cheeseburger '((beef .25) (bun 1) (am-cheese 1)) 0 0) 
         (make-dish 'swissburger '((beef .25) (bun 1) (sw-cheese 1)) 0 0) 
         (make-dish 'hotdog '((frankfurter 1) (hbun 1)) 0 0)
         (make-dish 'reg-coke '((coke-syrup .5)) 0 0)
         (make-dish 'large-coke '((coke-syrup 1)) 0 0)
         (make-dish 'doubleburger '((beef .5) (bun 1)) 0 0)))
;; note that coke has an empty list of ingredients, and that
;; the calories and cost have already been inserted into the structure

;; sample list of ingredients
(define ingredients
  (list (make-ingredient 'beef 200 1.50)
        (make-ingredient 'am-cheese 100 .10)
        (make-ingredient 'sw-cheese 200 .20)
        (make-ingredient 'bun 50 .15)
        (make-ingredient 'hbun 50 .10)
        (make-ingredient 'frankfurter 150 .20)
        (make-ingredient 'coke-syrup 75 .10)))

(define calories-of-dish
  (lambda (dish ings-list)                      
    ;arguments are a dish structure and a list of ingredient structures
    (let ((dish-ings (dish-ingredients dish)))  ; get the ingredients of the dish
      (accumulate-cals dish-ings ings-list))))  ; determine number of calories

;;general procedures for manipulating list structures
(define map
  (lambda (proc lst)
    (if (null? lst)   ; if lst is empty
        '()           ; then return an empty list
        (cons (proc (car lst))   ; otherwise apply procedure to first element
              (map proc (cdr lst))))))

(define filter
  (lambda (pred lst)
    (cond ((null? lst)   ; if lst is empty
           '())          ; then return an empty list
          ((pred (car lst))  ; if first element satisfies predicate
           (cons (car lst) (filter pred (cdr lst))))  ; then add to new list
          (else  ; otherwise discard from new list, and handle remainder
           (filter pred (cdr lst))))))

(define accumulate
  (lambda (combiner init lst)
     (if (null? lst)  ; if lst is empty
         init         ; return initial value
         (combiner (car lst)  ;; otherwise combine first element
                   (accumulate combiner init (cdr lst))))))


(define flatten
  (lambda (l)
    (accumulate append '() l)))