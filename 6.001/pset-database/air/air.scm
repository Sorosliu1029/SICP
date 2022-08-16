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





;;; This is the code for Problem Set 5

;;; Lookup, insert and delete of NJ-AIR

(define (lookup-unordered name file)
  (assq name (rest file)))

(define (insert-unordered name record file)
  (let ((pair (attach name record)))
    (cond ((empty? file) (list pair))
	  ((header? file)
	   (attach (header file) (insert-unordered name record (rest file))))
	  ((eq? (person-at-curr-folder file) name)
	   (attach pair (rest file)))
	  (else (attach (curr-folder file)
			(insert-unordered name record (rest file)))))))

(define (delete-unordered name file)
    (cond ((empty? (rest file)) file)
	  ((eq? (person-at-curr-folder (rest file)) name)
	   (attach (curr-folder file) (rest (rest file))))
	  (else (attach (curr-folder file)
		        (delete-unordered name (rest file))))))

;;; Abstraction definitions

(define attach cons)
(define header car)
(define curr-folder car)
(define person-at-curr-folder caar)
(define rest cdr)
(define (header? file) (atom? (car file)))
(define (tree-header? file) (and (atom? (car file)) (not (null? (car file)))))
(define empty? null?)

;;; Oops, lookup, insert and delete for People-Delay-Airline is destroyed
;;; in a fire.

;;; Lookup, insert and delete for Epsilon-Air

(define (lookup-tree name file)
  (if (tree-header? file)
      (lookup-tree name (rest file))
  (let ((next-branch (choose-branch name file)))
    (cond ((null? next-branch) nil)
	  ((leaf? next-branch) (content next-branch))
	  (else (lookup-tree name next-branch))))))

(define (choose-branch name file)
  (let ((left-branch (left file))
	(right-branch (right file)))
    (cond ((memq name (symbols left-branch)) left-branch)
	  ((memq name (symbols right-branch)) right-branch)
	  (else nil))))







(define (insert-tree name record file)
  (cond ((empty? file)
         (insert-tree name record (make-tree nil nil)))
	((tree-header? file)
	 (attach (header file) (insert-tree name record (rest file))))
	(else
	 (let ((path (choose-path name file))
	       (leaf (make-leaf name record)))
	   (if (null? path)
	       (let ((r (random 2)))
		 (if (= r 1)
		     (cond ((bare? (left file))
			    (make-tree leaf (right file)))
			   ((leaf? (left file))
			    (make-tree (make-tree leaf (left file))
				       (right file)))
			   (else
			    (make-tree (insert-tree name record (left file))
				       (right file))))
		     (cond ((bare? (right file))
			    (make-tree (left file) leaf))
			   ((leaf? (right file))
			    (make-tree (left file)
				       (make-tree (right file) leaf)))
			   (else
			    (make-tree (left file)
				       (insert-tree name record (right file)))))))
	       (if (leaf? (choose-branch name file))
		   (if (eq? path 'left)
		       (make-tree leaf (right file))
		       (make-tree (left file) leaf))
		   (if (eq? path 'left)
		       (make-tree (insert-tree name record (left file))
				  (right file))
		       (make-tree (left file)
				  (insert-tree name record (right file))))))))))
(define (delete-tree name file)
  (if (tree-header? file)
      (attach (header file) (delete-tree name (rest file)))
      (let ((path (choose-path name file)))
	(cond ((eq? 'left path)
	       (if (leaf? (left file))
		   (if (bare? (right file))
		       nil
		       (make-tree nil (right file)))
		   (make-tree (delete-tree name (left file))
			      (right file))))
	      ((eq? 'right path)
	       (if (leaf? (right file))
		   (if (bare? (left file))
		       nil
		       (make-tree (left file) nil))
		   (make-tree (left file)
			      (delete-tree name (right file)))))
	      (else file)))))

(define (choose-path name file)
  (cond ((memq name (symbols (left file))) 'left)
	((memq name (symbols (right file))) 'right)
	(else nil)))







;;; Structuring a tree (abstraction definitions for Epsilon-Air)

(define (make-tree left right)
  (list left right (append (symbols left) (symbols right))))

(define left car)
(define right cadr)

(define (make-leaf name record)
  (list 'leaf (cons name record)))

(define (symbols tree)
  (cond ((bare? tree) nil)
	((leaf? tree) (list (symbol tree)))
        (else (caddr tree))))

(define (leaf? obj)
  (eq? (car obj) 'leaf))

(define (bare? obj)
  (null? obj))

(define (symbol obj)
  (caadr obj))

(define (content obj)
  (cadr obj))

;;; Constructor of NJ-AIR and People-Delay Airline's employee record

(define (make-record-table id info)
  (list (cons id info)))

;;; Constructor of Epsilon-Air's employee record

(define (make-record-tree id info)
  (make-tree (make-leaf id info) nil))

;;; All the Personel files (minaturized versions)

(define nj-air (list 'nj-air
		     (list 'moe (cons 'salary 40000) '(address 88 main st))
		     (list 'joe (cons 'salary 30000) '(address 77 mass ave))))

(define people-delay (list 'people-delay
			   (list 'jane '(address 350 memorial dr)
				 (cons 'salary 34000))
			   (list 'ruth '(address 90 summer st))))

(define epsilon-air (list 'epsilon-air
			  '(leaf (bob ((leaf (address 23 summer st))
				       (leaf (salary . 45300))
				       (address salary))
				      ()
				      (address salary)))
			  '(leaf (amy ()
				      (leaf (address 79 winter st))
				      (address)))
			  '(bob amy)))



