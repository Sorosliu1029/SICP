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

;;; Code for 6.001 Fall 1989, PS 4
;;; Evaluate immediately everything up to the line of asterisks.
;;; Below that line are
;;;   - incomplete fragments to be filled out as part of the PS, and
;;;   - example queries for testing.

(define make-table
    (lambda (col-names rows)
      (cons col-names rows)))

(define EVENTS
  (make-table
    '(EVENT                            ZOMBIE     HOURS  RUNNER-UP  RU-HOURS)
    '((6.001-PS2-Pro-Am                 Ben        9      Alyssa     6)
      (6.004-Lab-6-Invitational         Lem-E      10     Alyssa     9)
      (French-IV-Open                   Cy-D       8      Alonzo     6)
      (18.06-HW3-All-Stars              Alonzo     11     Eva-Lu     8)
      (6.035-Masters                    Louis      19     Cy-D       15)
      (Tour-de-6.170                    Alyssa     25     Lem-E      22)
      (German-III-Weltmeisterschaft     Eva-Lu     12     Lem-E      10)
      (6.003-Coupe-le-Monde             Alyssa     14     Ben        11)
      (End-of-Term-Grand-Slam           Alyssa     30     Louis      25))))

(define ATHLETES
  (make-table
    '(NAME       SPONSOR                    LAST-YEAR-PANA-RANKING)
    '((Ben        Mocha-Java-of-Mexico       2)
      (Alyssa     Juan-Valdez-of-Colombia    3)
      (Lem-E      Santos-of-Guatemala        7)
      (Louis      Yrgacheffe-of-Ethiopia     6)
      (Alonzo     Celibe-de-Haiti            4)
      (Eva-Lu     11-7                       1)
      (Cy-D       Juan-Valdez-of-Colombia    5))))

(define map
  (lambda (proc lst)
    (if (null? lst)                    ; if lst is empty
        '()                            ; then return an empty list
        (cons (proc (car lst))         ; otherwise apply proc to first element 
              (map proc (cdr lst)))))) ; and add it to front of list
                                       ; obtained by processing rest of list

(define filter
  (lambda (pred lst)
    (cond
      ((null? lst)      '())                  ; if lst empty, return empty list
      ((pred (car lst))                       ; if car satisfies pred,
           (cons (car lst)                    ;    include it
                 (filter pred (cdr lst))))    ;            with  remainder
      (else                                   ; otherwise
           (filter pred (cdr lst))))))        ;    discard car, do remainder

(define cross-product
    (lambda (x-list y-list)
        (flatten2 (map (lambda (x) (map (lambda (y) (list x y))
                                        y-list))
                       x-list))))

(define op-of   (lambda (e) (car e)))
(define arg1-of (lambda (e) (cadr e)))
(define arg2-of (lambda (e) (caddr e)))

;;; ****************************************************************
;;; Incomplete fragments to be filled out as part of the PS.

(define lookup
  (lambda (col col-names row)
    'TO-BE-COMPLETED    ))

(define project-row
  (lambda (cols col-names row)
    'TO-BE-COMPLETED    ))

(define project
  (lambda (cols table)
    'TO-BE-COMPLETED    ))

(define evaluate
  (lambda (expr col-names row)
    (cond
      ((symbol? expr)      (lookup expr col-names row))
      ((number? expr)      expr)
      ((eq? (op-of expr) '=) (= (evaluate (arg1-of expr) col-names row )
                                (evaluate (arg2-of expr) col-names row)))
      ((eq? (op-of expr) '<) (< (evaluate (arg1-of expr) col-names row )
                                (evaluate (arg2-of expr) col-names row)))

;;;   ...  and so on for other operators ...
;;;        TO BE COMPLETED

      (else (error "EVALUATE: expression not well-formed" expr))
    )))

(define select
  (lambda (pred table)
    'TO-BE-COMPLETED   ))

(define join
  (lambda (table-1 table-2)
    (let  ((N1 (col-names-of table-1))
           (N2 (col-names-of table-2))
           (R1 (rows-of table-1))
           (R2 (rows-of table-2)))
      (make-table
        'TO-BE-COMPLETED   
        'TO-BE-COMPLETED    ))))

;;; ----------------------------------------------------------------
;;; Example queries for testing.
;;;
(define Q1 (lambda ()
    (project '(event zombie runner-up)
             EVENTS)))

(define Q2 (lambda ()
    (select '(eq? sponsor 'Juan-Valdez-of-Colombia)
             ATHLETES)))

(define Q3 (lambda ()
    (project '(event zombie)
        (select '(= hours (+ ru-hours 3))
            EVENTS))))

(define Q4 (lambda ()
    (project '(event)
             (select '(eq? sponsor 'Juan-Valdez-of-Colombia)   ;;; (1)
                     (select '(eq? zombie name)                ;;; (2)
                             (join EVENTS ATHLETES))))))

(define Q5 (lambda ()
    (project '(event)
             (select '(and (eq? sponsor 'Juan-Valdez-of-Colombia)
                           (eq? zombie name))
                     (join EVENTS ATHLETES)))))

(define Q6 (lambda ()
    (project '(event)
             (select '(eq? zombie name)
                     (join EVENTS
                           (select '(eq? sponsor 'Juan-Valdez-of-Colombia)
                                    ATHLETES))))))

(define Q7 (lambda ()
    (project '(zombie event)
             (select '(> hours 10)
                     (select '(eq? zombie name)
                             (select '(< last-year-pana-ranking 3)
                                     (join EVENTS
                                           ATHLETES)))))))

;;; ----------------------------------------------------------------
