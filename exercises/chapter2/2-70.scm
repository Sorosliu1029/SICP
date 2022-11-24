;;; Soros Liu
;;; Exercise 2.70
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.70

;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;; generate huffman tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (= (length pairs) 1)
    (car pairs)
    (successive-merge (adjoin-set (make-code-tree (first pairs)
                                                  (second pairs))
                                  (drop pairs 2)))))

;;; encode
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (recur subtree)
    (cond ((leaf? subtree) '())
          ((contain? symbol (left-branch subtree))
           (cons 0 (recur (left-branch subtree))))
          ((contain? symbol (right-branch subtree))
           (cons 1 (recur (right-branch subtree))))
          (else
            (error "symbol not in tree" symbol))))
  (define (contain? symbol tree)
    (not (not (find (lambda (i) (eq? i symbol)) (symbols tree)))))
  (recur tree))

;;; verify
(define pairs '((A 2) (NA	16) (BOOM	1) (SHA	3) (GET	2) (YIP	9) (JOB	2) (WAH	1)))
(define lyrics '(Get a job
                     Sha na na na na na na na na
                     Get a job
                     Sha na na na na na na na na
                     Wah yip yip yip yip yip yip yip yip yip
                     Sha boom))

(define tree (generate-huffman-tree pairs))
tree
;Value: ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)

(define bits (encode lyrics tree))
bits
;Value: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(length bits)
;Value: 84

(* (length lyrics) 3)
;Value: 108
