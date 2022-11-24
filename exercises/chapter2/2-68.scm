;;; Soros Liu
;;; Exercise 2.68
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (define (recur subtree)
    (cond ((leaf? subtree) '())
          ((contain? char (left-branch subtree))
           (cons 0 (recur (left-branch subtree))))
          ((contain? char (right-branch subtree))
           (cons 1 (recur (right-branch subtree))))
          (else
            (error "symbol not in tree" char))))
  (define (contain? char tree)
    (not (not (find (lambda (i) (eq? i char)) (symbols tree)))))
  (recur tree))

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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

;;; verify
(encode '(a d a b b c a) sample-tree)
;Value: (0 1 1 0 0 1 0 1 0 1 1 1 0)