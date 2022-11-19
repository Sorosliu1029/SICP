;;; Soros Liu
;;; Exercise 2.51
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.51

;;; analogous to beside
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;;; with beside and rotation
(define (below painter1 painter2)
  (compose rotate90 (beside (rotate270 painter1)
                            (rotate270 painter2))))
