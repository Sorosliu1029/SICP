;;; Soros Liu
;;; Exercise 2.52
;;; https://sicp.sorosliu.xyz/book/book-Z-H-15.html#%_thm_2.52

;;; a
(define (smiling-wave)
  (let ((p1 (make-vect 0 0.8))
        (p2 (make-vect 0.15 0.57))
        (p3 (make-vect 0.25 0.6))
        (p4 (make-vect 0.4 0.6))
        (p5 (make-vect 0.3 0.8))
        (p6 (make-vect 0.4 1))

        (q1 (make-vect 0.6 1))
        (q2 (make-vect 0.7 0.8))
        (q3 (make-vect 0.6 0.6))
        (q4 (make-vect 0.8 0.6))
        (q5 (make-vect 1 0.4))

        (r1 (make-vect 1 0.2))
        (r2 (make-vect 0.6 0.45))
        (r3 (make-vect 0.75 0))

        (s1 (make-vect 0.6 0))
        (s2 (make-vect 0.5 0.3))
        (s3 (make-vect 0.4))

        (t1 (make-vect 0.25 0))
        (t2 (make-vect 0.35 0.5))
        (t3 (make-vect 0.25 0.57))
        (t4 (make-vect 0.15 0.4))
        (t5 (make-vect 0 0.6))

        (smile1 (make-vect 0.4 0.8))
        (smile2 (make-vect 0.5 0.7))
        (smile3 (make-vect 0.6 0.8)))
    (segment->painter (list (make-segment p1 p2)
                            (make-segment p2 p3)
                            (make-segment p3 p4)
                            (make-segment p4 p5)
                            (make-segment p5 p6)

                            (make-segment q1 q2)
                            (make-segment q2 q3)
                            (make-segment q3 q4)
                            (make-segment q4 q5)

                            (make-segment r1 r2)
                            (make-segment r2 r3)

                            (make-segment s1 s2)
                            (make-segment s2 s3)
                            
                            (make-segment t1 t2)
                            (make-segment t2 t3)
                            (make-segment t3 t4)
                            (make-segment t4 t5)

                            (make-segment smile1 smile2)
                            (make-segment smile2 smile3)))))

;;; b
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
            (bottom-right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

;;; c
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))