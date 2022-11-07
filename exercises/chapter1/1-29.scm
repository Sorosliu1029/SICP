;;; Soros Liu
;;; Exercise 1.29
;;; https://sicp.sorosliu.xyz/book/book-Z-H-12.html#%_thm_1.29

;;; Simpson's Rule

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define inc 1+)
    (define (term k)
      (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
            ((odd? k) (* 4 (f (+ a (* k h)))))
            (else (* 2 (f (+ a (* k h)))))))
    (* (/ h 3.0) (sum term 0 inc n))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube-integral n)
  (simpson-integral cube 0 1 n))

(cube-integral 100)
;Value: .25

(cube-integral 1000)
;Value: .25

;;; numberically approximate

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(integral cube 0 1 0.01)
;Value: .24998750000000042

(integral cube 0 1 0.001)
;Value: .249999875000001

;;; Simpson's Rule is more accurate