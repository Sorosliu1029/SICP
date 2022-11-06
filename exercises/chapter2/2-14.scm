;;; Soros Liu
;;; Exercise 2.14
;;; https://sicp.sorosliu.xyz/book/book-Z-H-14.html#%_thm_2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(define (percent i)
  (/ (width i) (center i)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (dis-interval i)
  (newline)
  (display ";(")
  (display (center i))
  (display " /w ")
  (display (percent i))
  (display ")"))

(define r1 (make-center-percent 6.8 0.1))
(define r2 (make-center-percent 4.7 0.05))

r1
;Value: (6.12 . 7.4799999999999995)

r2
;Value: (4.465 . 4.9350000000000005)

(define par1_r (par1 r1 r2))
par1_r
;Value: (2.201031010873943 . 3.4873689182805854)
(dis-interval par1_r)
;(2.844199964577264 /w .22613352145193347)

(define par2_r (par2 r1 r2))
par2_r
;Value: (2.581558809636278 . 2.97332259363673)
(dis-interval par2_r)
;(2.777440701636504 /w .0705260392723452)

;;; Lem is right, the program gives different answers for the two ways of computing

(div-interval r1 r1)
;Value: (.8181818181818182 . 1.222222222222222)
(dis-interval (div-interval r1 r1))
;(1.02020202020202 /w .19801980198019795)

(div-interval r2 r2)
;Value: (.9047619047619047 . 1.105263157894737)
(dis-interval (div-interval r2 r2))
;(1.0050125313283207 /w .09975062344139662)

(div-interval r1 r2)
;Value: (1.2401215805471124 . 1.6752519596864501)
(dis-interval (div-interval r1 r2))
;(1.4576867701167813 /w .1492537313432836)

(div-interval r2 r1)
;Value: (.5969251336898396 . .806372549019608)
(dis-interval (div-interval r2 r1))
;(.7016488413547237 /w .14925373134328368)

(mul-interval r1 r1)
;Value: (37.4544 . 55.950399999999995)
(dis-interval (mul-interval r1 r1))
;(46.7024 /w .19801980198019797)

(mul-interval r2 r2)
;Value: (19.936225 . 24.354225000000007)
(dis-interval (mul-interval r2 r2))
;(22.145225000000003 /w .09975062344139664)

;;; multiply and divide intervals will ACCUMULATE their percentage (sum up)
;;; also proofed by exercise 2.13

(add-interval r1 r1)
;Value: (12.24 . 14.959999999999999)
(dis-interval (add-interval r1 r1))
;(13.6 /w .09999999999999996)

(add-interval r2 r2)
;Value: (8.93 . 9.870000000000001)
(dis-interval (add-interval r2 r2))
;(9.4 /w 5.0000000000000065e-2)

(add-interval r1 r2)
;Value: (10.585 . 12.415)
(dis-interval (add-interval r1 r2))
;(11.5 /w .07956521739130427)

;;; add interval will AVERAGE their percentage
