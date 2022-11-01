;;; Soros Liu
;;; Exercise 1.20
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; in normal-order evaluation
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= 6 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))) ; # of remainder performed: 1
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
	 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
	 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ; # of remainder performed: 2
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	 (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(if (= 2 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	 (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) ; # of remainder performed: 4
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	 (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
(if (= 0 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	 (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))) ; # of remainder performed: 7
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
2 ; # of remainder performed: 4

; # of remainder TOTAL performed: 18

; in applicative-order evaluation
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
(gcd 40 (remainder 206 40))
(gcd 40 6) ; # of remainder performed: 1
(if (= 6 0) 40 (gcd 6 (remainder 40 6)))
(gcd 6 (remainder 40 6))
(gcd 6 4) ; # of remainder performed: 1
(if (= 4 0) 6 (gcd 4 (remainder 6 4)))
(gcd 4 (remainder 6 4))
(gcd 4 2) ; # of remainder performed: 1
(if (= 2 0) 4 (gcd 2 (remainder 4 2)))
(gcd 2 (remainder 4 2))
(gcd 2 0) ; # of remainder performed: 1
(if (= 0 0) 2 (gcd 0 (remainder 2 0)))
2

; # of remainder TOTAL performed: 4