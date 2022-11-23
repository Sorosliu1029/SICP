;;; Soros Liu
;;; Exercise 2.55
;;; https://sicp.sorosliu.xyz/book/book-Z-H-16.html#%_thm_2.55

(car ''abracadabra)
;Value: quote

(car ''abracadabra) ;;; =>
(car '(quote abracadabra)) ;;; =>
;Value: quote

(cdr '(quote abracadabra))
;Value: (abracadabra)

(cadr '(quote abracadabra))
;Value: abracadabra