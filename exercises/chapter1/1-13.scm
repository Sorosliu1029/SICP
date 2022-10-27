;;; Soros Liu
;;; Exercise 1.13
;;; https://sicp.sorosliu.xyz/book/book-Z-H-11.html#%_thm_1.13

;;; let phi = (1+sqrt(5))/2, psi = (1-sqrt(5))/2,
;;; when n = 0, fib(0) = (phi^0 - psi^0)/sqrt(5) = 0
;;; when n = 1, fib(1) = (phi^1 - psi^1)/sqrt(5) = 1
;;; when n >= 2, fib(n) = fib(n-1) + fib(n-2) =
;;;  (phi^(n-1)-psi^(n-1))/sqrt(5) + (phi^(n-2)-psi^(n-2))/sqrt(5) =
;;;  ((phi^(n-1)+phi^(n-2)) - (psi^(n-1)+psi^(n-2))) / sqrt(5) =
;;;  ((phi^(n-2)*(phi+1)) - (psi^(n-2)*(psi+1))) / sqrt(5) =
;;;  ((phi^(n-2)*(3+sqrt(5))/sqrt(5)) - (psi^(n-2)*(3-sqrt(5))/sqrt(5))) / sqrt(5) =
;;;  ((phi^(n-2)*phi^2) - (psi^(n-2)*psi^2)) / sqrt(5) =
;;;  (phi^n - psi^n) / sqrt(5) =
;;;  fib(n)
;;;
;;; so phi^n/sqrt(5) = fib(n) + psi^n/sqrt(5)
;;; so just need to prove -0.5 < psi^n/sqrt(5) < 0.5
;;; psi ~= -0.618, so for n >= 1, it's true that -0.5 < psi^n/sqrt(5) < 0.5
;;;
;;; done

