;;;; RSA.SCM

;;;; fast modular exponentiation. From the textbook, section 1.2.4

(define (expmod b e m)
  (cond ((zero? e) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (square x) (* x x))


;;; An RSA key consists of a modulus and an exponent.

(define make-key cons)
(define key-modulus car)
(define key-exponent cdr)

(define (RSA-transform number key)
  (expmod number (key-exponent key) (key-modulus key)))

;;; The following routine compresses a list of numbers to a single
;;; number for use in creating digital signatures.

(define (compress intlist)
  (define (add-loop l)
    (if (null? l)
        0
        (+ (car l) (add-loop (cdr l)))))
  (modulo (add-loop intlist) (expt 2 28)))
;;;; generating RSA keys

;;; To choose a prime, we start searching at a random odd number in a
;;; specifed range

;;; exercise 2.1
;;; way 1
(define (choose-prime smallest range)
  (search-for-prime (+ smallest (choose-random range))))
;;; way 2
(define choose-prime
  (lambda (smallest range)
    (search-for-prime (+ smallest (choose-random range)))))
;;; actually, no difference. way 1 is just a syntactic sugar of way 2

(define (choose-prime smallest range)
  (let ((start (+ smallest (choose-random range))))
    (search-for-prime (if (even? start) (+ start 1) start))))

(define (search-for-prime guess)
  (if (fast-prime? guess 2)
      guess
      (search-for-prime (+ guess 2))))

;;; The following procedure picks a random number in a given range,
;;; but makes sure that the specified range is not too big for
;;; Scheme's RANDOM primitive.

(define choose-random
  ;; restriction of Scheme RANDOM primitive
  (let ((max-random-number (expt 10 18))) 
    (lambda (n)
      (random (floor->exact (min n max-random-number))))))


;;; The Fermat test for primality, from the texbook section 1.2.6

(define (fermat-test n)
  (let ((a (choose-random n)))
    (= (expmod a n n) a)))

(define (fast-prime? n times)
  (cond ((zero? times) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))
;;; RSA key pairs are pairs of keys

(define make-key-pair cons)
(define key-pair-public car)
(define key-pair-private cdr)

;;; exercise 2.2
;;; box-and-pointer structure of key pair
; key pair -> |*|*|-> |*|*|
;              |       | |
;             |*|*|    n d
;              | |
;              n e

;;; generate an RSA key pair (k1, k2).  This has the property that
;;; transforming by k1 and transforming by k2 are inverse operations.
;;; Thus, we can use one key as the public key and one as the private key.

(define (generate-RSA-key-pair)
  (let ((size (expt 2 14)))
    ;; we choose p and q in the range from 2^14 to 2^15.  This insures
    ;; that the pq will be in the range 2^28 to 2^30, which is large
    ;; enough to encode four characters per number.
    (let ((p (choose-prime size size))
          (q (choose-prime size size)))
      (if (= p q)       ;check that we haven't chosen the same prime twice
	  (generate-RSA-key-pair)     ;(VERY unlikely)
	  (let ((n (* p q))
		(m (* (- p 1) (- q 1))))
	    (let ((e (select-exponent m)))
	      (let ((d (invert-modulo e m)))
		(make-key-pair (make-key n e) (make-key n d)))))))))


;;; The RSA exponent can be any random number relatively prime to m

(define (select-exponent m)
  (let ((try (choose-random m)))
    (if (= (gcd try m) 1)
        try
        (select-exponent m))))


;;; Invert e modulo m

(define (invert-modulo e m)
  (if (= (gcd e m) 1)
      (let ((y (cdr (solve-ax+by=1 m e))))
        (modulo y m))                   ;just in case y was negative
      (error "gcd not 1" e m)))


;;; solve ax+by=1
;;; The idea is to let a=bq+r and solve bx+ry=1 recursively

;;;(define (solve-ax+by=1 a b)...) you must complete this procedure

;;; Actual RSA encryption and decryption

;;; exercise 2.4
(define (RSA-encrypt intlist key1)
  (map (lambda (int) (RSA-transform int key1))
       intlist))
;;; analogous RSA-decrypt procedure
(define (RSA-decrypt intlist key2)
  (RSA-encrypt intlist key2))
;;; this simple scheme is inadequate for secure encryption
; it is a one-to-one mapping of all characters in the message,
; which can be easily decrypted by statistic method and brute-force dictionary search

(define (RSA-encrypt string key1)
  (RSA-convert-list (string->intlist string) key1))

(define (RSA-convert-list intlist key)
  (let ((n (key-modulus key)))
    (define (convert l sum)
      (if (null? l)
          '()
          (let ((x (RSA-transform (modulo (- (car l) sum) n)
                                  key)))
            (cons x (convert (cdr l) x)))))
    (convert intlist 0)))

(define (RSA-decrypt intlist key2)
  (intlist->string (RSA-unconvert-list intlist key2)))

;;;(define (RSA-unconvert-list intlist key) ...)
;;;  You must complete this procedure 


;;; exercise 2.3
; well, it depends.
; - 'encrypt-then-sign' is better:
;    signature is for authority checking, it's not a sensitive message, which makes no
;    sense to encrypt it. Encrypting signature is a waste of time and computation resources.
; - 'sign-then-encrypt' is better:
;    only the right receiver can decrypt the message and check authority, no others can.
;    While 'encrypt-then-sign' allows others to check authority as long as they have sender's
;    public key

;;;; searching for divisors.

;;; The following procedure is very much like the find-divisor
;;; procedure of section 1.2.6 of the test, except that it increments
;;; the test divisor by 2 each time (compare exercise 1.18 of the
;;; text).  You should be careful to call it only with odd numbers n.

(define (smallest-divisor n)
  (find-divisor n 3))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))


;;;; converting between strings and numbers

;;; The following procedures are used to convert between strings, and
;;; lists of integers in the range 0 through 2^28.  You are not
;;; responsible for studying this code -- just use it.

;;; Convert a string into a list of integers, where each integer
;;; encodes a block of characters.  Pad the string with spaces if the
;;; length of the string is not a multiple of the blocksize.

(define (string->intlist string)
  (let ((blocksize 4))
    (let ((padded-string (pad-string string blocksize)))
      (let ((length (string-length padded-string)))
        (block-convert padded-string 0 length blocksize)))))

(define (block-convert string start-index end-index blocksize)
  (if (= start-index end-index)
      '()
      (let ((block-end (+ start-index blocksize)))
        (cons (charlist->integer
	       (string->list (substring string start-index block-end)))
              (block-convert string block-end end-index blocksize)))))

(define (pad-string string blocksize)
  (let ((rem (remainder (string-length string) blocksize)))
    (if (= rem 0)
        string
        (string-append string (make-string (- blocksize rem) #\Space)))))

;;; Encode a list of characters as a single number
;;; Each character gets converted to an ascii code between 0 and 127.
;;; Then the resulting number is c[0]+c[1]*128+c[2]*128^2,...

(define (charlist->integer charlist)
  (let ((n (char->integer (car charlist))))
    (if (null? (cdr charlist))
        n
        (+ n (* 128 (charlist->integer (cdr charlist)))))))

;;; Convert a list of integers to a string. (Inverse of
;;; string->intlist, except for the padding.)

(define (intlist->string intlist)
  (list->string
   (apply
    append
    (map integer->charlist intlist))))
;;; Decode an integer into a list of characters.  (This is essentially
;;; writing the integer in base 128, and converting each "digit" to a
;;; character.)

(define (integer->charlist integer)
  (if (< integer 128)
      (list (integer->char integer))
      (cons (integer->char (remainder integer 128))
            (integer->charlist (quotient integer 128)))))

;;;; the following procedure is handy for timing things

(define (timed f . args)
  (let ((init (runtime)))
    (let ((v (apply f args)))
      (display "; ")
      (write-line (list 'time: (- (runtime) init)))
      v)))

;;;; Some initial test data

(define test-key-pair1
  (make-key-pair
   (make-key 816898139 180798509)
   (make-key 816898139 301956869)))

(define test-key-pair2
  (make-key-pair
   (make-key 513756253 416427023)
   (make-key 513756253 462557987)))

;;;public keys for political figures

(define bill-clinton-public-key (make-key 833653283 583595407))
(define al-gore-public-key (make-key 655587853 463279441))
(define bob-dole-public-key (make-key 507803083 445001911))
(define ross-perot-public-key (make-key 865784123 362279729))
(define hillary-clinton-public-key (make-key 725123713 150990017))
(define tipper-gore-public-key (make-key 376496027 270523157))
(define chuck-vest-public-key (make-key 780450379 512015071))
(define rupert-murdoch-public-key (make-key 412581307 251545759))
(define newt-gingrich-public-key (make-key 718616329 290820109))
(define newt-gingrich-private-key (make-key 718616329 129033029))


;;;message received by Newt Gingrich -- Who sent it?
(define received-mystery-message
  '(510560918 588076790 115222453 249656722 408910590 69814552
	      690687967 281490047 41430131 256420885 184791295 75938032
	      693840839 663727111 593617709 335351412))

(define received-mystery-signature 65732336)

;;; section 4
(define test-public-key1 (key-pair-public test-key-pair1))
(define result1 (rsa-encrypt "This is a test message." test-public-key1))

result1
;Value: (209185193 793765302 124842465 169313344 117194397 237972864)

;;; exercise 4.1
(define (RSA-unconvert-list intlist key)
  (let ((n (key-modulus key)))
    (define (convert l ele)
      (if (null? l)
          '() 
          (cons (modulo (+ (RSA-transform (car l) key) ele) n)
                (convert (cdr l) (car l)))))
    (convert intlist 0)))

(define test-private-key1 (key-pair-private test-key-pair1))

(RSA-unconvert-list result1 test-private-key1)
;Value: (242906196 69006496 213717089 229128819 205322725 67875559)

(RSA-decrypt result1 test-private-key1)
;Value: "This is a test message. "

(define test-public-key2 (key-pair-public test-key-pair2))
(define test-private-key2 (key-pair-private test-key-pair2))

(RSA-decrypt (RSA-encrypt "Soros Liu Messaging..." test-public-key2) test-private-key2)
;Value: "Soros Liu Messaging...  "

;;; exercise 4.2
(define make-signed-message cons)
(define signed-message-message car)
(define signed-message-signature cdr)

(define (encrypt-and-sign message sender-private-key recipient-public-key)
  (let* ((encrypted (RSA-encrypt message recipient-public-key))
         (signature (car (RSA-convert-list (list (compress encrypted)) sender-private-key)))) 
    (make-signed-message encrypted signature)))

(define result2
  (encrypt-and-sign "Test message from user 1 to user 2"
                    test-private-key1
                    test-public-key2))

(signed-message-message result2)
;Value: (499609777 242153055 12244841 376031918 242988502 31156692 221535122 463709109 468341391)
(signed-message-signature result2)
;Value: 15378444

(define (authenticate-and-decrypt signed-message sender-public-key recipient-private-key)
  (let* ((encrypted-message (signed-message-message signed-message))
         (encrypted-signature (signed-message-signature signed-message))
         (decrypted-message (RSA-decrypt encrypted-message recipient-private-key))
         (unencrypted-signature (compress encrypted-message))
         (decrypted-signature (car (RSA-unconvert-list (list encrypted-signature) sender-public-key))))
    (if (= unencrypted-signature decrypted-signature)
	decrypted-message
	(error "signature check failed"))))

(authenticate-and-decrypt result2 test-public-key1 test-private-key2)
;Value: "Test message from user 1 to user 2  "

(authenticate-and-decrypt result2 test-public-key2 test-private-key2)
;signature check failed

;;; exercise 4.3
(define make-key-ownership cons)
(define key-owner car)
(define key-key cdr)

(define public-keys (list 
		     (make-key-ownership "bill-clinton" bill-clinton-public-key)
		     (make-key-ownership "al-gore" al-gore-public-key)
		     (make-key-ownership "bob-dole" bob-dole-public-key)
		     (make-key-ownership "ross-perot" ross-perot-public-key)
		     (make-key-ownership "hillary-clinton" hillary-clinton-public-key)
		     (make-key-ownership "tipper-gore" tipper-gore-public-key)
		     (make-key-ownership "chuck-vest" chuck-vest-public-key)
		     (make-key-ownership "rupert-murdoch" rupert-murdoch-public-key)
		     (make-key-ownership "newt-gingrich" newt-gingrich-public-key)))

(define (identify-and-decrypt signed-message public-keys recipient-private-key)
  (define (aux keys)
    (if (null? keys)
	'done
	(let* ((test-key (car keys))
	       (sender-public-key (key-key test-key))
	       (result (ignore-errors (lambda ()
					(authenticate-and-decrypt signed-message
								  sender-public-key
								  recipient-private-key)))))
	  (cond ((condition? result) (aux (cdr keys)))
		(else
		 (newline)
		 (display "; sender: ")
		 (display (key-owner test-key))
		 result)))))
  (aux public-keys))

(identify-and-decrypt (make-signed-message received-mystery-message
                                           received-mystery-signature)
                      public-keys
                      newt-gingrich-private-key)
; sender: rupert-murdoch
;Value: "The time has come for the Pineapple to stir up the Whitewater.  "

;;; exercise 4.4
(define (solve-ax+by=1 a b)
  (cond ((= a 1) (cons 1 0))
        ((= b 1) (cons 0 1))
        (else
	 (let* ((q (quotient a b))
		(r (remainder a b))
		(result (solve-ax+by=1 b r))
		(x (cdr result))
		(y (- (car result) (* q x))))
	   (cons x y)))))

;;; 233987973*x + 41111687*y = 1
(define xy (solve-ax+by=1 233987973 41111687))
xy
;Value: (-11827825 . 67318298)

(+ (* (car xy) 233987973) (* (cdr xy) 41111687))
;Value: 1

(define my-key-pair (generate-RSA-key-pair))
my-key-pair
;Value: ((941994797 . 105595915) 941994797 . 104160355)

(RSA-decrypt (RSA-encrypt "Message encrypted with my-key-pair" 
                          (key-pair-public my-key-pair)) 
             (key-pair-private my-key-pair))
;Value: "Message encrypted with my-key-pair  "

;;; exercise 4.5
(define (crack-RSA public-key)
  (let* ((n (key-modulus public-key))
         (e (key-exponent public-key))
         (p (smallest-divisor n))
         (q (/ n p))
         (m (* (-1+ p) (-1+ q))))
    (make-key n (invert-modulo e m))))

(crack-RSA (key-pair-public test-key-pair1))
;Value: (816898139 . 301956869)
(key-pair-private test-key-pair1)
;Value: (816898139 . 301956869)

(crack-RSA (key-pair-public test-key-pair2))
;Value: (513756253 . 462557987)
(key-pair-private test-key-pair2)
;Value: (513756253 . 462557987)

;;; exercise 4.6
(define (forge message sender-public-key recipient-public-key)
  (encrypt-and-sign message
                    (crack-RSA sender-public-key)
                    recipient-public-key))

(define forged-message (forge "I, Clinton, together with Gore, are planning a major tax increase" 
                              bill-clinton-public-key 
                              newt-gingrich-public-key))

(signed-message-message forged-message)
;Value: (5742574 193677883 55915667 409516844 530554374 371957850 432037720 236069385 405375956 398453515 564849095 398088203 86908665 423806422 282703438 659958493 718396659)
(signed-message-signature forged-message)
;Value: 499983473

(authenticate-and-decrypt forged-message
                          bill-clinton-public-key
                          newt-gingrich-private-key)
;Value: "I, Clinton, together with Gore, are planning a major tax increase   "

;;; exercise 4.7
(define forged1 (forge "bob dole say hi"
		       bob-dole-public-key
		       (key-pair-public my-key-pair)))
(authenticate-and-decrypt forged1
                          bob-dole-public-key
                          (key-pair-private my-key-pair))
;Value: "bob dole say hi "

(define forged2 (forge "DIVORCE!"
                       hillary-clinton-public-key
                       bill-clinton-public-key))
(authenticate-and-decrypt forged2
                          hillary-clinton-public-key
                          (crack-RSA bill-clinton-public-key))
;Value: "DIVORCE!"

;;; exercise 4.8
(timed smallest-divisor 780450379)
; (time: 9.999999999999787e-3)
;Value: 25057

(for-each (lambda (i)
            (timed smallest-divisor (key-modulus (key-pair-public (generate-RSA-key-pair)))))
          (iota 5))
; (time: 2.0000000000000018e-2)
; (time: 1.0000000000000231e-2)
; (time: 9.999999999999787e-3)
; (time: 0.)
; (time: 1.0000000000000231e-2)
;Unspecified return value

(define (seconds-to-years seconds)
  (/ seconds (* 3600 24 365)))

; let time to crack an RSA code of n digtis long prime should take T(n),
; then T(n+1) = sqrt(10) * T(n)
; so O(n) = sqrt(10)^n

; for n = 5, O(5) = sqrt(10)^5, it takes about 0.01 seconds,

; for n = 50, O(50) = sqrt(10)^50, T(50) = sqrt(10)^45 * T(5), it should take about 10027516679884 years
(seconds-to-years (* 0.01 (expt (sqrt 10) 45)))
;Value: 10027516679884.56

; for n = 100, O(100) = sqrt(10)^100, T(100) = sqrt(10)^95 * T(5), it should take about 1.0027516679884616e38 years
(seconds-to-years (* 0.01 (expt (sqrt 10) 95)))
;Value: 1.0027516679884616e38
