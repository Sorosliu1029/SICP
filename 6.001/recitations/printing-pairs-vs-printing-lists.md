Scheme generally prints a pair in the following form: (<car> . <cdr>)

Therefore, assuming we have the function write that can print atoms and character strings, we may write a procedure for printing structures made of pairs as follows:

(define (my-write e)
  (cond ((pair? e)
         (display “(”)
         (my-write (car e))
         (display " . “)
         (my-write (cdr e))
         (display “)”))
        (else (write e))))

Then, we get the following:

(my-write (cons (cons 1 2)
                (cons 3 (cons 4 5))))
((1 . 2) . (3 . (4 . 5)))

Note, however, that this method of printing makes lists look rather more complex than we ordinarily like to think of a list of objects:

(my-write (list 1 2 3 4))
(1 . (2 . (3 . (4 . #f))))

To get around this, Scheme in fact uses a printer more like the following, which is haired up to deal better with lists:

(define (your-write e)
  (define (list-write e)
    (cond ((pair? e)
           (your-write (car e))
           (display " “)
           (list-write (cdr e)))
          ((null? e))
          (else (display “. “)
                (write e))))
  (cond ((pair? e) (display “(”)
                   (list-write e)
                   (display “)”))
        (else (write e))))

Now, as desired, we get

(your-write
     (cons 1 (cons 2 (cons 3 ‘()))))
(1 2 3 )

(your-write
     (cons (cons 1 2) (cons 3 (cons 4 5))))
((1 . 2) 3 4 . 5)

In fact, with the exception of an extra space before the end of a list, this is just like what Scheme’s built-in printer does. You can verify that this algorithm is equivalent to the “hack” I suggested in section: print using the first method (fully dotted and parenthesized), and then erase all occurrences of “. (” and the corresponding “)”. (This also assumes that the empty list is printed as () rather than as #f.)