(define (f x) (- x 1))
(define (g x) (* (+ x 1) 2))
(define (h x y) (string-append (number->string x) (number->string y)))
(define this-year (h (g (f (f (f (g 5))))) (f (g (g 5)))))

(display this-year) (newline)
