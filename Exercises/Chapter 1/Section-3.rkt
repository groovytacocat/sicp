#lang sicp
; Procedures just to prevent IDE from showing errors in file
(define (square x) (* x x))
(define (abs x) (if (< x 0) (- x) x))
(define (even? x) (= (remainder x 2) 0))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

#|
Exercise 1.29
Simpson's Rule is a more accurate method of numerical integration thant he method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as

h/3 [y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_n-2 + 4y_n-1 + y_n]

where h = (b - a) / n, for some even integer n and y_k = f(a+kh).

Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule. 

Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000) compare the results to those of the integral procedure above
|#

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (simp-term k)
    (cond [(or (= k 0) (= k n)) (y k)]
          [(even? k) (* 2 (y k))]
          [else (* 4 (y k))]))
  (/ (* h (sum simp-term 0 inc n)) 3))

;(simpson cube 0 1 100) -> 1/4
;(simpson cube 0 1 1000) -> 1/4

#|
Exercise 1.30
The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expression in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (??)
        (??)
        (iter (??) (??))))
  (iter ?? ??))
|#
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

#|
Exercise 1.31
a. The sum procedure is only the simplest of a vast number of abstractions that can be captured as higher-order procedures.
Write a procedure called product that returns the product of the values of a function at points over a given range. 
Show how to define factorial in terms of product.
Also use product to compute approximations to pi using the formula

pi = 2 * 4 * 4 * 6 * 6 * 8 ...
--   ---------------------
4    3 * 3 * 5 * 5 * 7 * 7 ....


b. If your product procedure generates a recursive process, write one that generates an iterative process and vice-versa
|#

; a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx n)
  (define (pi-term k)
    (/ (* 4 (square k)) (- (* 4 (square k)) 1))) ; Looked stuff up online bc couldn't figure out how to get terms working. Initial solution involved even checking to determine when/what to add 2 to.
  (* 2 (product pi-term 1.0 inc n)))

; b

(define (product-iter term a next b)
  (define (iter a prod)
    (if (> a b)
        prod
        (iter (next a) (* prod (term a)))))
  (iter a 1))