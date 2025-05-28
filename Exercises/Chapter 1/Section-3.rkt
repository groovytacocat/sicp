#lang sicp
; Procedures just to prevent IDE from showing errors in file
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x) (if (< x 0) (- x) x))
(define (even? x) (= (remainder x 2) 0))
(define (inc n) (+ n 1))
(define (average x y)
  (/ (+ x y) 2))
(define (fast-expt b n)
  (cond [(= n 0)
         1]
        [(even? n)
         (square (fast-expt b (/ n 2)))]
        [else
         (* b (fast-expt b (- n 1)))]))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (next num)
      (if (= num 2)
          3
          (+ num 2)))
    (cond [(> (square test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (next test-divisor))]))
  (find-divisor n 2))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define tolerance 0.00001)
(define (average-damp f)
  (lambda (x) (average x (f x))))


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

#|
Exercise 1.32
a. Show that sum and product are both special cases ofa  still more general notion called accumulate that combines a collection of terms, using some general accumulation function:

(accumulate combiner null-value term a next b)

Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the proceeding terms
and a null-value that specifies what base value to use when the terms run out. Write accumulate to show how sum and product can both be defined as simple calls to accumulate

b. If your accumulate procedure generates a recursive process, write one that generates an iterative process and vice-versa
|#

; a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; b
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

#|
Exercise 1.33
You can obtain an even more general version of accumulate by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition.
The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter.
Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

a. The sum of the squares of the prime numbers in the interval a to b (assuming that have a prime? predicate already written)

b. The product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i, n) = 1)
|#

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate filter combiner null-value term (next a) next b))))

; a
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

; b
(define (rel-prime-prod n)
  (define (rel-prime i n)
    (= (gcd i n) 1))
  (filtered-accumulate rel-prime * 0 identity 1 inc (- n 1)))

#|
Exercise 1.34
Suppose we define the procedure

(define (f g)
  (g 2))

Then we have

(f square)
4

(f (lambda (z) (* z (+ z 1)))
6

What happens if we (perversely) ask the interpreter to evaluate the combination (f f) ? Explain
|#

; This fails to evaluate as (f f) will become (f 2) then (2 2) and 2 is not a procedure that can be applied/evaluated

#|
Exercise 1.35
Show that the golden ratio Phi is a fixed-point of the transformation x -> 1 + 1/x, and use this fact to compute Phi by means of the fixed-point procedure
|#

#|
x = 1 + 1/x
x^2 - x - 1 = 0
      
      1 + sqrt(5)
x =  ------------
          2
|#

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

#|
Exercise 1.36
Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and dispaly primitives shown in exercise 1.22.
Then find a solution to x^x = 100 by finding a fixed-point of x-> log(1000)/log(x)
Compare the number of steps this takes with an without average damping.
|#

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Without the average damping fixed-point took 35 steps to find a point within tolerance

; With average damping fixed-point took 10 steps

#|
Exercise 1.37
a. An infinite continued function is an expression of the form:
f = n1 / (d1 + (n2 / (d2 + (n3 / (d3 + ...)))))

As an example, one can show that the infinite continued fraction expansion with the N_i and D_i all equal to 1 produces 1/Phi, where Phi is the golden ratio.
One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation - a so-called k-term finited continued fraction -- has the form:

N1/(D1 + (N2/....+ NK/DK))

Suppose that n and d are procedures of one argument (the term index i) that return the N_i and D_i of the terms of the continued fraction.
Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction.

Check your procedure by approximating 1/Phi using:
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

b. If your cont-frac procedure generates a recursive process, write on that generates an iterative process and vice-versa
|#


; a
(define (cont-frac n d k)
  (define (help i)
    (/ (n i) (+ (d i)
                (if (= i k)
                    0
                    (help (+ i 1))))))
  (help 1))

; Spent too much time trying to figure out what was wrong before finding an archived post on schemewiki explaining why my recursive solution without an inner help function was incorrect
; k must be at least 12 to obtain 4 decimal place precision


; b
(define (cont-frac-iter n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (n k) (+ result (d k))))))
  (iter (- k 1) (/ (n k) (d k))))

#|
Exercise 1.38
Euler wrote a continued fraction expansion for e - 2, where e is the base of the natural logarithms.
In this fraction, the N_i are all 1, and the D_i are all successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8...

Write a program that uses your cont-frac procedure to approximate e, based on Euler's expansion
|#

(define (e-approx k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (= (remainder i 3) 2) ; This segment is weird as I initially had (= (remainder i 3) 1)
                        (* 2 (/ (+ i 1) 3.0)) ; and this segement being (* 2 (/ (i + 2) 3)) which produces the correct coefficients
                        1.0))                 ; however the resulting sum/approximation was always off/incorrect by a factor I could not determine
                  k)))

#|
Exercise 1.39

tan(x) = x / 1 - x^2/ 3 - x^2/ 5 - ...

Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert's formula. K specifies the number of terms to compute as in exercise 1.37
|#

(define (tan-cf x k)
  (let ((n (* (square x) -1)))
    (cont-frac (lambda (i) (if (= i 1) x n))
               (lambda (i) (- (* 2 i) 1))
               k)))

#|
Exercise 1.40
Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form

(newtons-method (cubic a b c) 1)

to approximate zeros of the cubic x^3 + ax^2 + bx + c
|#

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

#|
Exercise 1.41
Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice.
For example if inc is a procedure that adds 1 to its argument then (doubl inc) should be a procedure that adds 2.

What value is returned by (((double (double double)) inc) 5)
|#

(define (double f)
  (lambda (x) (f (f x))))

;(((double (double double)) inc) 5) -> 21

#|
Exercise 1.42
Let f and g be two one-argument functions. The composition of f after g is defined to be the function x-> f(g(x)).
Define a procedure compose that implements composition. For example, if inc is a procedure that addss 1 to its argument,

((compose square inc) 6)
49
|#

(define (compose f g)
  (lambda (x) (f (g x))))

#|
Exercise 1.43
If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f, which is defined to be the function
whose value at x is f(f(...(f(x))...)). For example, if f is the function x -> x + 1, then the nth repeated application of f is the function x -> x + n.
If f is the operation of squaring a number, then the nth repeated application of f is the function that raises its argument to the 2^n power.
Write a procedure that takes as inputs a procedure that computes f and a positive integer n and returns the procedure that copmutes the nth repeated application of f.
Your procedure should be able to be used as follows:

((repeated square 2) 5)
625
|#

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

#|
Exercise 1.44
The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value
at a point x is the average of f(x-dx), f(x), and f(x+dx). Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f.
It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function and so on) to obtain the n-fold smoothed function.
Show how to generate the n-fold smoothed function of any given function using smooth and repeated.
|#

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

#|
Exercise 1.45
We saw in Section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y -> x / y does nto converge, and that this can be fixed by average damping. The same method works for finding
cube roots as fixed points of the average-damped y -> x / y^2. For fourth roots average damping twice is required to converge. Do some experiments to determine how many average damps are required to compute 
nth roots as a fixed-point search based upon repeated average damping of y -> x / y^n-1. 
Use this to implement a procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure. Assume that any arithmetic operations you need are available as primitives
|#

; Funnily enough when reading Section 1.3.3 I tried to do this on my own, I couldn't figure out how to apply repeated average-dampings though at the time 
; but I did manage to see in my testing that the number of average-dampings seemed to fall in line with the floor of lg(n) where n is the root we are trying to calculate

(define (nth-root n x)
  (let ((a (floor (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp a) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0)))

#|
Exercise 1.46
Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement.
Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess
and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments:
a method for telling whether a guess is good enough and a method for improving a guess. 

Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough.

Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative improve
|#

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x)
        x
        ((iterative-improve good-enough? improve) (improve x)))))


; Took me a minute to figure this out and had to look up help before I realized I was using a different defintion of sqrt from slightly later that involved successive guesses
(define (improve-sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess)))) 
   1.0))

(define (improve-fix f x)
  ((iterative-improve (lambda (x) (< (abs (- x (f x))) tolerance)) f) x))