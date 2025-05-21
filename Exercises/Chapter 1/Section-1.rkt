#lang sicp

(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

#|
Exercise 1.1. What is the result printed by the interpreter in response to each expression?
Assume that the sequence is to be evaluated in the order in which it is presented
|#
10
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(define a 3)
; Nothing

(define b (+ a 1))
; Nothing

(+ a b (* a b))
; 19

(if (and (> b a) (< b (* a b))) b a)
; 4

(cond
  [(= a 4) 6]
  [(= b 4) (+ 6 7 a)]
  [else 25])
; 16 -> b = 4 -> (+ 6 7 3)

(+ 2 (if (> b a) b a))
; 6

(* (cond
     [(> a b) a]
     [(< a b) b]
     [else -1])
   (+ a 1))
; 16 -> (< a b) = #t -> (* 4 (+ 3 1)) -> 16

#|
Exercise 1.2
Translate the following expression into prefix form
5 + 4 + (2 - (3 - (6 + 4/5)))
-----------------------------
        3(6-2)(2-7)
|#
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

#|
Exercise 1.3
Define a procedure that takes three numbers as arguments and returns the usm of the squares of the two larger numbers
|#
(define (square-largest x y z)
  (cond
    [(and (>= x z) (>= y z)) (sum-of-squares x y)]
    [(and (>= x y) (>= z y)) (sum-of-squares x z)]
    [else (sum-of-squares y z)]))

#|
Exercise 1.4
Observe that our model of evaluation allows for combinations whose operators are compound expressions
Use this observation to describe the behavior of the following procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
|#
; This procedures adds a and the absolute value of b
; This occurs by checking if b is positive and non-zero, if so the conditional returns a '+' operator, otherwise it returns the '-' operator.
; E.g. if b is a positive number then the condition evaluates to (+ a b) and if b is not positive it evaluates to (- a b) -> a - (-b) -> a + b

#|
Exercise 1.5
Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation.
He defines the following two procedures:
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

Then he evaluates (test 0 (p))

What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
What behavior will he observe with an interpreter that uses normal-order evaluation?
Explain your answer.
Assume that the evaluation rule for the special form if is the same: Predicate expression is evaluated first and result determines whether to evaluate consequent or alternate expression
|#

#| Under normal order he should expect to see the interpeter evaluate/print a 0. This is because the interpeter will fully expand 'test' into
(if (= 0 0)
    0
    (p))
and then evaluate the if statement's predicate and return 0


Under Applicative-Order this will not evaluate to anything and will loop infinitely. This is because Applicative Order evaluates the arguments first then applies the procedure, meaning that
(test 0 (p)) will first evaluate 0 to 0 and then evaluate (p) which is an infinite recursion on itself.
|#

#|
Exercise 1.6
Alyssa P. Hacker doesn't see why if needs to be provided as a special form. "Why can't I just define it as an ordinary procedure in terms of cond?" she asks.
Alyssa's friend Eva Lu Ator claism this can indeed be done, and she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

Eva demonstratest the program for Alyssa:
(new-if (= 2 3) 0 5)
5
(new-if (= 1 1) 0 5)
0

Delighted, Alyssa uses new-if to rewrite the square-root program:
  (define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

What happens when Alyssa attempts to use this to compute square roots? Explain.
|#

; The sqrt procedure will never return as it gets caught in an infinite loop. This is a result of new-if being a procedure instead of a special form, meaning that all of its arguments are evaluated before new-if is applied
; This means that (sqrt-iter (improve guess x) x) will be evaluated as well regardless of what the predicate evaluates to, which calls itself recursively leading to an infinite loop

#|
Exercise 1.7
The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision.
This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers.
An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction
of the guess. Design a square root procedure that uses this kind of end test. Does this work better for small and large numbers?
|#

; For large numbers when performing the square operation the resulting value is too large leading to an overflow/not being able to accurately store the value in a floating point number
; For small numbers it will fail when the number is smaller than the initially hard-coded value of 0.001 leading to inaccurate approximations

(define (good-enough? last-guess guess)
  (<= (abs (- guess last-guess)) (* 0.00000000001 guess)))
; This method will now compute square roots for very large numbers and will provide accurate values for very small numbers

#|
Exercise 1.8
Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
 x
---   + 2y
y^2
------------
      3

Use this formula to implement a cube-root procedure analogous to the square-root procedure
|#
(define (cubert x)
  (cubert-iter 1.0 x))

(define (cubert-iter guess x)
  (if (good-enough? guess (cube-improve guess x))
      guess
      (cubert-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


#|
From Instructor's Manual - Define a procedure smallest-real-part that takes coefficients of a polynomial in the form ax^2 + bx + c and returns the real part of the root if complex or the real root having the smaller absolute value
|#

(define (discriminant a b c)
  (- (square b) (* 4 a c)))

(define (is-complex? a b c)
  (< (discriminant a b c) 0))

(define (minus-zero a b c)
  (/ (- (- b) (sqrt (discriminant a b c))) (* 2 a)))

(define (plus-zero a b c)
  (/ (+ (- b) (sqrt (discriminant a b c))) (* 2 a)))

(define (smallest-real-part a b c)
  (if (is-complex? a b c)
      (/ (- b) (* 2 a))
      (if (< (abs (minus-zero a b c)) (abs (plus-zero a b c)))
        (minus-zero a b c)
        (plus-zero a b c))))

#|
Exercise 1.9
Each of the following two procedures defines a method for adding two positive integeres in terms of the preocedures inc, which increments its argument by 1,
and dec, which decrements its argument by 1

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5). 
Are these processes iterative or recursive
|#

#|
First:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
This process is recursive

Second:
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
This process is iterative
|#

#|
Exercise 1.10
The following procedure computes a mathematical function called Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1)))))))

What are the values of the following expressions?
(A 1 10)
(A 2 4)
(A 3 3)

Consider the following procedures, where A is the procedure defined above:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n. For example, (k n) computes 5n^2
|#

#|
(A 1 10) -> 1024
(A 2 4) -> 65536
(A 3 3) -> 65536

f(n) = 2n
g(n) = 2^n
h(n) = 2^h(n-1)
|#
