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
Exercise 2.1
Define a better version of make-rat that handles both positive and negative arguments.
Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator
are positive, and if the rational number is negative, only the numerator is negative
|#

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

#|
Exercise 2.2
Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and ending point.
Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points.
Furthermore, a point can be represented as a pair of numbers: the x coordinate and y coordinate. Accordingly, specify a constructor make-point
and selectors x-point y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment
that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the end points).
To try your procedures, you'll need a way to print points:
|#

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (midpoint-segment line)
  (define (average x y)
    (/ (+ x y) 2))
  (make-point (average (x-point (start-segment line)) (x-point (end-segment line)))
              (average (y-point (start-segment line)) (y-point (end-segment line)))))

#|
Exercise 2.3
Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2)
In terms of your constructors and selectors, create procedures that compute the perimeter and area of a given rectangle.
Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers,
so that the same perimeter and area procedures will work using either representation
|#

(define (rect-area r)
  (* (rect-height r) (rect-width r)))

(define (rect-perim r)
  (* 2 (+ (rect-height r) (rect-width r))))

(define (rect-height r)
  (abs (- (y-point (cdr r))
          (y-point (car r)))))
(define (rect-width r)
  (abs (- (x-point (cdr r))
          (x-point (car r)))))

; Rep 1 - 2 Points * Initially tried to define 4 points before realizing cons specifically forms pairs *
(define (make-rectangle-one bottom-left top-right)
  (cons bottom-left top-right))

; Rep 2 - One of the height segments and one of the width segments of the rectangle
(define (make-rectangle-two height-seg width-seg)
  (define (min x y) (if (<= x y) x y))
  (define (max x y) (if (>= x y) x y))
  (define (minmax op seg1 seg2)
    (make-point (op (op (x-point (start-segment seg1))
                        (x-point (end-segment seg1)))
                    (op (x-point (start-segment seg2))
                        (x-point (end-segment seg2))))
                (op (op (y-point (start-segment seg1))
                        (y-point (end-segment seg1)))
                    (op (y-point (start-segment seg2))
                        (y-point (end-segment seg2))))))
  (let ((height (minmax min height-seg width-seg))
        (width (minmax max height-seg width-seg)))
    (cons height width)))


#|Notes/Observations:
Implementing a perimeter/area procedure that can work with both reps was easier by "working backward"
i.e. by defining the area/perim -> defining height/width -> defining constructors that return the same format with different data

Representation 2 took me longer as the original implementation only worked if the "left" height segment was given with either "top" or "bottom" width segment,
Managed to improve this such that any pairing of height/width segments of a rectangle could be provided

Additionally this method will work regardless of the order of points provided for the segments (e.g. the line segment from (1,1) to (1,5) is the same as (1,5) to (1,1))
|#

#|
Exercise 2.4
Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y))
yields x for any objects x and y

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

What is the corresponding definition of cdr? (Hint: TO verify that this works make use of the substitution model of section 1.1.5)
|#

#|
(car (cons x y))
(car (lambda (m) (m x y)))
(lambda (m) (m x y) (lambda (p q) p))
((lambda (p q) p) x y)
Returns x
|#

; cdr def
(define (cdr z)
  (z (lambda (p q) q)))

#|
Exercise 2.5
Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b
as the integer that is the product of 2^a 3^b. Give the corresponding defintions of the procedures cons, car, and cdr.
|#

(define (int-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (int-car num)
  (if (= (remainder num 2) 0)
      (+ 1 (int-car (/ num 2)))
      0))

(define (int-cdr num)
  (if (= (remainder num 3) 0)
      (+ 1 (int-cdr (/ num 3)))
      0))

#|
Exercise 2.6
In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures,
we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

This representation known as Church Numerals, after its inventor, Alonzo Church, the logician who invented lambda calculus.

Define one and two directly (not in terms of zero and add-1) (Hint: Use substitution to evaluate (add-1 zero)).
Give a direct definition of the addition procedure + (not in terms of repeated application of add-1)
|#

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


; Definition of one and two without subsitution seem straightforward from looking at the definition of add-1/The exercise in Ch1 re: Repeated
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; (add-1 zero) -- Struggled a lot with the lambda substitutions
; Looked online for help/instructor's manual and found some help, but also most explanations were semi hand wavey so it took me some time to work out

; Substituting body of add-1 with lambda and replacing the argument n with zero
(lambda (f) (lambda (x) (f ((zero f) x))))

; Substituting the body of zero in
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

; Zero is a procedure that takes as its argument a procedure, the inner lambda procedure is the identity function which is a procedure that takes 1 argument and returns the argument
; ((lambda (f) (lambda (x) x)) f) then will take the procedure f as its argument, and return the procedure (lambda (x) x)
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))

; The procedure ((lambda (x) x) x) is the identity function as mentioned before which will take an argument and return the argument
; ((lambda (x) x) x) will therfore just return x
(lambda (f) (lambda (x) (f x)))

; Thus we have derived the definition of one, rather than substitute this in to (add-1 one) we can see from the definitions of zero and (add-1 n) that the number will be the nth application of f on x

; Definition of add * This was semi-spoiled for me when looking up assistance with the lambda substitutions
(define (church-add x y)
  (lambda (f) (lambda (x) ((x f) ((y f) x)))))

#|
Exercise 2.7
Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction.
Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

Define selectors upper-bound and lower-bound to complete the implementation
|#

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

(define (make-interval a b) (cons a b))

; Selectors
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

#|
Exercise 2.8
Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed.
Define a corresponding subtraction procedure called sub-interval
|#

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

#|
Exercise 2.9
The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval.
For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others
the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function
only of the widths of the intervals being added (or subtracted).

Give examples to show that this is not true for multiplication or division
|#

#|

Interval 1: [1, 5] (Width = 2)

Interval 2: [7, 19] (Width = 6)

I1 + I2 = [8, 24] -> 24 - 8 = 16 -> 16 / 2 -> 8

[x1, y1]
[x2, y2]
   
   
   (y1 - x1)     (y2 - x2)       y1 - x1 + y2 - x2           (y1 + y2) - (x1 + x2)
  ----------- + ----------- = ------------------------- = ------------------------
       2              2                  2                             2

The width of Adding/Subtracting an interval is as seen above a function of the widths of the argument intervals


Multiplication
Interval 1: [2, 6] (Width = 2)
Interval 2: [3, 9] (Width = 3)

I1W * I2W = 2 * 3 = 6

I1 * I2
p1 = 2 * 3 = 6
p2 = 2 * 9 = 18
p3 = 6 * 3 = 18
p4 = 6 * 9 = 54
(min p1 p2 p3 p4) = 6
(max p1 p2 p3 p4) = 54

[6, 54] -> 54 - 6 = 48 -> 48 / 2 -> 24 != 6


Division
Interval 1: [15, 45] (Width = 15)
Interval 2: [3, 5] (Width = 1)

I1W / I2W = 15 / 1 = 15

I1 / I2
I1 * [1/5, 1/3]
p1 = 15 * 1/5 = 3
p2 = 15 * 1/3 = 5
p3 = 45 * 1/5 = 9
p4 = 45 * 1/3 = 15

(min p1 p2 p3 p4) = 3
(max p1 p2 p3 p4) = 15
[3, 15] -> 15 - 3 = 12 -> 12 / 2 = 6 != 15
|#

