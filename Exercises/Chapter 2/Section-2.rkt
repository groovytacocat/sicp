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
Exercise 2.17
Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

(last-pair (list 23 72 149 34))
(34)
|#

(define (last-pair items)
  (if (= (length items) 1)
      items
      (last-pair (cdr items))))

#|
Exercise 2.18
Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)
|#

(define (reverse-iter items)
  (define (iter cur)
    (if (>= cur 0)
        (cons (list-ref items cur) (iter (- cur 1)))
        nil))
  (iter (- (length items) 1)))

; Had to lookup help for determining the recursive process version of this, using append didn't occur to me
; struggled with figuring out how to shave the last item off the list without a secondary/temporary list

(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (list (car items)))))

#|
Exercise 2.19
Consider the change-counting program of section 1.2.2. It would be nice to able to easily change the currency,
used by the program, so that we could compute the number of ways to change a British pound, for example.
As the program is written, the knowledge of the current is distributed partly into the procedure first-denomination and
partly into the procedure count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be
able to supply a list of coins to be used for making change.

We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather
than an integer specifying which coins to use. We could then have lists taht defined each kind of currency:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

We could then call cc as follows:
(cc 100 us-coins)
292

To do this change cc to have same form but access second argument differently:

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))]))

Define the procedures first-denomination, except-first-denomination, and no-more? in terms of primitive operations on list structures.
Does the order of the list coin-values affect the answer produced by cc? Why or why not?
|#
(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))]))

(define (no-more? items)
  (null? items))

(define (first-denomination items)
  (car items))

(define (except-first-denomination items)
  (cdr items))

#|
Exercise 2.20
Use Dotted-Tail Notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that
have the same even-odd parity as the first argument. For example

(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)

(same-parity 2 3 4 5 6 7)
(2 4 6)
|#

; This one took me a lot longer than it should have as a result of not understanding how the dotted tail notation worked with recursive calls
; leading to arguments that aren't lists but a list with only one element that itself is a list
(define (same-parity first . rest)
  (define (parity-match first second)
    (or (and (odd? first) (odd? second))
        (and (even? first) (even? second))))
  (define (checker vals)
    (cond [(null? vals) nil]
          [(parity-match first (car vals))
           (cons (car vals) (checker (cdr vals)))]
          [else
           (checker (cdr vals))]))
  (cons first (checker rest)))

#|
Exercise 2.21
The procedure square-list takes a list of numbers as arugment and returns a list of the squares of those numbers

(square-list (list 1 2 3 4))
(1 4 9 16)

Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:

|#

; First Def
(define (square-list-one items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-one (cdr items)))))

; Second Def
(define (square-list-two items)
  (map (lambda (x) (* x x)) items))

#|
Exercise 2.22
Louis Reasoner tries to rewrite the first square-list procedure of Ex 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

This doesn't work either. Explain
|#

; Why
; This will produce the list in reverse as the 'cons' is being made inside the recursive call rather than the recursive call being made inside the cons
; e.g. (square-list (list 1 2 3 4)) -> (iter (1 2 3 4) nil) -> (iter (2 3 4) (cons (square 1) nil)) -> (iter (3 4) (cons (square 2) (cons (square 1) nil)))

; Explain
; As above the cons call being made inside the recursive call to iter will nest the cons, swapping the places of the arguments to cons
; just creates a list that has a list in the car and the value in cdr eg (square-list (list 1 2 3 4)) -> ((((nil 1) 4) 9) 16)

#|
Exercise 2.23
The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements.
However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right.
The values returned by applying the procedure to the elements are not used at all -- for-each is used with procedures that perform an action, such as printing.
For example

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
57
321
88

The value returned by the call to for-each (not illustrated above can be something arbitrary, such as true. Give an implementation of for-each)
|#

; Initially had the cond check being (null? items) but then whatever I had there was being returned/printed to console,
(define (for-each proc items)
  (cond [(not (null? items))
         (proc (car items))
         (for-each proc (cdr items))]))

#|
Exercise 2.24
Suppose we evaluate the expression (list 1 (list 2 (list 3 4))).
Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree
|#

; Without interpreter my assumption
; (1 (2 (3 4)))

; Run through interpreter
; (1 (2 (3 4)))

#|
Box-and-pointer
[*][*] ----> [*][/]
 |            |
 V            V
 1           [*][*] ---> [*][/]
              |           |
              V           V
              2          [*][*] --> [*][/] 
                          |          |
                          V          V
                          3          4
|#

 
#|
Tree
            (list 1 (list 2 (list 3 4)))
            /                       \
            1                       (list 2 (list 3 4))
                                    /                  \
                                    2               (list 3 4)
                                                    /         \
                                                    3         4
|#


#|
Exercise 2.25
Give combinations of cars and cdrs that will pick 7 from each of the following lists:

(1 3 (5 7) 9)

((7))

(1 (2 (3 (4 (5 (6 7))))))
|#

; (1 3 (5 7) 9)
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

; ((7))
(car (car (list (list 7))))

; (1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

#|
Exercise 2.26
Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

What result is printed by the interpreter in response to evaluating each of the following expressions:

(append x y)
(cons x y)
(list x y)

|#

; (append x y)
; (1 2 3 4 5 6)

; (cons x y)
; ((1 2 3) 4 5 6)

; (list x y)
; ((1 2 3) (4 5 6))

#|
Exercise 2.27
Modify the reverse procedure of Ex 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value
the list with its elements reversed and with all sublists deep-reversed as well

For example:
(define x (list (list 1 2) (list 3 4)))

x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))
|#

(define (deep-reverse items)
  (define (d-rev l)
    (if (not (pair? l))
        l
        (deep-reverse l)))
  (reverse (map d-rev items)))

#|
Exercise 2.28
Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list, whose elements are all the leaves
of the tree arranged in left-to-right order

For Example:
(define x (list (list 1 2) (list 3 4)))

(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)
|#

#|
This took me longer than I'd like to admit. I could see that it would be In-Order Tree Traversal, and I knew that would involve
Recursive calls all the way "left" then "right"
My initial attempts were relatively similar to the final output however, I originally was using cons and not append which ran into issues
of adding extraneous nils throughtout due to (1 2) actually being (cons 1 (cons 2 nil)) as well as issues with moving back up the tree to cons
the left subtree with the right leading ((1 2) (3 4)) to becoming ((1 2 '()) (3 4 '())) --> ((1 2) (3 4))

Before this I had one other working version that worked but was ugly/convoluted to handle the (1 2) -> 1 and (2) where I would append based on the cddr
|#

(define (fringe tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (fringe (car tree)) (fringe (cdr tree)))]))

#|
Exercise 2.29
A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight
or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:
(define (make-branch length structure)
  (list length structure))

a. Write the corresponding selectors left-branch right-branch, which return the branches of a mobile, and branch-length and branch-structure which return the components of a branch

b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by
the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. 
Design a predicate that tests whether a binary mobile is balanced.

d. Suppose we change the representation of mobiles so that the constructors are 

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

How much do you need to change your programs to convert the new representation
|#

; a 
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

; b
(define (total-weight mobile)
  (cond [(null? mobile) 0]
        [(not (pair? mobile)) mobile]
        [else
         (+ (total-weight (branch-structure (left-branch mobile)))
            (total-weight (branch-structure (right-branch mobile))))]))

; c
; My initial solution for this was basically the same but uglier due to a cond predicate that then had 2 if statements inside it 
; as I struggled with figuring out how to use the and conditional to make recursive calls when the left/right branches were not mobiles themselves
; after looking at solutions online to compare I saw 
(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (if (not (pair? mobile))
      #t
      (and (= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

; d
; The only changes necessary for the programs to convert the new representation, is to alter the right-branch and right-structure selectors
; changing (car (cdr <obj>)) to (cdr <obj>)

#|
Exercise 2.30
Define a procedure square-tree analogous to the square-list procedure of Ex 2.21. That is, square-tree should behave as follows:

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(1 (4 (9 16) 25) (36 49))
|#

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

#|
Exercise 2.31 
Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

(define (square-tree tree) (tree-map square tree))
|#

; Something feels wrong about using map, to implement tree-map but also look almost the same with 
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

; Non-map version
(define (map-tree proc tree)
  (cond [(null? tree) nil]
        [(pair? tree)
         (cons (map-tree proc (car tree)) (map-tree proc (cdr tree)))]
        [else (proc tree)]))

#|
Exercise 2.32
We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example,
if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). 
Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works
|#

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))