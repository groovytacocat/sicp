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
(define (divides? a b)
  (= (remainder b a) 0))
(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

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

; This one came to me in a surprise as I was trying to figure out how to avoid some common issues I've faced when performing recursive operations
; in conjunction with list structures/creating lists (e.g. too many nils/only nils/etc)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; This works by appending the list generated by map where the car of the set is paired with each element of the cdr.
; This in combination with the recursive calls to the cdr of the set will essentially recurse down and then build the list
; from the bottom up creating subsets from each element

#|
Exercise 2.33
Fill in the missing expressions to copmlete the following definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) <??>) nil sequence))

(define (append seq1 seq2)
  (accumulate cons <??> <??>))

(define (length sequence)
  (accumulate <??> 0 sequence))
|#

; Map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

; Append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; Length
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

#|
Exercise 2.34
Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate the polynomial

a_n x^n + a_n-1 x^n-1 + .. + a_1 x + a_0

using a well-known algorithm Horner's rule which structures computations as

(... (a_n x + a_n-1)x + ... + a_1)x + a_0

In other words we start with a_n, multiply by x, add a_n-1, multiply by x, and so on, until we reach a_0.

Fill in the following template to produce a procedure that evalutes a polynomial using Horner's rule.
Assume that the coefficients of the polynomial are arranged in a sequence, from a_0 through a_n.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) <??>)
              0
              coefficient-sequence))

For example: to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate
(horner-eval 2 (list 1 3 0 5 0 1))

|#

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms)
                   this-coeff))
              0
              coefficient-sequence))

#|
Exercise 2.35
Redefine count-leaves from section 2.2.2 as an accumulation

(define (count-leaves t)
  (accumulate <??> <??> (map <??> <??>)))
|#


; This took me a minute to figure out a procedure that could be applied by map to 'flatten' a tree
; I ended up with 2 lambdas that were kinda ugly, before looking back in the book/at notes to see if I was missing something
; and then remembering we had already written a procedure to enumerate a tree and combined with the identity function in map
; would flatten the tree as I wanted.
(define (count-leaves tree)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (map (lambda (x) x) (enumerate-tree tree))))

#|
Exercise 2.36
The procedure accumulate-n is similar to accumulate except that it takes as its third argument a sequence of sequences, which are all assumed
to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequence,
all the second elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence containing four
sequences ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30).

Fill in the missing expressions in the following definitions of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init <??>)
                   (accumulate-n op init <??>))))
|#

#|
This has been the exercise that has taken me the longest so far. Embarrassingly so, because first I had a typo in my problem description
where I had cons and 2 accumulates instead of accumulate and accumulate-n

I could see that I would need to car each subsequence and apply the accumulate procedure to that list, and that I would need to do that 
with successive cdr calls (i.e. (car s), (car (cdr s)), (car (cdr (cdr s)))) etc. 

I could not however see how to map the sequence to both eventually have the car be null to terminate, as well as apply the cdr procedure as many times as necessary

I looked up the exercise for some assistance and found someone providing some insight of "define a proc to get the first item from each nested sequence and another that returns the remaining parts" (shoutouts to jz of SchemeWiki)
then it clicked that I just needed to map cdr to each subsequence. (also I originally had (lambda (x) (car x)))/(lambda (x) (cdr x)) for my map procs before realizing that's just car/cdr 
|#
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))

#|
Exercise 2.37
Suppose we represent vectors v = (v_i) as a sequence of numbers, and matrices m = (m_ij) as sequences of vectors (the rows of the matrix). For example the matrix

1 2 3 4
4 5 6 6
6 7 8 9
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (transpose mat)
  (accumulate-n <??> <??> mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)))
|#

; matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

; transpose
(define (transpose mat)
  (accumulate-n cons nil mat))

; matrix-*-matrix --- This one took a minute for such a frustrating silly reason. I originally had (lambda (x) (matrix-*-vector x cols)) vs (... cols x), then had a janky nested lambda+map combo for dot products
; then trying to clean/optimize that because it was ugly/too complex for what I assumed the answer should be
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

#|
Exercise 2.38
The accumualte procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right.
There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

What are the values of: 

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.
|#

; / 1 (list 1 2 3)
; f-r -> 3 / 2 --> (/ 3 (/ 2 (/ 1 init)))
; f-l -> 1 / 6 --> (/ (/ (/ init 1) 2) 3)

; list nil (list 1 2 3)
; f-r -> (1 (2 (3 ()))) --> (list 1 (list 2 (list 3 init)))
; f-l -> (((() 1) 2) 3) --> (list (list (list init 1) 2) 3)

; For the property had to look some stuff up for guidance.
; Op should only need to satisfy Associativity, this can be seen from using Matrix Multiplication which is associative but not commutative
; e.g. (fold-right matrix-*-matrix i (list A B C)) = (fold-left matrix-*-matrix i (list A B C)) where i is the identity matrix,
; and A B C are matrices that can be multiplied -> (A x B) x C = A x (B x C)

#|
Exercise 2.39
Complete the following definitions of reverse in terms of fold-right and fold-left

(define (reverse sequence)
  (fold-right (lambda (x y) <??>) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) <??>) nil sequence))
|#

(define (right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

#|
Exercise 2.40
Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i, j) with
1 <= j < i <= n. Use unique-pairs to simplify the definition of prime-sum-pairs given above
|#

; Was initially confused as I thought there had to be more to this/this was harder than it was actually 
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

#|
Exercise 2.41
Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to
a given integer n that sum to a given integer s.
|#

(define (sum-triples n sum)
  (filter (lambda (trip) 
            (= (accumulate + 0 trip) sum))
          (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n))))

#|
Exercise 2.42
The "Eight-Queens Puzzle" asks how to place eight queens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal).
One possible solution is shown in figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k-1 queens, we must place
the kth queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively:
Assume that we have already generated the sequence of all possible ways to place k-1 queens in the first k-1 columns of the board. For each of these ways, generate an extended set of positions
by placing a queen in each row of the kth column. Now filter these, keeping only the queens for which the queen in the kth column is safe with respect to the other queens.
This produces the sequence of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.
We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n x n chessboard. Queens has an internal procedure called
queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

In this procedure rest-of-queens is a way to place k-1 queens in the first k-1 columns, and new-row is a proposed row in which to place the queen for the kth column. 
Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, 
and empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe 
with respect to the others. (note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other)
|#

; representation for sets of board positions
; (list x y z ...) x = row pos of queen in col 1, y = row pos of queen in col 2, z = row pos of queen in col 3, etc ...

; rest-of-queens is a way to place k-1 queens in the first k-1 columns
; Sequence of positions (list of lists)

; new-row is a proposed row in which to place the queen for the kth column

; adjoin-position, which adjoins a new row-column position to a set of positions,
(define (adjoin-position new-position queen rest)
  nil)

; empty-board, which represents an empty set of positions
(define empty-board nil)

; safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other)
(define (safe? queen positions)
  nil)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))