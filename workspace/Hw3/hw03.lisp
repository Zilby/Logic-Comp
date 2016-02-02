; **************** BEGIN INITIALIZATION FOR ACL2s B MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.


Pete Manolios
Thu Jan 27 18:53:33 EST 2011
----------------------------

The Beginner level is the next level after Bare Bones level.

|#

; Put CCG book first in order, since it seems this results in faster loading of this mode.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg/ccg" :uncertified-okp nil :dir :acl2s-modes :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "base-theory" :dir :acl2s-modes)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil :ttags :all)

;Settings common to all ACL2s modes
(acl2s-common-settings)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading trace-star and evalable-ld-printing books.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil)
(include-book "hacking/evalable-ld-printing" :uncertified-okp nil :dir :system :ttags ((:evalable-ld-printing)) :load-compiled-file nil)

;theory for beginner mode
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s beginner theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "beginner-theory" :dir :acl2s-modes :ttags :all)


#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Beginner mode.") (value :invisible))
;Settings specific to ACL2s Beginner mode.
(acl2s-beginner-settings)

; why why why why 
(acl2::xdoc acl2s::defunc) ; almost 3 seconds

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner
#|

CS 2800 Homework 3 - Spring 2016

This homework is done in groups. The groups are normally the same ones as in 
Assignment #2.  Changes can be made on request.  However, all such requests
must be made two days before the assignment is due, and all the students 
involved must agree to the changes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw03.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.

- when done, save your file and submit it as hw03.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file.

Instructions for programming problems:

For each function definition, you must provide both contracts and a body.

As in hw02, you will be programming some functions, and you must ALWAYS
supply tests for these functions. Your tests are in addition to the
tests sometimes provided. Make sure you produce sufficiently many new test
cases. This means: cover at least the possible scenarios according to the
data definitions of the involved types. For example, a function taking two
lists should have at least 4 tests: all combinations of each list being
empty and non-empty.

Beyond that, the number of tests should reflect the difficulty of the
function. For very simple ones, the above coverage of the data definition
cases may be sufficient. For complex functions with numerical output, you
want to test whether it produces the correct output on a reasonable
number if inputs.

Use good judgment. For unreasonably few test cases we will deduct points.

We will be using both the check= and test? functions of ACL2s.

IMPORTANT NOTICE ABOUT YOUR TEST CASES

It is a violation of academic integrity to publish or discuss your test
cases.  These are part of the solution to your assignment.  Copying or
allowing one to copy your test cases is therefore unacceptable.  Please
be careful to prevent your solutions from being seen by other students.

|#

#|

We will simplify the interaction with ACL2s somewhat: instead of asking it
to formally *prove* the various conditions for admitting a function, we
will just require that they be *tested* on a reasonable number of inputs.
This is achieved using the following directive (do not remove it!):

|#

:program

#|

Notes:

1. Testing is cheaper but less powerful than proving. So, by turning off
proving and doing only testing, it is possible that the functions we are
defining cause runtime errors even if called on valid inputs. In the future
we will require functions complete with admission proofs, i.e. without the
above directive. For this homework, the functions are simple enough
that there is a good chance ACL2s's testing will catch any contract or
termination errors you may have.

2. The tests ACL2s runs test only the conditions for admitting the
function. They do not test for "functional correctness", i.e. does the
function do what it is supposed to do? ACL2s has no way of telling what
your function is supposed to do. That is what your own tests are for!

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Contract Fulfillment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The following are functions that may or may not be correct.  For each function,
list all of the calls that this function makes.  For each call decide whether
the contract is satisfied or not.  If it is satisfied, then explain why.  If it
is not satisfied then explain why not.  In other words, show all of the body
contracts.  Do the same for the overall function contract.

For the first problem, we provide part of the answer to make it 
clear what we expect you to do.  

1. The following function supposedly computes the remainder after dividing
a nat by a nonzero nat.

(defunc rem (x y)
  :input-contract (and (natp x) (natp y) (not (equal y 0)))
  :output-contract (natp (rem x y))
  (if (< x y)
    0
    (+ 1 (rem (- x y) y))))

Body contracts:

(< x y) The input contract is satisfied because the input contract requires
rationals, and x and y are both nats.

(- x y) The input contract is satsified because the input contract requires
rationals, and x and y are both nats.

(rem (- x y) y) The input contract is satisfied because the input contract
requires rationals and a non-zero y, and (- x y) and y are both nats, with y 
being tested by the input (not (equal y 0))).

(+ 1 (rem (- x y) y)) The input contract is satisfied because the input contract
reuires a rational, and (rem (- x y) y) is a nat, tested by (natp (rem x y)).

(if ...) The input contract is satisfied because the input contract requires a
boolean, and (< x y) returns a boolean. The other input contracts are simply t, 
and so they are satisfied.

Function contract: 

(and (natp x) (natp y) (not (equal y 0))) The input contract is satisfied 
because the input contract requires natural numbers, the second of which is 
not equal to 0, and the tests (natp x) (natp y), and (not (equal y 0)) 
ensure that, when coupled with the "and" operator, the tests prove false if 
even one sub-test fails.

(natp (rem x y) The output contract is satisfied because both the possible outputs
0 and (+ 1 (rem (- x y) y) produce a natp that will always be their remainder given
the other function contracts

2. The following function supposedly computes the floor function of a rational
number.  Check the body and function contracts for this function.

(defunc floor (x)
  :input-contract (and (rationalp x) (>= x 0))
  :output-contract (natp (floor x))
  (if (< x 1)
    0
    (- (floor (+ x 1)) 1)))

Body contracts:

(< x 1) The input contract is satisfied because the input contract requires
rationals, and x is a rational.

(+ x 1) The input contract is satisfied because the input contract requires
rationals, and x is a rational.

(floor (+ x 1)) The input contract is satisfied because the input contract 
requires positive rationals, and x is a positive rational, and after adding
1 to a positive rational, the input will always be a potitive rational.

(- (floor (+ x 1)) 1) The input contract is satisfied because the input 
contract requires rationals, and the output of floor is a nat (as guaranteed
by the output contract for floor).

(if ...) The input contract is satisfied because the input contract requires a
boolean, and (< x 1) returns a boolean. The other input contracts are simply t, 
and so they are satisfied.

Function contract:

(and (rationalp x) (>= x 0)) The input contract is satsified, though the
output contract is NOT satisfied because it is never fulfilled. The function
never terminates due to (floor (+ x 1)) continuously increasing with every
recursive call. As such, no result is produced so there is not way to verify
if (natp (floor x)) is true or false.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Testing Sorting Programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The purpose of a sorting algorithm is to rearrange a list of elements
; in order.  To test whether a sorting algorithm is correct, one must show
; not only that the algorithm produces a list that is in order, one must
; also show that the algorithm rearranges the elements of the list,
; including making sure that any elements that occur more than once will
; have the same multiplicity.

; We begin by developing a function that determines whether a list is a
; rearrangement of another list.  This will require some helper functions.  
; The first one counts the number of occurrences of an element in a list.

; multiplicity-in: All x List -> Nat
; (multiplicity-in e l) is the number of occurrences of e in l.

(defunc multiplicity-in (e l)
  :input-contract (listp l)
  :output-contract (natp (multiplicity-in e l))
  (if (endp l)
    0
    (if (equal e (first l))
      (+ (multiplicity-in e (rest l)) 1)
      (multiplicity-in e (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; same-multiplicity: List x List x List -> Boolean
; (same-multiplicity l l1 l2) returns t iff every element of l occurs in l1
; and l2 the same number of times.

(defunc same-multiplicity (l l1 l2)
  :input-contract (and (listp l) (and (listp l1) (listp l2)))
  :output-contract (booleanp (same-multiplicity l l1 l2))
  (if (endp l)
    t
    (and (equal (multiplicity-in (first l) l1)
                (multiplicity-in (first l) l2))
         (same-multiplicity (rest l) l1 l2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; is-rearrangement: List x List -> Boolean
; (is-rearrangement l m) is t iff l and m are rearrangements of the same
; elements (including multiplicities).

(defunc is-rearrangement (l m)
  :input-contract (and (listp l) (listp m))
  :output-contract (booleanp (is-rearrangement l m))
  (and (same-multiplicity l l m)
       (same-multiplicity m l m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We will be sorting nats, so we need to define a list of nats:

(defdata natlist (listof nat))

; The main test for a sorting algorithm is to determine whether it produces a
; list that is in sorted order.  This next function tests whether a list is in
; sorted order.  This function compares adjacent pairs of elements and returns
; t if all adjacent pairs are in order and nil if not.

; is-ordered: Natlist -> Boolean
; (is-ordered l) is t iff the elements in l are in increasing order.

(defunc is-ordered (l)
  :input-contract (natlistp l)
  :output-contract (booleanp (is-ordered l))
  (if (or (endp l) (endp (rest l)))
    t
    (if (<= (first l) (first (rest l)))
      (is-ordered (rest l))
      nil
      )))

(check= (is-ordered (list 1 2 3 3 5)) t)
(check= (is-ordered (list 1 2 3 2 5)) nil)
(check= (is-ordered (list 3 6 3 4 1 9)) nil)
(check= (is-ordered (list 6 6 6 6 6 6)) t)
(check= (is-ordered ()) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We will be looking at several variations on a sorting algorithm called
; bubble-sort.  In bubble-sort, one examines the list for adjacent pairs
; of elements that are out of order, and swaps them.  While all bubble-sort
; algorithms swap out-of-order pairs of elements, the variations differ in
; how the out-of-order pairs are found and when they are swapped.  However,
; all variations must repeat the bubbling operation repeatedly until the
; the list is sorted.

; The first variation checks whether the first two nats in the list are out
; of order, and if they are, then it swaps them.  The function is then 
; applied recursively to the rest of the list (after the first two elements).

; bubble1: Natlist -> Natlist
; (bubble1 l) rearranges the list l by recursively swapping the initial
; pair of elements if they are out of order.

(defunc bubble1 (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (bubble1 l))
  (if (or (endp l) (endp (rest l)))         ; The base cases
    l
    (let ( (f (first l))                    ; First element of the pair
           (s (first (rest l)))             ; Second element of the pair
           (b (bubble1 (rest (rest l)))) )  ; Recursively apply bubble1 to the rest
      (if (> f s)                           ; Should we swap?
        (cons s (cons f b))                 ; Yes, swap the pair
        (cons f (cons s b))))))             ; No, leave them alone

; First check whether bubble1 produces a rearrangement.  Rather than just
; checking a few cases, use test? to be thorough.

(test? (implies (natlistp l) (is-rearrangement l (bubble1 l))))

; Explain what happened.
; ....

; Now try some checks. As usual, add more checks.

(check= (bubble1 (list 4 3 2 1)) (list 3 4 1 2))
(check= (bubble1 ()) ())
(check= (bubble1 (list 6 8 5 3 9)) (list 6 8 3 5 9)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; It appears that bubble1 is improving the order.  However, to determine
; whether bubble1 can actually be used to sort a list, we need to repeat
; bubble1 several times.

; bubble1-repeated: Nat x Natlist -> Natlist
; (bubble1-repeated n l) applies bubble1 n times to the list l.

(defunc bubble1-repeated (n l)
  :input-contract (and (natp n) (natlistp l))
  :output-contract (natlistp (bubble1-repeated n l))
  (if (equal 0 n)
    l
    (bubble1-repeated (- n 1) (bubble1 l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We can now test whether bubble1 is sorting the list.  The number
; times we need to repeat a bubble operation is at least the length
; of the list, and possibly more.  Try using twice the length of
; the list to be thorough.  As usual, test both whether the list is
; being rearranged and whether the list is being sorted.

; Test for rearrangement
(test? (implies (and (natp n) (natlistp l)) 
                (is-rearrangement (bubble1-repeated n l) l)))

; Test for sorting in order
;(test? (implies (and (equal n (* 2 (len l))) (natlistp l))
;                (is-ordered (bubble1-repeated n l))))

; Explain what happened.  Would increasing the number of repetitions help?
; Calling bubble1 repeatedly doesn't work because it only sorts each pair 
; in the list, not the whole list, never changing which pairs get sorted. 
; Calling it more than once will have absolutely no additional effects. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's try another variation.  This time we first apply recursion
; and then check whether the initial pair is out of order.  Unlike bubble1,
; we cannot compute the second nat in the pair until after the recursion
; has been applied.

; bubble2: Natlist -> Natlist
; (bubble2 l) rearranges the list l by recursively swapping the initial
; pair of elements if they are out of order.

(defunc bubble2 (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (bubble2 l))
  (if (or (endp l) (endp (rest l)))         ; The base cases
    l
    (let* ( (f (first l))                   ; First element of the pair
            (b (bubble2 (rest l)))          ; Recursively apply bubble2 to the rest
            (s (first b)) )                 ; Second element of the pair
      (if (> f s)                           ; Should we swap?
        (cons s (cons f b))                 ; Yes, swap the pair
        (cons f (cons s b))))))             ; No, leave them alone


; As with bubble1, test whether this variation rearranges the list.


; Test for rearrangement
;(test? (implies (natlistp l) (is-rearrangement l (bubble2 l)))) ;Test Fails

; As with bubble1, to test whether bubble2 sorts the list, we will
; need a function that repeatedly applies bubble2, and then tests
; whether the list is sorted after many repetitions.

; Explain whether this test is needed for bubble2.
; This test is not needed since bubble2 does not create a rearrangement of 
; the list in several cases and thus cannot be guaranteed to produce a sorted list.

; If you conclude that the test is necessary, then define the 
; repetition function and test it.

; We've concluded that the test is unnecessary due to bubble2's disfunctionality. 

; Explain what you learned.
; We learned that you cannot make a sorted list using an algorithm that does not
; produce rearrangements of a list

; What is the mistake in bubble2?
; The mistake in bubble2 is that it can duplicate items in a list adding them 
; in multiple times and creating an entirely new list of a longer length. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The final variation improves bubble2.

; bubble3: Natlist -> Natlist
; (bubble3 l) rearranges the list l by recursively swapping the initial
; pair of elements if they are out of order.

(defunc bubble3 (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (bubble3 l))
   (if (or (endp l) (endp (rest l)))
     l
     (if (< (first l) (first (rest l)))
       (cons (first l) (bubble3 (rest l))) 
       (cons (first (rest l)) (bubble3 (cons (first l) (rest (rest l))))))))

; Perform the tests for bubble3 as we did for bubble1.

; Test for rearrangement
(test? (implies (natlistp l) (is-rearrangement l (bubble3 l))))

; Explain what happened.
; Because the algorithm only rearranged the elements rather than copying and 
; adding them back in like in bubble2 the test for whether bubble3 is a 
; rearrangement algorithm passed. 

; Now try some checks to determine whether bubble3 is 
; improving the order of elements in the list.
; As usual, add some more checks.

(check= (bubble3 (list 4 3 2 1)) (list 3 2 1 4))
(check= (bubble3 ()) ())
(check= (bubble3 (list 1 2 3 4 4 6 8)) (list 1 2 3 4 4 6 8))
(check= (bubble3 (list 8 6 4 4 3 2 1)) (list 6 4 4 3 2 1 8))
(check= (bubble3 (list 7 4 6 9 65)) (list 4 6 7 9 65))

; It appears that bubble3 is improving the order. Describe
; what bubble3 is doing to the list.
; bubble3 takes the first element in the list that it finds that is out
; of order and moves it through the list until it reaches a number greater
; than it. It then repeats this process until it reaches the end of the list, 
; always leaving the largest element it found out of order in the correct place. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now define a function that repeatedly applies bubble3.

; bubble3-repeated: Nat x Natlist -> Natlist
; (bubble3-repeated n l) applies bubble3 n times to the list l.

(defunc bubble3-repeated (n l)
  :input-contract (and (natp n) (natlistp l))
  :output-contract (natlistp (bubble3-repeated n l))
  (if (equal 0 n)
    l
    (bubble3-repeated (- n 1) (bubble3 l))))

; We can now test whether bubble3 is sorting the list.  The number
; times we need to repeat a bubble operation is at least the length
; of the list, and possibly more.  Try using twice the length of
; the list to be thorough.  As usual, test both whether the list is
; being rearranged and whether the list is being sorted.

; Test for rearrangement
(test? (implies (and (natp n) (natlistp l)) 
                (is-rearrangement (bubble3-repeated n l) l)))

; Test for sorting in order
(test? (implies (and (equal n (* 2 (len l))) (natlistp l))
                (is-ordered (bubble3-repeated n l))))

; Explain what happened.
; Since one element is guaranteed to be placed in the correct spot
; each call, it will always take a maximum of n-1 (n being the length of
; the list) calls for the sort algorithm to finish sorting the list
; (this is n-1 not n since the last element will always be in the correct
; place if every other element is in theirs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now try to reduce the number of repetitions required for sorting
; the list.  Use test? to determine the smallest number of times
; required in general.

; Test for sorting in order
(test? (implies (and (equal n (len l)) (natlistp l))
                (is-ordered (bubble3-repeated n l))))

; Test for sorting in order
(test? (implies (and (equal n (- (len l) 1)) (natlistp l))
                (is-ordered (bubble3-repeated n l))))

; This test fails since it cannot guarantee the list is sorted in 
; less than n-1 calls. 
;(test? (implies (and (equal n (- (len l) 2)) (natlistp l))
;                (is-ordered (bubble3-repeated n l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Once you have determined the best choice for the number of times
; to repeat bubble3, define a function that sorts a natlist.

; sort3: Natlist -> Natlist
; (sort3 l) sorts a natlist by repeating the bubble3 function.

(defunc sort3 (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (sort3 l))
  (if (endp l)
    l
    (bubble3-repeated (- (len l) 1) l)))

; Perform tests for rearrangement and sorting in order.

; Test for rearrangement
(test? (implies (and (natp n) (natlistp l)) 
                (is-rearrangement (sort3 l) l)))

; Test for sorting in order
(test? (implies (and (equal n (len l)) (natlistp l))
                (is-ordered (sort3 l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Finally, rather than use a fixed number of repetitions,
; define a sorting function that sorts until the elements
; are in order.

; sort: Natlist -> Natlist
; (sort l) sorts a natlist by the bubble3 function until
; the elements are in order.

(defunc sort (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (sort3 l))
  (if (endp l)
    l
    (if (is-ordered l)
      l
      (sort (bubble3 l)))))

; Perform tests for rearrangement and sorting in order.

; Test for rearrangement
(test? (implies (and (natp n) (natlistp l)) 
                (is-rearrangement (sort l) l)))

; Test for sorting in order
(test? (implies (and (equal n (len l)) (natlistp l))
                (is-ordered (sort l))))#|ACL2s-ToDo-Line|#
