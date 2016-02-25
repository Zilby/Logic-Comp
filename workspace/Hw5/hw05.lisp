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

CS 2800 Homework 5 - Spring 2015

This homework is done in groups. The groups are normally the same ones as in 
Assignment #4.  Changes can be made on request.  However, all such requests
must be made two days before the assignment is due, and all the students 
involved must agree to the changes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw05.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. If you comment out any pre-existing text in this
  file, then give a brief explanation, such as "The test failed".

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. If a test fails, comment it out but give a brief
  explanation such as "The test failed". Comments should also be used for
  any English text that you may add. This file already contains many
  comments, so you can see what the syntax is.

- when done, save your file and submit it as hw05.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file.

Instructions for programming problems:

For each function definition, you must provide both contracts and a body.
You may define helper functions. For such functions, you must provide 
contracts and tests the same as any other function.

This assignment must pass all contracts and terminate. You should avoid
the :program command in the file you submit, although you can, of course,
use it while developing your programs and you can use :program if ACL2s
fails to accept your function definition, but you think your definition
is correct.

As in hw04, you will be programming some functions.  If one or more check=
tests are provided, then you must add more check= or test? tests.  Your 
check=/ test? tests are in addition to the check= tests provided.  Make 
sure you produce sufficiently many new test cases.  This means: 
cover at least the possible scenarios according to the data definitions 
of the involved types. For example, a function taking two lists should 
have at least 4 additional check= tests: all combinations of each list 
being empty and non-empty. Each datatype should have at least 2 additional 
tests.

Beyond that, the number of tests should reflect the difficulty of the
function. For very simple ones, the above coverage of the data definition
cases may be sufficient. For complex functions with numerical output, you
want to test whether it produces the correct output on a reasonable
number of inputs.

If the function asks for a test? or a thm, then follow the instructions
for that function.  If ACL2s fails to prove a thm form, but you think that 
it is actually valid, you can replace the thm with a test? and add a comment
explaining what you did.

IMPORTANT NOTICE ABOUT YOUR TEST CASES

It is a violation of academic integrity to publish or discuss your test
cases.  These are part of the solution to your assignment.  Copying or
allowing one to copy your test cases is therefore unacceptable.  Please
be careful to prevent your solutions from being seen by other students.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Propositional Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

We use the following ASCII character combinations to represent the Boolean
connectives:

  NOT     ~

  AND     /\
  OR      \/

  IMPLIES =>

  EQUIV   =
  XOR     <>

The binding powers of these functions are listed from highest to lowest
in the above table. Within one group (no blank line), the binding powers
are equal, and operators are applied from left to right (i.e., left
associative). This is the same as in hw04.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simplification of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

There are many ways to
represent a formula. For example: 

p \/ (p => q)) 

is equivalent to 

true 

Another example:

((~p) \/ (~q))

is equivalent to

~p \/ ~q

By removing 6 parentheses we simplified the formula, but we 
can simplify it further as follows

p => ~q

We now have 2 connectives instead of 3.

For each of the following, try to find the simplest equivalent
formula. By simplest, we mean the one with the least number of
connectives and parentheses. You can use any unary or binary
connective shown above in the propositional logic section.

(A) ~(p <> q <> r)

~((p <> q) <> r)
(p <> q = r)

(B) p => (~q \/ r \/ ((q => r) /\ p))

~p \/ (~q \/ r \/ ((q => r) /\ p))
~p \/ (~q \/ r \/ ((~q \/ r) /\ p))
~p \/ (~q \/ r) (absorption law)
~p \/ ~q \/ r

(C) ~(r => ~q) \/ ~(p => r)

~(~r \/ ~q) \/ ~(~p \/ r)
(r /\ q) \/ (p /\ ~r)

(D) ~((p => q) /\ ~r)

~((~p \/ q) /\ ~r)
~(~p \/ q) \/ r
p /\ ~q \/ r

(E)  p => p => p

T => p
p

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Characterization of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

For each of the following formulas, determine if they are valid,
satisfiable, unsatisfiable, or falsifiable. These labels can
overlap (e.g. formulas can be both satisfiable and valid), so
keep that in mind and indicate all characterizations that
apply. In fact, exactly two characterizations always
apply. (Think about why that is the case.) Provide proofs of your
characterizations, using a truth table or simplification
argument (for valid or unsatisfiable formulas) or by exhibiting
assignments that show satisfiability or falsifiability.

(A) q => (q => p) <> false

Satisfiable 
Example: p=T q=T
T => (T => T) <> F 
T => T <> F
T <> F
T

Falsifiable
Example: p=F q=T
T => (T => F) <> F
T => F <> F
F <> F
F

(B) p <> p => ~p /\ q \/ ~p
p <> p => ~p
p <> ~p \/ ~p
p <> ~p
T

Valid & Satisfiable
p | q | p <> p => ~p /\ q \/ ~p
-------------------------------
T | T |           T
T | F |           T
F | T |           T
F | F |           T



(C) ~(~r /\ (p <> ~p) <> r /\ q \/ r)
~(~r /\ T <> r)
~(~r <> r)
~(T)
F

Unsatisfiable & Falsifiable

p | q | r | ~(~r /\ (p <> ~p) <> r /\ q \/ r)
---------------------------------------------
T | T | T |                 F
T | T | F |                 F
T | F | T |                 F
T | F | F |                 F
F | T | T |                 F
F | T | F |                 F
F | F | T |                 F
F | F | F |                 F


(D) p => q => r => p

Valid & Satisfiable
p | q | r | p => q => r => p
----------------------------
T | T | T |        T
T | T | F |        T
T | F | T |        T
T | F | F |        T
F | T | T |        T
F | T | F |        T
F | F | T |        T
F | F | F |        T


(E) ~(p <> ~q \/ q)

Satisfiable
Example: p=T q=T
~(T <> ~T \/ T)
~(T <> T)
~(F)
T

Falsifiable
Example: p=F q=T
~(F <> ~T \/ T)
~(F <> T)
~(T)
F

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Computing Homework Grades
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We will be producing lists of nats so we will need the natlist type:

(defdata natlist (listof nat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Rounding is halfway between the ceiling and floor functions.

; Define:
; round: {x:Rational | x >= 0} -> Nat
; (round x) for a nonnegative rational x is the nat that is closest to x.
; If x is exactly between two nats, then use the larger nat.

(defunc round (x)
  :input-contract (and (rationalp x) (>= x 0))
  :output-contract (natp (round x))
  (cond ((< x 1/2) 0)
        ((< x 3/2) 1)
        (t (+ (round (- x 1)) 1))))

(check= (round 1/2) 1)
(check= (round 24/49) 0)
(check= (round (+ 85 1/2)) 86)

; Prove that (round x) is no more than 1/2 larger than x and
; cannot be less than 1/2 smaller than x.

;(thm (hypothesis) (conclusion))
(thm (implies (and (rationalp x) (>= x 0)) 
              (and (>= (round x) (- x 1/2)) (<= (round x) (+ x 1/2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The average is a common operation for combining a collection of values.

; Created a sum function to calculate thre sum of the elments of a natlist
; Natlist -> Rational
(defunc sum (l)
  :input-contract (natlistp l)
  :output-contract (rationalp (sum l))
  (if (endp l)
    0
    (+ (first l) (sum (rest l)))))

; Define:
; average: Natlist -> Rational
; (average l) is the average (mean) of the list l of nats.  If the list
; is empty, then the average is 0.

(defunc average (l)
  :input-contract (natlistp l)
  :output-contract (rationalp (average l))
  (if (endp l)
    0
    (/ (sum l) (len l))))

(check= (average (list 80 90 100)) 90)
(check= (average ()) 0)
(check= (average (list 2 4 6)) 4)
(check= (average (list 1 2)) 3/2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The bubble3 function from the solution to hw03 is correct, but it does
; not pass contract testing because first and rest are applied to the
; variable b even though it is never tested for nil. Modify the solution so
; that it tests b appropriately.

; Define:
; bubble: Natlist -> Natlist
; (bubble l) rearranges the list l by recursively swapping the initial
; pair of elements if they are out of order.

(defunc bubble (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (bubble l))
  (if (or (endp l) (endp (rest l)) (endp (bubble (rest l))))         
    l
    (let* ( (f (first l))                 
            (b (bubble (rest l)))
            (s (first b)) )               
      (if (> f s)                         
        (cons s (cons f (rest b)))        
        (cons f (cons s (rest b)))))))    

(check= (bubble (list 5 4 3 1 2)) (list 1 5 4 3 2))
(check= (bubble ()) ())
(check= (bubble (list 4 6 3 2)) (list 2 4 6 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The sort function can now be defined using bubble as in the solution
; to hw03.

; Define:
; bubble-repeated: Nat x Natlist -> Natlist
; (bubble-repeated n l) applies bubble n times to the list l.

(defunc bubble-repeated (n l)
  :input-contract (and (natp n) (natlistp l))
  :output-contract (natlistp (bubble-repeated n l))
  (if (equal n 0)
    l
    (bubble-repeated (- n 1) (bubble l))))

(check= (bubble-repeated 2 (list 5 4 3 1 2)) (list 1 2 5 4 3))
(check= (bubble-repeated 99 ()) ())
(check= (bubble-repeated 3 (list 5 393 4 2 1)) (list 1 2 4 5 393))

; Define:
; sort: Natlist -> Natlist
; (sort l) sorts the list l in ascending order

(defunc sort (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (sort l))
  (if (< (len l) 2)
    l
    (bubble-repeated (- (len l) 1) l)))

(check= (sort (list 5 4 3 1 2)) (list 1 2 3 4 5))
(check= (sort ()) ())
(check= (sort (list 5 3 2 39 3 2 9 45)) (list 2 2 3 3 5 9 39 45))

; Test that sort does not change the length of the list.

(test? (implies (natlistp l) (equal (len (sort l)) (len l))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To select the largest values from a list, the list should be sorted and
; then the last values should be selected.  Modify the select-after
; function from hw02 so that it selects a specified number of nats, rather
; than the list of nats after a given position.

; Define
; select-last: Natlist x Nat -> Natlist
; (select-last l n) is the sublist of elements of l containing the last n
; elements of l.  If n is greater than or equal to the length of the list then
; return the entire list.

; select-last is the modified select-after 

(defunc select-last (l n)
  :input-contract (and (natlistp l) (natp n))
  :output-contract (natlistp (select-last l n))
  (cond ((endp l) ())
        ((>= n (len l)) l)
        (t (select-last (rest l) n))))

(check= (select-last (list 1 2 3 4) 2) (list 3 4))
(check= (select-last () 5) ())
(check= (select-last (list 1 2 3) 1) (list 3))

; Prove that select-last produces a list with no more than n elements.

(thm (implies (and (natlistp l) (natp n))
              (<= (len (select-last l n)) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We will be developing programs for storing and averaging assignment, quiz
; and exam grades. The first step is to define appropriate datatypes.
; First define a datatype for the kinds of grades in the spreadsheet.

(defdata grade-type (oneof 'assignment 'quiz 'exam))

(check= (grade-typep 'exam) t)
(check= (grade-typep 'assignment) t)
(check= (grade-typep 'quiz) t)
(check= (grade-typep 'test) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The quizzes have a section, but other grades are not by section.  Define
; a datatype that allows one to specify one of the sections to be S1, S3,
; S4, S5, or not to have a section.

(defdata section (oneof 'S1 'S3 'S4 'S5 nil))

(check= (sectionp 'S1) t)
(check= (sectionp 'S2) nil)
(check= (sectionp 'S3) t)
(check= (sectionp 'S4) t)
(check= (sectionp 'S5) t)
(check= (sectionp nil) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The grades of each type are numbered, starting from 1. For example, hw02
; is the assignment grade with number 2. Define a record type that
; specifies a student id, a grade type, a grade number for the type, a
; section, and a score.  The student ids are positive nats.  The scores
; are nats. The field names should be id, type, number, sect and score. 

; Define this record type:

(defdata grade-rec (record (id . pos)
                           (type . grade-type)
                           (number . nat)
                           (sect . section)
                           (score . nat)))

(check= (grade-rec-number (grade-rec 101 'assignment 5 nil 85)) 5)
(check= (grade-rec-id (grade-rec 101 'assignment 5 nil 85)) 101)
(check= (grade-rec-sect (grade-rec 101 'assignment 5 nil 85)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Finally, the spreadsheet of all grades in the course is a set of grade
; records. Implement this using a list:

; using listof to specify that a grade-spreadsheet is a list of type grade-rec
(defdata grade-spreadsheet (listof grade-rec))

(check= (grade-spreadsheetp (list (grade-rec 101 'assignment 5 nil 85))) t)
(check= (grade-recp (first (list (grade-rec 101 'assignment 5 nil 85)))) t)
(check= (grade-rec-number (first (list (grade-rec 101 'assignment 5 nil 85)))) 
        5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To avoid having to keep typing a long list of grade records, it is handy
; to define a global constant which can then be used for many check= tests.
; You should put all of the grades of your team in a spreadsheet. Use your
; group name for the name of this spreadsheet. Your spreadsheet should
; contain your actual grades, but do not use your actual NU ids as your
; student ids. Your check= tests should use your own spreadsheet.

(defconst *cs2800* (list (grade-rec 101 'assignment 5 nil 85)
                         (grade-rec 102 'quiz 1 'S1 6)
                         (grade-rec 102 'quiz 6 'S1 21)))

(check= (grade-rec-sect (first (rest *cs2800*))) 'S1)
(check= (grade-rec-id (second (rest *cs2800*))) 102)
(check= (grade-rec-type (first (rest *cs2800*))) 'quiz)
(check= (grade-rec-score (first (rest *cs2800*))) 6)

(defconst *cs2800r* (list (grade-rec 154 'assignment 1 nil 76)
                          (grade-rec 154 'assignment 2 nil 87)
                          (grade-rec 154 'quiz 5 'S4 12)
                          (grade-rec 154 'quiz 6 'S3 0)
                          (grade-rec 154 'quiz 6 'S4 24)))

(check= (grade-rec-sect (first (rest *cs2800r*))) nil)
(check= (grade-rec-id (second (rest *cs2800r*))) 154)
(check= (grade-rec-type (first (rest *cs2800r*))) 'assignment)
(check= (grade-rec-score (first *cs2800r*)) 76)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now develop a function that extracts the list of all scores for one
; student, of one grade type in one section.

; Define:
; extract-scores: Grade-spreadsheet x Pos x Grade-type x Section -> Natlist
; (extract-scores ss id gt s) is the set of scores from the spreadsheet ss
; for the student with the given id whose grade type is gt and section is s.

(defunc extract-scores (ss id gt s)
  :input-contract (and (grade-spreadsheetp ss) (posp id) 
                       (grade-typep gt) (sectionp s))
  :output-contract (natlistp (extract-scores ss id gt s))
  (if (endp ss)
    ()
    (if (and (equal (grade-rec-id (first ss)) id) 
             (equal (grade-rec-sect (first ss)) s)
             (equal (grade-rec-type (first ss)) gt))
      (append (list (grade-rec-score (first ss))) 
              (extract-scores (rest ss) id gt s))
      (extract-scores (rest ss) id gt s))))

(check= (extract-scores *cs2800* 102 'quiz 'S1) (list 6 21))
(check= (extract-scores *cs2800r* 154 'quiz 'S3) (list 0))
(check= (extract-scores *cs2800r* 154 'quiz 'S4) (list 12 24))
(check= (extract-scores () 154 'quiz 'S3) ())




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The assignment grade for one student is obtained by extracting the top 10
; assignment scores for the student, and then averaging the scores.  You
; can use the sort function programmed in hw03. However, you should make
; sure that it passes contract and termination testing. The posted solution
; does not achieve this.

; Define:
; average-assignment-grade: Grade-spreadsheet x Pos x Pos -> Rational
; (average-assignment-grade ss id top) is the average of the top best
; assignments for the student with the given id in the spreadsheet ss.

; Essentially we want to average the last 'top' elements of the list of all 
; assignment grades.
(defunc average-assignment-grade (ss id top)
  :input-contract (and (grade-spreadsheetp ss) (posp id) (posp top))
  :output-contract (rationalp (average-assignment-grade ss id top))
  (if (endp ss)
    0
    (average (select-last (sort (extract-scores ss id 'assignment nil)) top))))

(check= (average-assignment-grade *cs2800* 101 1) 85)
(check= (average-assignment-grade *cs2800r* 101 2) 0)
(check= (average-assignment-grade *cs2800r* 154 2) 163/2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The quiz grade for one student is obtained by dropping the first quiz,
; dropping 10% of the ones that are left, and then averaging what remains.
; If the number of quizzes is not divisible by 10, then round the number.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define:
; average-quiz-grade: Grade-spreadsheet x Pos x Section -> Rational
; (average-quiz-grade ss id s) is the average grade of all quizzes taken
; by the student with given id in section s from the spreadsheet ss.
; Quiz number 1 is not used, and the lowest 10% of the remaining scores
; are dropped before computing the average.

;function to drop quiz number q
(defunc drop-quiz (ss q)
  :input-contract (and (grade-spreadsheetp ss) (natp q))
  :output-contract (grade-spreadsheetp (drop-quiz ss q))
  (if (endp ss)
    ss
    (if (equal (grade-rec-number (first ss)) q)
      (drop-quiz (rest ss) q)
      (cons (first ss) (drop-quiz (rest ss) q)))))

(check= (drop-quiz *cs2800* 1) (list (grade-rec 101 'assignment 5 nil 85)
                                     (grade-rec 102 'quiz 6 'S1 21)))
(check= (drop-quiz () 5) ())

(defunc average-quiz-grade (ss id s)
  :input-contract (and (grade-spreadsheetp ss) (posp id) (sectionp s))
  :output-contract (rationalp (average-quiz-grade ss id s))
  (if (or (endp ss) (equal s nil))
    0
    (let* ((l (sort (extract-scores (drop-quiz ss 1) 
                                    id 'quiz s)))
           (ten-percent (round (* 9/10 (len l)))))
      (average (select-last l ten-percent)))))

(check= (average-quiz-grade *cs2800* 102 'S1) 21)
(check= (average-quiz-grade () 293 'S3) 0)

;For testing purposes
(defconst *quiz-class* (list (grade-rec 154 'quiz 1 'S1 76)
                             (grade-rec 154 'quiz 2 'S1 87)
                             (grade-rec 182 'quiz 3 'S1 12)
                             (grade-rec 154 'quiz 3 'S1 9)
                             (grade-rec 154 'quiz 4 'S1 87)
                             (grade-rec 154 'quiz 5 'S1 13)
                             (grade-rec 154 'quiz 6 'S1 88)
                             (grade-rec 154 'quiz 7 'S1 100)
                             (grade-rec 154 'quiz 8 'S1 12)
                             (grade-rec 48 'quiz 2 'S1 3)
                             (grade-rec 154 'quiz 9 'S1 66)
                             (grade-rec 154 'quiz 10 'S1 69)
                             (grade-rec 154 'quiz 11 'S3 99)
                             (grade-rec 154 'quiz 12 'S1 487)))

(check= (average-quiz-grade *quiz-class* 154 'S1) 1009/9)#|ACL2s-ToDo-Line|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;