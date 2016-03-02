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
:program
#|

CS 2800 Homework 7 - Spring 2016

This homework is done in groups. The groups are normally the same ones as in 
Assignment #6.  Changes can be made on request.  However, all such requests
must be made at least two days before the assignment is due, and all the 
students involved must agree to the changes.  When making a request for a
change in the groups, please specify all of the changes, and remember that
a group cannot consist of a single student.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw07.lisp

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

- when done, save your file and submit it as hw07.lisp

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

As in hw05, you will be programming some functions.  If one or more check=
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Equational Proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Questions 1 to 6 ask for equational proofs about ACL2
programs. In each question you will be given one or more function
definitions. The definitional axioms and contract theorems for
these functions can be used in the proof. All of the proofs you
are asked to do are trivial for ACL2s. You can use ACL2s to check
the conjectures you come up with, but you are not required to do
so. 

Here are some notes about equational proofs:

1. The context. Remember to use propositional logic to rewrite
the context so that it has as many hypotheses as possible.  See
the lecture notes for details [1]. Label the facts in your
context with C1, C2, ... as in the lecture notes.

2. The derived context. Draw a line after the context and add
anything interesting that can be derived from the context.  Use
the same labeling scheme as was used in the context. Each derived
fact needs a justification. Again, look at the lecture notes for
more information.

3. Use the proof format shown in class and in the lecture notes,
which requires that you justify each step [1].

4. When using an axiom, theorem or lemma, show the name of the
axiom, theorem or lemma, and then show the instantiation you are
using.

5. When using the definitional axiom of a function, the
justification should say "Def function name".  When using the
contract axiom of a function, the justification should say
"Contract function name".

6. Arithmetic facts such as commutativity of addition can be
used. The name for such facts is "arithmetic".

7. You can refer to the axioms for cons, consp, first and rest as
the "cons axioms". The axioms for if are named "if axioms" Any
propositional reasoning used can be justified by "propositional
reasoning", except that we will often use "MP" for modus
ponens. 

8. For this homework, you can only use theorems we explicitly
tell you you can use. You can use a preceding theorem even if you
have not succeeded in proving it. You can, of course, use the
definitional axiom and contract theorem for any function used or
defined in this homework. You can use any theorem we mention in
a *previous* problem. For example, for problem 4 you can use a
theorem mentioned in problem 2, but not the other way around.

The definitions of listp and endp are in the lecture notes on
page 10 [1]:

(defunc listp (l)
  :input-contract t
  :output-contract (booleanp (listp l))
  (if (consp l)
      (listp (rest l))
    (equal l ())))

(defunc endp (l)
  :input-contract (listp l)
  :output-contract (booleanp  (endp l))
  (not (consp l)))

We also use the following functions:

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; len2: List -> Nat
;
; (len2 l) is number of elements in l

(defunc len2 (x)
  :input-contract (listp x)
  :output-contract (natp (len2 x))
  (if (endp x)
    0
    (+ 1 (len2 (rest x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; index: Any x List -> Nat
;
; (index a l) is the position of the first occurrence of a in l.
; If a is not in l, then the length of the list is returned.

(defunc index (a l)
  :input-contract (listp l)
  :output-contract (natp (index a l))
  (cond ((endp l) 0)
        ((equal a (first l)) 0)
        (t (+ 1 (index a (rest l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; in2: Any x List -> Nat
;
; (in2 a l) is true if and only if a occurs in the list l.

(defunc in2 (a l)
  :input-contract (listp l)
  :output-contract (booleanp (in2 a l))
  (cond ((endp l) nil)
        ((equal a (first l)) t)
        (t (in2 a (rest l)))))

#|

You may also use the following theorem (Question 4 of HW06):

Length of rest:
(implies (and (listp l) (not (endp l)))
         (equal (len2 (rest l)) (- (len2 l) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Question 1 :

Show that the index of a in l is at most the length of l.

a. Prove the following using equational reasoning:

(implies (listp l)
  (implies (endp l) (<= (index a l) (len2 l))))

C1. (listp l)
C2. (endp l)
--------------
C3. (equal l nil) {C1, C2, Def endp, Def listp}

Proof:

(<= (index a l) (len2 l))
= {Def index, C2, if axiom}
(<= 0 (len2 l))
= {Def len2, C2, if axiom}
(<= 0 0)
={Propositional Reasoning}
T QED

b. Prove the following using equational reasoning:

(implies (listp l)
  (implies
    (and (not (endp l))
         (equal a (first l)))
    (<= (index a l) (len2 l))))

C1. (listp l)
C2. (not (endp l))
C3. (equal a (first l))
-----------------------
C4. (listp (rest l)) {C1, C2, Def listp}

Proof: 

(<= (index a l) (len2 l))
= {Def index, C2, C3, cond axiom}
(<= 0 (len2 l))
= {Def len2, C2, if axiom}
(<= 0 (+ 1 (len2 (rest l))))
= {contract len2, C4, arithmetic}
(<= 0 1)
={Propositional Reasoning}
T QED

c. Prove the following using equational reasoning:

(implies (listp l)
  (implies
    (and (not (endp l))
         (not (equal a (first l)))
         (<= (index a (rest l)) (len2 (rest l))))
    (<= (index a l) (len2 l))))

C1. (listp l)
C2. (not (endp l))
C3. (not (equal a (first l)))
C4. (<= (index a (rest l)) (len2 (rest l)))
-------------------------------------------
C5. (listp (rest l)) {C1, C2, Def listp}

Proof: 

(<= (index a l) (len2 l))
= {Def index, C2, C3, cond axiom}
(<= (+ 1 (index a (rest l))) (len2 l))
= {Def len2, C2, if axiom}
(<= (+ 1 (index a (rest l))) (+ 1 (len2 (rest l))))
= {arithmetic}
(<= (index a (rest l)) (len2 (rest l)))
= {C4}
T QED


Question 2 :

Show that if a is in l, then the index of a in l is
less than the length of l.

a. Prove the following using equational reasoning:

(implies (listp l)
  (implies (endp l)
    (implies (in2 a l) (< (index a l) (len2 l)))))

...

b. Prove the following using equational reasoning:

(implies (listp l)
   (implies 
     (and (not (endp l))
          (equal a (first l)))
     (implies (in2 a l) (< (index a l) (len2 l)))))

...

c. Prove the following using equational reasoning:

(implies (listp l)
   (implies 
     (and (not (endp l))
          (not (equal a (first l)))
          (implies (in2 a (rest l)) (< (index a (rest l)) (len2 (rest l)))))
     (implies (in2 a l) (< (index a l) (len2 l)))))

...

Question 3 :

Show that if a is not in l, then the index of a in l is
equal to the length of l.

a. Prove the following using equational reasoning:

(implies (listp l)
  (implies (endp l)
    (implies (not (in2 a l)) (equal (index a l) (len2 l)))))

...

b. Prove the following using equational reasoning:

(implies (listp l)
   (implies 
     (and (not (endp l))
          (equal a (first l)))
     (implies (not (in2 a l)) (equal (index a l) (len2 l)))))

...

c. Prove the following using equational reasoning:

(implies (listp l)
   (implies 
     (and (not (endp l))
          (not (equal a (first l)))
          (implies (not (in2 a (rest l))) (equal (index a (rest l)) (len2 (rest l)))))
     (implies (not (in2 a l)) (equal (index a l) (len2 l)))))

...

Question 4 :

This question uses this function from [2]:

(defunc a (m n)
  :input-contract (and (natp m) (natp n))
  :output-contract (natp (a m n))
  (cond ((equal m 0) (+ n 1))
        ((equal n 0) (a (- m 1) 1))
        (t (a (- m 1) (a m (- n 1))))))

You may use these two theorems:

Theorem A0:
(implies (natp n) (equal (a 0 n) (+ 1 n)))

Theorem A1:
(implies (natp n) (equal (a 1 n) (+ 2 n)))

a. Prove the following using equational reasoning:

(implies (and (natp n)
              (equal n 0))
         (equal (a 2 n) (+ (* 2 n) 3)))

...

b. Prove the following using equational reasoning:

(implies (and (natp n)
              (not (equal n 0))
              (implies (natp (- n 1))
                       (equal (a 2 (- n 1)) (+ (* 2 (- n 1)) 3))))
         (equal (a 2 n) (+ (* 2 n) 3)))

...

|#

; For the rest of this assignment, we will reuse some functions from hw03:

; Define
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
; A Decision Procedure for Satisfiability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

In hw04 and hw05, you were asked to determine whether a boolean formula
was satisfiable (among the other labels for boolean formulas).  In this
assignment we develop software for automating the process of deciding
whether a boolean formula is satisfiable.  The main function will be
solve, which checks whether a given propositional function is satisfiable,
and if it is satisfiable, returns a solution.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; UnaryOp:   '~ means "not"

(defdata UnaryOp '~)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BinaryOp: '&& means "and", '|| means "or", '=> means "implies",
; and '<> means exclusive or.

(defdata BinaryOp (enum '(&& || => <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A BooleanFormula can be a boolean (t or nil),
; a symbol denoting a variable (e.g. 'p or 'q), 
; or a list denoting a unary or binary operation.

(defdata BooleanFormula 
  (oneof boolean 
         symbol 
         (list UnaryOp BooleanFormula) 
         (list BooleanFormula BinaryOp BooleanFormula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IGNORE THESE THEOREMS. USED TO HELP ACL2S REASON
(defthm formula-expand1
  (implies (and (BooleanFormulap x)
                (not (symbolp x)))
           (equal (second x)
                  (acl2::second x))))

(defthm formula-expand2
  (implies (and (BooleanFormulap x)
                (not (symbolp x))
                (not (equal (first (acl2::double-rewrite x)) '~)))
           (equal (third (acl2::double-rewrite x))
                  (acl2::third (acl2::double-rewrite x)))))

(defthm formula-expand3
  (implies (and (BooleanFormulap formula)
                (consp formula)
                (not (UnaryOpp (first formula))))
           (and (equal (third formula)
                       (acl2::third formula))
                (equal (second formula)
                       (acl2::second formula))
                (equal (first formula)
                       (acl2::first formula)))))

(defthm formula-expand2a
  (implies (and (BooleanFormulap x)
                (not (symbolp x))
                (not (UnaryOpp (first (acl2::double-rewrite x)))))
           (equal (third (acl2::double-rewrite x))
                  (acl2::third (acl2::double-rewrite x)))))

(defthm formula-lemma2
  (implies (and (BooleanFormulap x)
                (not (symbolp x))
                (not (equal (first (acl2::double-rewrite x)) '~)))
           (and (BooleanFormulap (first x))
                (BooleanFormulap (acl2::first x))
                (BooleanFormulap (third x))
                (BooleanFormulap (acl2::third x)))))

(defthm formula-lemma1
  (implies (and (BooleanFormulap x)
                (not (symbolp x))
                (equal (first (acl2::double-rewrite x)) '~))
           (and (BooleanFormulap (second x))
                (BooleanFormulap (acl2::second x)))))


(defthm first-rest-listp
  (implies (and l (listp l))
           (and (equal (first l)
                       (acl2::first l))
                (equal (rest l)
                       (acl2::rest l)))))

; END IGNORE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An assignment is a pair consisting of a symbol
; and a boolean value. For example (cons 'x t) means
; that the variable x is being assigned the value t.
(defdata Assignment (cons symbol boolean))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A collection of assignments
(defdata Assignlist (listof Assignment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A collection of symbols
(defdata Symlist (listof symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; get-variables: BooleanFormula x Symlist -> Symlist
;
; get-variables returns a list of variables appearing in formula, including
; those in the provided accumulator acc. If acc has no duplicates in it, then
; the returned list should not have any duplicates either. See the check='s
; below.
;
; Hint: you can use a helper function

; add-to-list: List x List -> List
; 
; add-to-list takes two lists and adds any elements of the first list
; that are not already in the second list to the second list
(defunc add-to-list (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (listp (add-to-list l1 l2))
  (cond ((endp l1) l2)
        ((equal 0 (multiplicity-in (first l1) l2))
         (add-to-list (rest l1) (cons (first l1) l2)))
        (t (add-to-list (rest l1) l2))))

(check= (is-rearrangement (add-to-list '(1 2 3 4 5 6) ()) '(1 2 3 4 5 6)) t)
(check= (is-rearrangement (add-to-list '(1 2 3 4 5 6) '(2 4 6 8 10)) 
                          '(1 2 3 4 5 6 8 10)) t)
(check= (is-rearrangement (add-to-list () ()) ()) t)
(check= (is-rearrangement (add-to-list () '(1 2 3 4 5 6)) '(1 2 3 4 5 6)) t)

(defunc get-variables (b acc)
  :input-contract (and (BooleanFormulap b) (Symlistp acc))
  :output-contract (Symlistp (get-variables b acc))
  (cond ((booleanp b) acc)
        ((symbolp b) (add-to-list (list b) acc))
        ((UnaryOpp (first b)) (add-to-list (get-variables (second b) ()) acc))
        (t (add-to-list (get-variables (first b) ())
                   (add-to-list (get-variables (third b) ()) acc)))))

(check= (is-rearrangement (get-variables '(q && (r || q)) '(r)) '(q r)) t)
(check= (is-rearrangement (get-variables '(q && (r || q)) '(r s)) '(q r s)) t)
(check= (is-rearrangement (get-variables '(q && (r || q)) '(r s s)) '(q r s s)) t)
(check= (is-rearrangement (get-variables '((q <> s) && (r || q)) ()) '(q r s)) t)
(check= (is-rearrangement (get-variables '(~ q) '(r)) '(q r)) t)
(check= (is-rearrangement (get-variables t '(r)) '(r)) t)
(check= (is-rearrangement (get-variables 'q '(r)) '(q r)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; update: BooleanFormula x Symbol x Boolean -> BooleanFormula
; The update function updates a variable by replacing all instances
; of the variable with the boolean val in the boolean formula.

(defunc update (formula name val)
  :input-contract (and (BooleanFormulap formula) (Symbolp name) (booleanp val))
  :output-contract (Booleanformulap (update formula name val))
  (cond ((booleanp formula) formula)
        ((atom formula) (if (equal formula name) val formula))
        ((UnaryOpp (first formula)) (list (first formula)
                                          (update (second formula) name val)))
        (t (list (update (first formula) name val)
                 (second formula)
                 (update (third formula) name val)))))

(check= (update '((nil || q) && (~ r)) 'q t) '((nil || t) && (~ r)))
(check= (update t 'q nil) t)
(check= (update 's 's t) t)
(check= (update '((q <> s) && (r || q)) 'q t) '((t <> s) && (r || t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; evaluate: BooleanFormula -> Boolean
; evaluate evaluates a constant boolean formula and return its value.  
; A constant boolean formula is a BooleanFormula with no variables, 
; just booleans and operators.  If the given BooleanFormula does
; contain variables, you can return whatever you like.

(defunc evaluate (formula)
  :input-contract (BooleanFormulap formula)
  :output-contract (booleanp (evaluate formula))
  (cond ((booleanp formula) formula)
        ((atom formula) t)
        ((UnaryOpp (first formula)) (not (evaluate (second formula))))
        ((equal '&& (second formula)) (and (evaluate (first formula))
                                           (evaluate (third formula))))
        ((equal '|| (second formula)) (or (evaluate (first formula))
                                          (evaluate (third formula))))
        ((equal '=> (second formula)) (if (evaluate (first formula))
                                        (evaluate (third formula)) t))
        (t (if (evaluate (first formula))
             (not (evaluate (third formula)))
             (evaluate (third formula))))))

(check= (evaluate '((t && (~ nil)) || t)) t)
(check= (evaluate 'b) t)
(check= (evaluate nil) nil)
(check= (evaluate '(~ t)) nil)
(check= (evaluate '(t && nil)) nil)
(check= (evaluate '(t || nil)) t)
(check= (evaluate '(t => nil)) nil)
(check= (evaluate '(t <> nil)) t)
(check= (evaluate '((t <> nil) => nil)) nil)
(check= (evaluate '((t && (~ nil)) || ((t <> nil) => nil))) t)

; You should have at least one check= tests for every operator and
; every pair of operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The type of a solution to a boolean formula.

(defdata Solution (oneof Assignlist 'unsat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; solve-helper: BooleanFormula x Symlist x Assignlist -> Solution
;
; solve-helper checks a boolean formula for satisfiability, but also expects a
; list of variables from the formula (this simplifies the definition) and a
; list of assignments that have already been made (an accumulator).  The idea
; is that we construct the truth table ``on the fly'' by picking the first
; variable in vars, assigning it to t, substituting t for formula, and
; recurring. If that gives a solution, return it; otherwise, try assigning the
; var to nil and recur again.

(defunc solve-helper (formula vars assignments)
  :input-contract (and (BooleanFormulap formula) 
                       (Symlistp vars) (Assignlistp assignments))
  :output-contract (Solutionp (solve-helper formula vars assignments))
  (if (endp vars)
    (if (evaluate formula)
      assignments
      'unsat)
    (let* 
      ((x (solve-helper (update formula (first vars) t) (rest vars) 
                        (cons (cons (first vars) t) assignments)))
       (y (solve-helper (update formula (first vars) nil) (rest vars) 
                        (cons (cons (first vars) nil) assignments))))
      (if (equal x 'unsat)
        (if (equal y 'unsat)
          'unsat
          y)
        x))))

(check= (solve-helper t '() (list (cons 'q t))) (list (cons 'q t)))
(check= (is-rearrangement (solve-helper '(q => p) '(q p) ()) 
                          (list (cons 'q t) (cons 'p t))) t)
(check= (is-rearrangement (solve-helper '(q => (q => nil)) '(q) ()) 
        (list (cons 'q nil))) t)
(check= (is-rearrangement (solve-helper '((q => (q => (~ p))) <> nil) '(q p) ()) 
        (list (cons 'q t) (cons 'p nil))) t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; solve: BooleanFormula -> Solution
; This is our procedure for finding a solution to a boolean formula.  Solve
; attempts to find a set of assignments to the variables in the formula such
; that the formula will evaluate to true. If successful, solve returns a
; list of assignments. If the formula is not satisfiable, solve returns
; 'unsat.  This function just calls solve-helper to do the work.

(defunc solve (formula)
  :input-contract (Booleanformulap formula)
  :output-contract (Solutionp (solve formula))
  (solve-helper formula (get-variables formula ()) ()))

(check= (solve '(p && (~ p))) 'unsat)
(check= (is-rearrangement (solve '((q || (~ r)) && (~ s))) 
                          (list (cons 'q t) (cons 'r t) (cons 's nil))) t)
(check= (solve '(~ (((~ r) && (p <> (~ p))) <> ((r && q) || r))) ) 
        'unsat)
(check= (is-rearrangement (solve '(~ (p <> ((~ q) || q)))) 
        (list (cons 'q t) (cons 'p t))) t)
(check= (is-rearrangement (solve '(p <> (p => (((~ p) && q) || (~ p))))) 
        (list (cons 'q t) (cons 'p t))) t)#|ACL2s-ToDo-Line|#


#|

[1] http://www.ccs.neu.edu/home/pete/courses/Logic-and-Computation/2016-Spring/rappg.pdf

[2] Raphael M. Robinson, "Recursion and Double Recursion". 
Bulletin of the American Mathematical Society 54 (10):
987â€“93. doi:10.1090/S0002-9904-1948-09121-2. 1948.

|#