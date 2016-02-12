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

Lab 5. Propositional Logic

Open this file in Beginner Mode

We use the following ascii character combinations to represent the Boolean
connectives:

  NOT     ~

  AND     &
  OR      +

  IMPLIES =>

  EQUIV   =
  XOR     <>

The binding powers of these functions are listed from highest to lowest
in the above table. Within one group (no blank line), the binding powers
are equal. This is the same as in class.

Note that AND is & for this lab as opposed to /\.  Different
books use different symbols, so it is good to get accustomed to
that.

|#

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)
(set-defunc-timeout 20)
(acl2s-defaults :set cgen-timeout 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simplification of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

There are many ways to represent a formula. For example:

p + (p => q))

is equivalent to

true

For each of the following, try to find the simplest equivalent
formula. By simplest, we mean the one with the least number of
connectives and parentheses. You can use any unary or binary
connective shown above in the propositional logic section. State the equalities you used (it's a good habit).

Simplify.

(1) a + (a & b)
(a + a) & (a + b) Distributive
a & (a + b) Idempotence
a Absorption

(2) (~ a & ~ b) & (a + b) 
~(a + b) & (a + b) De Morgan's
nil Complement

(3) (a <> c) & (a <> b) & ~ a
~a & c & b 
   
|#

(thm (implies (and (booleanp a)
                   (booleanp b))
              (iff (or a (and a b))
                   a)))

(thm (implies (and (booleanp a)
                   (booleanp b))
              (iff (and (and (not a) (not b)) (or a b))
                   nil)))

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

(1) ((q => q) => q)

q | q => q | (q => q) => q 
--------------------------
T |   T    |       T
F |   T    |       F

satisfiable, falsifiable

(2) (p <> q <> true)

p | q | p <> q | p <> q <> true 
-------------------------------
T | T |   F    |      T
T | F |   T    |      F
F | T |   T    |      F
F | F |   F    |      T

satisfiable, falsifiable

(3) (p => (q => (p & q)))

p | q | p & q | q => (p & q) | p => (q => (p & q))
--------------------------------------------------
T | T |   T   |      T       |          T
T | F |   F   |      T       |          T
F | T |   F   |      F       |          T 
F | F |   F   |      T       |          T

valid, satisfiable

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Set Manipulations and defdata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Let's help you with your homework by helping you with your
groceries.  Let's make a function that takes in a week's grocery
list and lets you quickly find all items to purchase from a
particular section of the store.  For example, if you are in the
dairy section, get-sublist takes a grocery list & 'dairy and
returns all the dairy products you need to buy that week.

First we define all the grocery store sections.
Please ignore the fact that 'other really does include a lot of
stuff
Then we will define units of measurement, grocery items, and
finally the lists themselves
|#

(defdata grocery-section (enum '(dairy produce meat fish cereal bread other)))

(check= (grocery-sectionp 'dairy) t)
(check= (grocery-sectionp 'veggies) nil)

(defdata unit-type (enum '(pounds count)))
(defdata unit-pounds (list 'pounds rational))
(defdata unit-count (list 'count nat))
(defdata unit-measure (oneof unit-pounds unit-count))

(check= (unit-measurep (list 'count 4)) t)
(check= (unit-measurep (list 'pounds 3/4)) t)
(check= (unit-measurep (list 'count 3/4)) nil)
(check= (unit-measurep (list 'count)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Each grocery item on the list consists of a section, name, and amount to buy
(defdata grocery-item (list grocery-section String unit-measure))

(check= (grocery-itemp (list 'dairy "yogurt" (list 'count 3))) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finally, the grocery list is a set of grocery-items. Implement this using a list:
(defdata grocery-list (listof grocery-item))
; Add appropriate checks to ensure the list works as expected

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To avoid having to keep typing a long grocery list, let's pre-define a couple lists
(defconst *groceries-1_28* (list (list 'dairy "yogurt" (list 'count 3))
                                 (list 'meat "beef tenderloin" (list 'pounds 3/4))
                                 (list 'produce "oranges" (list 'pounds 3))
                                 (list 'cereal "mini-wheats" (list 'count 1))
                                 (list 'dairy "Oka cheese" (list 'pounds 1/10))))

(check= (grocery-listp *groceries-1_28*) t)
; Make two more grocery lists to test against
;...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now develop a function that extracts the list of all items from a particular 
; section of the store

; Define:
; get-sublist: grocery-list x  grocery-section-> grocery-list
; Filter your grocery list to only give the items you need 
; to pick up from a particular section of the store.
(defunc get-sublist (gl s)
  :input-contract (and (grocery-listp gl) (grocery-sectionp s))
  :output-contract (grocery-listp (get-sublist gl s))
  (if (endp gl)
    gl
    (if (equal (first (first gl)) s)
      (cons (first gl) (get-sublist (rest gl) s))
      (get-sublist (rest gl) s))))

(check= (get-sublist *groceries-1_28* 'dairy) 
        (list (list 'dairy "yogurt" (list 'count 3))
              (list 'dairy "Oka cheese" (list 'pounds 1/10))))

; Add more checks
(check= (get-sublist *groceries-1_28* 'meat) 
        (list (list 'meat "beef tenderloin" (list 'pounds 3/4))))
(check= (get-sublist *groceries-1_28* 'cereal) 
        (list (list 'cereal "mini-wheats" (list 'count 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Operator reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Boolean operators can often be reduced to one another.  In the
following problems, you will write functions to perform operator
reductions on propositional expressions. At the end, you will
take any Boolean expression and transform it into an equivalent
expression using only not and or.

|#

; UnaryOp:   '~ means "not"

(defdata UnaryOp '~)

; BinaryOp: '& means "and", '+ means "or", '=> means "implies",
; and '== means "iff". 

(defdata BinaryOp (enum '(& + => ==)))

; PropEx: A Propositional Expression (PropEx) can be a boolean (t
; or nil), a symbol denoting a variable (e.g. 'p or 'q), or a
; list denoting a unary or binary operation. 

(defdata PropEx 
  (oneof boolean 
         symbol 
         (list UnaryOp PropEx) 
         (list PropEx BinaryOp PropEx)))

; IGNORE THESE THEOREMS. USED TO HELP ACL2S REASON
(defthm propex-expand1
  (IMPLIES (AND (PROPEXP X)
                (NOT (SYMBOLP X)))
           (equal (SECOND X)
                  (acl2::second x))))

(defthm propex-expand2
  (implies (and (propexp x)
                (not (symbolp x))
                (not (equal (first (acl2::double-rewrite x)) '~)))
           (equal (third (acl2::double-rewrite x))
                  (acl2::third (acl2::double-rewrite x)))))

(defthm propex-expand3
  (implies (AND (PROPEXP PX)
                (CONSP PX)
                (NOT (UNARYOPP (FIRST PX))))
           (and (equal (THIRD PX)
                       (acl2::third px))
                (equal (second px)
                       (acl2::second px))
                (equal (first px)
                       (acl2::first px)))))

(defthm propex-expand2a
  (implies (and (propexp x)
                (not (symbolp x))
                (not (unaryopp (first (acl2::double-rewrite x)))))
           (equal (third (acl2::double-rewrite x))
                  (acl2::third (acl2::double-rewrite x)))))

(defthm propex-lemma2
  (implies (and (propexp x)
                (not (symbolp x))
                (not (equal (first (acl2::double-rewrite x)) '~)))
           (and (propexp (first x))
                (propexp (acl2::first x))
                (propexp (third x))
                (propexp (acl2::third x)))))

(defthm propex-lemma1
  (implies (and (propexp x)
                (not (symbolp x))
                (equal (first (acl2::double-rewrite x)) '~))
           (and (propexp (second x))
                (propexp (acl2::second x)))))

; END OF
; IGNORE THESE THEOREMS. USED TO HELP ACL2S REASON 
                         

#|
(A) Write a function that takes a PropEx and replaces all
occurrences of

p /\ q 

with 

~ ((~ p) + (~ q))

where p and q stand for arbitrary expressions.

This is not that easy. See the check='s to get a better 
sense of what you need to do.
|#

(defunc RemoveAnd (px)
  :input-contract (PropExp px)
  :output-contract (PropExp (RemoveAnd px))
  (if (and (listp px)  (not (endp px))) 
    ;Needs the "not (endp px)" or else the driver hates you
    (if (> (len px) 2)
      (if (equal (second px) '&)
        (list '~ (list (list '~ (RemoveAnd (first px))) 
                       '+ 
                       (list '~ (RemoveAnd (third px)))))
        (list (RemoveAnd (first px)) 
              (first (rest px)) 
              (RemoveAnd (third px))))
      (list (first px) (RemoveAnd (second px))))
    px))
 
(check= (RemoveAnd '(p & q)) '(~ ((~ p) + (~ q))))
(check= (RemoveAnd '(~ (p & q))) '(~ (~ ((~ p) + (~ q)))))
(check= (RemoveAnd '(p + (q & r))) '(p + (~ ((~ q) + (~ r)))))

#| Better way to write RemoveAnd 
(defunc RemoveAnd (px)
  :input-contract (PropExp px)
  :output-contract (PropExp (RemoveAnd px))
  (cond ((booleanp px) px)
        ((sumbolp px) px)
        ((UnaryOpp (first px))
         (list (first px) (RemoveAnd (second px))))
        ((equal (second px) '&)
         (list '~ (list 
                   (list '~ (RemoveAnd (first px)))
                   '+
                   (list '~ (RemoveAnd (third px))))))
        (t (list (RemoveAnd (first px)) (second px) (RemoveAnd (third px))))))
|#

#|
(B) Write a function that takes a PropEx and replaces all
occurrences of 

p => q 

with 

~ p + q 

where p and q stand for arbitrary expressions.

|#

(defunc RemoveImplies (px)
  :input-contract (PropExp px)
  :output-contract (PropExp (RemoveImplies px))
  (if (and (listp px)  (not (endp px))) 
    ;Needs the "not (endp px)" or else the driver hates you
    (if (> (len px) 2)
      (if (equal (second px) '=>)
        (list (list '~ (RemoveImplies (first px))) 
              '+ 
              (RemoveImplies (third px)))
        (list (RemoveImplies (first px)) 
              (second px)
              (RemoveImplies (third px))))
      (list (first px) (RemoveImplies (second px))))
    px))


(check= (RemoveImplies '(p => q)) '((~ p) + q))
(check= (RemoveImplies '(~ (p => q))) '(~ ((~ p) + q)))
(check= (RemoveImplies '(p + (q => r))) '(p + ((~ q) + r)))

#|
(C) Write a function that takes a PropEx and replaces all
occurrences of

p = q 

with 

(p => q) & (q => p) 

where p and q stand for arbitrary expressions.  
|#

(defunc RemoveIff (px)
  :input-contract (PropExp px)
  :output-contract (PropExp (RemoveIff px))
  (if (and (listp px)  (not (endp px))) 
    ;Needs the "not (endp px)" or else the driver hates you
    (if (> (len px) 2)
      (if (equal (second px) '==)
        (list (list (RemoveIff (first px)) 
                    '=>
                    (RemoveIff (third px))) 
              '&
              (list (RemoveIff (third px)) 
                    '=>
                    (RemoveIff (first px)))) 
        (list (RemoveIff (first px)) 
              (second px) 
              (RemoveIff (third px))))
      (list (first px) (RemoveIff (second px))))
    px))

(check= (RemoveIff '(p == q)) '((p => q) & (q => p)))
(check= (RemoveIff '(~ (p == q))) '(~ ((p => q) & (q => p))))
(check= (RemoveIff '(p + (q == r))) '(p + ((q => r) & (r => q))))

#|
(D) Write a function that takes a PropEx and returns another
PropEx that only uses the OR and NOT connectives. Write 
at least 4 interesting check='s.

|#

(defunc Reduce (px)
  :input-contract (PropExp px)
  :output-contract (PropExp (Reduce px))
  (RemoveAnd (RemoveImplies (RemoveIff px))))

(Reduce '(((p => q) & q) == q))

(check= (Reduce '(((p => q) & q) == q)) 
        '(~ ((~ ((~ (~ ((~((~ p) + q)) + (~ q)))) + q))
             + (~((~ q) + (~((~((~ p) + q)) + (~ q))))))))#|ACL2s-ToDo-Line|#



#|

Bonus (E): Change of spec! We want to add another binary connective, !,
that is the NOR operator:

a!b = ~(a+b)

Changes in spec happen all the time! You want to design your code
so that it is robust with respect to code changes.

(E1) Update the data definition for BinaryOp so that ! is one of
the binary operators.  Rerun your code. Any problems? Why or why
not?

(E2) We want to express any formula in terms of ! only, so use
the functions you already wrote and any new helper functions you
need to make that happen.

Write at least 4 interesting check='s. 

|#

...