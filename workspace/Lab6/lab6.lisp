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
Lab 6: Fri 2/19   
CS 2800   
Spring 2016

For this lab, we will not be writing code.  Instead we will focus on equational reasoning.  Write your solutions in this file.


Question 1: Applying a substitution.

Below you are given a set of ACL2 terms and substitutions. Recall 
that a substitution is a list of 2-element lists, where the first 
element of each list is a variable and the second an
expression. Also, variables can appear only once as a left
element of a pair in any substitution. For example, the
substitution ((y (cons a b)) (x m)) maps y to (cons a b) and x to
m. For each term/substitution pair below, show what you get when
you apply the substitution to the term (i.e., when you
instantiate the term using the given substitution). 

a. (app x y)
   ((x (app b a)) (y (list w)))
   
   (app (app b a) (list w))

b. (app a 'b)
   ((a (cons d nil)) (b (cons c nil)))
   
   (app (a (cons d nil)) 'b)

c. (cons (rev2 (app x y)) z)
   ((x (cons ‘b nil)) (y (app b c)) (z (list a b)))

   (cons (rev2 (app (cons 'b nil) (app b c))) (list a b))
   
d. (or (endp x) (listp (app x y)))
   ((x nil) (y (list (first a))) )
   
   (or (endp nil) (listp (app nil (list (first a)))))


Question 2: Find a substitution, if it exists.
For each pair of ACL2 terms, give a substitution that instantiates the first to the second. If no substitution exists write "None".

a. (in a b)
   (in (first x) (app x y)) 
   
   ((a (first x)) (b (app x y)))

b. (app (rev2 x) y)
   (app b (rev2 c))
   
   (((rev2 x) b) (y (rev2 c)))

c. (app (rev2 a) (list b))
   (app (rev2 (cons y (list (first x)))) (list (+ (len (rest x)) z)))
   
   ((a (cons y (list (first x)))) (b (+ (len (rest x)) z))

d. (app x x)
   (app w z)
   
   None

e. (merge x y)
   (app 2 3)

   (((merge x y) (app 2 3)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equational Reasoning
Questions 3 and 4 ask for equational proofs about ACL2 programs. 
When writing your equational reasoning be sure to justify each step 
in the style shown in class, eg.

 (sum (app x y))
= { Def app, (consp x) }
 (sum (cons (first x) (app (rest x) y)))

You can use basic arithmetic facts for free, but in the 
justification write "arithmetic", e.g.,

 (first x) + (sum (rest x)) + (sum y) + 0
= { Arithmetic }
 (sum (rest x)) + (sum y) + (first x)

Notice also that it is OK to use infix notation (like x+y+z) for
arithmetic operators instead of the standard prefix notation
(like (+ x (+ y z)) or (+ x y z))

Here are the common definitions used for questions 3 & 4:

(defunc len2 (x)
  :input-contract (listp x)
  :output-contract (natp (len2 x))
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

(defunc app2 (x y)
  :input-contract (and (listp x) (listp y))
  :output-contract (listp (app2 x y))
  (if (endp x)
      y
    (cons (first x) (app2 (rest x) y))))

(defunc rev2 (x)
  :input-contract (listp x) 
  :output-contract (listp (rev2 x))
  (if (endp x)
      nil
    (app2 (rev2 (rest x)) (list (first x)))))



Recall that for each of the defunc's above we have both a
definitional axiom, and a contract theorem. 
For example for len2, we have the definitional axiom:

(implies (listp x)
         (equal (len2 x)
                (if (endp x)
                    0
                  (+ 1 (len2 (rest x))))))

The contract theorem is:

(implies (listp x)
         (natp (len2 x)))

You can use definitional axioms and contract theorems for free,
i.e., you don't have to prove anything.

By the way ACL2s can prove all of these theorems trivially.

For the rest of your lab questions, you can assume that the following is a theorem

(implies (and (listp x) 
              (listp y))
         (equal (len2 (app2 x y))
                (+ (len2 x) (len2 y))))

Question 3:

a. Prove the following using equational reasoning:

(implies (and (listp x) 
              (listp y))
         (equal (len2 (app2 x y)) 
                (len2 (app2 y x))))
                
= { Above Theorem (listp x) (listp y) }
(equal (+ (len2 x) (len2 y)) (+ (len2 y) (len2 x)))
= { Arithmetic } 
(equal (+ (len2 x) (len2 y)) (+ (len2 x) (len2 y)))
= { Substituting (i (+ (len2 x) (len2 y))) }
(equal i i)
QED

b. Given that x and y are lists, prove the following using
equational reasoning: 

(implies (and (listp x) 
              (listp y))
         (implies (equal (len2 (app2 x y)) 0)
                  (equal (len2 (app2 y x)) 0)))
BTW, you can assume 3a is a theorem whether or not you proved it.

= { Transitivity Axiom }
(equal (len2 (app2 x y)) (len2 (app2 y x)))
= { Theorem 3a } 
(equal i i)
QED


Question 4: Prove the following conjecture.  
You may want to break it into parts.

(implies (listp x) 
     (and   (implies (endp x)
                   (equal (len2 (rev2 x))(len2 x)))
              (implies (and (not (endp x))
                         (equal (len2 (rev2 (rest x)))
                              (len2 (rest x))))
                  (equal (len2 (rev2 x))(len2 x)))))
|#