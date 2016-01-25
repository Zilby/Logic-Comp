#|$ACL2s-Preamble$;
(include-book ;; Newline to fool ACL2/cert.pl dependency scanner
 "portcullis")
(begin-book t :ttags :all);$ACL2s-Preamble$|#

(acl2::in-package "ACL2S T")

(acl2::include-book "defunc" :ttags :all)



(defun acl2s-bb-identity-bool-guard (x)
  (acl2::declare (acl2::xargs :guard (acl2::booleanp x)))
  x)

(acl2::defmacro if (test tb fb)
  `(acl2::if (acl2s-bb-identity-bool-guard ,test) ,tb ,fb))

(acl2::defthm acl2s-bb-identity-bool-guard-backchain
  (acl2::implies (acl2::booleanp x)
                 (equal (acl2s-bb-identity-bool-guard x)
                        x)))

(acl2::defthm acl2s-bb-identity-bool-guard-equal
  (equal (acl2s-bb-identity-bool-guard (equal x y))
         (equal x y)))

;skip testing in defunc events for faster loading
(acl2::table acl2s::defunc-defaults-table :skip-tests t :put)

(defunc first (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::car x))

(defunc second (x)
  :input-contract (and (consp x) (consp (cdr x)))
  :output-contract t
  (acl2::cadr x))

(defunc third (x)
  :input-contract (and (consp x) (consp (cdr x)) (consp (cddr x)))
  :output-contract t
  (acl2::caddr x))

(defunc fourth (x)
  :input-contract (and (consp x) (consp (cdr x)) (consp (cddr x))
                       (consp (cdddr x)))
  :output-contract t
  (acl2::cadddr x))

(defunc fifth (x)
  :input-contract (and (consp x) (consp (cdr x)) (consp (cddr x))
                       (consp (cdddr x)) (consp (cdr (cdddr x))))
  :output-contract t
  (acl2::car (acl2::cddddr x)))

(defunc rest (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::cdr x))

(defunc unary-- (x)
  :input-contract (rationalp x)
  :output-contract t
  (acl2::unary-- x))

(defunc unary-/ (x)
  :input-contract (acl2::and (rationalp x) (acl2::not (equal x 0)))
  :output-contract t
  (acl2::unary-/ x))

(defunc < (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (acl2::booleanp (< x y))
  (acl2::< x y))

(defunc + (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (rationalp (+ x y))
  (acl2::binary-+ x y))

(defunc * (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (rationalp (+ x y))
  (acl2::binary-* x y))

(defun my-preprocess (term wrld)
  (acl2::declare (acl2::ignore wrld))
  (acl2::cond ((acl2::and (consp term)
                          (acl2::or 
                           (equal (acl2::car term) 'acl2s-bb-identity-bool-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-consp-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-rationalp-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-rationalp-not-0-guard)))
               (acl2::cadr term))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::implies))
               (cons 'implies (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::car))
               (cons 'first (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::cdr))
               (cons 'rest (acl2::cdr term)))
              (t nil)))

; A hack to help proofs go through in this mode.
(acl2::in-theory (acl2::enable rest))

(acl2::table acl2::user-defined-functions-table
             'acl2::untranslate-preprocess
             'my-preprocess)

(defunc len (a) 
  :input-contract t 
  :output-contract (natp (len a))
  (if (atom a)
      0
    (+ 1 (len (cdr a)))))

(defthm intp-len 
  (integerp (len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(defunc append (a b) 
  :input-contract (and (true-listp a) (true-listp b))
  :output-contract (and (true-listp (append a b))
                        (equal (len (append a b)) (+ (len a) (len b))))
  (acl2::append a b))

(defunc app (a b) 
  :input-contract (and (true-listp a) (true-listp b))
  :output-contract (and (true-listp (app a b))
                        (equal (len (app a b)) (+ (len a) (len b))))
  (acl2::append a b))

(sig acl2::append ((listof :a) (listof :a)) => (listof :a))

#|
In the followin defunc, this subgoal is failing
(IMPLIES (AND (CONSP A)
              (EQUAL (LEN (REV (REST A)))
                     (LEN (REST A)))
              (TRUE-LISTP (REST A)))
         (EQUAL (LEN (APPEND (REV (REST A))
                             (LIST (FIRST A))))
                (COMMON-LISP::+ 1 (LEN (REST A)))))
|#
(defunc rev (a) 
  :input-contract (true-listp a) 
  :output-contract (and (true-listp (rev a))
                        (equal (len (rev a)) (len a)))
  (if (endp a)
      nil
    (append (rev (cdr a)) (list (car a)))))

(sig rev ((listof :a)) => (listof :a))

(defunc in (a X) 
  :input-contract (true-listp x)
  :output-contract (booleanp (in a X))
  (if (endp x)
      nil
    (or (equal a (car X))
        (in a (cdr X)))))

(defunc remove-dups (a) 
  :input-contract (true-listp a) 
  :output-contract (true-listp (remove-dups a))
  (if (endp a)
      nil
    (if (in (car a) (cdr a))
        (remove-dups (cdr a))
      (cons (car a) (remove-dups (cdr a))))))

(sig remove-dups ((listof :a)) => (listof :a))

(defunc nth (n l)
  :input-contract (and (natp n) (true-listp l))
  :output-contract t
  (if (endp l)
      nil
    (if (zp n)
        (car l)
      (nth (- n 1) (cdr l)))))
(sig nth (nat (listof :a)) => :a 
     :satisfies (< acl2s::x1 (len acl2s::x2)))

(defunc nthcdr (n l)
  :input-contract (and (natp n) (true-listp l))
  :output-contract (true-listp (nthcdr n l))
  (if (endp l)
      nil
    (if (zp n)
        l
      (nthcdr (- n 1) (cdr l)))))

(sig nthcdr (nat (listof :a)) => (listof :a))

(defthm natp-acl2-len-tp 
  (natp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(defthm integerp-acl2-len-tp 
  (integerp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(defunc string-len (l)
  :input-contract (stringp l)
  :output-contract (natp (string-len l))
  (acl2::length l))


;*************************************************************
; Adapted from the coi list books
;*************************************************************

(defthm len-non-negative-linear
  (<= 0 (len x))
  :rule-classes :linear)

(defthm len-non-negative
  (equal (< (len x) 0)
         nil))

(defthm len-when-consp-linear
  (implies (consp x)
           (< 0 (len x))))
;  :rule-classes :linear)

(defthm len-pos-rewrite
  (equal (< 0 (len x))
         (consp x)))

(defthm len-of-non-consp
  (implies (not (consp x))
           (equal (len x)
                  0)))

(defthm len-equal-0-rewrite
  (equal (equal 0 (len x))
         (not (consp x))))


(defthm consp-when-len-is-known-and-positive
  (implies (and (equal (len x) foo) ;foo is a free variable
                (< 0 foo))
           (consp x))
  :rule-classes :forward-chaining)

(defthm len-cons
  (equal (len (cons a x))
         (+ 1 (len x))))

(defthm consp-append
  (implies (and (true-listp x)
                (true-listp y))
           (equal (consp (append x y))
                  (or (consp x) 
                      (consp y)))))

(defthm append-consp-type-one
  (implies (and (true-listp x)
                (true-listp y)
                (consp x))
           (consp (append x y)))
    :rule-classes ((:type-prescription)))

(defthm append-consp-type-two
  (implies (and (true-listp x)
                (true-listp y)
                (consp y))
           (consp (append x y)))
    :rule-classes ((:type-prescription)))

(defthm car-append-not-consp
  (implies (and (true-listp x)
                (true-listp y)
                (not (consp x)))
   (equal (car (append x y))
          (car y))))

(defthm car-append-consp
  (implies (and (true-listp x)
                (true-listp y)
                (consp x))
           (equal (car (append x y))
                  (car x))))

(defthm cdr-append-not-consp
  (implies (and (true-listp x)
                (true-listp y)
                (not (consp x)))
           (equal (cdr (append x y))
                  (cdr y))))

(defthm cdr-append-consp
  (implies (and (true-listp x)
                (true-listp y)
                (consp x))
           (equal (cdr (append x y))
                  (append (cdr x) y))))

(defthm len-append
  (implies (and (true-listp x)
                (true-listp y))
           (equal (len (append x y))
                  (+ (len x)
                     (len y)))))

(defun len-len-induction (x y)
  (if (and (consp x)
           (consp y))
      (len-len-induction (cdr x) (cdr y))
    nil))

(defthm equal-len-append
  (implies (and (true-listp x)
                (true-listp y)
                (true-listp p)
                (true-listp q)
                (equal (len x) (len y)))
           (equal (equal (append x p) (append y q))
                  (and (equal x y)
                       (equal p q))))
  :hints (("Goal" 
           :induct (len-len-induction x y))))

(defthm append-of-non-consp-one
  (implies (and (true-listp x)
                (true-listp y)
                (not (consp x)))
           (equal (append x y) y))
  :rule-classes ((:rewrite :backchain-limit-lst 0)))

(defthm append-iff
  (implies (and (true-listp x)
                (true-listp y))
           (iff (append x y)
                (or (consp x) y))))

(defthm equal-append-x-append-x
  (implies (and (true-listp x)
                (true-listp y)
                (true-listp z))
           (equal (equal (append x y) 
                         (append x z))
                  (equal y z))))

(defthm append-associative
  (implies (and (true-listp x)
                (true-listp y)
                (true-listp z))
           (equal (append (append x y) z)
                  (append x (append y z)))))

(defthm append-of-nil-two
  (implies (true-listp x)
           (equal (append x nil)
                  x)))

(defthm acl2-count-of-append-increases 
  (implies (and (true-listp x)
                (true-listp y)
                (consp y))
           (< (acl2::acl2-count x) 
              (acl2::acl2-count (append y x))))
  :hints (("Goal" :in-theory (acl2::enable acl2::append))))

(acl2::in-theory (acl2::enable true-listp))
(defthm append-equal-self-one
  (implies (and (true-listp x)
                (true-listp y))
           (equal (equal x (append x y))
                  (equal y nil))))


; harshrc commented the following failing defthm out on [2015-02-01 Sun] for ACL2s 1.1.7.0 release.
;; (defthm append-equal-self-two
;;   (implies (and (true-listp x)
;;                 (true-listp y))
;;            (equal (equal y (append x y))
;;                   (equal x nil)))
;;   :hints(("Goal" 
;;           :cases ((consp y)))
;;          ("Subgoal 1" :use (:instance len-append)
;;           :in-theory (acl2::disable len-append))))
  
#!ACL2S
(progn
(defconst *testing-upper-bound* 1000)  

(defun nth-symbol-t-builtin (n)
;  (declare (xargs :guard (natp n)))
;:verify-guards nil)) 
  (intern$ (nth-var-string n) "ACL2S T"))

;; (defttag t)
;; (defattach (nth-symbol nth-symbol-t-builtin) :skip-checks t)
;; (defttag nil)

(defdata-attach symbol :enumerator nth-symbol-t-builtin)

(defun nth-character-t-builtin (n)
  (declare (xargs :guard (natp n)))
  (nth (mod n *len-alpha-num-chars*) *alpha-num-chars*))

;(defattach nth-character nth-character-t-builtin)
(defdata-attach character :enumerator nth-character-t-builtin)

(defun nth-acl2-number-t-builtin (n)
  (declare (xargs :guard (natp n)))
  (b* (((mv choice seed)
        (defdata::switch-nat 3 n)))
    (case choice
          (0 (nth-nat-builtin seed))
          (1 (nth-integer-builtin seed))
          (t (nth-rational-builtin seed)))))

;(defattach nth-acl2-number nth-acl2-number-t-builtin)
(defdata-attach acl2-number :enumerator nth-acl2-number-t-builtin)

(defun nth-small-pos-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-pos-builtin n-small)))

(defun nth-small-integer-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-integer-builtin n-small)))

(defun nth-small-nat-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-nat-builtin n-small)))

(defun nth-small-neg-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-neg-builtin n-small)))

(defun nth-small-positive-ratio-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-positive-ratio-builtin n-small)))

(defun nth-small-negative-ratio-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-negative-ratio-builtin n-small)))

(defun nth-small-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-rational-builtin n-small)))

(defun nth-small-positive-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-positive-rational-builtin n-small)))

(defun nth-small-negative-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-negative-rational-builtin n-small)))

(defun nth-small-acl2-number-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-acl2-number-t-builtin n-small)))

(defun nth-small-all (n)
  (declare (xargs ;:guard (natp n) ))
            :mode :program
            :verify-guards nil))
  (mv-let (choice seed) 
          (defdata::weighted-switch-nat 
            '(1 ;nil
              1 ;t
              1 ;0
              1 ;nat

              1 ;sym
              1 ;string
              1 ;integer
              1 ;char
              1 ;rational
              1 ;acl2-number
              1 ;atom

              2 ;boolean-list
              5 ;list-aa
              5 ;list-la-la 

              5 ;int-list
              5 ;sym-list
              5 ;stringlist
              5 ;charlist
              5 ;acl2-num-list

              10 ;atom-list
              ) n)
          
          (case choice
            (0 'nil)
            (1 't)
            (2  0)
            (3  (nth-small-nat-testing seed))

            (4  (nth-symbol seed))
            (5  (nth-string seed))
            (6  (nth-small-integer-testing seed))
            (7  (nth-character seed))
            (8  (nth-small-rational-testing seed))
            (9  (nth-small-acl2-number-testing seed))
            (10  (nth-atom seed))

            (11 (nth-boolean-list seed))
            (12 (b* (((list i1 i2) (defdata::split-nat 2 seed))) (list (nth-atom i1) (nth-atom i2)))) ;(nth-list-aa seed))
            (13 (b* (((list i1 i2 i3 i4) (defdata::split-nat 4 seed))) 
                  (list (list (nth-atom i1) (nth-atom i2)) 
                        (list (nth-atom i3) (nth-atom i4))))) ;(list-la-la seed))

            (14 (nth-integer-list seed))
            (15 (nth-symbol-list seed))
            (16 (nth-string-list seed))
            (17 (nth-character-list seed))
            (18 (nth-acl2-number-list seed))

            (19 (nth-atom-list seed))

            (t 'nil)))) ;this case should not come up

(defdata-attach pos :enumerator nth-small-pos-testing)
(defdata-attach integer :enumerator nth-small-integer-testing)
(defdata-attach nat :enumerator nth-small-nat-testing)
(defdata-attach neg :enumerator nth-small-neg-testing)
(defdata-attach positive-ratio :enumerator nth-small-positive-ratio-testing)
(defdata-attach negative-ratio :enumerator nth-small-negative-ratio-testing)
(defdata-attach rational :enumerator nth-small-rational-testing)
(defdata-attach positive-rational :enumerator nth-small-positive-rational-testing)
(defdata-attach negative-rational :enumerator nth-small-negative-rational-testing)
(defdata-attach acl2-number :enumerator nth-small-acl2-number-testing)
(defdata-attach all :enumerator nth-small-all)

(acl2s-defaults :set num-trials 50)
)
