#|$ACL2s-Preamble$;
(include-book ;; Newline to fool ACL2/cert.pl dependency scanner
 "portcullis")
(begin-book t :ttags :all);$ACL2s-Preamble$|#


(acl2::in-package "ACL2S BB")

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

(defunc rest (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::cdr x))

;(acl2s::sig acl2::cdr ((acl2s::listof :a)) acl2s::=> (acl2s::listof :a))

(acl2s::sig rest ((acl2s::listof :a)) acl2s::=> (acl2s::listof :a)
           :satisfies (acl2::consp acl2s::x1))

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
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::car))
               (cons 'first (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::cdr))
               (cons 'rest (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::true-listp))
               (cons 'listp (acl2::cdr term)))
              
              ;; Due to a call to translate in get-free-vars in testing
              ;; code, the following functions/macros that have not been
              ;; defined in bare-bones should not be
              ;; pre-processed. --harshrc Jan 27 2012
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::implies))
              ;;  (cons 'implies (acl2::cdr term)))
              
              ;; ((acl2::and (consp term)
              ;;             (consp (acl2::cdr term))
              ;;             (equal (acl2::car term) 'acl2::not)
              ;;             (equal (acl2::caadr term) 'acl2::>))
              ;;  (cons '<= (acl2::cdadr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::not))
              ;;  (cons 'not (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::*))
              ;;  (cons '* (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::+))
              ;;  (cons '+ (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::/))
              ;;  (cons '/ (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::-))
              ;;  (cons '- (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::<))
              ;;  (cons '< (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::>))
              ;;  (cons '> (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::acl2-number))
              ;;  (cons 'acl2-number (acl2::cdr term)))
              (t nil)))

; A hack to help proofs go through in this mode.
(acl2::in-theory (acl2::enable rest))

(acl2::defmacro listp (a)
  `(acl2::true-listp ,a))

;(acl2::verify-guards acl2::nth-true-list)

; harshrc 29 March 2012 -- added nth-list for Pete
;(acl2::defdata |ACL2S BB|::list acl2::true-list)
;(defun listp (x) (acl2::declare (acl2::xargs :guard T)) (acl2::true-listp x))

;(defun nth-list (n) 
;  (acl2::declare (acl2::xargs :mode :program
;                              :guard (acl2::natp n))) 
;  (acl2::nth-true-list n))

;(acl2::register-custom-type list t nth-list listp)

;(COMMON-LISP::DEFMACRO |ACL2S BB|::LIST (COMMON-LISP::&REST ACL2::ARGS)
;                       (ACL2::LIST-MACRO ACL2::ARGS))

(acl2::table acl2::user-defined-functions-table
             'acl2::untranslate-preprocess
             'my-preprocess)

(acl2s::defdata-attach list :enumerator acl2s::nth-true-list :override-ok t)


#!ACL2S
(progn

(defconst *testing-upper-bound* 77)  

(defun nth-symbol-bb-builtin (n)
;  (declare (xargs :guard (natp n)))
;:verify-guards nil)) 
  (intern$ (nth-var-string n) "ACL2S BB"))

;; (defttag t)
;; (defattach (nth-symbol nth-symbol-bb-builtin) :skip-checks t)
;; (defttag nil)

(defdata-attach symbol :enumerator nth-symbol-bb-builtin)

(defun nth-character-bb-builtin (n)
  (declare (xargs :guard (natp n)))
  (nth (mod n *len-alpha-num-chars*) *alpha-num-chars*))

;(defattach nth-character nth-character-bb-builtin)
(defdata-attach character :enumerator nth-character-bb-builtin)

(defun nth-acl2-number-bb-builtin (n)
  (declare (xargs :guard (natp n)))
  (b* (((mv choice seed)
        (defdata::switch-nat 3 n)))
    (case choice
          (0 (nth-nat-builtin seed))
          (1 (nth-integer-builtin seed))
          (t (nth-rational-builtin seed)))))

;(defattach nth-acl2-number nth-acl2-number-bb-builtin)
(defdata-attach acl2-number :enumerator nth-acl2-number-bb-builtin)

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
    (nth-acl2-number-bb-builtin n-small)))

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
            (13 (b* (((list i1 i2 i3 i4) (defdata::split-nat 4 seed))) ;list-la-la 
                  (list (list (nth-atom i1) (nth-atom i2)) 
                        (list (nth-atom i3) (nth-atom i4)))))

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

(acl2s-defaults :set num-trials 30)
)
