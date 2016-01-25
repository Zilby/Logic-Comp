; Computational Object Inference
; Copyright (C) 2005-2014 Kookamara LLC
;
; Contact:
;
;   Kookamara LLC
;   11410 Windermere Meadows
;   Austin, TX 78759, USA
;   http://www.kookamara.com/
;
; License: (An MIT/X11-style license)
;
;   Permission is hereby granted, free of charge, to any person obtaining a
;   copy of this software and associated documentation files (the "Software"),
;   to deal in the Software without restriction, including without limitation
;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;   and/or sell copies of the Software, and to permit persons to whom the
;   Software is furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in
;   all copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;   DEALINGS IN THE SOFTWARE.


(in-package "BAG")

(include-book "two-level")

;meta rules about bags of bags (the fucntions defined in two-level.lisp)
;eventually, this books should have its own evaluator.

;;
;; any-subbagp meta rule ..
;;

#|
(syn::defevaluator syntax-ev2 syntax-ev2-list
  (
   (hide x)
   (hide-unique list)
   (perm x y)
   (unique list)
   (implies-if a term)
   (if a b c)
   (consp x)
   (true-listp x)
   (binary-append x y)
   (cons a b)
   (meta-subbagp list1 list2)
   (remove-bag x list)
   (meta-remove-bag x list)
   (remove-1 x list)
   (meta-remove-1 x list)
   (unique-subbagps x y list)
   (subbagp-pair x1 x2 list1 list2)
   (meta-memberp x list)
   (any-subbagp x list)
   ))
|#

#|
(encapsulate
 ()

 (local
  (defthm syntax-subbagp-implements-subbagp-syntax-ev2-nil
    (implies
     (and
      (v0 (syntax-remove-bag list1 list2 flg))
      (equal (syntax-ev2 (v2 (syntax-remove-bag list1 list2 flg)) a)
	     nil)
      )
     (subbagp (syntax-ev2 list2 a) (syntax-ev2 list1 a))))
  )

(DEFTHM SYNTAX-SUBBAGP-IMPLEMENTS-SUBBAGP-2
  (IMPLIES (AND (V0 (SYNTAX-REMOVE-BAG LIST1 LIST2 FLG))
                (EQUAL (V2 (SYNTAX-REMOVE-BAG LIST1 LIST2 FLG))
                       ''NIL))
           (SUBBAGP (SYNTAX-EV2 LIST2 A)
                   (SYNTAX-EV2 LIST1 A))))
 )
|#


(defun syntax-deconsp (list)
  (declare (type t list))
  (if (and (consp list)
	   (equal (car list) 'cons)
	   (consp (cdr list))
	   (consp (cddr list))
	   (null  (cdddr list)))
      (mv t (cadr list) (caddr list))
    (mv nil nil nil)))

(defignored syntax-subbagp-list a (x list)
  (declare (type t x list)
           (xargs :guard (and (pseudo-termp x)
                              (pseudo-termp list)
                              )
                  )
           )
  (met ((consp entry rst) (syntax-deconsp list))
       (if consp
	   (if (syntax-subbagp x entry) t
	     (syntax-subbagp-list x rst))
	 nil)))

(defirrelevant syntax-subbagp-list 1 a (x list)
  :hints (("goal" :in-theory (enable
			      syntax-subbagp-list
			      syntax-subbagp-irrelevant
			      ))))

(defun syntax-subbagp-list-wrapper (term)
  (declare (type t term)
           (xargs :guard (pseudo-termp term)
                  ))
  (if (and (consp term)
	   (equal (car term) 'any-subbagp)
	   (consp (cdr term))
	   (consp (cddr term))
	   (null  (cdddr term))
	   (syntax-subbagp-list-fn nil (cadr term) (caddr term)))
      '(quote t)
    term))

(defthm syntax-subbagp-implies-any-subbagp
  (implies (syntax-subbagp-list term1 term2)
	   (any-subbagp (syntax-ev term1 a) (syntax-ev term2 a)))
  :rule-classes (:rewrite :forward-chaining)
  :hints (("Goal" :in-theory (enable syntax-subbagp-list any-subbagp))))

(defthm *meta*-subbagp-list
  (equal (syntax-ev term a)
         (syntax-ev (syntax-subbagp-list-wrapper term) a))
  :hints (("goal" :in-theory (enable syntax-subbagp-list-irrelevant)))
  :rule-classes ((:meta :trigger-fns (any-subbagp))))