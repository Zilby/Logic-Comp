; Arithmetic-3 Library
; Copyright (C) 2004 Robert Krug <rkrug@cs.utexas.edu>
;
; This program is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free Software
; Foundation; either version 2 of the License, or (at your option) any later
; version.
;
; This program is distributed in the hope that it will be useful but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
; details.
;
; You should have received a copy of the GNU General Public License along with
; this program; if not, write to the Free Software Foundation, Inc., 51
; Franklin Street, Suite 500, Boston, MA 02110-1335, USA.

;;
;; numerator-and-denominator.lisp
;;

(in-package "ACL2")


(local (include-book "basic-arithmetic"))
(local (include-book "inequalities"))
(local (include-book "prefer-times"))
(local (include-book "non-linear"))
(local (include-book "num-and-denom-helper"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facts about numerator and denominator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defthm numerator-minus
   (equal (numerator (- i))
          (- (numerator i)))
  :hints (("Goal"
	   :cases ((rationalp i)))
	  ("Subgoal 1"
	   :use (:instance
		 Unique-rationalp
		 (d (denominator i))
		 (n (- (numerator i)))))))

(defthm denominator-minus
  (implies (rationalp x)
           (equal (denominator (- x))
                  (denominator x)))
 :hints (("Goal"
	  :use (:instance
		Unique-rationalp
		(d (denominator x))
		(n (- (numerator x)))))))

(encapsulate
 ()

 (local
  (defthm numerator-integerp-lemma-1
    (implies (rationalp x)
             (equal (* (* (numerator x) (/ (denominator x))) (denominator x))
                    (numerator x)))
    :rule-classes nil
    :hints (("Goal" :in-theory (disable rational-implies2)))))

 (local
  (defthm numerator-integerp-lemma
    (implies (and (rationalp x)
                  (equal (* (numerator x) (/ (denominator x)))
                         x))
             (equal (numerator x)
                    (* x (denominator x))))
    :rule-classes nil
    :hints (("Goal" :use (numerator-integerp-lemma-1)
             :in-theory (disable rational-implies2))
	    ("Goal'" :in-theory (e/d (prefer-*-to-/) (rational-implies2))))))

 (defthm numerator-when-integerp
   (implies (integerp x)
            (equal (numerator x)
                   x))
   :hints (("Goal" :in-theory (disable rational-implies2)
            :use ((:instance lowest-terms (r x)
                             (q 1)
                             (n (denominator x)))
                  rational-implies2
                  numerator-integerp-lemma))))
 )

(defthm integerp==>denominator=1
  (implies (integerp x)
           (equal (denominator x) 1))
  :hints
  (("Goal"
    :use (rational-implies2 numerator-when-integerp)
    :in-theory (disable rational-implies2))))

(defthm equal-denominator-1
  (equal (equal (denominator x) 1)
         (or (integerp x)
             (not (rationalp x))))
  :hints (("Goal" :use (rational-implies2 completion-of-denominator)
           :in-theory (disable rational-implies2))))

;; Note that if I enable prefer-*-to-/ right
;; away the following fails.

(defthm *-r-denominator-r
  (equal (* r (denominator r))
         (if (rationalp r)
             (numerator r)
           (fix r)))
  :hints (("Goal" :use ((:instance rational-implies2 (x r)))
           :in-theory (disable rational-implies2))
	  ("Goal''" :in-theory (enable prefer-*-to-/))))
