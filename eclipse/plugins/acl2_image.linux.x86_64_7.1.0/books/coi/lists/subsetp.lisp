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

(in-package "LIST")

(include-book "../adviser/adviser")
(include-book "memberp")

;; Here is a version of subset with minimal guards.

(defun subsetx (x y)
  (declare (type t x y))
  (if (not (consp x)) t
    (and (memberp (car x) y)
	 (subsetx (cdr x) y))))

(defthm subsetp-reduction
  (equal (subsetx x y)
	 (subsetp x y)))

(defthm subsetp-cons-1
  (equal (subsetp (cons a x) y)
	 (and (memberp a y)
	      (subsetp x y))))

(defthm subsetp-append-1
  (equal (subsetp (append x y) z)
	 (and (subsetp x z)
	      (subsetp y z))))

(defthm subset-membership-free-subsetp
  (implies
   (and
    (subsetp x y)
    (memberp a x))
   (memberp a y))
  :rule-classes (:rewrite :forward-chaining))

(defthm subset-not-memberp-forward
  (implies
   (and
    (subsetp x y)
    (not (memberp a y)))
   (not (memberp a x)))
  :rule-classes (:forward-chaining))

(defthm subset-membership-free-memberp
  (implies
   (and
    (memberp a x)
    (subsetp x y))
   (memberp a y))
  :rule-classes (:rewrite :forward-chaining))


(defthm subset-chaining-1
  (implies
   (and
    (subsetp x y)
    (subsetp y z))
   (subsetp x z))
  :rule-classes (:rewrite :forward-chaining))

(defthm subset-chaining-2
  (implies
   (and
    (subsetp x y)
    (subsetp z x))
   (subsetp z y))
  :rule-classes (:rewrite :forward-chaining))

(defthm subsetp-not-consp-1
  (implies
   (not (consp x))
   (subsetp x y)))

(defthm subsetp-not-consp-2
  (implies
   (not (consp y))
   (equal (subsetp x y)
	  (not (consp x)))))

(defcong equiv equal (subsetp x y) 2)
(defcong equiv equal (subsetp x y) 1
  :hints (("Goal" :induct (len-len-induction x-equiv x))))

(encapsulate
 ()

 (encapsulate
  (((set-hyps) => *)
   ((subset) => *)
   ((superset) => *))

  (local (defun set-hyps () nil))
  (local (defun subset () nil))
  (local (defun superset () nil))

  (defthm multiplicity-constraint
    (implies
     (and
      (set-hyps)
      (memberp arbitrary-element (subset)))
     (memberp arbitrary-element (superset)))
    :rule-classes nil)
  )

 (local (defun badguy (x y)
          (cond ((atom x)
                 nil)
                ((not (memberp (car x) y))
                 (car x))
                (t (badguy (cdr x) y)))))

 (local (defthm badguy-witness
          (implies (not (subsetp x y))
                   (not (memberp (badguy x y) y)))))

 (local (defthm badguy-not-member
	  (implies
	   (not (memberp (badguy x y) x))
	   (subsetp x y))))

 (defthm subset-by-multiplicity-driver
   (implies (set-hyps)
            (subsetp (subset) (superset)))
   :rule-classes nil
   :hints(("Goal"
           :use ((:instance
                  multiplicity-constraint
                  (arbitrary-element (badguy (subset) (superset))))))))

 (ADVISER::defadvice subset-by-multiplicity
   (implies (set-hyps)
; [Changed by Matt K. to handle changes to member, assoc, etc. after ACL2 4.2
;  (replaced subsetp by subsetp-equal).]
            (subsetp-equal (subset) (superset)))
   :rule-classes (:pick-a-point :driver subset-by-multiplicity-driver))

 )

(defthm subsetp-id
  (subsetp x x))

(defthm equal-subsetp-reduction-1
  (equal (equal (subsetp x y) z)
	 (and
	  (booleanp z)
	  (implies
	   (subsetp x y)
	   z)
	  (implies
	   z
	   (subsetp x y)))))

(defthm equal-subsetp-reduction-2
  (equal (equal z (subsetp x y))
	 (and
	  (booleanp z)
	  (implies
	   (subsetp x y)
	   z)
	  (implies
	   z
	   (subsetp x y)))))