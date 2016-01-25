; Milawa - A Reflective Theorem Prover
; Copyright (C) 2005-2009 Kookamara LLC
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
;
; Original author: Jared Davis <jared@kookamara.com>

(in-package "MILAWA")
(include-book "eqtracep")
(include-book "hypbox-arities")
(set-verify-guards-eagerness 2)
(set-case-split-limitations nil)
(set-well-founded-relation ord<)
(set-measure-function rank)



(defund rw.slow-flag-eqtrace-arities (flag x)
  (declare (xargs :guard (if (equal flag 'trace)
                             (rw.eqtracep x)
                           (rw.eqtrace-listp x))
                  :measure (two-nats-measure (rank x) (if (equal flag 'trace) 1 0))))
  (if (equal flag 'trace)
      (app (rw.slow-flag-eqtrace-arities 'list (rw.eqtrace->subtraces x))
           (app (logic.slow-term-arities (rw.eqtrace->rhs x))
                (logic.slow-term-arities (rw.eqtrace->lhs x))))
    (if (consp x)
        (app (rw.slow-flag-eqtrace-arities 'trace (car x))
             (rw.slow-flag-eqtrace-arities 'list (cdr x)))
      nil)))

(definlined rw.slow-eqtrace-arities (x)
  (declare (xargs :guard (rw.eqtracep x)))
  (rw.slow-flag-eqtrace-arities 'trace x))

(definlined rw.slow-eqtrace-list-arities (x)
  (declare (xargs :guard (rw.eqtrace-listp x)))
  (rw.slow-flag-eqtrace-arities 'list x))

(defthmd definition-of-rw.slow-eqtrace-arities
  (equal (rw.slow-eqtrace-arities x)
         (app (rw.slow-eqtrace-list-arities (rw.eqtrace->subtraces x))
              (app (logic.slow-term-arities (rw.eqtrace->rhs x))
                   (logic.slow-term-arities (rw.eqtrace->lhs x)))))
  :rule-classes :definition
  :hints(("Goal"
          :expand ((rw.slow-flag-eqtrace-arities 'trace x))
          :in-theory (enable rw.slow-eqtrace-list-arities rw.slow-eqtrace-arities))))

(defthmd definition-of-rw.slow-eqtrace-list-arities
  (equal (rw.slow-eqtrace-list-arities x)
         (if (consp x)
             (app (rw.slow-eqtrace-arities (car x))
                  (rw.slow-eqtrace-list-arities (cdr x)))
           nil))
  :rule-classes :definition
  :hints(("Goal"
          :expand ((rw.slow-flag-eqtrace-arities 'list x))
          :in-theory (enable rw.slow-eqtrace-list-arities rw.slow-eqtrace-arities))))

(ACL2::theory-invariant (not (ACL2::active-runep '(:definition rw.slow-eqtrace-arities))))
(ACL2::theory-invariant (not (ACL2::active-runep '(:definition rw.slow-eqtrace-arities-list))))

(defthm rw.slow-eqtrace-list-arities-when-not-consp
  (implies (not (consp x))
           (equal (rw.slow-eqtrace-list-arities x)
                  nil))
  :hints(("Goal" :in-theory (enable definition-of-rw.slow-eqtrace-list-arities))))

(defthm rw.slow-eqtrace-list-arities-of-cons
  (equal (rw.slow-eqtrace-list-arities (cons a x))
         (app (rw.slow-eqtrace-arities a)
              (rw.slow-eqtrace-list-arities x)))
  :hints(("Goal" :in-theory (enable definition-of-rw.slow-eqtrace-list-arities))))



(defund rw.flag-eqtrace-arities (flag x acc)
  (declare (xargs :guard (and (if (equal flag 'trace)
                                  (rw.eqtracep x)
                                (rw.eqtrace-listp x))
                              (true-listp acc))
                  :measure (two-nats-measure (rank x) (if (equal flag 'trace) 1 0))
                  :verify-guards nil))
  (if (equal flag 'trace)
      (let* ((acc (logic.term-arities (rw.eqtrace->lhs x) acc))
             (acc (logic.term-arities (rw.eqtrace->rhs x) acc)))
        (rw.flag-eqtrace-arities 'list (rw.eqtrace->subtraces x) acc))
    (if (consp x)
        (rw.flag-eqtrace-arities 'trace (car x)
                                 (rw.flag-eqtrace-arities 'list (cdr x) acc))
      acc)))

(definlined rw.eqtrace-arities (x acc)
  (declare (xargs :guard (and (rw.eqtracep x)
                              (true-listp acc))
                  :verify-guards nil))
  (rw.flag-eqtrace-arities 'trace x acc))

(definlined rw.eqtrace-list-arities (x acc)
  (declare (xargs :guard (and (rw.eqtrace-listp x)
                              (true-listp acc))
                  :verify-guards nil))
  (rw.flag-eqtrace-arities 'list x acc))

(defthmd definition-of-rw.eqtrace-arities
  (equal (rw.eqtrace-arities x acc)
         (let* ((acc (logic.term-arities (rw.eqtrace->lhs x) acc))
                (acc (logic.term-arities (rw.eqtrace->rhs x) acc)))
           (rw.eqtrace-list-arities (rw.eqtrace->subtraces x) acc)))
  :rule-classes :definition
  :hints(("Goal"
          :in-theory (e/d (rw.eqtrace-arities rw.eqtrace-list-arities)
                          ((:executable-counterpart acl2::force)))
          :expand (rw.flag-eqtrace-arities 'trace x acc))))

(defthmd definition-of-rw.eqtrace-list-arities
  (equal (rw.eqtrace-list-arities x acc)
         (if (consp x)
             (rw.eqtrace-arities (car x)
                                 (rw.eqtrace-list-arities (cdr x) acc))
           acc))
  :rule-classes :definition
  :hints(("Goal"
          :in-theory (e/d (rw.eqtrace-arities rw.eqtrace-list-arities)
                          ((:executable-counterpart acl2::force)))
          :expand (rw.flag-eqtrace-arities 'list x acc))))

(defthm rw.flag-eqtrace-arities-of-trace
  (equal (rw.flag-eqtrace-arities 'trace x acc)
         (rw.eqtrace-arities x acc))
  :hints(("Goal" :in-theory (enable rw.eqtrace-arities))))

(defthm rw.flag-eqtrace-arities-of-list
  (equal (rw.flag-eqtrace-arities 'list x acc)
         (rw.eqtrace-list-arities x acc))
  :hints(("Goal" :in-theory (enable rw.eqtrace-list-arities))))

(ACL2::theory-invariant (not (ACL2::active-runep '(:definition rw.eqtrace-arities))))
(ACL2::theory-invariant (not (ACL2::active-runep '(:definition rw.eqtrace-list-arities))))

(defthm rw.eqtrace-list-arities-when-not-consp
  (implies (not (consp x))
           (equal (rw.eqtrace-list-arities x acc)
                  acc))
  :hints(("Goal" :in-theory (enable definition-of-rw.eqtrace-list-arities))))

(defthm rw.eqtrace-list-arities-of-cons
  (equal (rw.eqtrace-list-arities (cons a x) acc)
         (rw.eqtrace-arities a
                             (rw.eqtrace-list-arities x acc)))
  :hints(("Goal" :in-theory (enable definition-of-rw.eqtrace-list-arities))))


(defthms-flag
  :shared-hyp (force (true-listp acc))
  :thms ((trace true-listp-of-rw.eqtrace-arities
               (equal (true-listp (rw.eqtrace-arities x acc))
                      t))
         (t true-listp-of-rw.eqtrace-list-arities
            (equal (true-listp (rw.eqtrace-list-arities x acc))
                   t)))
  :hints(("Goal"
          :induct (rw.flag-eqtrace-arities flag x acc)
          :in-theory (enable (:induction rw.flag-eqtrace-arities))
          :expand ((rw.eqtrace-arities x acc)))))

(verify-guards rw.flag-eqtrace-arities)
(verify-guards rw.eqtrace-arities)
(verify-guards rw.eqtrace-list-arities)


(defthms-flag
  :shared-hyp (force (true-listp acc))
  :thms ((trace rw.eqtrace-arities-removal
               (equal (rw.eqtrace-arities x acc)
                      (app (rw.slow-eqtrace-arities x)
                           acc)))
         (t rw.eqtrace-list-arities-removal
            (equal (rw.eqtrace-list-arities x acc)
                   (app (rw.slow-eqtrace-list-arities x)
                        acc))))
  :hints(("Goal"
          :induct (rw.flag-eqtrace-arities flag x acc)
          :in-theory (enable (:induction rw.flag-eqtrace-arities))
          :expand ((rw.eqtrace-arities x acc)
                   (rw.slow-eqtrace-arities x)))))

(defthms-flag
  :thms ((trace rw.slow-eqtrace-arities-correct
               (implies (force (rw.eqtracep x))
                        (equal (logic.arities-okp (rw.slow-eqtrace-arities x) atbl)
                               (rw.eqtrace-atblp x atbl))))
         (t rw.slow-eqtrace-list-arities-correct
            (implies (force (rw.eqtrace-listp x))
                     (equal (logic.arities-okp (rw.slow-eqtrace-list-arities x) atbl)
                            (rw.eqtrace-list-atblp x atbl)))))
  :hints(("Goal"
          :induct (rw.flag-eqtrace-atblp flag x atbl)
          :expand ((rw.eqtrace-atblp x atbl)
                   (rw.slow-eqtrace-arities x))
          :in-theory (e/d ((:induction rw.flag-eqtrace-atblp)
                           (:executable-counterpart acl2::force))))))


