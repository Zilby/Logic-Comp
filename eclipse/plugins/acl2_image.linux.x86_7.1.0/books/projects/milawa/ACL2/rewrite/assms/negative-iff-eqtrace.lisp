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
(set-verify-guards-eagerness 2)
(set-case-split-limitations nil)
(set-well-founded-relation ord<)
(set-measure-function rank)



(definlined rw.negative-iff-eqtrace (okp nhyp)
  ;; Generate a negative-iff eqtrace from an nhyp.
  (declare (xargs :guard (logic.termp nhyp)))
  (and okp
       (clause.negative-termp nhyp)
       (let ((guts (clause.negative-term-guts nhyp)))
         (if (and (logic.constantp guts)
                  (not (equal guts ''nil)))
             ;; The nhyp is (not const) and const is non-nil
             ;; Effectively the nhyp is (not t).  So we are trying to
             ;; assume (not t) = nil.  That's useless.
             nil
           ;; Else the hyp is (not nil), which is an instant contradiction and
           ;; of course is interesting, or it's (not foo) for some non-constant
           ;; called foo, which we want to observe.
           (if (logic.term-< ''t guts)
               (rw.eqtrace 'negative-iff t ''t guts nil)
             (rw.eqtrace 'negative-iff t guts ''t nil))))))

(encapsulate
 ()
 (local (in-theory (e/d (rw.negative-iff-eqtrace)
                        (forcing-booleanp-of-rw.eqtrace->iffp
                         (:e rw.eqtrace)))))

 (defthm forcing-rw.eqtrace->method-of-rw.negative-iff-eqtrace
   (implies (force (rw.negative-iff-eqtrace okp nhyp))
            (equal (rw.eqtrace->method (rw.negative-iff-eqtrace okp nhyp))
                   'negative-iff)))

 (defthm forcing-rw.eqtrace->iffp-of-rw.negative-iff-eqtrace
   (implies (force (rw.negative-iff-eqtrace okp nhyp))
            (equal (rw.eqtrace->iffp (rw.negative-iff-eqtrace okp nhyp))
                   t)))

 (defthm forcing-rw.eqtrace->subtraces-of-rw.negative-iff-eqtrace
   (implies (force (rw.negative-iff-eqtrace okp nhyp))
            (equal (rw.eqtrace->subtraces (rw.negative-iff-eqtrace okp nhyp))
                   nil)))

 (defthm forcing-rw.eqtracep-of-rw.negative-iff-eqtrace
   (implies (force (and (rw.negative-iff-eqtrace okp nhyp)
                        (logic.termp nhyp)))
            (equal (rw.eqtracep (rw.negative-iff-eqtrace okp nhyp))
                   t)))

 (defthm forcing-rw.eqtrace-atblp-of-rw.negative-iff-eqtrace
   (implies (force (and (rw.negative-iff-eqtrace okp nhyp)
                        (logic.termp nhyp)
                        (logic.term-atblp nhyp atbl)))
            (equal (rw.eqtrace-atblp (rw.negative-iff-eqtrace okp nhyp) atbl)
                   t)))

 (defthm rw.negative-iff-eqtrace-normalize-okp-1
   (implies (and (rw.negative-iff-eqtrace okp nhyp)
                 (syntaxp (not (equal okp ''t))))
            (equal (rw.negative-iff-eqtrace okp nhyp)
                   (rw.negative-iff-eqtrace t nhyp))))

 (defthm rw.negative-iff-eqtrace-normalize-okp-2
   (implies (not (rw.negative-iff-eqtrace t nhyp))
            (equal (rw.negative-iff-eqtrace okp nhyp)
                   nil))
   :hints(("Goal" :in-theory (disable (:e ACL2::force)))))

 (defthm rw.negative-iff-eqtrace-normalize-okp-3
   (equal (rw.negative-iff-eqtrace nil nhyp)
          nil)))




(defund rw.find-nhyp-for-negative-iff-eqtracep (nhyps x)
  ;; Find the first nhyp in a list that would generate this negative-iff eqtrace.
  (declare (xargs :guard (and (logic.term-listp nhyps)
                              (rw.eqtracep x))))
  (if (consp nhyps)
      (if (equal (rw.negative-iff-eqtrace t (car nhyps)) x)
          (car nhyps)
        (rw.find-nhyp-for-negative-iff-eqtracep (cdr nhyps) x))
    nil))

(encapsulate
 ()
 (local (in-theory (enable rw.find-nhyp-for-negative-iff-eqtracep)))

 (defthm rw.find-nhyp-for-negative-iff-eqtracep-of-nil
   (equal (rw.find-nhyp-for-negative-iff-eqtracep nil x)
          nil))

 (defthm forcing-logic.termp-of-rw.find-nhyp-for-negative-iff-eqtracep
   (implies (force (and (rw.find-nhyp-for-negative-iff-eqtracep nhyps x)
                        (logic.term-listp nhyps)))
            (equal (logic.termp (rw.find-nhyp-for-negative-iff-eqtracep nhyps x))
                   t)))

 (defthm forcing-logic.term-atblp-of-rw.find-nhyp-for-negative-iff-eqtracep
   (implies (force (and (rw.find-nhyp-for-negative-iff-eqtracep nhyps x)
                        (logic.term-list-atblp nhyps atbl)))
            (equal (logic.term-atblp (rw.find-nhyp-for-negative-iff-eqtracep nhyps x) atbl)
                   t)))

 (defthm forcing-memberp-of-rw.find-nhyp-for-negative-iff-eqtracep
   (implies (force (rw.find-nhyp-for-negative-iff-eqtracep nhyps x))
            (equal (memberp (rw.find-nhyp-for-negative-iff-eqtracep nhyps x) nhyps)
                   t)))

 (defthm forcing-rw.negative-iff-eqtrace-of-rw.find-nhyp-for-negative-iff-eqtracep
   (implies (force (rw.find-nhyp-for-negative-iff-eqtracep nhyps x))
            (equal (rw.negative-iff-eqtrace t (rw.find-nhyp-for-negative-iff-eqtracep nhyps x))
                   x))))




(defund rw.negative-iff-eqtrace-okp (x box)
  ;; Check if any nhyp in the hypbox would generate this negative-iff eqtrace.
  (declare (xargs :guard (and (rw.eqtracep x)
                              (rw.hypboxp box))))
  (and (equal (rw.eqtrace->method x) 'negative-iff)
       (equal (rw.eqtrace->iffp x) t)
       (if (or (rw.find-nhyp-for-negative-iff-eqtracep (rw.hypbox->left box) x)
               (rw.find-nhyp-for-negative-iff-eqtracep (rw.hypbox->right box) x))
           t
         nil)))

(encapsulate
 ()
 (local (in-theory (enable rw.negative-iff-eqtrace-okp)))

 (defthm booleanp-of-rw.negative-iff-eqtrace-okp
   (equal (booleanp (rw.negative-iff-eqtrace-okp x box))
          t)
   :hints(("Goal" :in-theory (disable forcing-booleanp-of-rw.eqtrace->iffp))))

 (defthmd lemma-for-forcing-rw.negative-iff-eqtrace-okp-rw.negative-iff-eqtrace
   (implies (and (logic.termp nhyp)
                 (logic.term-listp nhyps)
                 (memberp nhyp nhyps)
                 (rw.negative-iff-eqtrace okp nhyp))
            (iff (rw.find-nhyp-for-negative-iff-eqtracep nhyps (rw.negative-iff-eqtrace okp nhyp))
                 t))
   :hints(("Goal"
           :in-theory (e/d (rw.find-nhyp-for-negative-iff-eqtracep)
                           ((:e rw.negative-iff-eqtrace)))
           :induct (cdr-induction nhyps))))

 (defthm forcing-rw.negative-iff-eqtrace-okp-rw.negative-iff-eqtrace
   (implies (force (and (rw.negative-iff-eqtrace okp nhyp)
                        (rw.hypboxp box)
                        (or (memberp nhyp (rw.hypbox->left box))
                            (memberp nhyp (rw.hypbox->right box)))))
            (equal (rw.negative-iff-eqtrace-okp (rw.negative-iff-eqtrace okp nhyp) box)
                   t))
   :hints(("Goal" :in-theory (e/d (lemma-for-forcing-rw.negative-iff-eqtrace-okp-rw.negative-iff-eqtrace)
                                  (rw.negative-iff-eqtrace-normalize-okp-1))))))



