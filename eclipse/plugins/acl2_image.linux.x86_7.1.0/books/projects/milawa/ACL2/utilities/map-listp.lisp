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
(include-book "tuple-listp")
(set-verify-guards-eagerness 2)
(set-case-split-limitations nil)
(set-well-founded-relation ord<)
(set-measure-function rank)

(deflist map-listp (x)
  (mapp x)
  :elementp-of-nil t)

(deflist submap-of-eachp (map x)
  (submapp map x)
  :guard (and (mapp map)
              (map-listp x)))

(encapsulate
 ()
 (local (defthm lemma
          (implies (true-listp y)
                   (equal (revappend x y)
                          (app (rev x) y)))))

 (local (in-theory (disable forcing-revappend-removal)))

 (defprojection :list (revappend-onto-each list x)
                :element (revappend list x)
                :guard (true-list-listp x)))

(defthm forcing-submap-of-eachp-of-revappend-onto-each
  (implies (force (and (uniquep (domain a))
                       (true-list-listp x)))
           (equal (submap-of-eachp a (revappend-onto-each a x))
                  t))
  :hints(("Goal" :induct (cdr-induction x))))

