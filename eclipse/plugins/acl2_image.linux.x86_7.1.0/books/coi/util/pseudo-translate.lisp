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

(in-package "ACL2")

; Pseudo-translate is like translate, except that you give it the
; function/formals pairs that ACL2 doesn't already know about.  This is really
; useful when you want to do macro expansion but don't really care (yet) about
; the underlying functions.

; Modified 11/17/10 by Matt Kaufmann and Dave Greve to reflect changes in the
; definitions of translate11 etc.

(set-state-ok t)

(program)

(defun extend-wrld-with-fn-args-list (fn-args-lst wrld)
  (cond ((endp fn-args-lst) wrld)
        (t (let ((fn (caar fn-args-lst))
                 (formals (cdar fn-args-lst)))
             (putprop
              fn 'symbol-class :COMMON-LISP-COMPLIANT
              (putprop
               fn 'stobjs-out '(nil)
               (putprop
                fn 'stobjs-in (make-list (length formals))
                (putprop
                 fn 'formals formals
                 (extend-wrld-with-fn-args-list (cdr fn-args-lst) wrld)))))))))

(defun translate1-cw (x stobjs-out bindings known-stobjs ctx w)
  (mv-let (erp msg-or-val bindings)
          (translate1-cmp x stobjs-out bindings known-stobjs ctx w
                          (default-state-vars nil))
          (cond (erp ; erp is a ctx and val is a msg
                 (prog2$ (cw "~%~%ERROR in translate1-cw:  ~@0~%~%"
                             msg-or-val)
                         (mv t x bindings)))
                (t (mv nil msg-or-val bindings)))))

(defun pseudo-translate (form fn-args-lst wrld)
  (let
      ((wrld (extend-wrld-with-fn-args-list fn-args-lst wrld)))
    (mv-let
     (flg val bindings)
     (translate1-cw form t
                    '((:stobjs-out . :stobjs-out))
                    t 'pseudo-translate
                    wrld)
     (declare (ignore bindings))
     (mv flg val))))
