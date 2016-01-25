#|$ACL2s-Preamble$;
(ld "acl2s-pkg.lsp")

(begin-book t :ttags ((:acl2s-super-history)
                      ;(:acl2s-contentassist)
                      (:redef+)
                      (:acl2s-protection)
                      (:acl2s-interaction)
                      (:acl2s-markup)
                      (defcode)));$ACL2s-Preamble$|#


(in-package #+acl2s "ACL2S-DEV" #-acl2s "ACL2S-HOOKS")

(defconst *acl2s-hooks-version* "1.1.8.1")

; for inlining most of it
(include-book "make-event/inline-book" :dir :system)
(include-book "hacking/hacker" :dir :system)
(include-book "hacking/defcode" :dir :system :ttags ((defcode)))
(include-book "hacking/redefun" :dir :system)
(include-book "hacking/rewrite-code" :dir :system)
(include-book "hacking/raw" :dir :system)
;(include-book "contentassist" :load-compiled-file :comp)

; do not inline this one; we reuse it
(include-book "canonical-print" :load-compiled-file :comp)

(inline-book "acl2s-book-support" :load-compiled-file :comp)

(inline-book "categorize-input" :load-compiled-file :comp)

(inline-book "super-history"
             :load-compiled-file :comp
             :ttags ((:acl2s-super-history) (defcode)))

(inline-book "protection-hooks"
             :load-compiled-file :comp
             :ttags ((:acl2s-protection) (:acl2s-super-history)
                     (defcode)))

(inline-book "interaction-hooks"
             :load-compiled-file :comp
             :ttags ((:acl2s-protection)
                     (:acl2s-super-history)
                     (:acl2s-interaction)
                     (defcode)))

(inline-book "markup-hooks"
             :load-compiled-file :comp
             :ttags ((:acl2s-markup) (defcode)))


(program)
(set-state-ok t)

(defun acl2s-begin (secret state)
  (declare (ignorable secret))
  (er-progn
   (acl2s-interaction-begin state)
   (acl2s-markup-begin state)
   (acl2s-protection-begin secret state)
   ))


; Suppress printing of TTAG Notes in ACL2s session modes
(acl2::redef+)
(defun acl2::print-ttag-note (val active-book-name include-bookp deferred-p state)
  (declare (xargs :stobjs state)
           (ignore val active-book-name include-bookp deferred-p))
  state)
(acl2::redef-)
