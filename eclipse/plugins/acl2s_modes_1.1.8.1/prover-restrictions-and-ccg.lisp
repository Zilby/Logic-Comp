#|$ACL2s-Preamble$;
(acl2::begin-book t :ttags ((:ccg) (:prover-restrictions)));$ACL2s-Preamble$|#


; this is necessary because of a weakness in hacking/table-guard book--which
; cannot combine extensions to table guards that were not combined at
; certification time.

(in-package "ACL2")

(include-book "ccg/ccg" :ttags :all :load-compiled-file nil)

; presently, table-guard code doesn't stack post facto, so to combine with ccg,
; we inline this one here.
(include-book "make-event/inline-book" :dir :system)
(inline-book "prover-restrictions" :ttags :all)
