#|

Here is how to test ACL2s in emacs.

(add-include-book-dir :acl2s-modes "/Users/pete/projects/acl2s/trunk/acl2s-modes") 
(ld "acl2s-mode.lsp" :dir :acl2s-modes)

; To load a different mode do something like
; (ld "acl2s-bare-bones.lsp" :dir :acl2s-modes)

(reset-prehistory t)

|#
