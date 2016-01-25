; Correctness of Fact

; Problem: Define an M1 program to compute the factorial of its natural number
; input.  Prove your program correct.

; Design Plan: I will count n down to 0 by 1 and multiply each successive
; result into an accumulator, a, initially 1.

; (0) Start ACL2
; (include-book "m1")

(in-package "M1")

; (1) Write your specification, i.e., define the expected inputs and the
; desired output, theta.

(defun ok-inputs (n)
  (natp n))

(defun ! (n)
  (if (zp n)
      1
      (* n (! (- n 1)))))

(defun theta (n)
  (! n))

; (2) Write your algorithm.  This will consist of a tail-recursive helper
; function and a wrapper, fn.

(defun helper (n a)
  (if (zp n)
      a
      (helper (- n 1) (* n a))))

(defun fn (n) (helper n 1))

; (3) Prove that the algorithm satisfies the spec, by proving first that the
; helper is appropriately related to theta and then that fn is theta on ok
; inputs.

; Important Note: When you verify your helper function, you must consider the
; most general case.  For example, if helper is defined with formal parameters
; n and a and fn calls helper initializing a to 0, your helper theorem must
; be about (helper n a), not just about the special case (helper n 0).

(defthm helper-is-theta
  (implies (and (ok-inputs n)
                (natp a))
           (equal (helper n a)
                  (* a (theta n)))))

(defthm fn-is-theta
  (implies (ok-inputs n)
           (equal (fn n)
                  (theta n))))

; Disable these two lemmas because they confuse the theorem prover when it is
; dealing with the code versus fn.

(in-theory (disable helper-is-theta fn-is-theta))

; (4) Write your M1 program with the intention of implementing your algorithm.

(defconst *pi*
  '((iconst 1)  ;  0
    (istore 1)  ;  1
    (iload 0)   ;  2
    (ifeq 10)   ;  3
    (iload 1)   ;  4
    (iload 0)   ;  5
    (imul)      ;  6
    (istore 1)  ;  7
    (iload 0)   ;  8
    (iconst 1)  ;  9
    (isub)      ; 10
    (istore 0)  ; 11
    (goto -10)  ; 12
    (iload 1)   ; 13
    (halt))     ; 14
  )

; (5) Define the ACL2 function that clocks your program, starting with the
; loop clock and then using it to clock the whole program.  The clock
; should take the program from pc 0 to a HALT statement.  (Sometimes your
; clocks will require multiple inputs or other locals, but our example only
; requires the first local.)

(defun loop-clk (n)
  (if (zp n)
      3
      (clk+ 11
            (loop-clk (- n 1)))))

(defun clk (n)
  (clk+ 2
        (loop-clk n)))

; (6) Prove that the code implements your algorithm, starting with the lemma
; that the loop implements the helper.  Each time you prove a lemma relating
; code to algorithm, disable the corresponding clock function so the theorem
; prover doesn't look any deeper into subsequent code.

; Important Note: Your lemma about the loop must consider the general case.
; For example, if the loop uses the locals n and a, you must characterize
; its behavior for arbitrary, legal n and a, not just a special case (e.g.,
; where a is 0).

(defthm loop-is-helper
  (implies (and (ok-inputs n)
                (natp a))
           (equal (m1 (make-state 2
                                  (list n a)
                                  nil
                                  *pi*)
                      (loop-clk n))
                  (make-state 14
                              (list 0 (helper n a))
                              (push (helper n a) nil)
                              *pi*))))

(in-theory (disable loop-clk))

(defthm program-is-fn
  (implies (ok-inputs n)
           (equal (m1 (make-state 0
                                  (list n)
                                  nil
                                  *pi*)
                      (clk n))
                  (make-state 14
                              (list 0 (fn n))
                              (push (fn n) nil)
                              *pi*))))

(in-theory (disable clk))

; (7) Put the two steps together to get correctness.

(in-theory (enable fn-is-theta))

(defthm program-correct
  (implies (ok-inputs n)
           (equal (m1 (make-state 0
                                  (list n)
                                  nil
                                  *pi*)
                      (clk n))
                  (make-state 14
                              (list 0 (theta n))
                              (push (theta n)
                                    nil)
                              *pi*))))

; This corollary just shows we did what we set out to do:

(defthm total-correctness
  (implies (and (natp n)
                (equal sf (m1 (make-state 0
                                          (list n)
                                          nil
                                          *pi*)
                              (clk n))))
           (and (equal (next-inst sf) '(HALT))
                (equal (top (stack sf))
                       (! n))))
  :rule-classes nil)

; Think of the above theorem as saying: for all natural numbers n, there exists
; a clock (for example, the one constructed by (clk n)) such that running
; *pi* with (list n) as input produces a state, sf, that is halted and which
; contains (! n) on top of the stack.  Note that the algorithm used by *pi* is
; not specified or derivable from this formula.
