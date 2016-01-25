#|$ACL2s-Preamble$;
(acl2::begin-book t :ttags ((:prover-restrictions)));$ACL2s-Preamble$|#


(in-package "ACL2")


(defttag :prover-restrictions)
(include-book "hacking/hacker" :dir :system)
(progn+all-ttags-allowed
 (include-book "hacking/all" :dir :system :ttags ((defcode) (table-guard))))
(subsume-ttags-since-defttag)


(add-acl2-defaults-table-key :auto-induct-depth-limit
                             (or (null val)
                                 (natp val)))

(add-acl2-defaults-table-key :quiet-do-not
                             (subsetp-eq val *preprocess-clause-ledge*))

(defttag nil)

(program)
(set-state-ok t)

(defun get-auto-induct-depth-limit (state)
  (if (and (f-boundp-global 'supress-auto-induct-depth-limit state)
           (f-get-global 'supress-auto-induct-depth-limit state))
    nil
    (cdr (assoc :auto-induct-depth-limit (table-alist 'acl2-defaults-table (w state))))))

(defun get-quiet-do-not (state)
  (if (and (f-boundp-global 'supress-quiet-do-not state)
           (f-get-global 'supress-quiet-do-not state))
    nil
    (cdr (assoc :quiet-do-not (table-alist 'acl2-defaults-table (w state))))))

(defun induct-msg/depth-limit (pool-name induct-hint-val state)
  (pprogn
   (increment-timer 'prove-time state)
   (fms "No induction schemes ~#H~[are suggested by the induction ~
         hint~/were specified for ~@n~], and the depth limit for automatic ~
         inductions, currently ~xd, has been reached.  Consequently, the proof attempt ~
         has failed.~|"
     (list (cons #\H (cond (induct-hint-val 0)(t 1)))
           (cons #\n pool-name)
           (cons #\d (get-auto-induct-depth-limit state)))
     (proofs-co state)
     state
     (term-evisc-tuple nil state))
   (increment-timer 'print-time state)))


(defttag :prover-restrictions)

(progn+touchable :all
(redefun+rewrite
 induct
 (:simul
  (:carpat (select-x-cl-set %cl-set% %induct-hint-val%)
   :vars (%cl-set% %induct-hint-val%)
   :mult +
   :repl
   (select-x-cl-set (if (let ((val (get-auto-induct-depth-limit state)))
                          (or (null val)
                              (<= (len pool-lst) val)))
                      %cl-set%
                      nil)
                    %induct-hint-val%))
  (:carpat (io? %type% nil state
                (induct-hint-val pool-name)
                (induct-msg/lose pool-name induct-hint-val state))
   :vars (%type%)
   :mult +
   :repl
   (io? %type% nil state
        (induct-hint-val pool-name pool-lst)
        (if (let ((val (get-auto-induct-depth-limit state)))
              (or (null val)
                  (<= (len pool-lst) val)))
          (induct-msg/lose pool-name induct-hint-val state)
          (induct-msg/depth-limit pool-name induct-hint-val state))))))

(redefun+rewrite
 waterfall-step
 (:carpat %body%
  :vars (%body%)
  :repl
  (if (member-eq processor (get-quiet-do-not state))
    (mv@par step-limit 'miss nil hist nil nil state)
    %body%)))
)

(defttag nil)#|ACL2s-ToDo-Line|#




;***** THEOREM macro for J Moore's  Recursion & Induction mode *****

(defmacro theorem (&whole event-form name term &rest rst)
  (declare (xargs :guard t) (ignore term rst))
  `(with-output
    :stack :push
    :off :all
    (make-event
    (let ((curr-auto-induct-depth-limit (get-auto-induct-depth-limit state))
          (curr-quiet-do-not (get-quiet-do-not state))
          (event-form ',event-form))
      `(progn
        (set-auto-induct-depth-limit 0)
        (set-quiet-do-not (generalize-clause
                           eliminate-irrelevance-clause))
        (with-output :stack :pop :on :all (defthm ,@(cdr event-form)))
        (with-output :stack :pop :off summary
                     (in-theory (disable ,',name)))
        (set-auto-induct-depth-limit ,curr-auto-induct-depth-limit)
        (set-quiet-do-not ,curr-quiet-do-not)
        (with-output :stack :pop :on :all (value-triple '(:theorem ,',name))))))))
    
(defmacro acl2-user::theorem (&whole event-form name term &rest rst)
  (declare (xargs :guard t) (ignore term rst))
  `(with-output
    :stack :push
    :off :all
    (make-event
    (let ((curr-auto-induct-depth-limit (get-auto-induct-depth-limit state))
          (curr-quiet-do-not (get-quiet-do-not state))
          (event-form ',event-form))
      `(progn
        (set-auto-induct-depth-limit 0)
        (set-quiet-do-not (generalize-clause
                           eliminate-irrelevance-clause))
        (with-output :stack :pop :on :all (defthm ,@(cdr event-form)))
        (with-output :stack :pop :off summary
                     (in-theory (disable ,',name)))
        (set-auto-induct-depth-limit ,curr-auto-induct-depth-limit)
        (set-quiet-do-not ,curr-quiet-do-not)
        (with-output :stack :pop :on :all (value-triple '(:theorem ,',name))))))))

#|| tests:

(thm (implies (consp x)
              (equal (len (append (car x) (cdr x)))
                     (+ (len (car x)) (len (cdr x)))))
     :otf-flg t)

(set-quiet-do-not (eliminate-destructors-clause))

; now it fails

(thm (implies (consp x)
              (equal (len (append (car x) (cdr x)))
                     (+ (len (car x)) (len (cdr x)))))
     :otf-flg t)

;unless...
(state-global-let*
 ((supress-quiet-do-not t))
 (thm (implies (consp x)
               (equal (len (append (car x) (cdr x)))
                      (+ (len (car x)) (len (cdr x)))))
      :otf-flg t))


(logic)
(set-auto-induct-depth-limit 1)
(thm  
  (<= (acl2-count (+ a b))
      (acl2-count (append a b))))

(set-auto-induct-depth-limit 0)

(thm (<= (acl2-count (+ a b)) (acl2-count (append a b))))

(state-global-let* ((supress-auto-induct-depth-limit t))
                   (thm (<= (acl2-count (+ a b)) (acl2-count (append a b)))))

||#