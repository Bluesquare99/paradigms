(defpackage :paradigms/16
  (:use :cl))

(in-package :paradigms/16)

;;;
;;; Helpers
;;;

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun print-db (db)
  (maphash (lambda (k v) (print k) (print v)) db))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

;;;
;;; Core
;;;

(defconstant true   +1.0)
(defconstant false  -1.0)
(defconstant unknown 0.0)

(defun cf-or (a b)
  "Combine the certainty factors for the formula (A or B).
  This is used when two rules support the same conclusion."
  (cond ((and (> a 0) (> b 0))
         (+ a b (* -1 a b)))
        ((and (< a 0) (< b 0))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defun cf-and (a b)
  "Combine the certainty factors for the formula (A and B)."
  (min a b))

(defconstant cf-cut-off 0.2
  "Below this certainty we cut off search.")

(defun true-p (cf)
  "Is this certainty factor considered true?"
  (and (cf-p cf) (> cf cf-cut-off)))

(defun false-p (cf)
  "Is this certainty factor considered false?"
  (and (cf-p cf) (< cf (- cf-cut-off 1.0))))

(defun cf-p (x)
  "Is X a valid numeric certainty factor?"
  (and (numberp x) (<= false x true)))

(let ((db (make-hash-table :test #'equal)))
  (defun get-db (key) (gethash key db))
  (defun put-db (key val) (setf (gethash key db) val))
  (defun clear-db () (clrhash db)))

(defun get-vals (parm inst)
  "Return a list of (val cf) pairs for this (parm inst)."
  (get-db (list parm inst)))

(defun get-cf (parm inst val)
  "Look up the certainty factor or return unknown."
  (or (second (assoc val (get-vals parm inst)))
      unknown))

(defun update-cf (parm inst val cf)
  "Change the certainty factor for (parm inst is val),
  by combining the given cf with the old."
  (let ((new-cf (cf-or cf (get-cf parm inst val))))
    (put-db (list parm inst)
            (cons (list val new-cf)
                  (remove val (get-db (list parm inst))
                          :key #'first)))))

(defconstant help-string
  "~&Type one of the following:
 ?     - to see possible answers for this parameter
 rule  - to show the current rule
 why   - to see why this question is asked
 help  - to see this list
 xxx   - (for some specific xxx) if there is a definite answer
 (xxx .5 yyy .4) - If there are several answers with
                   different certainty factors.")

(defun ask-vals (parm inst)
  "Ask the user for the value(s) of inst's parm parameter,
  unless this has already been asked.  Keep asking until the
  user types UNKNOWN (return nil) or a valid reply (return t)."
  (unless (get-db `(asked ,parm ,inst))
    (put-db `(asked ,parm ,inst) t)
    (loop
      (let ((ans (prompt-and-read-vals parm inst)))
        (case ans
          (help (format t help-string))
          (why  (print-why (get-db 'current-rule) parm))
          (rule (princ (get-db 'current-rule)))
          ((unk unknown) (RETURN nil))
          (?    (format t "~&A ~a must be of type ~a"
                        parm (parm-type parm)) nil)
          (t    (if (check-reply ans parm inst)
                    (RETURN t)
                    (format t "~&Illegal reply.  ~
                             Type ? to see legal ones."))))))))

(defun prompt-and-read-vals (parm inst)
  "Print the prompt for this parameter (or make one up) and
  read the reply."
  (let ((phrase
          (format nil
                  (parm-prompt (get-parm parm))
                  (inst-name inst)
                  parm)))
    (print-with-indent phrase)
    (princ " ")
    (finish-output)
    (funcall (parm-reader (get-parm parm)))))

(defun inst-name (inst)
  "The name of this instance."
  ;; The stored name is either like (("Jan Doe" 1.0)) or nil
  (or (first (first (get-vals 'name inst)))
      inst))

(defun check-reply (reply parm inst)
  "If reply is valid for this parm, update the DB.
  Reply should be a val or (val1 cf1 val2 cf2 ...).
  Each val must be of the right type for this parm."
  (let ((answers (parse-reply reply)))
    (when (every #'(lambda (pair)
                     (and (typep (first pair) (parm-type parm))
                          (cf-p (second pair))))
                 answers)
      ;; Add replies to the data base
      (dolist (pair answers)
        (update-cf parm inst (first pair) (second pair)))
      answers)))

(defun parse-reply (reply)
  "Convert the reply into a list of (value cf) pairs."
  (cond ((null reply) nil)
        ((atom reply) `((,reply ,true)))
        (t (cons (list (first reply) (second reply))
                 (parse-reply (rest2 reply))))))

(defstruct (parm (:constructor
                  new-parm (name &optional context type-restriction
                            prompt ask-first read-xer)))
  name
  (context nil)
  (prompt "~&What is the ~*~a of ~2:*~a?")
  (ask-first nil)
  (type-restriction t)
  (reader 'read))

(defmacro defparm (parm &rest args)
  "Define a parameter."
  `(setf (get ',parm 'parm) (apply #'new-parm ',parm ',args)))

(defun parm-type (parm-name)
  "What type is expected for a value of this parameter?"
  (parm-type-restriction (get-parm parm-name)))

(defun get-parm (parm-name)
  "Look up the parameter structure with this name."
  ;; If there is none, make one
  (get parm-name 'parm))

(deftype yes/no () '(member yes no))

(defstruct context
  "A context is a sub-domain, a type."
  name (number 0) initial-data goals)

(defmacro defcontext (name &optional initial-data goals)
  "Define a context."
  `(make-context :name ',name :initial-data ',initial-data
                 :goals ',goals))

(defun new-instance (context)
  "Create a new instance of this context."
  (let ((instance (format nil "~a-~d"
                          (context-name context)
                          (incf (context-number context)))))
    (let ((phrase (format nil "------ ~a ------" instance)))
      (fresh-line)
      (print-with-indent phrase)
      (fresh-line))
    (put-db (context-name context) instance)
    (put-db 'current-instance instance)))

(defstruct (rule (:print-function print-rule))
  number premises conclusions cf)

(let ((rules (make-hash-table)))

  (defun put-rule (rule)
    "Put the rule in a table, indexed under each
    parm in the conclusion."
    (dolist (concl (rule-conclusions rule))
      (push rule (gethash (first concl) rules)))
    rule)

  (defun get-rules (parm)
    "A list of rules that help determine this parameter."
    (gethash parm rules))

  (defun clear-rules () (clrhash rules)))

(defun find-out (parm &optional (inst (get-db 'current-instance)))
  "Find the value(s) of this parameter for this instance,
  unless the values are already known.
  Some parameters we ask first; others we use rules first."
  (or (get-db `(known ,parm ,inst))
      (put-db `(known ,parm ,inst)
              (if (parm-ask-first (get-parm parm))
                  (or (ask-vals parm inst) (use-rules parm))
                  (or (use-rules parm) (ask-vals parm inst))))))

(defun use-rules (parm)
  "Try every rule associated with this parameter.
  Return true if one of the rules returns true."
  (let ((lst (mapcar #'use-rule (get-rules parm))))
    (some #'true-p lst)))

(defun use-rule (rule)
  "Apply a rule to the current situation."
  (format t "~%Looking into this rule: ~a~%" rule)
  ;; Keep track of the rule for the explanation system:
  (put-db 'current-rule rule)
  ;; If any premise is known false, give up.
  ;; If every premise can be proved true, then
  ;; draw conclusions (weighted with the certainty factor).
  (unless (some #'reject-premise (rule-premises rule))
    (let ((cf (satisfy-premises (rule-premises rule) true)))
      (when (true-p cf)
        (format t "~%A rule was found true: ~a~%The cf now is ~a~%" rule cf)
        (dolist (conclusion (rule-conclusions rule))
          (conclude conclusion (* cf (rule-cf rule))))
        cf))))

(defun satisfy-premises (premises cf-so-far)
  "A list of premises is satisfied if they are all true.
  A combined cf is returned."
  ;; cf-so-far is an accumulator of certainty factors
  (cond ((null premises) cf-so-far)
        ((not (true-p cf-so-far)) false)
        (t (satisfy-premises
            (rest premises)
            (cf-and cf-so-far
                    (eval-condition (first premises)))))))

(defun eval-condition (condition &optional (find-out-p t))
  "See if this condition is true, optionally using FIND-OUT
  to determine unknown parameters."
  (multiple-value-bind (parm inst op val)
      (parse-condition condition)
    (when find-out-p
      (find-out parm inst))
    ;; Add up all the (val cf) pairs that satisfy the test
    (loop for pair in (get-vals parm inst)
          when (funcall op (first pair) val)
          sum (second pair))))

(defun reject-premise (premise)
  "A premise is rejected if it is known false, without
  needing to call find-out recursively."
  (false-p (eval-condition premise nil)))

(defun conclude (conclusion cf)
  "Add a conclusion (with specified certainty factor) to DB."
  (multiple-value-bind (parm inst op val)
      (parse-condition conclusion)
    (update-cf parm inst val cf)))

(defun is (a b) (equal a b))

(defun parse-condition (condition)
  "A condition is of the form (parm inst op val).
  So for (age patient is 21), we would return 4 values:
  (age patient-1 is 21), where patient-1 is the current patient."
  (values (first condition)
          (get-db (second condition))
          (third condition)
          (fourth condition)))

(defun emycin (contexts)
  "An Expert System Shell.  Accumulate data for instances of each
  context, and solve for goals.  Then report the findings."
  (clear-db)
  (get-context-data contexts))

(defvar *indent* 0)
(defun indent (var &optional (amt 2))
  (declare (type integer var amt))
  (incf var amt))
(defun print-with-indent (phrase)
  (dotimes (_ *indent*)
    (format t " "))
  (format t phrase))

(defun get-context-data (contexts)
  "For each context, create an instance and try to find out
  required data.  Then go on to other contexts, depth first,
  and finally ask if there are other instances of this context."
  (unless (null contexts)
    (let* ((context (first contexts))
           (inst (new-instance context)))
      (put-db 'current-rule 'initial)
      (mapc #'find-out (context-initial-data context))
      (put-db 'current-rule 'goal)
      (mapc #'find-out (context-goals context))
      (report-findings context inst)
      (let ((*indent* (indent *indent*)))
        (get-context-data (rest contexts))
        (when (y-or-n-p "Is there another ~a?"
                        (context-name context))
          (let ((*indent* (indent *indent*)))
            (get-context-data contexts)))))))

(defmacro defrule (number &body body)
  "Define a rule with conditions, a certainty factor, and
  conclusions.  Example: (defrule R001 if ... then .9 ...)"
  (assert (eq (first body) 'if))
  (let* ((then-part (member 'then body))
         (premises (ldiff (rest body) then-part))
         (conclusions (rest2 then-part))
         (cf (second then-part)))
    ;; Do some error checking:
    (check-conditions number premises 'premise)
    (check-conditions number conclusions 'conclusion)
    (when (not (cf-p cf))
      (warn "Rule ~a: Illegal certainty factor: ~a" number cf))
    ;; Now build the rule:
    `(put-rule
       (make-rule :number ',number :cf ,cf :premises ',premises
                  :conclusions ',conclusions))))

(defun check-conditions (rule-num conditions kind)
  "Warn if any conditions are invalid."
  (when (null conditions)
    (warn "Rule ~a: Missing ~a" rule-num kind))
  (dolist (condition conditions)
    (when (not (consp condition))
      (warn "Rule ~a: Illegal ~a: ~a" rule-num kind condition))
    (multiple-value-bind (parm inst op val)
        (parse-condition condition)
      (declare (ignore inst))
      (when (null (get-parm parm))
          (warn "Parameter ~a has not yet been defined." parm))
      (when (and (eq kind 'conclusion) (not (eq op 'is)))
        (warn "Rule ~a: Illegal operator (~a) in conclusion: ~a"
              rule-num op condition))
      (when (not (typep val (parm-type parm)))
        (warn "Rule ~a: Illegal value (~a) in ~a: ~a"
              rule-num val kind condition)))))

(defun report-findings (context inst)
  "Print findings on each goal for this instance."
  (when (context-goals context)
    (print-with-indent
     (format nil "Findings for ~a:" (inst-name inst)))
    (dolist (goal (context-goals context))
      (let ((values (get-vals goal inst)))
        ;; If there are any values for this goal,
        ;; print them sorted by certainty factor.
        (if values
            (let ((phrase
                    (if values
                        (format nil " ~a:~{~{ ~a (~,3f)  ~}~}"
                                goal (sort (copy-list values) #'> :key #'second))
                        (format nil " ~a: unknown" goal))))
              (print-with-indent phrase)))))))

(defun print-rule (rule &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "~&Rule ~a:~&  If" (rule-number rule))
  (print-conditions (rule-premises rule) stream)
  (format stream "~&  Then ~a (~a) that"
          (cf->english (rule-cf rule)) (rule-cf rule))
  (print-conditions (rule-conclusions rule) stream))

(defun print-conditions (conditions &optional
                         (stream t) (num 1))
  "Print a list of numbered conditions."
  (dolist (condition conditions)
    (print-condition condition stream num)))

(defun print-condition (condition stream number)
  "Print a single condition in pseudo-English."
  (format stream "~&    ~d)~{ ~a~}" number
          (let ((parm (first condition))
                (inst (second condition))
                (op (third condition))
                (val (fourth condition)))
            (case val
              (YES `(the ,inst ,op ,parm))
              (NO  `(the ,inst ,op not ,parm))
              (T   `(the ,parm of the ,inst ,op ,val))))))

(defun cf->english (cf)
  "Convert a certainy factor to an English phrase."
  (cond ((= cf  1.0) "there is certain evidence")
        ((> cf   .8) "there is strongly suggestive evidence")
        ((> cf   .5) "there is suggestive evidence")
        ((> cf  0.0) "there is weakly suggestive evidence")
        ((= cf  0.0) "there is NO evidence either way")
        ((< cf  0.0) (concatenate 'string (cf->english (- cf))
                                  " AGAINST the conclusion"))))

(defun print-why (rule parm)
  "Tell why this rule is being used.  Print what is known,
  what we are trying to find out, and what we can conclude."
  (format t "~&[Why is the value of ~a being asked for?]" parm)
  (if (member rule '(initial goal))
      (format t "~&~a is one of the ~a parameters."
              parm rule)
      (multiple-value-bind (knowns unknowns)
          (partition-if #'(lambda (premise)
                            (true-p (eval-condition premise nil)))
                        (rule-premises rule))
        (when knowns
          (format t "~&It is known that:")
          (print-conditions knowns)
          (format t "~&Therefore,"))
        (let ((new-rule (copy-rule rule)))
          (setf (rule-premises new-rule) unknowns)
          (print new-rule)))))

(defun mycin ()
  "Determine what organism is infecting a patient."
  (emycin
    (list (defcontext patient  (name sex age)  ())
          (defcontext culture  (site days-old) ())
          (defcontext organism ()              (identity)))))


#+nil
(progn
  ;;; Parameters for patient:
  (defparm name patient t "Patient's name: " t read-line)
  (defparm sex patient (member male female) "Sex:" t)
  (defparm age patient number "Age:" t)
  (defparm burn patient (member no mild serious)
    "Is ~a a burn patient?  If so, mild or serious?" t)
  (defparm compromised-host patient yes/no
    "Is ~a a compromised host?")

  ;;; Parameters for culture:
  (defparm site culture (member blood)
    "From what site was the specimen for ~a taken?" t)
  (defparm days-old culture number
    "How many days ago was this culture (~a) obtained?" t)


  ;;; Parameters for organism
  (defparm identity organism
    (member pseudomonas klebsiella enterobacteriaceae
            staphylococcus bacteroides streptococcus)
    "Enter the identity (genus) of ~a:" t)
  (defparm gram organism (member acid-fast pos neg)
    "The gram stain of ~a:" t)
  (defparm morphology organism (member rod coccus)
    "Is ~a a rod or coccus (etc.):")
  (defparm aerobicity organism (member aerobic anaerobic))
  (defparm growth-conformation organism
    (member chains pairs clumps))

  (clear-rules)

  (defrule 52
    if (site culture is blood)
    (gram organism is neg)
    (morphology organism is rod)
    (burn patient is serious)
    then .4
    (identity organism is pseudomonas))

  (defrule 71
    if (gram organism is pos)
    (morphology organism is coccus)
    (growth-conformation organism is clumps)
    then .7
    (identity organism is staphylococcus))

  (defrule 73
    if (site culture is blood)
    (gram organism is neg)
    (morphology organism is rod)
    (aerobicity organism is anaerobic)
    then .9
    (identity organism is bacteroides))

  (defrule 75
    if (gram organism is neg)
    (morphology organism is rod)
    (compromised-host patient is yes)
    then .6
    (identity organism is pseudomonas))

  (defrule 107
    if (gram organism is neg)
    (morphology organism is rod)
    (aerobicity organism is aerobic)
    then 1.0
    (identity organism is enterobacteriaceae))

  (defrule 165
    if (gram organism is pos)
    (morphology organism is coccus)
    (growth-conformation organism is chains)
    then .7
    (identity organism is streptococcus)))
