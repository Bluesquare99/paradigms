(in-package :paradigms/16)

;; Exercise 16.4
;;
;; Currently, the rule writer can introduce a new parameter without
;; defining it first. That is handy for rapid testing, but it means
;; that the user of the system won't be able to see a nice English
;; prompt, nor ask for the type of the parameter. In addition, if the
;; rule writer simply misspells a parameter, it will be treated as a
;; new one. Make a simple change to fix these problems.
;;

;; Parameter definitions allow for someone to see an English sentence
;; for a prompt and ask for the type of a parameter.

(defun get-parm (parm-name)
  "Look up the parameter structure with this name."
  ;; If there is none, make one
  (get parm-name 'parm))

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
