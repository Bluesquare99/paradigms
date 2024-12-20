;; We said that our emycin looks at all possible rules for each
;; parameter, because there is no telling how a later rule may affect
;; the certainty factor. Actually, that is not quite true. If there is
;; a rule that leads to a conclusion with certainty 1, then no other
;; rules need be considered. This was called a unity path. Modify the
;; program to look for unity paths first.

(defun certain-p (cf)
  "Is this certainty factor considered certainly true, ie sufficient to make a conclusion without considering other rules?"
  (= cf 1.0))

(defun use-rules (parm)
  "Try every rule associated with this parameter.
  Return true if one of the rules returns true."
  (let* ((rules (get-rules parm))
         (sorted-rules (sort rules #'> :key #'rule-cf)))

    (loop :for rule :in sorted-rules
          :for (cf . certain) := (use-rule rule)
          :if certain :do
          (return-from use-rules)
          :else :collect cf :into collected-cfs
          :finally (return (some #'true-p collected-cfs)))))

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
        (cons cf (if (certain-p (rule-cf rule)) t nil))))))
