(in-package :paradigms/16)

;; Exercise 16.3
;;
;; Change prompt-and-read-vals so that it gives a better prompt for
;; parameters of type yes/no.
;;

(defun prompt-and-read-vals (parm inst)
  "Print the prompt for this parameter (or make one up) and
  read the reply."
  (fresh-line)
  (format t (parm-prompt (get-parm parm)) (inst-name inst) parm)
  (when (eq 'yes/no (parm-type parm))
    (format t " Yes or no."))
  (princ " ")
  (finish-output)
  (funcall (parm-reader (get-parm parm))))
