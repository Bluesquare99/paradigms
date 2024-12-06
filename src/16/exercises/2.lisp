(in-package :paradigms/16)

;; Suppose the rule writer wanted to be able to use symbolic certainty
;; factors instead of numbers. What would you need to change to
;; support rules like this:

#+nil(defrule 100 if ... then true ...)
#+nil(defrule 101 if ... then probably ...)

;; What would be necessary would be to:
;;
;; a. Change the warning within DEFRULE that
;;    `(when (not (cf-p cf))
;;       (warn "Rule ~a: Illegal certainty factor: ~a" number cf)`
;;
;;    This would be done through changing `cf-p` to also include the
;;    terms we want to allow, including "true", "probably", etc., and
;;    then transform them to double floats within the DEFRULE macro,
;;    perhaps through a function called INTERPRET-CF.
