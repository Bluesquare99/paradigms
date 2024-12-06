(defpackage :paradigms/2
  (:use :cl))

(in-package :paradigms/2)

;; Write a trivial grammar for some other language. This can be a
;; natural language other than English, or perhaps a subset of a
;; computer language.

(defparameter *street-grammar*
  '((trip -> (Road Light Turn)*)
    (Road -> Highway Main-Street Side-Street)
    (Light -> Red Yellow Green)
    (Turn -> Right Left Straight)))
