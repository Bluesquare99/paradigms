(defpackage :paradigms/2
  (:use :cl))

(in-package :paradigms/2)

;; Write a version of generate that uses cond but avoids calling
;; rewrites twice.

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          (choices
           (generate (random-elt choices)))
          (t (list phrase)))))
