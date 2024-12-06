(defpackage :paradigms/2
  (:use :cl))

(in-package :paradigms/2)

;; Write a version of generate that explicitly differentiates between
;; terminal symbols (those with no rewrite rules) and nonterminal
;; symbols.

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let* ((choices (rewrites phrase))
         (non-terminal (and choices (consp (first choices))))
         (terminal (and choices (atom (first choices)))))

    (cond ((listp phrase)
           (mappend #'generate phrase))
          (non-terminal
           (generate (random-elt choices)))
          (terminal
           (generate (random-elt choices)))
          (t (list phrase)))))
