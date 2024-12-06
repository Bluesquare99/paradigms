(in-package :paradigms/16)

;; Suppose you read the headline "Elvis Alive in Kalamazoo" in a
;; tabloid newspaper to which you attribute a certainty factor of
;; .01. If you combine certainties using EMYCIN's combination rule,
;; how many more copies of the newspaper would you need to see before
;; you were .95 certain Elvis is alive?

;; The answer is 299, solved using the following code.

(loop :with certainty := 0
      :for i :from 0
      :while (> 0.95 certainty)
      :do (print certainty)
          (setf certainty (cf-or certainty .01))
      :finally (print i))
