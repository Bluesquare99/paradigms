(defpackage :paradigms/2
  (:use :cl))

(in-package :paradigms/2)

;; One way of describing combine-all is that it calculates the
;; cross-product of the function append on the argument lists. Write
;; the higher-order function cross-product, and define combine-all in
;; terms of it.

(defun cross-product (list1 list2 func)
  (declare (type list list1 list2)
           (type function func)
           (values list))

  (loop :for el1 :in list1
        :append (loop :for el2 :in list2
                      :collect (apply func (list el1) (list el2)))))
