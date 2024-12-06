(fiasco:define-test-package #:paradigms/15-tests
  (:local-nicknames
   (#:p15 #:paradigms/15)))

(in-package :paradigms/15)

;;;
;;;  Exercise 15.3
;;;

;;; Modify prefix->canon to accept input of the form x / y and to
;;; return rational expressions instead of polynomials. Also allow for
;;; input of the form x ^ - n.

(defun prefix->canon (x)
  "Convert a prefix Lisp expression to canonical form.
  Exs: (+ (^ x 2) (* 3 x)) => #(x 0 3 1)
       (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) => 0"
  (cond ((numberp x) x)
        ((symbolp x) (poly x 0 1))
        ((and (exp-p x)
              (symbolp (exp-op x))
              (get (exp-op x) 'prefix->canon))
         (apply (get (exp-op x) 'prefix->canon)
                (mapcar #'prefix->canon (exp-args x))))
        ((and (consp x))
         (make-rat (prefix->canon (car x))
                   (prefix->canon (cdr x))))
        (t (error "Not a polynomial or rational expression : ~a" x))))

(in-package #:paradigms/15-tests)

(deftest test-ex-3 ()
  ;; Test that simple rationals can be created
  (is (= 1/2 (p15::prefix->canon (cons 1 2))))
  
  ;; Test that fractions can be added together
  (is (= 5/4 (p15::prefix->canon `(+ ,(cons 1 2) ,(cons 3 4)))))
  
  ;; Test that fractions can be multiplied together
  (is (= 3/8 (p15::prefix->canon `(* ,(cons 1 2) ,(cons 3 4))))))


;;;
;;; Exercise 15.4
;;;

;;; Add arithmetic routines for multiplication, addition, and division
;;; of rational expressions. Call them rat*rat, rat+rat, and rat/rat
;;; respectively. They will call upon poly*poly. poly+poly and a new
;;; function, poly/poly, which is defined in the next exercise.

(in-package :paradigms/15)

(defun rat+rat (p q)
  "Add two rationals."
  ;; We form the numerator by adding each numerator multiplied by the
  ;; other rational's denominator.
  (labels ((num*den (rat1 rat2)
             "Multiply the numerator of rat1 with the denominator of rat2."
             (let ((a (rat-numerator rat1))
                   (b (rat-denominator rat2)))
               (dolist (x (list a b))
                 (assert (or (typep x 'number)
                             (typep x 'polynomial))))
               (poly*poly a b))))
    (make-rat (poly+poly (num*den p q) (num*den q p))
              (poly*poly (rat-denominator p) (rat-denominator q)))))

(defun rat*rat (p q)
  "Multiply two rationals."
  (make-rat (poly*poly (rat-numerator p) (rat-numerator q))
            (poly*poly (rat-denominator p) (rat-denominator q))))

(in-package #:paradigms/15-tests)

(deftest test-ex-4 ()
  ;; Test addition  of two rational numbers
  (is (= 1 (p15::rat+rat (p15::make-rat 1 2) (p15::make-rat 1 2))))

  ;; Test addition of a rational number with a rational variable
  (is (equalp '(#(x 2 1) . #(x 0 2))
              (p15::rat+rat (p15::make-rat 1 2) (p15::make-rat 1 (p15::poly 'x 0 1)))))

  ;; Test multiplication of two rational numbers
  (is (= 1/4 (p15::rat*rat (p15::make-rat 1 2) (p15::make-rat 1 2)))))

;;;
;;; Exercise 15.5
;;;

;;; Define poly-gcd, which computes the greatest common divisor of two
;;; polynomials.

(in-package :paradigms/15)

(defun poly-coefs (poly)
  "Returns the coefficients of a polynomial."
  (declare (type polynomial poly)
           (values (vector integer)))
  (subseq poly 1))

(defun poly-gcd (poly-1 poly-2)
  "Computes the greatest common divisor of two polynomials."
  (declare (type polynomial poly-1 poly-2)
           (values integer))
  (gcd% (concatenate 'vector
                     (poly-coefs poly-1)
                     (poly-coefs poly-2))))

(defun gcd% (nums)
  "Computes the greatest common divisor of a set of numbers."
  (declare (type vector nums)
           (values integer))
  (let ((min (reduce #'min nums)))
    (loop :for i :downfrom min :to 1
          :if (every (lambda (n) (zerop (mod n i))) nums)
            :do (return i)
          :finally (return 1))))

(defun poly/k (poly k)
  "Divide a polynomial by a constant."
  (declare (type polynomial poly)
           (type integer k))
  (poly
   (main-var poly)
   (map 'vector (lambda (x) (/ x k)) (poly-coefs poly))))






;;;
;;; Exercise 15.6
;;;

;;; Using poly-gcd, define the function poly/poly, which will
;;; implement division for polynomials. Polynomials are closed under
;;; addition and multiplication, so poly+poly and poly*poly both
;;; returned polynomials. Polynomials are not closed under division,
;;; so poly/poly will return a rational expression.

(defun poly/poly (poly-1 poly-2)
  "Divide one polynomial by another."
  (let ((gcd (poly-gcd poly-1 poly-2)))
    (if (= 1 gcd)
        (progn
          (format t "These polynomials cannot be simplified through division.")
          (make-rat poly-1 poly-2))
        (make-rat (poly/k poly-1 gcd)
                  (poly/k poly-2 gcd)))))

(defun divide-polys ()
  "This wants to be a test."
  (let ((poly-1 (poly 'x 2 4 6))
        (poly-2 (poly 'x 4 8 10)))
    
    (poly/poly poly-1 poly-2)))


;; Create tests for the other two exercises -- did I go wrong somewhere?
;; Work a bit more on the presentation
