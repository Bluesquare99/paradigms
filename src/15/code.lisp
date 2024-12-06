(defpackage :paradigms/15
  (:use :cl))

(in-package :paradigms/15)

;;;
;;; Before this chapter
;;;

;;;
;;; Misc
;;;


#+nil
(defun rule-pattern (rule) (first rule))
#+nil
(defun rule-response (rule) (second rule))

#+nil
(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

;;;
;;; Pattern Matcher
;;;

(defconstant no-bindings '((t . t)))
(defconstant fail nil)

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))


(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns)
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))


#+nil
(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
          (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator
       (input rules &key (matcher 'pat-match)
        (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some
    #'(lambda (rule)
        (let ((result (funcall matcher (funcall rule-if rule)
                               input)))
          #+dbg
          (progn
            (format t "Input ~a, rule ~a, result ~a~%"
                    input rule result)
            (break))
          
          (if (not (eq result fail))
              (funcall action result (funcall rule-then rule)))))
    rules))

(defun ^ (x y) "Exponentiation" (expt x y))

(setf (get '?+  'segment-match) 'segment-match+)

#+nil
(defun variable-p (exp)
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  (member exp '(x y z m n o p q r s t u v w)))

;; Define x+ and y+ as a sequence:
(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))



(defstruct (rule (:type list)) pattern response)

(defun exp-p (x) (consp x))
(defun exp-op (x) (first x))
(defun exp-args (x) (rest x))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
          '(((x+ = y+) (= x y))
            ((- x+)    (- x))
            ((+ x+)    (+ x))
            ((x+ + y+) (+ x y))
            ((x+ - y+) (- x y))
            ((d y+ / d x) (d y x))      ;*** New rule
            ((Int y+ d x) (int y x))    ;*** New rule
            ((x+ * y+) (* x y))
            ((x+ / y+) (/ x y))
            ((x+ ^ y+) (^ x y)))))


(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
        ((= (length exp) 1)
         (infix->prefix (first exp)))
        ((rule-based-translator exp *infix->prefix-rules*
                                :rule-if #'rule-pattern :rule-then #'rule-response
                                :action
                                #'(lambda (bindings response)
                                    (sublis (mapcar
                                             #'(lambda (pair)
                                                 (cons (first pair)
                                                       (infix->prefix (rest pair))))
                                             bindings)
                                            response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

(defparameter *simplification-rules* (mapcar #'infix->prefix '(
                                                               (x + 0  = x)
                                                               (0 + x  = x)
                                                               (x + x  = 2 * x)
                                                               (x - 0  = x)
                                                               (0 - x  = - x)
                                                               (x - x  = 0)
                                                               (- - x  = x)
                                                               (x * 1  = x)
                                                               (1 * x  = x)
                                                               (x * 0  = 0)
                                                               (0 * x  = 0)
                                                               (x * x  = x ^ 2)
                                                               (x / 0  = undefined)
                                                               (0 / x  = 0)
                                                               (x / 1  = x)
                                                               (x / x  = 1)
                                                               (0 ^ 0  = undefined)
                                                               (x ^ 0  = 1)
                                                               (0 ^ x  = 0)
                                                               (1 ^ x  = 1)
                                                               (x ^ 1  = x)
                                                               (x ^ -1 = 1 / x)
                                                               (x * (y / x) = y)
                                                               ((y / x) * x = y)
                                                               ((y * x) / x = y)
                                                               ((x * y) / x = y)
                                                               (x + - x = 0)
                                                               ((- x) + x = 0)
                                                               (x + y - x = y)
                                                               )))

;;;
;;; To Be Erased
;;;

(defun simplifier ()
  "Read a mathematical expression, simplify it, and print the result."
  (loop
    (print 'simplifier>)
    (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp)
  "Simplify an expression by first simplifying its components."
  (if (atom exp) exp
      (simplify-exp (mapcar #'simplify exp))))

;;; simplify-exp is redefined below
(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic."
  (cond ((rule-based-translator exp *simplification-rules*
           :rule-if #'exp-lhs :rule-then #'exp-rhs
           :action #'(lambda (bindings response)
                       (simplify (sublis bindings response)))))
        ((evaluable exp) (eval exp))
        (t exp)))

(defun evaluable (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every #'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
           (and (eq (exp-op exp) '^)
                (integerp (second (exp-args exp)))))))
;;;
;;; Chapter 15
;;;

(proclaim '(inline main-var degree coef
            var= var> poly make-poly))

(deftype polynomial () 'simple-vector)

(defsetf main-var (p) (val)
  `(setf (svref (the polynomial ,p) 0) ,val))

(defun main-var (p) (svref (the polynomial p) 0))
(defun coef (p i)   (svref (the polynomial p) (+ i 1)))
(defun degree (p)   (- (length (the polynomial p)) 2))

(defun poly (x &rest coefs)
  "Make a polynomial with main variable x
  and coefficients in increasing order."
  (apply #'vector x coefs))

(defun make-poly (x degree)
  "Make the polynomial 0 + 0*x + 0*x^2 + ... 0*x^degree"
  (let ((p (make-array (+ degree 2) :initial-element 0)))
    (setf (main-var p) x)
    p))

(defsetf coef (p i) (val)
  `(setf (svref (the polynomial ,p) (+ ,i 1)) ,val))

#+nil
(defun prefix->canon (x)
  "Convert a prefix Lisp expression to canonical form.
  Exs: (+ (^ x 2) (* 3 x)) => #(x 0 3 1)
       (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) => 0"
  (cond ((numberp x) x)
        ((symbolp x) (poly x 0 1))
        ((and (exp-p x) (get (exp-op x) 'prefix->canon))
         (apply (get (exp-op x) 'prefix->canon)
                (mapcar #'prefix->canon (exp-args x))))
        (t (error "Not a polynomial: ~a" x))))

(dolist (item '((+ poly+) (- poly-) (* poly*poly)
                (^ poly^n) (D deriv-poly)))
  (setf (get (first item) 'prefix->canon) (second item)))

(defun poly+ (&rest args)
  "Unary or binary polynomial addition."
  (ecase (length args)
    (1 (first args))
    (2 (poly+poly (first args) (second args)))))

(defun poly- (&rest args)
  "Unary or binary polynomial subtraction."
  (ecase (length args)
    (0 0)
    (1 (poly*poly -1 (first args)))
    (2 (poly+poly (first args) (poly*poly -1 (second args))))))

(defun var= (x y) (eq x y))
(defun var> (x y) (string> x y))

(defun poly+poly (p q)
  "Add two polynomials."
  (normalize-poly
    (cond
      ((numberp p)                      (k+poly p q))
      ((numberp q)                      (k+poly q p))
      ((var= (main-var p) (main-var q)) (poly+same p q))
      ((var> (main-var q) (main-var p)) (k+poly q p))
      (t                                (k+poly p q)))))

(defun k+poly (k p)
  "Add a constant k to a polynomial p."
  (cond ((eql k 0) p)                 ;; 0 + p = p
        ((and (numberp k) (numberp p))
         (+ k p))                     ;; Add numbers
        (t (let ((r (copy-poly p)))   ;; Add k to x^0 term of p
             (setf (coef r 0) (poly+poly (coef r 0) k))
             r))))

(defun poly+same (p q)
  "Add two polynomials with the same main variable."
  ;; First assure that q is the higher degree polynomial
  (if (> (degree p) (degree q))
      (poly+same q p)
      ;; Add each element of p into r (which is a copy of q).
      (let ((r (copy-poly q)))
        (loop for i from 0 to (degree p) do
              (setf (coef r i) (poly+poly (coef r i) (coef p i))))
        r)))

(defun copy-poly (p)
  "Make a copy a polynomial."
  (copy-seq p))

(defun poly*poly (p q)
  "Multiply two polynomials."
  (let ((val (cond
               ((numberp p)                      (k*poly p q))
               ((numberp q)                      (k*poly q p))
               ((var= (main-var p) (main-var q)) (poly*same p q))
               ((var> (main-var q) (main-var p)) (k*poly q p))
               (t                                (k*poly p q)))))
    (normalize-poly val)))

(defun k*poly (k p)
  "Multiply a polynomial p by a constant factor k."
  (cond
    ((eql k 0)         0) ;; 0 * p = 0
    ((eql k 1)         p) ;; 1 * p = p
    ((and (numberp k) (numberp p))
     (* k p)) ;; Multiply numbers
    (t        ;; Multiply each coefficient
     (let ((r (make-poly (main-var p) (degree p))))
       ;; Accumulate result in r;  r[i] = k*p[i]
       (loop for i from 0  to (degree p) do
         (setf (coef r i) (poly*poly k (coef p i))))
       r))))

(defun poly*same (p q)
  "Multiply two polynomials with the same variable."
  ;; r[i] = p[0]*q[i] + p[1]*q[i-1] + ...
  (let* ((r-degree (+ (degree p) (degree q)))
         (r (make-poly (main-var p) r-degree)))
    (loop :for i :from 0 :to (degree p) :do
      (unless (eql (coef p i) 0)
        (loop :for j :from 0 :to (degree q) :do
          (setf (coef r (+ i j))
                (poly+poly (coef r (+ i j))
                           (poly*poly (coef p i)
                                      (coef q j)))))))
    r))

(defun normalize-poly (p)
  "Alter a polynomial by dropping trailing zeros."
  (if (numberp p)
      p
      (let ((p-degree (- (position 0 p :test (complement #'eql)
                                       :from-end t)
                         1)))
        (cond ((<= p-degree 0) (normalize-poly (coef p 0)))
              ((< p-degree (degree p))
               (delete 0 p :start p-degree))
              (t p)))))

(defun poly^n (p n)
 "Raise polynomial p to the nth power, n>=0."
 (check-type n (integer 0 *))
 (cond ((= n 0) (assert (not (eql p 0))) 1)
   ((integerp p) (expt p n))
   (t (poly*poly p (poly^n p (- n 1))))))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun intersperse (op args)
  "Place op between each element of args.
  Ex: (intersperse '+ '(a b c)) => '(a + b + c)"
  (if (length=1 args)
      (first args)
      (rest (loop for arg in args
                  collect op
                  collect arg))))

(defun canon->prefix (p)
  "Convert a canonical polynomial to a lisp expression."
  (if (numberp p)
      p
      (args->prefix
        '+ 0
        (loop :for i :from (degree p) :downto 0
              :collect (args->prefix
                        '* 1
                        (list (canon->prefix (coef p i))
                              (exponent->prefix
                                (main-var p) i)))))))

(defun exponent->prefix (base exponent)
  "Convert canonical base^exponent to prefix form."
  (case exponent
    (0 1)
    (1 base)
    (t `(^ ,base ,exponent))))

(defun mappend (fn &rest lists)
  "Apply fn to each element of lists and append the results."
  (apply #'append (apply #'mapcar fn lists)))

(defun args->prefix (op identity args)
  "Convert arg1 op arg2 op ... to prefix form."
  (let* ((useful-args (remove identity args))
        (return-val
          (cond ((null useful-args) identity)
                ((and (eq op '*) (member 0 args)) 0)
                ;; This is slightly flawed though because in the case of a minus sign we 
                ((length=1 useful-args) (first useful-args))
                (t
                 (cons op (mappend
                           #'(lambda (exp)
                               (print exp)
                               (if (starts-with exp op)
                                   (exp-args exp)
                                   (list exp)))
                           useful-args))))))
    return-val))

(defun canon (infix-exp)
  "Canonicalize argument and convert it back to infix"
  (let ((step-one (infix->prefix infix-exp)))
    (print step-one)
    (break)
    (let ((step-two (prefix->canon step-one)))
      (print step-two)
      (break)
      (let ((step-three (canon->prefix step-two)))
        (print step-three)
        (break)
        (let ((step-four (prefix->infix step-three)))
          (print step-four)
          (break)
          step-four))))
  #+nil
  (prefix->infix (canon->prefix (prefix->canon (infix->prefix infix-exp)))))

(defun canon-simplifier ()
  "Read an expression, canonicalize it, and print the result."
  (loop
    (print 'canon>)
    (print (canon (read)))))

;;;
;;; Rational Expressions
;;;

(defun make-rat (numerator denominator)
  "Build a rational: a quotient of two polynomials."
  (if (numberp denominator)
      (k*poly (/ 1 denominator) numerator)
      (cons numerator denominator)))

(defun rat-numerator (rat)
  "The numerator of a rational expression."
  (typecase rat
    (cons (car rat))
    (number (numerator rat))
    (t rat)))

(defun rat-denominator (rat)
  "The denominator of a rational expression."
  (typecase rat
    (cons (cdr rat))
    (number (denominator rat))
    (t 1)))
