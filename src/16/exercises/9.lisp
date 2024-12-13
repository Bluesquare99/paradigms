;; Add a spelling corrector to ask-vals.
;;
;; If the user enters an invalid reply, and the parameter type is a
;; member expression, check if the reply is "close" in spelling to one
;; of the valid values, and if so, use that value.
;;
;; That way, the user can type just "entero" instead of
;; "enterobacteriaceae." You may experiment with the definition of
;; "close," but you should certainly allow for prefixes and at least
;; one instance of a changed, missing, inserted, or transposed letter.

(defun add-to-graph (chars graph)
  "Create a path through the graph of the characters."
  (declare (type list chars)
           (type hash-table graph))

  (let* ((c (pop chars))
         (val (gethash c graph)))

    (cond (;; When a hash table already exists as the value for this
           ;; character key, continue on building the tree from that point
           val
           (add-to-graph chars val))

          (;; If there are still CHARS to add, create a new entry and
           ;; continue building
           (and (null val)
                (not (null chars)))

           (let ((table (make-hash-table)))
             (setf (gethash c graph) table)
             (add-to-graph chars table)))
          
          (;; When there was not a preexisting entry but CHARS is
           ;; kaput, add t as value then return.
           (and (null val)
                (null chars))
           (setf (gethash c graph) t)
           graph))))

(defun create-word-graph (options)
  "Create a graph of characters where a return value of t represents that
a word was found."
  (let ((graph (make-hash-table)))
    (dolist (word options)
      (add-to-graph (coerce (string word) 'list) graph))

    graph))

(defun probe-for-continuation (next-char graph)
  "Determine if a word was spelled with a skipped letter."
  (loop :for outer-val :being :the :hash-values :of graph
        :if (hash-table-p outer-val) :do
          (loop :for k :being :the :hash-keys :of outer-val
                :for inner-val :being :the :hash-values :of outer-val
                :if (char= next-char k) :do
                  (return-from probe-for-continuation inner-val))))

(defun walk-graph (answer graph)
  "Walk the graph of acceptable answers, determining if ANSWER is
valid. We accept the following submissions as valid:

- An exact match, ie 'male' when 'male' is an acceptable answer.
- An incomplete but accurate version of an acceptable answer, ie 'mal' for 'male'.
- A submission in which some letters are missing in an otherwise acceptable answer, ie 'mae' for 'male'."
  
  (declare (type list answer)
           (type hash-table graph))

  (let* ((next-char (pop answer))
         (found (gethash next-char graph)))

    (cond ((equal found t)
           found)

          ;; If the submitted word forms a portion of an acceptable
          ;; answer, accept it.
          ((and (hash-table-p found)
                (null answer))
           t)

          ((hash-table-p found)
           (walk-graph answer found))

          ((null found)
           (let ((continue (probe-for-continuation next-char graph)))
             (cond ((null continue)
                    nil)

                   ((hash-table-p continue)
                    (walk-graph answer continue))

                   ((equal continue t)
                    t)))))))

(defun spell-check (answer options)
  (walk-graph
   (coerce (string answer) 'list)
   (create-word-graph options)))

(defun check-parm-type (ans parm-type)
  "Check if an answer matches the given parameter type."
  (if (and (listp parm-type)
           (eql 'member (first parm-type)))
      (spell-check ans (rest parm-type))
      (typep ans parm-type)))

(defun check-reply (reply parm inst)
  "If reply is valid for this parm, update the DB.
  Reply should be a val or (val1 cf1 val2 cf2 ...).
  Each val must be of the right type for this parm."
  (let ((answers (parse-reply reply)))
    (when (every #'(lambda (pair)
                     (and (check-parm-type (first pair) (parm-type parm))
                          #+nil(typep (first pair) (parm-type parm))
                          (cf-p (second pair))))
                 answers)
      ;; Add replies to the data base
      (dolist (pair answers)
        (update-cf parm inst (first pair) (second pair)))
      answers)))
