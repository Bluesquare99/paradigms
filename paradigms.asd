(asdf:defsystem #:paradigms
  :author "Max McCready"
  :description "Code and exercises form Paradigms of Artificial Intelligence"
  :depends-on (#:fiasco)
  :in-order-to ((asdf:test-op (asdf:test-op :pulsescript/tests)))
  :pathname "src"
  :serial t
  :components ((:module "2"
                :serial t
                :components ((:file "code")
                             (:module "exercises"
                              :serial t
                              :components ((:file "1")))))
               (:module "15"
                :serial t
                :components ((:file "code")
                             (:file "exercise-5")))
               (:module "16"
                :serial t
                :components ((:file "code")
                             #+nil
                             (:module "exercises"
                              :serial t
                              :components ((:file "1")
                                           (:file "2")))))))
