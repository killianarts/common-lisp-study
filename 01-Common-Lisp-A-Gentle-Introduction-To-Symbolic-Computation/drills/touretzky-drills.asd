(defsystem :touretzky-drills
  :serial t
  :version "1.0"
  :description "Drills using exercises from David S. Touretzky's book Common Lisp: A Gentle Introduction to Symbolic Computation."
  :components (
               (:file "package")
               (:module "src"
                :serial t
                :components ((:file "ch-01")
                             (:file "ch-02")))
               )
  ;; :perform (test-op (o c) (load (merge-pathnames "run-test.lisp" (system-source-directory c))))
  )

(defsystem :touretzky-drills/test
  :description "Tests to check answers to drill exercises."
  :components ((:module "test"
                :serial t
                :components ((:file "packages")
                             (:file "ch-01-test")
                             (:file "ch-02-test"))))
  :depends-on (:touretzky-drills :fiveam))
