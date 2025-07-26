(defsystem :paip
  :components ((:module src
                :serial t
                :components ((:file "common")
                             (:file "01-introduction-to-lisp")
                             (:file "02-a-simple-lisp-program")
                             (:file "03-overview-of-lisp")
                             (:file "04-gps-the-general-problem-solver")
                             (:file "05-eliza-dialogue-with-a-machine")
                             (:file "06-building-software-tools")))))

(defsystem :paip-test
  :depends-on ("rove")
  :components ((:module t
                :serial t
                :components ((:file "01-test")))))
