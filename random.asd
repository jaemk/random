(defsystem "random"
  :version "0.1.0"
  :author "James Kominick"
  :license "MIT"
  :depends-on ("cl-argparse" "uuid" "ironclad" "arrow-macros")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "random/tests"))))

(defsystem "random/tests"
  :author "James Kominick"
  :license "MIT"
  :depends-on ("random"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for random"
  :perform (test-op (op c) (symbol-call :rove :run c)))
