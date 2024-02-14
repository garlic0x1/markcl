(asdf:defsystem "markcl-test"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:alexandria :fiveam :markcl)
  :components ((:module "t"
                :components ((:file "markcl-test")))))
