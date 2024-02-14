(asdf:defsystem "markcl"
  :description "Markdown generator for Common Lisp"
  :author "garlic0x1"
  :version "0.1"
  :license "MIT"
  :depends-on (:alexandria)
  :components ((:module "src"
                :components ((:file "markcl")))))
