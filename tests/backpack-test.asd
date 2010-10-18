;;; -*- lisp -*-

(asdf:defsystem backpack-test
  :depends-on (:backpack)
  :components
    ((:file "lisp-unit")
     (:file "package")
     (:file "unit-tests"))
  :serial t)
