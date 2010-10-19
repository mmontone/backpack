(cl:defpackage :backpack-test
  (:nicknames :bp-test)
  (:use :common-lisp :backpack :lisp-unit)
  (:export #:run-tests))

(cl:defpackage :backpack-test-schema-update
  (:nicknames :bp-tsu)
  (:use :common-lisp :backpack))

