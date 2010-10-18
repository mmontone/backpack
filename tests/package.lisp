
(cl:defpackage :backpack-test
  (:nicknames :rs-test)
  (:use :common-lisp :backpack :lisp-unit)
  (:export #:run-tests))

(cl:defpackage :backpack-test-schema-update
  (:nicknames :rs-tsu)
  (:use :common-lisp :backpack))

