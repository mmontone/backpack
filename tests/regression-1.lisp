
(in-package :backpack-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-backpack (rs *test-suite* :if-exists :supersede)
    (with-transaction ()
      (defclass broken ()
        ((string-key :initarg :data :accessor string-key
                     :index :string-index))
        (:index t)
        (:metaclass persistent-class)))))

(progn
  ;; This used to break because LEAF-DELETE-KEY would cause a
  ;; comparison between an object id (i.e. an integer) and the
  ;; symbol KEY-IRRELEVANT.  Fixed in version 0.1.11.
  (with-backpack (rs *test-suite*)
    (with-transaction ()
      (make-instance 'broken :data "foo1")
      (make-instance 'broken :data "foo2")
      (backpack-map-class rs 'broken
                          (lambda (obj)
                            (format t "Found ~A~%" (string-key obj))))))
  (with-backpack (rs *test-suite*)
    (with-transaction ()
      (let (foo1)
        (backpack-map-slot rs 'broken 'string-key
                           (lambda (object) (setq foo1 object))
                           :equal "foo1")
        (assert foo1)
        (backpack::backpack-delete-object rs foo1)
        (format t "Deleted foo1~%")
        (backpack-map-class rs 'broken
                            (lambda (obj)
                              (format t "Found ~A~%" (string-key obj))))
        (let ((index (backpack-class-index rs 'broken :errorp t)))
          (check-order (rs::index-data index)))))))

