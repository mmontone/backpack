(in-package :backpack-test)

;; You can run all basic unit tests with:
;;
;;   (in-package :backpack-test)
;;   (run-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic unit tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In many of the tests, I use multiple with-transaction forms.  This is to
;; force the opening and closing of the test backpack, and hopefully ensure 
;; that we are actually persisting data.

(defparameter *backpack-unit-tests* #p"/tmp/backpack-unit-tests/")

(defmacro with-backpack-and-transaction ((&key (if-exists :overwrite))
                                         &body body)
  `(with-backpack (rs *backpack-unit-tests* :if-exists ,if-exists)
     (with-transaction ()
       ,@body)))

(define-test serialization-basics
  ;; Test basic serialization of Rucksack
  (let ((store (merge-pathnames *backpack-unit-tests* "store")))
    (backpack::save-objects (list store) store)
    (assert-equal (list store) (backpack::load-objects store))))

(define-test basic-p-cons
  ;; Basic functions of Rucksack's P-CONS.
  (with-backpack-and-transaction ()
    (let ((pc (p-cons 1 2)))
      (assert-true (and (= 1 (p-car pc)) 
                        (= 2 (p-cdr pc))))
      (assert-true (p-eql 1 1))
      (assert-false (p-eql (p-cons 1 2) pc))
      (assert-equal 3 (setf (p-car pc) 3))
      (assert-equal 4 (setf (p-cdr pc) 4))
      (assert-true (and (= 3 (p-car pc)) (= 4 (p-cdr pc)))))))

(define-test basic-p-array
  ;; Basic functions of Rucksack's P-ARRAY.
  (with-backpack-and-transaction ()
    (let ((array (p-make-array 10)))
      (assert-true (not (null array)))
      (assert-equal (list 10) (p-array-dimensions array))
      (assert-equal 1 (setf (p-aref array 0) 1))
      (assert-equal 1 (p-aref array 0))
      (assert-equal 10 (p-length array))
      (assert-equal 1 (p-aref 
                       (p-replace array (p-make-array 3 :initial-contents '(1 2 3))) 0))
      (assert-equal 3 (p-find 3 array))
      (assert-false (p-find t array))
      (assert-error 'error (p-aref array -1)))))

(define-test basic-p-list
  ;; Basic persistent list functions.
  (with-backpack-and-transaction ()
    (flet ((unwrap (l) (unwrap-persistent-list l)))
      (let ((plist (p-list 0 1 2 3 4 5 6 7 8 9))
	    (rlist   (list 0 1 2 3 4 5 6 7 8 9)))
	(assert-equal 10 (p-length plist))
	(assert-equal rlist (unwrap plist))
	(assert-equal 3 (p-find 3 plist))
	(assert-false (p-find t plist))
	(assert-equal (member-if #'evenp rlist)
                      (unwrap (p-member-if #'evenp plist)))
	(assert-false (p-member-if (constantly nil) plist))
	(assert-equal (delete-if #'oddp rlist :start 1 :count 3)
                      (unwrap (p-delete-if #'oddp plist :start 1 :count 3)))
	(assert-equal (delete-if #'oddp rlist)
                      (unwrap (p-delete-if #'oddp plist)))
        (assert-equal 1 (p-position 2 plist))

	; mapping functions
	(assert-equal (mapl #'identity rlist)
                      (unwrap (p-mapl #'identity plist)))
        (assert-equal (mapcar #'identity rlist)
                      (unwrap (p-mapcar #'identity plist)))
        (assert-equal (maplist #'identity rlist)
                      (mapcar #'unwrap
                              (unwrap (p-maplist #'identity plist))))
        #+(or)
        ;; DO: Implement P-REPLACE for persistent lists.
        (assert-equal (replace rlist (list 4 5 6))
                      (unwrap (p-replace plist (p-list 4 5 6))))))))


(defclass basic-persist ()
  ((data :initarg :data :accessor data)
   (cached :initform nil :initarg :cached :accessor cached :persistence nil))
  (:metaclass persistent-class))

(define-test basic-persistence 
  "Tests basic objects existing over an open/close of a backpack"
  (with-backpack-and-transaction (:if-exists :supersede)
    (add-backpack-root (p-cons 1 2) rs)
    (add-backpack-root (p-make-array 3 :initial-contents '(1 2 3)) rs)
    (add-backpack-root (make-instance 'basic-persist 
                                      :data "foo"
                                      :cached t)
                       rs))
  ;; Reopen the backpack.
  (with-backpack-and-transaction ()
    (let ((roots (backpack-roots rs)))
      (assert-equal 3 (length roots))
      (dolist (r roots)
	(typecase r
	  (persistent-cons 
	   (assert-equal (list 1 2) (list (p-car r) (p-cdr r))))
	  (persistent-array
	   (assert-equal '(3) (p-array-dimensions r))
	   (assert-equal '(1 2 3)
                         (list (p-aref r 0)
                               (p-aref r 1)
                               (p-aref r 2))))
	  (basic-persist
	   (assert-equal "foo" (data r))
	   (assert-error 'unbound-slot (cached r))))))))



(defun make-multiple-instances (class-type data)
  (mapcar (lambda (d) (make-instance class-type :data d))
          data))

(defun find-indexed (backpack class data)
  (let (result)
    (backpack-map-slot backpack class 'data
		       (lambda (obj) (setf result obj))
		       :equal data)
    result))

(defun delete-object (backpack class data)
  (backpack::backpack-delete-object backpack (find-indexed backpack class data)))

(defun ensure-exists (backpack class data)
  (every (lambda (d)
	   (find-indexed backpack class d))
	 data))

(defun count-instances (backpack class)
  (let ((count 0))
    (backpack-map-class backpack class
			(lambda (obj)
			  (declare (ignore obj))
			  (incf count)))
    count))

(defmacro indexed-class-maker (name)
  `(defclass ,name ()
     ((data :initarg :data
	    :index ,(intern (string-upcase (format nil "~A" name)) :keyword)))
     (:index t)
     (:metaclass persistent-class)))

(defun make-indexed-test-instances ()
  (indexed-class-maker number-index)
  (indexed-class-maker string-index)
  (indexed-class-maker symbol-index)
  (indexed-class-maker case-insensitive-string-index)
  (indexed-class-maker trimmed-string-index)

  (make-multiple-instances 'number-index '(1 2 3 4))
  (make-multiple-instances 'string-index '("foo" "bar" "baz" "frob"))
  (make-multiple-instances 'symbol-index '(foo bar baz frob))
  (make-multiple-instances 'case-insensitive-string-index '("Foo" "Bar" "Baz" "Frob"))
  (make-multiple-instances 'trimmed-string-index '(" foo " " bar" "baz " "frob")))


(define-test indexed-persistence
  ;; Open a clean backpack.
  (with-backpack-and-transaction (:if-exists :supersede)
    (make-indexed-test-instances))
  ;; Close the backpack, reopen below.
  (with-backpack-and-transaction ()
    (assert-true (ensure-exists rs 'number-index '(1 2 3 4)))
    (assert-false (ensure-exists rs 'number-index '(5)))
    
    (assert-true (ensure-exists rs 'string-index '("foo" "bar" "baz" "frob")))
    (assert-false (ensure-exists rs 'string-index '("food")))
    
    (assert-true (ensure-exists rs 'symbol-index '(foo bar baz frob)))
    (assert-false (ensure-exists rs 'symbol-index '(food)))
    
    (assert-true (ensure-exists rs 'case-insensitive-string-index '("fOO" "bAr" "bAz" "fROb")))
    (assert-false (ensure-exists rs 'case-insensitive-string-index '("food")))
    
    (assert-true (ensure-exists rs 'trimmed-string-index '("foo " "  bar" "baz " "frob")))
    (assert-false (ensure-exists rs 'trimmed-string-index '("food")))
    
    (dolist (class '(number-index symbol-index
                     string-index case-insensitive-string-index trimmed-string-index))
      (assert-equal 4 (count-instances rs class)))))


(define-test basic-deletion
  (with-backpack-and-transaction (:if-exists :supersede)
    (dotimes (x 4)
      (add-backpack-root (p-cons x x) rs)))
  (with-backpack-and-transaction ()
    (let ((roots (backpack-roots rs)))
      (assert-equal 4 (length roots))
      (assert-true (backpack::backpack-root-p (car roots) rs))
      (backpack::delete-backpack-root (car roots) rs)))
  (with-backpack-and-transaction ()
    (assert-equal 3 (length (backpack-roots rs)))))

(define-test indexed-deletion
  (with-backpack-and-transaction (:if-exists :supersede)
    (make-indexed-test-instances))
  (with-backpack-and-transaction ()
    (assert-true (find-indexed rs 'number-index 1))
    (delete-object rs 'number-index 1)
    (assert-false (find-indexed rs 'number-index 1))
    
    (assert-true (find-indexed rs 'symbol-index 'foo))
    (delete-object rs 'symbol-index 'foo)
    (assert-false (find-indexed rs 'symbol-index 'foo))
    
    ;; The tests below caused LEAF-DELETE-KEY to fail (fixed in version 0.1.11).
    (assert-true (find-indexed rs 'string-index "foo"))
    (delete-object rs 'string-index "foo" )
    (assert-false (find-indexed rs 'string-index "foo"))
    ;;
    (assert-true (find-indexed rs 'case-insensitive-string-index "foo"))
    (delete-object rs 'case-insensitive-string-index "foo")
    (assert-false (find-indexed rs 'case-insensitive-string-index "foo"))
    ;;
    (assert-true (find-indexed rs 'trimmed-string-index "foo"))
    (delete-object rs 'trimmed-string-index "foo" )
    (assert-false (find-indexed rs 'trimmed-string-index "foo"))))


(define-test basic-rollback
  (with-backpack-and-transaction (:if-exists :supersede)
    (add-backpack-root (p-cons 1 2) rs))
  (with-backpack-and-transaction ()
    (let ((pc (first (backpack-roots rs))))
      (setf (p-car pc) 4)
      ;; Abort the transaction.  WITH-TRANSACTION will take care of
      ;; calling TRANSACTION-ROLLBACK.
      (abort)))
  (with-backpack-and-transaction ()
    (let ((pc (car (backpack-roots rs))))
      (assert-equal 1 (p-car pc))))
  ;; Test that transactions are also rolled back when we throw an
  ;; error inside the body of a WITH-TRANSACTION form.
  (assert-error 'error
                (with-backpack-and-transaction ()
                  (let ((pc (first (backpack-roots rs))))
                    (setf (p-car pc) 5)
                    ;; Abort the transaction by causing an error.
                    (error "Something went wrong"))))
  (with-backpack-and-transaction ()
    ;; Verify that the error caused a transaction rollback.
    (let ((pc (car (backpack-roots rs))))
      (assert-equal 1 (p-car pc)))))


