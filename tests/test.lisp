;; $Id: test.lisp,v 1.2 2008-02-11 12:47:53 alemmens Exp $

(in-package :backpack-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A few quick tests to make sure the basics work.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-suite* #p"/tmp/backpack-test-suite/")

(defmacro p-test (form test)
  `(progn
     (with-backpack (in *test-suite* :if-exists :supersede)
       (with-transaction ()
         (add-backpack-root ,form in)))
     (with-backpack (out *test-suite* :if-exists :overwrite)
       (with-transaction ()
         (let ((all (backpack-roots out)))
           (assert (= 1 (length all)))
           (let ((it (car all)))
             (assert ,test)))))))

(defmacro test (form)
  `(assert ,form))

(defclass p-thing-1 ()
  ()
  (:metaclass persistent-class))
  
(defclass p-thing-2 ()
  ((x :initarg :x :reader x-of :persistence t))
  (:metaclass persistent-class))
  
(defun test-basics ()
  ;;
  ;; Serializing/deserializing pathnames.
  ;;

  (let ((store (merge-pathnames *test-suite* "store")))
    (backpack::save-objects (list store) store)
    (test (equal (list store) (backpack::load-objects store))))

  (test (not (current-backpack)))

  ;;
  ;; P-CONS, P-CAR, P-CDR, P-LIST, P-MAKE-ARRAY, P-AREF
  ;;

  (p-test (p-cons 1 2)
          (and (= 1 (p-car it)) (= 2 (p-cdr it))))
  
  (test (not (current-backpack))) ; WITH-RUCKSACK should not leave one around
  
  (p-test (p-list 1 2 3)
	  (equal '(1 2 3)
		 (list (p-car it) (p-car (p-cdr it)) (p-car (p-cdr (p-cdr it))))))
  
  (p-test (p-make-array 2 :initial-contents '(a b))
	  (equal '(a b)
	       (list (p-aref it 0) (p-aref it 1))))


  ;;
  ;; Persistent-objects
  ;;
  
  (p-test (make-instance 'p-thing-1)
	  (eq (find-class 'p-thing-1) (class-of it)))
  
  (p-test (make-instance 'p-thing-2 :x "-x-")
	  (equal (x-of it) "-x-"))

  ;;
  ;; Btree basics
  ;;

  (p-test (let ((btree (make-instance 'btree)))
            (btree-insert btree 0 'zero)
            (btree-insert btree 15 'fifteen)
            (btree-insert btree 10 'ten)
            btree)
          (equal (list (btree-search it 0)
                       (btree-search it 10)
                       (btree-search it 15)
                       (btree-search it 42 :errorp nil))
                 '(zero ten fifteen nil)))

  (test (not (current-backpack)))
  (write-line "basic tests ok"))

(eval-when (:load-toplevel)
  (test-basics))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-objects ()
  ;; P-DELETE-IF
  (p-test (p-list 1 2 3 4 5)
          (equal '(1 3 5)
                 (unwrap-persistent-list (p-delete-if #'evenp it))))
  (p-test (p-list 1 2 3 4 5)
          (equal '(2 4)
                 (unwrap-persistent-list (p-delete-if #'oddp it))))
  (p-test (p-list 1 2 4 6)
          (equal '(1)
                 (unwrap-persistent-list (p-delete-if #'evenp it ))))
  (p-test (p-list 1 2 3 4 5)
          (equal '()
                 (unwrap-persistent-list (p-delete-if (constantly t) it ))))
  (p-test (p-list 1 2 3 4 5)
          (equal '(3 4 5)
                 (unwrap-persistent-list (p-delete-if (constantly t) it :count 2))))
  (p-test (p-list 1 2 3 4 5)
          (equal '(1 2 3 4 5)
                 (unwrap-persistent-list (p-delete-if (constantly t) it :count 0))))
  ;; DO: We need a lot more tests here for other functions like
  ;; P-MEMBER-IF, P-FIND, P-REPLACE, etcetera.
  :ok)
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test basic create, load and update functionality with many objects, so
;;; the incremental garbage collector needs to do some work too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *names* '("David" "Jim" "Peter" "Thomas"
                        "Arthur" "Jans" "Klaus" "James" "Martin"))

(defclass person ()
  ((name :initform (elt *names* (random (length *names*)))
         :accessor name)
   (age :initform (random 100) :accessor age))
  (:metaclass persistent-class))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type t)
    (format stream "called ~S of age ~D"
            (name person)
            (age person))))

(defun test-create (&key (nr-objects 100000))
  "Test creating a backpack with many persons."
  (with-backpack (backpack *test-suite* :if-exists :supersede)
    (with-transaction ()
      (loop for i below nr-objects
            do (let ((person (make-instance 'person)))
                 (when (zerop (mod i 1000))
                   (format t "~D " i))
                 (add-backpack-root person backpack))))))

  
(defun test-update (&key (new-age 27))
  "Test updating all persons by changing their age."
  (with-backpack (backpack *test-suite*)
    (with-transaction ()
      (map-backpack-roots (lambda (person)
                            (setf (age person) new-age))
                          backpack))))

(defun test-load ()
  "Test loading all persons by computing their average age."
  (with-backpack (backpack *test-suite*)
    (with-transaction ()
      (let ((nr-persons 0)
            (total-age 0))
        (map-backpack-roots (lambda (person)
                              (incf nr-persons)
                              (incf total-age (age person)))
                            backpack)
        ;; Return the average age as a float.
        ;; (An average age of 1200/75 doesn't seem right.)
        (coerce (/ total-age nr-persons) 'float)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Some transactions semantics tests.
;;

(defun test-transaction-nesting ()
  (with-backpack (backpack *test-suite* :class 'standard-backpack)
    (with-transaction ()
      (with-transaction ()
	(map-backpack-roots (lambda (person)
                            (setf (age person) 25))
                          backpack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Btrees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Test btrees as just another persistent data structure.
;;

(defparameter *format-strings* 
  ;; Different ways of printing integers.
  '("~R" "~:R" "... ~R" "~D"))

(defun shuffle (array)
  (loop with n = (array-dimension array 0)
        repeat n
        for i = (random n)
        for j = (random n)
        when (/= i j)
        do (rotatef (aref array i) (aref array j))))


(defun check-size (btree expected)
  (format t "~&Counting~%")
  (let ((count (btree-nr-values btree)))
    (unless (=  count expected)
      (error "Wrong btree size - expected ~A, got ~A."
             expected count))))

(defun check-order (btree)
  (format t "~&Checking order and balance~%")
  (bp::check-btree btree)
  (format t " and keys~%")
  (bp::check-bnode-keys btree (bp::btree-root btree)))

(defun check-contents (btree)
  (format t "~&Checking contents~%")
  (map-btree btree
             (lambda (key value)
               (unless (string= value (format nil "~R" key))
                 (error "Value mismatch: Expected ~S, got ~S."
                        (format nil "~R" key) value)))))

(defmacro with-transaction* ((&rest args) &body body)
  `(with-transaction ,args
     (prog1 (progn ,@body)
       (format t "~&Committing..."))))

(defun test-btree (&key (n 20000) (node-size 100) (delete (floor n 10))
                        check-contents)
  ;; Create a backpack with a btree of size N that maps random
  ;; integers to the equivalent strings as a cardinal English number.
  ;; Use node size NODE-SIZE for the btree.
  ;; If DELETE is not NIL, delete and reinsert that number of elements
  ;; as well.
  (let ((array (make-array n :initial-contents (loop for i below n collect i))))
    (shuffle array)
    (with-backpack (backpack *test-suite* :if-exists :supersede)
      (with-transaction* ()
        (format t "~&Inserting~%")
        (let ((btree (make-instance 'btree :value= 'string-equal
                                    :max-node-size node-size)))
          (loop for key across array
                for i from 1
                when (zerop (mod i 1000))
                do (format t "~D " i)
                do (btree-insert btree key
                                 (format nil (first *format-strings*) key)))
          (add-backpack-root btree backpack))))
    (with-backpack (backpack *test-suite*)
      (with-transaction ()
        (let ((btree (first (backpack-roots backpack))))
          (check-order btree)
          (check-size btree n)
          (when check-contents
            (check-contents btree))))
      (when delete
        (shuffle array)
        (setq array (subseq array 0 delete))
        (shuffle array)
        (with-transaction* ()
          (format t "~&Deleting~%")
          (let ((btree (first (backpack-roots backpack))))
            (dotimes (i delete)
              (when (zerop (mod (1+ i) 100))
                (format t "~D " (1+ i)))
              (btree-delete-key btree (aref array i)))
            (check-order btree)
            (check-contents btree)))
        (with-transaction* ()
          (let ((btree (first (backpack-roots backpack))))
            (check-order btree)
            (check-size btree (- n delete))
            (when check-contents
              (check-contents btree))
            (format t "~&Reinserting~%")
            (shuffle array)
            (dotimes (i delete)
              (when (zerop (mod (1+ i) 1000))
                (format t "~D " (1+ i)))
              (let ((key (aref array i)))
                (btree-insert btree key (format nil "~R" key))))))
        (with-transaction ()
          (let ((btree (first (backpack-roots backpack))))
            (check-order btree)
            (check-size btree n)
            (when check-contents
              (check-contents btree)))))))
  :ok)

;;
;; Btrees with non-unique keys

(defun check-non-unique-contents (btree)
  (format t "~&Checking contents~%")
  (map-btree btree
             (lambda (key value)
               (let ((strings (loop for format-string in *format-strings*
                                    collect (format nil format-string key))))
                 (unless (member value strings :test #'string-equal)
                   (error "Value mismatch: Expected one of ~S for ~S, got ~S."
                          strings key value))))))


(defun test-non-unique-btree (&key (n 20000) (node-size 100) (delete (floor n 10))
                                   check-contents)
  ;; Create a backpack with a btree of size N (N must be a multiple of 4) that
  ;; maps random integers to four different equivalent strings (in Roman and
  ;; English notation).
  ;; Use node size NODE-SIZE for the btree.
  ;; If DELETE is not NIL, it must be a multiple of 4; delete that number of
  ;; elements as well.
  (let* ((nr-formats (length *format-strings*))
         (array-size (floor n nr-formats))
         (array (make-array array-size
                            :initial-contents (loop for i from 1 to array-size collect i))))
    (assert (zerop (mod n nr-formats)))
    (assert (zerop (mod delete nr-formats)))
    (shuffle array)
    (with-backpack (backpack *test-suite* :if-exists :supersede)
      (with-transaction* ()
        (format t "~&Inserting~%")
        (let ((btree (make-instance 'btree :value= 'string-equal
                                    :max-node-size node-size
                                    :unique-keys-p nil)))
          (loop for key across array
                for i from 1
                when (zerop (mod i 200))
                do (format t "~D " i)
                do (loop for format-string in *format-strings*
                         do (btree-insert btree key (format nil format-string key))))
          (add-backpack-root btree backpack))))
    (with-backpack (backpack *test-suite*)
      (with-transaction ()
        (let ((btree (first (backpack-roots backpack))))
          (check-order btree)
          (check-size btree n)
          (when check-contents
            (check-non-unique-contents btree))))
      (when delete
        (shuffle array)
        (setq array (subseq array 0 (floor delete nr-formats)))
        (shuffle array)
        (with-transaction* ()
          (format t "~&Deleting~%")
          (let ((btree (first (backpack-roots backpack))))
            (loop for i below (floor delete nr-formats)
                  do (loop for j below nr-formats
                           do (when (zerop (mod (+ j (* nr-formats i)) 10))
                                (format t "~D " (+ j (* nr-formats i))))
                           do (let* ((key (aref array i))
                                     (from-end (oddp key))
                                     (index (if from-end
                                                j
                                              (- nr-formats (1+ j))))
                                     (format-string (elt *format-strings* index))
                                     (value (format nil format-string key)))
                                (btree-delete btree key value
                                              :if-does-not-exist :error))))
            (check-order btree)
            (check-size btree (- n delete))
            (check-non-unique-contents btree)))
        (with-transaction* ()
          (let ((btree (first (backpack-roots backpack))))
            (check-order btree)
            (check-size btree (- n delete))
            (when check-contents
              (check-non-unique-contents btree))
            (format t "~&Reinserting~%")
            (shuffle array)
            (dotimes (i (floor delete nr-formats))
              (when (zerop (mod (1+ i) 10))
                (format t "~D " (1+ i)))
              (let ((key (aref array i)))
                (loop for format-string in *format-strings*
                      do (btree-insert btree key (format nil format-string key)))))))
        (with-transaction ()
          (let ((btree (first (backpack-roots backpack))))
            (check-order btree)
            (check-size btree n)
            (when check-contents
              (check-non-unique-contents btree)))))))
  :ok)

(defun btree-stress-test (&key (n 1000))
  (loop for i below n
        do (print i)
        do (test-non-unique-btree :n 1600 :node-size 10 :delete 1500)))

(defun test-btree-map (&key (display t) min max include-min include-max
                            (order :ascending))
  ;; Print out the contents of the btree.
  (with-backpack (backpack *test-suite*)
    (with-transaction ()
      (let ((btree (first (backpack-roots backpack))))
        (map-btree btree
                   (lambda (key value)
                     (when display
                       (format t "~&~D -> ~A~%" key value)))
                   :min min
                   :include-min include-min
                   :max max
                   :include-max include-max
                   :order order)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Garbage collector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-gc (n)
  ;; This used to fail for large values of N (e.g. 10,000).
  (with-backpack (backpack *test-suite* :if-exists :supersede)
    (with-transaction ()
      ;; after this, INNER can be reached directly from the root
      (let* ((inner (p-cons "Waldorf" "Statler"))
             (root (p-cons 42 inner)))
        (add-backpack-root root backpack)))
    (with-transaction ()
      (let* ((root (first (backpack-roots backpack)))
             (inner (p-cdr root))
             (array (p-make-array n)))
        ;; after this, INNER can't be reached from the root anymore
        (setf (p-cdr root) 43)
        ;; now let the GC do some work
        (dotimes (i n)
          (let ((string (format nil "~R" i)))
            (setf (p-aref array i) (p-cons string string))))
        ;; hook INNER back to the root again before we finish the
        ;; transaction
        (setf (p-car root) array
              (p-cdr root) (p-cons 'bar (p-cons 'foo inner)))))
    (with-transaction ()
      (let* ((root (first (backpack-roots backpack)))
             (inner (p-cdr (p-cdr (p-cdr root)))))
        ;; we expect the list ("Waldorf" "Statler") here
        (list (p-car inner) (p-cdr inner))))))
