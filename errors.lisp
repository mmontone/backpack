;; $Id: errors.lisp,v 1.3 2007-01-20 18:17:55 alemmens Exp $

(in-package :backpack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rucksack errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition backpack-error (error)
  ((backpack :initarg :backpack :initform (current-backpack)
             :reader backpack)))

(defmethod print-object ((error backpack-error) stream)
  (format stream "Rucksack error in ~A." (backpack error)))

(defun backpack-error (class &rest args)
  (apply #'error class
         :backpack (current-backpack)
         args))

;;
;; Transaction conflict
;;

(define-condition transaction-conflict (backpack-error)
  ((transaction :initarg :transaction :initform (current-transaction)
                :reader transaction)
   (old-transaction :initarg :old-transaction
                    :initform (error "OLD-TRANSACTION initarg required
for transaction-conflict.")
                    :reader old-transaction)
   (object-id :initarg :object-id
              :initform (error "OBJECT-ID initarg required for
transaction-conflict.")
              :reader object-id)))

(defmethod print-object :after ((error transaction-conflict) stream)
  (format stream "~&~A can't modify object #~D, because ~A already
modified it and hasn't committed yet."
          (transaction error)
          (object-id error)
          (old-transaction error)))

;;
;; Simple backpack error
;;

(define-condition simple-backpack-error (backpack-error simple-error)
  ())

(defmethod print-object :after ((error simple-backpack-error) stream)
  (format stream "~&~A~%"
          (apply #'format nil (simple-condition-format-control error)
                 (simple-condition-format-arguments error))))

(defun simple-backpack-error (format-string &rest format-args)
  (backpack-error 'simple-backpack-error
                  :format-control format-string
                  :format-arguments format-args))


;;
;; Internal backpack errors
;;

(define-condition internal-backpack-error (backpack-error simple-error)
  ())

(defmethod print-object :after ((error internal-backpack-error) stream)
  (format stream "~&Internal error: ~A~%"
          (apply #'format nil (simple-condition-format-control error)
                 (simple-condition-format-arguments error))))

(defun internal-backpack-error (format-string &rest format-args)
  (backpack-error 'internal-backpack-error
                  :format-control format-string
                  :format-arguments format-args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition slot-error (backpack-error)
  ;; Q: Maybe this should inherit from CELL-ERROR??
  ((object :initarg :object :reader slot-error-object)
   (slot-name :initarg :slot-name :reader slot-error-name)
   (value :initarg :value :reader slot-error-value)))

(define-condition duplicate-slot-value (slot-error)
  ((other-object :initarg :other-object
                 :reader slot-error-other-object)))

(defmethod print-object :after ((error duplicate-slot-value) stream)
  (format stream
          "Attempt to assign the value ~S to the unique slot ~S of ~S. ~
The value is already present in ~S."
          (slot-error-value error)
          (slot-error-name error)
          (slot-error-object error)
          (slot-error-other-object error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun not-implemented (operator)
  (error "~S not implemented for ~A" operator (lisp-implementation-type)))
