(in-package :backpack)

(defvar *auto-commit-children* nil
  "If automatically commit a transaction's child if the child has not been committed when committing a transaction")

(defvar *default-logging-categories*
  '(transaction
    cache
    mop
    index
    schema
					;serialize
    gc))

(defun start-logging (&optional (categories *default-logging-categories*))
  (start-sender 'all  
		(stream-sender :location *error-output*)  
		:category-spec categories
		:output-spec '(message)))

(defun stop-logging ()
  (stop-sender 'all))