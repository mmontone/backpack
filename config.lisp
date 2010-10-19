(in-package :backpack)

(defvar *auto-commit-children* nil
  "If automatically commit a transaction's child if the child has not been committed when committing a transaction")

(defun start-logging ()
  (start-sender 'all  
		(stream-sender :location *error-output*)  
		:category-spec '(transaction
				 cache
				 mop
				 index
				 schema
				 ;serialize
				 gc)  
		:output-spec '(message)))

(defun stop-logging ()
  (stop-sender 'all))