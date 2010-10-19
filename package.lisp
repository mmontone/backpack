;; $Id: package.lisp,v 1.14 2008-02-19 22:44:06 alemmens Exp $

#-(or allegro lispworks sbcl openmcl)
  (error "Unsupported implementation: ~A" (lisp-implementation-type))

(defpackage :backpack
  (:nicknames :bp)

   (:use :queue :cl :log5
    #+allegro :mop
    #+lispworks :clos
    #+sbcl :sb-mop
    #+openmcl :openmcl-mop)

  (:export

   ;; Cache
   #:cache #:standard-cache
   #:open-cache #:close-cache #:with-cache
   #:cache-size #:cache-count
   #:cache-create-object #:cache-get-object #:cache-touch-object
   #:cache-commit #:cache-rollback #:cache-recover
   #:open-transaction #:close-transaction #:map-transactions

   ;; MOP related
   #:persistent-class
   #:update-persistent-instance-for-redefined-class

   ;; Objects
   #:persistent-object
   #:persistent-data #:persistent-array #:persistent-cons
   #:object-id
   #:p-cons #:p-array
   #:p-eql
   #:p-car #:p-cdr #:p-list #:p-last
   #:p-endp #:p-consp
   #:p-caar #:p-cadr #:p-cdar #:p-cddr
   #:unwrap-persistent-list
   #:p-mapcar #:p-mapc #:p-maplist #:p-mapl
   #:p-member-if
   #:p-pop #:p-push
   #:p-make-array #:p-aref #:p-array-dimensions
   #:p-length #:p-find #:p-replace #:p-delete-if #:p-position

   ;; Heaps
   #:heap #:free-list-heap #:mark-and-sweep-heap #:simple-free-list-heap
   #:open-heap #:close-heap
   #:heap-stream #:heap-end

   ;; Backpacks
   #:*backpack*
   #:open-backpack #:close-backpack #:with-backpack #:current-backpack
   #:backpack #:standard-backpack
   #:backpack-cache
   #:backpack-directory
   #:backpack-commit #:backpack-rollback
   #:add-backpack-root #:map-backpack-roots #:backpack-roots
   #:commit #:rollback

   ;; Class and slot indexing
   #:add-class-index #:add-slot-index
   #:remove-class-index #:remove-slot-index
   #:map-class-indexes #:map-slot-indexes
   #:backpack-add-class-index #:backpack-add-slot-index
   #:backpack-make-class-index
   #:backpack-remove-class-index #:backpack-remove-slot-index
   #:backpack-class-index #:backpack-slot-index
   #:backpack-map-class-indexes #:backpack-map-slot-indexes
   #:backpack-maybe-index-changed-slot #:backpack-maybe-index-new-object
   #:backpack-map-class #:backpack-map-slot
   #:backpack-do-class #:backpack-do-slot
   #:backpack-delete-object

   ;; Transactions
   #:current-transaction

   #:transaction-start #:transaction-commit #:transaction-rollback
   #:with-transaction #:*transaction*
   #:transaction #:standard-transaction
   #:transaction-start-1 #:transaction-commit-1
   #:transaction-id

   ;; Conditions
   #:backpack-error #:simple-backpack-error #:transaction-conflict
   #:internal-backpack-error
   #:duplicate-slot-value #:slot-error 

   ;; Indexes
   #:map-index #:index-insert #:index-delete #:make-index
   #:define-index-spec #:find-index-spec

   ;; Btrees
   #:btree
   #:btree-key< #:btree-key<= #:btree-key= #:btree-key>= #:btree-key>
   #:btree-value=
   #:btree-max-node-size #:btree-unique-keys-p
   #:btree-key-type #:btree-value-type
   #:btree-node-class #:btree-node
   #:btree-nr-keys #:btree-nr-values
   ;; Functions
   #:btree-search #:btree-insert #:btree-delete #:btree-delete-key
   #:map-btree #:map-btree-keys
   ;; Conditions
   #:btree-error #:btree-search-error #:btree-insertion-error
   #:btree-key-already-present-error #:btree-type-error
   #:btree-error-btree #:btree-error-key #:btree-error-value))


