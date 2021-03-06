(in-package :backpack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import/export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The contents of a backpack can be exported to a single file.  The file will
;; contain enough information to reconstruct the original backpack objects.
;; Backpack export files use a relatively simple s-expression format.
;;
;; There are two reasons for exporting a backpack:
;; - backup
;;   The export file has a simple format, so it's a lot less sensitive
;;   to data corruption bugs.
;; - migration
;;   Export files can be imported by newer versions of Backpack.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import/export API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric export-backpack (backpack pathname)
  (:documentation "Export all objects in a backpack to a file.  The
resulting file can be imported by newer versions of Backpack."))

(defun import-backpack (pathname directory-designator
                        &rest args
                        &key (if-exists :error)
                        &allow-other-keys)
  "Creates a new backpack in the directory specified by
DIRECTORY-DESIGNATOR, opens the new backpack and imports all objects
that were exported to the file specified by PATHNAME."
  (declare (ignore pathname directory-designator if-exists args))
  (error "Not implemented yet"))



