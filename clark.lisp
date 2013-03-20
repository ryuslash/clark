(in-package :org.ryuslash.clark)

(export '(clark))

(defvar *db* nil
  "The database connection.")

(defun db-connect (name)
  "Connect to the database, possibly creating it."
  (let ((db-exists (probe-file name)))
    (connect (list name) :database-type :sqlite3)
    (unless db-exists
      (create-view-from-class 'bookmark)
      (create-view-from-class 'tag)
      (create-view-from-class 'bookmark-tag))))

(defun print-bookmark (bm)
  "Print information about bookmark BM.

BM should be a list containing the url and name of the bookmark."
  (destructuring-bind (url name) bm
    (format t "~A~%~A~%~%" url name)))

(defun get-bookmarks ()
  "Get a list of all bookmarks.

The result contains the url and the name of the bookmark."
  (loop
     with statement = (prepare-statement
                       *db* "select url, name from bookmark")
     while (step-statement statement)
     collect (list (statement-column-value statement 0)
                   (statement-column-value statement 1))
     finally (finalize-statement statement)))

(defun clark ()
  (setf *db* (connect "test.db"))
  (map nil #'print-bookmark (get-bookmarks))
  (disconnect *db*))
