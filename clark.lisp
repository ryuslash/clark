(in-package :org.ryuslash.clark)

(export '(clark))

(defvar *db* nil
  "The database connection.")

(defconstant *version* "0.1.0"
  "Clark's version.")

(defun check-db (name)
  "Connect to the database, possibly creating it."
  (let ((db-exists (probe-file name)))
    (setf *db* (connect name))
    (unless db-exists
      (execute-non-query *db* "CREATE TABLE bookmark (url VARCHAR(255) UNIQUE, date INTEGER, name VARCHAR(255), description TEXT)")
      (execute-non-query *db* "CREATE TABLE tag (name VARCHAR(255) UNIQUE);")
      (execute-non-query *db* "CREATE TABLE bookmark_tag (bookmark_id INTEGER REFERENCES bookmark(rowid), tag_id INTEGER REFERENCES tag(rowid), PRIMARY KEY (bookmark_id, tag_id))"))))

(defun get-bookmarks ()
  "Get a list of all bookmarks.

The result contains the url and the name of the bookmark."
  (let ((statement
         (prepare-statement *db* "select url, name from bookmark")))
    (loop
       while (step-statement statement)
       collect (list (statement-column-value statement 0)
                     (statement-column-value statement 1))
       finally (finalize-statement statement))))

(defun help-command (args)
  "Show a help message."
  (format t (concatenate
             'string
             "Usage: clark [command [options]...]~%"
             "~%"
             "Possible commands:~%"
             "~%"
             "help     Display this help and exit~%"
             "version  Output version information and exit~%")))

(defun make-command-name (base)
  "Turn BASE into the name of a possible command."
  (intern (concatenate 'string (string-upcase base) "-COMMAND")
          :org.ryuslash.clark))

(defun print-bookmark (bm)
  "Print information about bookmark BM.

BM should be a list containing the url and name of the bookmark."
  (destructuring-bind (url name) bm
    (format t "~A~%~A~%~%" url name)))

(defun version-command (args)
  "Display clark's version number."
  (format t "clark version ~A~%" *version*))

(defun clark (args)
  "Main function.

Connect to the database, parse command-line arguments, execute and
then disconnect."
  (check-db "test2.db")
  (if (> (length args) 1)
      (let* ((cmd-name (make-command-name (cadr args))))
        (when (fboundp cmd-name) (funcall cmd-name (cdr args))))
      (map nil #'print-bookmark (get-bookmarks)))
  (disconnect *db*))
