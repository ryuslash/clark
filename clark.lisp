(in-package :org.ryuslash.clark)

(export '(clark))

(defvar *db* nil
  "The database connection.")

(defconstant *version* "0.1.0"
  "Clark's version.")

(defun add-command (args)
  "Add a new bookmark to the database."
  (with-transaction *db*
    (destructuring-bind (url name description &rest tags) args
      (insert-bookmark url name description)
      (add-tags tags))))

(defun add-tags (tags)
  "Add tags to the bookmark_tag table and possibly to tag."
  (let ((bookmark-id (last-insert-rowid *db*)))
    (map nil (lambda (tag)
               (let ((tag-id (handler-case (insert-tag tag)
                               (sqlite-error () (get-tag-id tag)))))
                 (insert-bookmark-tag bookmark-id tag-id))) tags)))

(defun check-db (name)
  "Connect to the database, possibly creating it."
  (let ((db-exists (probe-file name)))
    (setf *db* (connect name))
    (unless db-exists
      (execute-non-query *db* "CREATE TABLE bookmark (url VARCHAR(255) UNIQUE, date INTEGER, name VARCHAR(255), description TEXT)")
      (execute-non-query *db* "CREATE TABLE tag (name VARCHAR(255) UNIQUE)")
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

(defun get-tag-id (name)
  "Get the rowid of tag NAME."
  (execute-single *db* "SELECT rowid FROM tag WHERE name = ?" name))

(defun help-command (args)
  "Show a help message."
  (format t (concatenate
             'string
             "Usage: clark [<command> [<options> ...]]~%"
             "       clark add <url> <name> <description> [<tags> ...]~%"
             "~%"
             "Possible commands:~%"
             "~%"
             "add      Add a bookmark to the database~%"
             "help     Display this help and exit~%"
             "version  Output version information and exit~%")))

(defun insert-bookmark (url name description)
  "Insert URL, NAME and DESCRIPTION into the bookmark table."
  (execute-non-query *db* "INSERT INTO bookmark VALUES (?, ?, ?, ?)"
                     url (get-universal-time) name description))

(defun insert-bookmark-tag (bookmark-id tag-id)
  "Insert BOOKMARK-ID and TAG-ID into the bookmark_tag table."
  (execute-non-query *db* "INSERT INTO bookmark_tag VALUES (?, ?)"
                     bookmark-id tag-id))

(defun insert-tag (name)
  "Insert tag NAME into the database and return its rowid."
    (execute-non-query *db* "INSERT INTO tag VALUES (?)" name)
    (last-insert-rowid *db*))

(defun make-command-name (base)
  "Turn BASE into the name of a possible command."
  (intern (concatenate 'string (string-upcase base) "-COMMAND")
          :org.ryuslash.clark))

(defun parse-args (args)
  "Parse command-line arguments ARGS.

The executable name should already have been removed."
  (let ((cmd-name (make-command-name (car args))))
    (if (fboundp cmd-name)
        (funcall cmd-name (cdr args))
        (progn
          (format t "Unknown command: ~A~%" (car args))
          (help-command nil)))))

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
      (parse-args (cdr args))
      (map nil #'print-bookmark (get-bookmarks)))
  (disconnect *db*))
