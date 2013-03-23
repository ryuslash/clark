;; Copyright (C) 2013  Tom Willemsen <tom at ryuslash dot org>

;; This file is part of CLark

;; CLark is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; CLark is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with CLark. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(in-package :org.ryuslash.clark)

(export '(clark))

(defvar *db* nil
  "The database connection.")

(defvar *help-messages* nil
  "Help texts for commands.")

(defvar *max-command-name-length* 0
  "Lenght of the longest command name.")

(defmacro call-command (name &rest args)
  (let ((command-name (make-command-name (symbol-name name))))
    `(,command-name ,@args)))

(defmacro defcommand (name sdoc ldoc &body body)
  "Define a new command usable on the command-line."
  (let* ((sname (symbol-name name))
         (command-name (make-command-name sname)))
    `(progn
       (defun ,command-name (args)
         ,sdoc
         ,@body)
       (setf *help-messages*
             (nconc *help-messages*
                    '((,(string-downcase sname) ,sdoc ,ldoc)))
             *max-command-name-length*
             (max *max-command-name-length* (length ,sname))))))

(defconstant *version* "0.1.0"
  "Clark's version.")

(defun add-tags (tags)
  "Add tags to the bookmark_tag table and possibly to tag."
  (let ((bookmark-id (last-insert-rowid *db*)))
    (map nil (lambda (tag)
               (let ((tag-id (handler-case (insert-tag tag)
                               (sqlite-error () (get-tag-id tag)))))
                 (insert-bookmark-tag bookmark-id tag-id))) tags)))

(defun ensure-db-exists (name)
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

(defun get-db-location ()
  "Get the location of the database."
  (pathname
   (apply 'concatenate 'string
          (or (sb-ext:posix-getenv "XDG_DATA_HOME")
              (list (sb-ext:posix-getenv "HOME") "/.local/share"))
          '("/clark/bookmarks.db"))))

(defun get-tag-id (name)
  "Get the rowid of tag NAME."
  (execute-single *db* "SELECT rowid FROM tag WHERE name = ?" name))

(defun help-message ()
  (format t (concatenate
             'string
             "Usage: clark [<command> [<options> ...]]~%"
             "~%"
             "Possible commands:~%"
             "~%"))
  (map nil (lambda (hlp)
             (destructuring-bind (name short long) hlp
               (declare (ignore long))
               (format t "  ~vA  ~A~%" *max-command-name-length*
                       name short))) *help-messages*)
  (format t "~%~A~%"
          (concatenate 'string "Use `clark help <command>' to get more "
                       "information on a command.")))

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

(defun load-db ()
  "Load the database."
  (let ((db-location (get-db-location)))
    (ensure-directories-exist db-location)
    (ensure-db-exists db-location)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun make-command-name (base)
    "Turn BASE into the name of a possible command."
    (intern (concatenate 'string (string-upcase base) "-COMMAND")
            :org.ryuslash.clark)))

(defun parse-args (args)
  "Parse command-line arguments ARGS.

The executable name should already have been removed."
  (let ((cmd-name (make-command-name (car args))))
    (if (fboundp cmd-name)
        (funcall cmd-name (cdr args))
        (progn
          (format t "Unknown command: ~A~%" (car args))
          (call-command help nil)))))

(defun print-bookmark (bm)
  "Print information about bookmark BM.

BM should be a list containing the url and name of the bookmark."
  (destructuring-bind (url name) bm
    (format t "~A~%~A~%~%" url name)))

(defcommand add
    "Add a new bookmark."
    "Usage: clark add <url> <name> <description> [<tags> ...]

Add URL with NAME, DESCRIPTION and TAGS to the database. TAGS may be
omitted or any number of tag names."
  (with-transaction *db*
    (destructuring-bind (url name description &rest tags) args
      (insert-bookmark url name description)
      (add-tags tags))))

(defcommand exists
    "Check if a bookmark exists in the database."
    "Usage: clark exists <url>

Check if URL exists in the database. Prints `yes' when found and `no'
otherwise."
  (if (execute-single *db* "SELECT rowid FROM bookmark WHERE url = ?"
                      (car args))
      (progn
        (format t "yes~%")
        0)
      (progn
        (format t "no~%")
        1)))

(defcommand help
    "Show help message."
    help-message
  (if (> (length args) 0)
      (let ((ldoc
             (nth 2 (car (member
                          (car args) *help-messages*
                          :test #'(lambda (x y) (equal x (car y))))))))
        (cond
          ((null ldoc) (format t "Unkown command: ~A~%" (car args)))
          ((and (symbolp ldoc) (fboundp ldoc)) (funcall ldoc))
          (t (format t "~A~%" ldoc))))
      (call-command help '("help"))))

(defcommand search
    "Search through bookmarks."
    "Usage: clark search <str>

Search the database for STR. Matches are made for substrings of a
bookmark's name or an exact match for a tag."
  (map
   nil (lambda (bm)
         (destructuring-bind (url name description) bm
           (format t "~A~%  ~A~%  ~A~%~%" url name description)))
   (execute-to-list
    *db* (concatenate 'string
                      "SELECT url, name, description "
                      "FROM bookmark "
                      "WHERE name LIKE ? "
                      "OR ? IN (SELECT name "
                      "FROM tag "
                      "JOIN bookmark_tag ON (tag_id = tag.rowid) "
                      "WHERE bookmark_id = bookmark.rowid)")
    (format nil "%~A%" (car args)) (car args))))

(defcommand version
    "Show version."
    "Usage: clark version

Print the version number and exit."
  (declare (ignore args))
  (format t "clark version ~A~%" *version*))

(defun clark (args)
  "Main function.

Connect to the database, parse command-line arguments, execute and
then disconnect."
  (load-db)
  (if (> (length args) 1)
      (parse-args (cdr args))
      (map nil #'print-bookmark (get-bookmarks)))
  (disconnect *db*))
