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
  "Length of the longest command name.")

(defvar *script* nil
  "Whether or not to output in a machine-readable format.")

(defmacro call-command (name &rest args)
  (let ((command-name (make-command-name (symbol-name name))))
    `(,command-name ',args)))

(defmacro defcommand (name (&key (min-args 0) (max-args nil)) sdoc ldoc
                      &body body)
  "Define a new command usable on the command-line."
  (let* ((sname (string-downcase (symbol-name name)))
         (command-name (make-command-name (symbol-name name))))
    `(progn
       (defun ,command-name (args)
         ,sdoc
         (let ((min-args ,min-args)
               (max-args ,max-args)
               (num-args (length args)))
           (cond
             ((< num-args min-args)
              (format t "Too few arguments, need at least ~D, got ~D~%"
                      min-args num-args)
              (call-command help ,sname))
             ((and max-args (> num-args max-args))
              (format t "Too many arguments, need at most ~D, got ~D~%"
                      max-args num-args)
              (call-command help ,sname))
             (t ,@body))))
       (setf *help-messages*
             (nconc *help-messages* '((,sname ,sdoc ,ldoc)))
             *max-command-name-length*
             (max *max-command-name-length* (length ,sname))))))

(defconstant *version* "0.1.0"
  "Clark's version.")

(defun add-tags (url-or-id tags)
  "Add tags to the bookmark_tag table and possibly to tag."
  (when url-or-id
    (if (integerp url-or-id)
        (map nil (lambda (tag)
                   (let ((tag-id (handler-case (insert-tag tag)
                                   (sqlite-error () (get-tag-id tag)))))
                     (insert-bookmark-tag url-or-id tag-id))) tags)
        (add-tags (get-bookmark-id url-or-id) tags))))

(defun clear-tags (url-or-id)
  "Remove all tags from the bookmark URL."
  (when url-or-id
    (if (integerp url-or-id)
        (execute-non-query
         *db* "DELETE FROM bookmark_tag WHERE bookmark_id = ?" url-or-id)
        (clear-tags (get-bookmark-id url-or-id)))))

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

The result contains the url, name and the description of the bookmark."
  (execute-to-list *db* "select url, name, description from bookmark"))

(defun get-bookmark-id (url)
  "Get the id of the bookmark for URL."
  (execute-single *db* "SELECT rowid FROM bookmark WHERE url = ?" url))

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
  (loop while (and args (char= (char (car args) 0) #\-))
     do (case (intern (string-upcase (string-left-trim "-" (car args)))
                      :org.ryuslash.clark)
          (script (setf *script* t args (cdr args)))))
  (if args
      (let ((cmd-name (make-command-name (car args))))
        (if (fboundp cmd-name)
            (funcall cmd-name (cdr args))
            (progn
              (format t "Unknown command: ~A~%" (car args))
              (call-command help))))
      (map nil #'print-bookmark (get-bookmarks))))

(defun print-bookmark (bm)
  "Print information about bookmark BM.

BM should be a list containing the url, name and description of the
bookmark."
  (destructuring-bind (url name description) bm
    (if *script*
        (format t "~A~A~A" name description url)
        (format t "~A~%  ~A~%  ~A~%~%" url name description))))

(defcommand add (:min-args 3)
    "Add a new bookmark."
    "Usage: clark add <url> <name> <description> [<tags> ...]

Add URL with NAME, DESCRIPTION and TAGS to the database. TAGS may be
omitted or any number of tag names."
  (with-transaction *db*
    (destructuring-bind (url name description &rest tags) args
      (insert-bookmark url name description)
      (add-tags (last-insert-rowid *db*) tags))))

(defcommand edit (:min-args 3)
    "Edit a bookmark."
    "Usage: clark edit <url> [--name <name>] \\
                        [--description <description>]

Edit the information for URL, specifying which part(s) to edit. Each
option will replace the previous value for that part."
  (let ((name-lst (member "--name" args :test #'string=))
        (desc-lst (member "--description" args :test #'string=))
        query qargs)
    (when name-lst
      (setf query (concatenate 'string query "name = ? ")
            qargs (nconc qargs (list (cadr name-lst)))))
    (when desc-lst
      (setf query (concatenate 'string query (when qargs ", ")
                               "description = ? ")
            qargs (nconc qargs (list (cadr desc-lst)))))
    (when qargs
      (apply #'execute-non-query *db*
             (format nil "UPDATE bookmark SET ~A WHERE url = ?" query)
             (append qargs (list (car args)))))))

(defcommand exists (:min-args 1 :max-args 1)
    "Check if a bookmark exists in the database."
    "Usage: clark exists <url>

Check if URL exists in the database. Prints `yes' when found and `no'
otherwise."
  (format t "~:[no~;yes~]~%"
          (execute-single
           *db* "SELECT rowid FROM bookmark WHERE url = ?" (car args))))

(defcommand help (:max-args 1)
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
      (call-command help "help")))

(defcommand remove (:min-args 1 :max-args 1)
    "Remove a bookmark from the database."
    "Usage: clark remove <url>

Remove URL from the database."
  (clear-tags (car args))
  (execute-non-query
   *db* "DELETE FROM bookmark WHERE url = ?" (car args)))

(defcommand search (:min-args 1 :max-args 1)
    "Search through bookmarks."
    "Usage: clark search <str>

Search the database for STR. Matches are made for substrings of a
bookmark's name or an exact match for a tag."
  (map nil #'print-bookmark
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

(defcommand set-tags (:min-args 1)
    "Set a bookmark's tags."
    "Usage: clark set-tags <url> [<tags> ...]

Set bookmark URL's tags to the given list, overwriting the previous
list of tags."
  (clear-tags (car args))
  (add-tags (car args) (cdr args)))

(defcommand version (:max-args 0)
    "Show version."
    "Usage: clark version

Print the version number and exit."
  (format t "clark version ~A~%" *version*))

(defun clark (args)
  "Main function.

Connect to the database, parse command-line arguments, execute and
then disconnect."
  (load-db)
  (parse-args (cdr args))
  (disconnect *db*))
