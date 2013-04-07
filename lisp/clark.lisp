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

(defmacro sql (&body body)
  (apply 'concatenate 'string
         (mapcar (lambda (itm) (format nil "~A " itm)) body)))

(defvar *db* nil
  "The database connection.")

(defvar *help-messages* nil
  "Help texts for commands.")

(defvar *max-command-name-length* 0
  "Length of the longest command name.")

(defvar *script* nil
  "Whether or not to output in a machine-readable format.")

(defvar *exit-status* 0
  "The exit status to use on quit.")

(defmacro call-command (name &rest args)
  (let ((command-name (make-command-name (symbol-name name))))
    `(,command-name ,@args)))

(defmacro defcommand (name (&rest args) sdoc ldoc &body body)
  "Define a new command usable on the command-line."
  (let* ((sname (string-downcase (symbol-name name)))
         (command-name (make-command-name (symbol-name name))))
    `(progn
       (defun ,command-name (,@args)
         ,sdoc
         ,@body
         (sb-ext:exit :code *exit-status*))
       (setf *help-messages*
             (nconc *help-messages* '((,sname ,sdoc ,ldoc)))
             *max-command-name-length*
             (max *max-command-name-length* (length ,sname))))))

(defmacro with-error-and-help (code cmd fmt &rest args)
  "Call `with-error-status' with CODE, format FMT with ARGS and call
the help command."
  `(with-error-status ,code
     (format t ,fmt ,@args)
     (call-command help ,cmd)))

(defmacro with-error-status (code &body body)
  "Bind `*exit-status*' to CODE, `*standard-output*' to
`*error-output*' and execute BODY."
  `(let ((*standard-output* *error-output*)
         (*exit-status* ,code))
     ,@body))

(defconstant version "0.1.0"
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
         *db* (sql delete from "bookmark_tag" where "bookmark_id" = ?)
         url-or-id)
        (clear-tags (get-bookmark-id url-or-id)))))

(defun ensure-db-exists (name)
  "Connect to the database, possibly creating it."
  (let ((db-exists (probe-file name)))
    (setf *db* (connect name))
    (unless db-exists
      (execute-non-query
       *db* (sql create table "bookmark" ("url" varchar (255) unique\,
                                          "date" integer\,
                                          "name" varchar (255)\,
                                          "description" text)))
      (execute-non-query
       *db* (sql create table "tag" ("name" varchar (255) unique)))
      (execute-non-query
       *db* (sql create table "bookmark_tag"
                 ("bookmark_id" integer references "bookmark(rowid)"\,
                  "tag_id" integer references "tag(rowid)"\,
                  primary key ("bookmark_id"\,  "tag_id")))))))

(defun get-bookmarks ()
  "Get a list of all bookmarks.

The result contains the url, name and the description of the bookmark."
  (execute-to-list
   *db* (sql select "url, name, description" from "bookmark")))

(defun get-bookmark-id (url)
  "Get the id of the bookmark for URL."
  (execute-single
   *db* (sql select "rowid" from "bookmark" where "url" = ?) url))

(defun get-db-location ()
  "Get the location of the database."
  (let ((xdg (sb-ext:posix-getenv "XDG_DATA_HOME"))
        (home (sb-ext:posix-getenv "HOME")))
    (pathname
     (apply 'concatenate 'string
            (or xdg home)
            (unless xdg "/.local/share")
            '("/clark/bookmarks.db")))))

(defun get-rc-location ()
  "Get the location of the RC file."
  (let ((xdg (sb-ext:posix-getenv "XDG_CONFIG_HOME"))
        (home (sb-ext:posix-getenv "HOME")))
    (pathname
     (apply 'concatenate 'string
            (or xdg home)
            (unless xdg "/.config")
            '("/clark/rc.lisp")))))

(defun get-tag-id (name)
  "Get the rowid of tag NAME."
  (execute-single
   *db* (sql select "rowid" from "tag" where "name" = ?) name))

(defun help-message ()
  (format t (concatenate
             'string
             "Usage: clark [options] [<command> [<options> ...]]~%"
             "~%"
             "Possible options:~%"
             "~%"
             "  --script  Output in a machine-readable format.~%"
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
  (execute-non-query
   *db* (sql insert into "bookmark" values (?\, ?\, ?\, ?))
   url (get-universal-time) name description))

(defun insert-bookmark-tag (bookmark-id tag-id)
  "Insert BOOKMARK-ID and TAG-ID into the bookmark_tag table."
  (execute-non-query
   *db* (sql insert into "bookmark_tag" values (?\, ?))
   bookmark-id tag-id))

(defun insert-tag (name)
  "Insert tag NAME into the database and return its rowid."
  (execute-non-query *db* (sql insert into "tag" values (?)) name)
  (last-insert-rowid *db*))

(defun load-db ()
  "Load the database."
  (let ((db-location (get-db-location)))
    (ensure-directories-exist db-location)
    (ensure-db-exists db-location)))

(defun load-rc ()
  "Load the RC file."
  (let ((*package* (in-package :org.ryuslash.clark)))
    (load (get-rc-location) :if-does-not-exist nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
          (script (setf *script* t args (cdr args)))
          (t (with-error-and-help
                 1 "help" "Unknown option: ~a~%" (car args)))))
  (if args
      (let ((cmd-name (make-command-name (car args))))
        (if (fboundp cmd-name)
            (handler-case (apply cmd-name (cdr args))
              (sb-int:simple-program-error (err)
                (if (string-equal (format nil "~A" err)
                                  "invalid number of arguments" :end1 27)
                    (with-error-and-help
                        1 (car args) "Wrong number of arguments given.~%")
                    (signal err))))
            (with-error-and-help
                1 "help" "Unknown command: ~A~%" (car args))))
      (map nil #'print-bookmark (get-bookmarks))))

(defun print-bookmark (bm)
  "Print information about bookmark BM.

BM should be a list containing the url, name and description of the
bookmark."
  (destructuring-bind (url name description) bm
    (if *script*
        (format t "~A~A~A" url name description)
        (format t "~A~%  ~A~%  ~A~%~%" url name description))))

(defcommand add (url name description &rest tags)
    "Add a new bookmark."
    "Usage: clark add <url> <name> <description> [<tags> ...]

Add URL with NAME, DESCRIPTION and TAGS to the database. TAGS may be
omitted or any number of tag names."
  (with-transaction *db*
    (insert-bookmark url name description)
    (add-tags (last-insert-rowid *db*) tags)))

(defcommand edit (url &rest rest)
    "Edit a bookmark."
    "Usage: clark edit <url> [--name <name>] \\
                        [--description <description>]

Edit the information for URL, specifying which part(s) to edit. Each
option will replace the previous value for that part."
  (let ((name-lst (member "--name" rest :test #'string=))
        (desc-lst (member "--description" rest :test #'string=))
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
             (format
              nil (sql update "bookmark" set "~A" where "url" = ?) query)
             (append qargs (list url))))))

(defcommand exists (url)
    "Check if a bookmark exists in the database."
    "Usage: clark exists <url>

Check if URL exists in the database. Prints `yes' when found and `no'
otherwise."
  (format
   t "~:[no~;yes~]~%"
   (execute-single
    *db* (sql select "rowid" from "bookmark" where "url" = ?) url)))

(defcommand help (&optional command)
    "Show help message."
    help-message
  (if command
      (let ((ldoc
             (nth 2 (car (member
                          command *help-messages*
                          :test #'(lambda (x y) (equal x (car y))))))))
        (cond
          ((null ldoc)
           (with-error-and-help
               1 "help" "Unknown command: ~a~%" command))
          ((and (symbolp ldoc) (fboundp ldoc)) (funcall ldoc))
          (t (format t "~A~%" ldoc))))
      (call-command help "help")))

(defcommand remove (url)
    "Remove a bookmark from the database."
    "Usage: clark remove <url>

Remove URL from the database."
  (clear-tags url)
  (execute-non-query
   *db* (sql delete from "bookmark" where "url" = ?) url))

(defcommand search (str)
    "Search through bookmarks."
    "Usage: clark search <str>

Search the database for STR. Matches are made for substrings of a
bookmark's name or an exact match for a tag."
  (map nil #'print-bookmark
       (execute-to-list
        *db* (sql select "url, name, description"
                  from "bookmark"
                  where "name" like ?
                  or ? in (select "name"
                           from "tag"
                           join "bookmark_tag" on ("tag_id = tag.rowid")
                           where "bookmark_id" = "bookmark.rowid"))
        (format nil "%~A%" str) str)))

(defcommand set-tags (url &rest tags)
    "Set a bookmark's tags."
    "Usage: clark set-tags <url> [<tags> ...]

Set bookmark URL's tags to the given list, overwriting the previous
list of tags."
  (clear-tags url)
  (add-tags url tags))

(defcommand version ()
    "Show version."
    "Usage: clark version

Print the version number and exit."
  (format t "clark version ~A~%" version))

(defun clark (args)
  "Main function.

Connect to the database, parse command-line arguments, execute and
then disconnect."
  (load-rc)
  (load-db)
  (parse-args (cdr args))
  (disconnect *db*))
