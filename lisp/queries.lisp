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

(defun url-list-no-tag ()
  "Get a list of all URLs stored."
  (mapcar #'car
          (execute-to-list
           *db* (sql select "url"
                     from "bookmark"))))

(defun url-list-for-tag (tag)
  "Get a list of all the stored URLs with tag TAG."
  (mapcar
   #'car
   (execute-to-list
    *db* (sql select "url"
              from "bookmark"
              where ? in (select "name"
                          from "tag"
                          join "bookmark_tag" on ("tag_id = tag.rowid")
                          where "bookmark_id" = "bookmark.rowid"))
    tag)))

(defun url-list (&optional tag)
  "Get a list of URLs, possibly with tag TAG."
  (if tag
      (url-list-for-tag tag)
      (url-list-no-tag)))
