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

(defun bookmark-exists-p (url)
  "Check if URL can be found in the database."
  (execute-single
   *db* (sql select "rowid" from "bookmark" where "url" = ?) url))

(defun bookmark-list ()
  "Get a list of all stored bookmarks."
  (execute-to-list
   *db* (sql select "url, name, description" from "bookmark")))

(defun bookmark-search (name-or-tag)
  "Get bookmarks by NAME-OR-TAG."
  (execute-to-list
   *db* (sql select "url, name, description"
             from "bookmark"
             where "name" like ?
             or ? in (select "name"
                      from "tag"
                      join "bookmark_tag" on ("tag_id = tag.rowid")
                      where "bookmark_id" = "bookmark.rowid"))
   (format nil "%~A%" name-or-tag) name-or-tag))

(defun create-table-bookmark ()
  "Create the bookmark table."
  (execute-non-query
   *db* (sql create table "bookmark"
             ("url" varchar (255) unique\,
              "date" integer\,
              "name" varchar (255)\,
              "description" text))))

(defun create-table-bookmark_tag ()
  "Create the bookmark_tag table."
  (execute-non-query
   *db* (sql create table "bookmark_tag"
             ("bookmark_id" integer references "bookmark(rowid)"\,
              "tag_id" integer references "tag(rowid)"\,
              primary key ("bookmark_id"\,  "tag_id")))))

(defun create-table-tag ()
  "Create the tag table."
  (execute-non-query
   *db* (sql create table "tag" ("name" varchar (255) unique))))

(defun delete-bookmark (url)
  "Delete URL from collection."
  (execute-non-query
   *db* (sql delete from "bookmark" where "url" = ?) url))

(defun delete-tags (id)
  "Clear the tags for bookmark with id ID."
  (execute-non-query *db* (sql delete from "bookmark_tag"
                               where "bookmark_id" = ?) id))

(defun get-bookmark-id (url)
  "Get the id of the bookmark for URL."
  (execute-single
   *db* (sql select "rowid" from "bookmark" where "url" = ?) url))

(defun get-tag-id (name)
  "Get the rowid of tag NAME."
  (execute-single
   *db* (sql select "rowid" from "tag" where "name" = ?) name))

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

(defun url-list (&optional tag)
  "Get a list of URLs, possibly with tag TAG."
  (if tag
      (url-list-for-tag tag)
      (url-list-no-tag)))

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

(defun url-list-no-tag ()
  "Get a list of all URLs stored."
  (mapcar #'car
          (execute-to-list
           *db* (sql select "url"
                     from "bookmark"))))
