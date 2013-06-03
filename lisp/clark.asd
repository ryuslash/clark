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

(defpackage :clark-system
  (:use :cl :asdf))
(in-package :clark-system)

(defsystem :clark
  :name "CLark"
  :author "Tom Willemsen <tom@ryuslash.org>"
  :version "0.1.1"
  :maintainer "Tom Willemsen <tom@ryuslash.org>"
  :description "Keep bookmarks, in lisp."
  :serial t
  :depends-on (:sqlite)
  :components ((:file "package")
               (:file "clark")
               (:file "queries")))
