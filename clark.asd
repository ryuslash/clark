(defpackage :clark-system
  (:use :cl :asdf))
(in-package :clark-system)

(defsystem :clark
  :name "CLark"
  :author "Tom Willemsen <tom@ryuslash.org>"
  :version "0.0.1"
  :maintainer "Tom Willemsen <tom@ryuslash.org>"
  :description "Keep bookmarks, in lisp."
  :serial t
  :depends-on (:mcclim)
  :components ((:file "package")
               (:file "clark")))
