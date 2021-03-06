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

#-sbcl
(error "This lisp implementation iss not supported.")

(require 'asdf)

(asdf:oos 'asdf:load-op 'clark)

(save-lisp-and-die
 "clark" :toplevel
 (lambda ()
   (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
   (org.ryuslash.clark:clark sb-ext:*posix-argv*)
   0)
 :executable t)
