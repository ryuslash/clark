#-sbcl
(error "This lisp implementation iss not supported.")

(require 'asdf)
(require 'sqlite)
(require 'mcclim)

(asdf:oos 'asdf:load-op 'clark)

(save-lisp-and-die
 "clark" :toplevel
 (lambda ()
   (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
   (org.ryuslash.clark:clark sb-ext:*posix-argv*)
   0)
 :executable t)
