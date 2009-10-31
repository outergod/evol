;;;; evol - evol.asd
;;;; Copyright (C) 2009  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of evol.
;;;; evol is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; evol is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage :evol-system
  (:use :cl :asdf))

(in-package :evol-system)

(asdf:defsystem :evol
                :description "evol- entrenched virtues of lisp / love reversed. Multi-purpose build system."
                :version "0.0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :depends-on (:external-program :cl-fad :cl-ppcre)
                :components
                ((:module "src"
                          :components
                          ((:file "package")
                           (:file "path"        :depends-on ("package"))
                           (:file "environment" :depends-on ("package"))
                           (:file "shell"       :depends-on ("environment" "package"))))))
