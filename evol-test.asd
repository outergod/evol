;;;; evol - evol-test.asd
;;;; Copyright (C) 2009 2011  Alexander Kahl <e-user@fsfe.org>
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

(defpackage :evol-test-system
  (:use :cl :asdf))

(in-package :evol-test-system)

(asdf:defsystem :evol-test
                :description "evol test package."
                :version "0.0.1"
                :author "Alexander Kahl <e-user@fsfe.org>"
                :license "GPLv3+"
                :depends-on (:evol :hu.dwim.stefil)
                :components
                ((:module "test"
                          :components
                          ((:file "package")
                           (:file "environment" :depends-on ("package"))
                           (:file "util"        :depends-on ("package"))
                           (:file "path"        :depends-on ("package"))
                           (:file "shell"       :depends-on ("package"))
                           (:file "dependency"  :depends-on ("package"))))))
