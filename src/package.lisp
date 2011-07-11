;;;; evol - package.lisp
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

(in-package :evol-system)

(defpackage :evol
  (:nicknames :love)
  (:use :cl :external-program :alexandria)
  (:shadow :copy-stream :copy-file :format)
  (:export :pathname-suffix-p
           :pathname-change-suffix
           :run-command
           :run-bash
           :getenv
           :*environment*
           :*evolvables*
           :repl
           :in
           :out
           :format
           :evolvable-p
           :expand
           :uri-of
           :file
           :pipe
           :redirect
           :lisp
           :find-program))

(defpackage :evol-user
  (:shadowing-import-from :cl :lambda :&optional :&key :&rest
                          :*standard-output* :*error-output*
                          :nil :t :string :list)
  (:shadowing-import-from :evol :in :out :mutagen :elambda :env-bind-let* :env-let :format :evolvable-p :run-bash :*environment* :*evolvables* :pipe :redirect :lisp))
