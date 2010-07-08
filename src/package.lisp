;;;; evol - package.lisp
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

(in-package :evol-system)

(defpackage :cffi-regex
  (:use :cl :cffi)
  (:export :regex-search :regex-match :regex-compilation-failure :regex-internal-error)
  (:shadow :regoff-t :size-t :reg-syntax :active-reg :s-reg))

(defpackage :evol
  (:nicknames :love)
  (:use :cl :external-program :cl-fad :alexandria :cffi-regex)
  (:shadow :copy-stream :copy-file)
  (:export :pathname-suffix-p
           :pathname-change-suffix
           :run-command
           :default-sourcefn
           :defenv
           :getenv
           :*environment*
           :repl
           :process-m4
           :read-heredoc))
