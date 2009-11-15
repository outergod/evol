;;;; evol - toplevel.lisp
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

(in-package :evol)

(defparameter *default-evolution* nil)

(defmacro devolution (name &rest args &key type &allow-other-keys)
  `(make-instance ,type :name ,name ,@(remove-from-plist args :type)))

(defmacro default (name)
  `(setq *default-evolution* ,name))

(defun repl ()
  "For now, this is just a stub for testing standalone execution with core
files."
  (in-package :evol)
  (load (cl-fad:pathname-as-file "Evolution"))
  (let ((code (evolve
                (getenv (or *default-evolution*
                            (car (hash-table-keys *environment*)))
                        :expanded nil))))
    (sb-ext:quit :unix-status
                 (cond
                  ((integerp code) code)
                  ((null code) 1)
                  (t 0)))))
