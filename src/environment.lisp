;;;; evol - environment.lisp
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

(in-package :evol)

(defparameter *environment* (make-hash-table :test #'equal))

(defgeneric expand (standard-object)
  (:documentation "expand standard-object => string

Return a suitable form of the object for %-style rule expansion.
Defined here to prevent circular dependencies.")
  (:method ((object standard-object))
    "expand object => object

Just return IDENTITY of the OBJECT."
    object))

(defun getenv (var &key (env *environment*) (expanded t) (default ""))
  "getenv var &key env expanded default => mixed

Return object stored for key var from hash :env and :expand the object for
external command use if desired."
  (let ((result (gethash var env default)))
    (if (and expanded
             (typep result 'standard-object))
        (expand result)
      result)))

(defun (setf getenv) (val var &optional (environment *environment*))
  "(setf geten) var val &optional environment => val

Store VAL for key VAR in hash table ENVIRONMENT."
  (setf (gethash var environment) val))

(defun posix-getenv (name)
  "posix-getenv name => string

Return value for POSIX environmental key name, empty string if nothing found."
  (or (osicat-posix:getenv name) ""))
