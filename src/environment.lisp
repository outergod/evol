;;;; evol - environment.lisp
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

(defparameter *environment* (make-hash-table))

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
  (let ((result (gethash (internify var) env default)))
    (if (and expanded
             (typep result 'standard-object))
        (expand result)
      result)))

(defmacro defenv (var val &optional (environment '*environment*))
  "defenv var val &optional environment => val

Store val for key var in hash environment."
  `(setf (gethash (symbolize ,var) ,environment) ,val))

(defun posix-getenv (name)
  "posix-getenv name => string

Return value for POSIX environmental key name, empty string if nothing found.
Only implemented for sbcl right now."
  #+:sbcl (or (sb-ext:posix-getenv name) "")
  #-:sbcl "")

(defun internify (name)
  "internify name => symbol

Return interned symbol for arbitrarily typed name; useful for use as hash keys."
  (cond ((symbolp name) name)
        ((stringp name) (intern (string-upcase name) 'evol))
        (t (intern (string-upcase (write-to-string name)) 'evol))))

(defmacro symbolize (name)
  "symbolize name => symbol

Quotes, transforms and interns unquoted variable names."
  `(quote
    ,(internify
      (if (symbolp name)
          (symbol-name name)
        (eval name)))))
