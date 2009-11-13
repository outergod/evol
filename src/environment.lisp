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

(eval-when (:load-toplevel)
  (defun getenv (var &key (env *environment*) (expanded t))
    (let ((result (gethash (internify var) env "")))
      (if (and expanded
               (typep result 'evolvable))
          (expand result)
        result))))

(defmacro defenv (var val &optional (environment *environment*))
  `(setf (gethash (symbolize ,var) ,environment) ,val))

(defun posix-getenv (name)
  #+:sbcl (or (sb-ext:posix-getenv name) "")
  #-:sbcl "")

(defun internify (name)
  (cond ((symbolp name) name)
        ((stringp name) (intern (string-upcase name)))
        (t (intern (string-upcase (write-to-string name))))))

(defmacro symbolize (name)
  `(quote
    ,(if (symbolp name) name
       (internify (eval name)))))
