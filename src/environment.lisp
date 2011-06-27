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

(defparameter *evolvables* (make-hash-table :test #'equal))
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

(defun stringify (object)
  "stringify object => string

Evaluate OBJECT to a STRING, casting WRITE-TO-STRING if necessary."
  (if (stringp object)
      object
      (string-downcase (if (keywordp object)
                           (symbol-name object)
                           (write-to-string object)))))

(defun (setf getenv) (val var &key (env *environment*))
  "(setf geten) var val &optional environment => val

Store VAL for key VAR in hash table ENVIRONMENT."
  (setf (gethash (stringify var) env) val))

(defun posix-getenv (name)
  "posix-getenv name => string

Return value for POSIX environmental key name, empty string if nothing found."
  (or (osicat-posix:getenv name) ""))

(defmacro env-let (bindings &body body)
  "env let bindings &body body => context

Evaluate BODY in scope of overridden *ENVIRONMENT* that is extended by
LET-style key/value BINDINGS list."
  `(let ((*environment* (copy-hash-table *environment*)))
     (setf ,@(mapcan #'(lambda (binding)
                         (list `(getenv ',(car binding)) (cadr binding)))
                    bindings))
     ,@body))

(defmacro env-rebind (bindings &body body)
  "env-rebind bindings &body body => context

Evaluate BODY in scope of an env-let of BINDINGS that are already bound in
scope. In effect, all current dynamic and lexical BINDINGS will be bound in
*ENVIRONMENT* as well."
  `(env-let ,(mapcar #'(lambda (binding)
                         (list binding binding))
                     bindings)
     ,@body))

(defmacro elet (bindings &body body)
  "elet bindings &body body => context

Evaluate BODY in scope of LET-style BINDINGS that also get recorded in the
*ENVIRONMENT*."
  `(let ,bindings
     (env-rebind ,(mapcar #'car bindings)
       ,@body)))

(defmacro elet* (bindings &body body)
  "elet* bindings &body body => context

ELET* is to ELET what LET* is to LET, i.e. ELET* is the recursive version of
ELET which means that each binding is available for all subsequent in turn, both
lexically and in *ENVIRONMENT*."
  (if bindings
      `(elet (,(car bindings))
         (elet* ,(cdr bindings)
            ,@body))
      (car body)))

(defmacro elambda (args &body body)
  "elambda args &body body => lambda

Evaluates to LAMBDA with an additional first IN argument and BODY within an
implicit ENV-LET of all other ARGS."
  `(lambda (in ,@args)
     (env-let ,(mapcar #'(lambda (arg)
                           (let ((key (if (consp arg)
                                          (car arg)
                                          arg)))
                             `(,key ,key)))
                       (cons 'in (cdr (member '&key args))))
       ,@body)))
