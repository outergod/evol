;;;; evol - common-lisp.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
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

;;; helpers
(defun cl-load-ops (list)
  "cl-load-ops list => list

Prepares a list of ASDF:LOAD-op forms for op in input LIST."
  (mapcar #'(lambda (elt)
              `(asdf:oos 'asdf:load-op ',elt))
          list))

(defun cl-run-command (implementation packages cmd &key (verbose t) (fatal nil))
  "cl-run-command implementation packages cmd &key verbose fatal => (integer string string)

Run CL command sequence CMD with the appropriate IMPLEMENTATION. Load all
PACKAGES with ASDF first.
Wrap CMD in an implicit PROGN and HANDLER-CASE."
  (let ((eval-option (ecase implementation
                       (:sbcl "--eval")
                       (:ccl  "--eval"))))
    (run-command
     (nconc (split-commandline 
             (ecase implementation
               (:sbcl "sbcl --noinform --disable-debugger")
               (:ccl  "ccl")))
            (mapcan #'(lambda (form)
                        (list eval-option
                              (stringify form)))
                    (append (cons #>eof>(require 'asdf)eof
                                  (cl-load-ops packages))
                            (list
                             (concatenate 'string
#>eof>
(handler-case
 (progn eof
cmd
#>eof>)
 (error (condition)
   (format *error-output* "~a~&" condition)
eof
(ecase implementation
  (:sbcl "   (sb-ext:quit :unix-status 1)")
  (:ccl  "   (ccl:quit 1)"))
#>eof>))eof))))) :verbose verbose :fatal fatal)))

  
;;;; Common Lisp Evolvables
;;; cl-transformator class
(defclass cl-transformator (definite)
  ((implementation :accessor cl-implementation
                   :initarg :implementation
                   :initform (alexandria:required-argument :implementation))
   (rule :accessor rule
         :initarg :rule
         :initform (alexandria:required-argument :rule)))
  (:documentation "Evolution takes place here through running a freshly forked
Common Lisp copy that expects rule to be a list of forms to execute in order.
sourcefn is expected to return list of valid Common Lisp forms that will each be
grouped as a single argument to be passed to (eval) so no special quoting aside
from \\\" is required.
Variable expansion is only performed against sourcefn's return forms."))

(defmethod evolve ((trans cl-transformator) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (cl-run-command (cl-implementation trans) (cl-packages trans)
   (interpolate-%-argument (rule trans) (name trans) (sourcefn trans) *environment*)))


;;; cl-core class
(defclass cl-core (cl-transformator file)
  ((sourcefn :initform #'(lambda (target modifier)
                           (declare (ignore modifier))
                           (format nil ":~a" target))
             :reader sourcefn
             :allocation :class
             :documentation "Preinitialized for this class; returns a list of
forms to first load asdf, then in turn additional asdf packages from rule and
finally a form to have sbcl create a core file.")
   (rule :reader rule
         :initarg :rule
         :initform
#>eof>
(in-package %<)
(%cl-save "%@" %cl-save-options)eof)
   (packages     :accessor cl-packages
                 :initarg :packages
                 :initform (alexandria:required-argument :packages)
                 :documentation "Package to load before creating heap image.")
   (cl-save-options :accessor cl-save-options
                    :initarg :save-options
                    :initform ""
                    :documentation "Additional arguments to pass to the
implementation-dependent save call.")
   (env-slots :initform (list 'packages 'cl-save-options)))
  (:documentation "This evolvable enables creation of non-standalone Common Lisp
core (heap image) files.
Feed rule with a list of asdf-recognized package symbols to load into the
core."))

(defmethod evolve :around ((core cl-core) &rest args &key &allow-other-keys)
  (let ((*environment* (alexandria:copy-hash-table *environment*)))
    (setf (gethash 'cl-save *environment*) (ecase (cl-implementation core)
                                             (:sbcl "sb-ext:save-lisp-and-die")
                                             (:ccl  "ccl:save-application")))
    (call-next-method)))


;;; cl-exe class
(defclass cl-exe (cl-core executable)
  ((rule :reader rule
         :initarg :rule
         :initform
#>eof>
(in-package %<)
(%cl-save "%@" %cl-save-options %cl-executable)eof))
  (:documentation "In line with cl-core, a complete dump is generated but with
the engine fully runable contained within so the resulting file is a real
executable.
Feed rule with a list of asdf-recognized package symbols to load into the
binary."))

(defmethod evolve :around ((exe cl-exe) &rest args &key &allow-other-keys)
  (let ((*environment* (alexandria:copy-hash-table *environment*)))
    (setf (gethash 'cl-executable *environment*) (ecase (cl-implementation exe)
                                                   (:sbcl ":executable t")))
    (call-next-method)))
