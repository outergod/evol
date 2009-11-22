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
  "devolution name &rest args &key type &allow-other-keys => object

Top-level syntactic sugar macro to create evolvables. Name will be the
environmental hash key, :type must be a valid class name and all other keys will
be proxied to (make-instance)."
  `(make-instance ,type :name ,name ,@(remove-from-plist args :type)))

(defmacro default (name)
  "default name => mixed

Top-level syntactic sugar macro to set the default evolvable to name."
  `(setq *default-evolution* ,name))

(defun posix-argv ()
  "posix-argv => list

Return command line argument list. Implementation dependent."
  #+sbcl sb-ext::*posix-argv*
  #-sbcl nil)

(defun parse-commandline (argv)
  "parse-commandline argv => (args opts errors)

Parse command line argument list argv with getopt and defined available
options."
  (getopt:getopt argv
                 '(("verbose" :none nil))))

(defun targets (args)
  "targets args => list

List of what to evolve based on list of command line args, default evolution and
- as a last resort - the first defined evolvable."
  (or (cdr args)
      (list
       (or *default-evolution*
           (car (hash-table-keys *environment*))))))

(defun print-help ()
  "print-help => void

Print help. Stubbed for now."
  (format t "This could be helpful one day. Now it isn't. :)~%"))

(defun repl ()
  "repl => void

For now, this is just a stub for testing standalone execution with core files."
  (in-package :evol)
  (load (cl-fad:pathname-as-file "Evolution"))
  (multiple-value-bind (args opts errors) (parse-commandline (posix-argv))
    (declare (ignore opts))
    (sb-ext:quit
     :unix-status (if errors
                      (progn (print-help) 1)
                    (reduce #'logior
                       (mapcar #'(lambda (name)
                                   (let ((code (breed (make-instance 'breeder)
                                                      (getenv name :expanded nil))))
                                     (cond
                                      ((integerp code) code)
                                      ((null code) 1)
                                      (t 0))))
                               (targets args)))))))
