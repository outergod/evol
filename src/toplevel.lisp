;;;; evol - toplevel.lisp
;;;; Copyright (C) 2009 2010 2011  Alexander Kahl <e-user@fsfe.org>
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

;;; conditions
(define-condition illegal-evolvable (error)
  ((target :initarg :evolvable
           :reader  illegal-evolvable)))

(define-condition unemployment (error) ())

(define-condition unloadable-evolution (error)
  ((pathname :initarg :path
             :reader unloadable-evolution-pathname)))


(defparameter *default-evolution* nil)

(defparameter *options* '((help   :options ("h")                    :default nil
                                  :argument nil    :description "Display this help and exit.")
                          (silent :options ("s" "silent" "quiet")   :default nil
                                  :argument nil    :description "Don't echo commands.")
                          (file   :options ("f" "file" "evolution") :default "Evolution"
                                  :argument "FILE" :description "Use FILE as evolution.")
                          (jobs   :options ("j" "jobs")             :default "1"
                                  :argument "JOBS" :description "Breed JOBS evolvables simultaneously.")))

(defmacro devolution (name inputs &rest args &key type &allow-other-keys)
  "devolution name inputs &rest args &key type &allow-other-keys => evolvable

Top-level syntactic sugar macro to create evolvables. Name will be the
environmental hash key, :TYPE must be a valid class name and all other keys will
be proxied to MAKE-INSTANCE."
  `(make-instance ,type :name ,name ,@(remove-from-plist args :type)
                  :inputs (list ,@inputs)))

(defmacro default (name)
  "default name => mixed

Top-level syntactic sugar macro to set the default evolvable to name."
  `(setq *default-evolution* ,name))

(defun evolution-arguments (args)
  "evolution-arguments args => list

List of what to evolve based on list of command line args, default evolution and
- as a last resort - the first defined evolvable."
  (let ((evolvables (or (cdr args)
                     (list
                      (or *default-evolution*
                          (car (hash-table-keys *evolvables*)))))))
    (if (null (car evolvables))
        (error 'unemployment)
      (mapcar #'(lambda (evolvable)
                  (or (getenv evolvable :env *evolvables* :expanded nil :default nil)
                      (error 'illegal-evolvable :evolvable evolvable)))
              evolvables))))

(defmacro getf-option (option keyword)
  "getf-option option keyword => result

GETF for options."
  `(getf (cdr ,option) ,keyword))

(defun argument-option (argument)
  "argument-option argument => option

Find option owning ARGUMENT (command line option)."
  (find argument *options* :key #'(lambda (option)
                                    (getf-option option :options))
                           :test #'(lambda (option options)
                                     (find option options :test #'equal))))

(defun format-option (stream string &optional (argument nil))
  "format-option stream string argument => result

Print option STRING with optional ARGUMENT to STREAM; formatting depends on
whether STRING is a short or a long option, determined by its length."
  (if (> (length string) 1)
      (format stream "--~a~@[=~a~]" string argument)
    (format stream "-~a~@[ ~a~]" string argument)))

(defun unix-options-options ()
  "unix-options-options => (bool-options parameter-options)

Transform *OPTIONS* so that they are suitable for use with
UNIX-OPTIONS:MAP-PARSED-OPTIONS."
  (let ((options (copy-tree *options*)))
    (labels ((collector (predicate)
                        (mapcan #'(lambda (option)
                                    (getf-option option :options))
                                (remove-if predicate options))))
      (values
       (collector #'(lambda (option)
                      (getf-option option :argument)))
       (collector #'(lambda (option)
                      (not (getf-option option :argument))))))))

(defun parse-commandline (argv)
  "parse-commandline argv => (args opts)

Parse command line argument list ARGV with UNIX-OPTIONS:MAP-PARSED-OPTIONS and
defined available *OPTIONS*."
  (let ((args (list))
        (opts (list)))
    (labels ((pushnew-option (option value)
               (pushnew (cons (car (argument-option option)) value)
                        opts :key #'car)))
      (multiple-value-bind (bool-options parameter-options) (unix-options-options)
        (unix-options:map-parsed-options argv bool-options parameter-options
                                         #'(lambda (option value)
                                             (pushnew-option option value))
                                         #'(lambda (arg)
                                             (push arg args)))
        (mapc #'(lambda (option)
                  (when (and (getf-option option :default)
                             (not (find (car option) opts :key #'car :test #'equal)))
                    (push (cons (car option) (getf-option option :default))
                          opts)))
              *options*)
        (values (nreverse args) opts)))))

(defun print-help ()
  "print-help => string

Print help."
  (format t (concatenate 'string
                         "Usage: evol [options] [target] ...~%"
                         "Options:~%"
                         "~{~a~%~}")
          (mapcar #'(lambda (option)
                      (let ((option-list
                             (funcall #'format nil "  ~30@<~{~a~^, ~}~>"
                             (mapcar #'(lambda (name)
                                         (format-option nil name (getf-option option :argument)))
                                     (getf-option option :options))))
                            (description (getf-option option :description)))
                        (if (<= (length option-list) 32)
                            (concatenate 'string option-list description)
                          (format nil "~a~%  ~0,1,30:<~a~>" option-list description))))
                  *options*)))

(defun jobs-breeder (jobs)
  "jobs-breeder jobs => breeder

Create the appropriate BREEDER for the number of JOBS."
  (if (> jobs 1)
      (make-instance 'swarm :worker-capacity jobs
                     :job-capacity 0
                     :worker-timeout-duration 600)
      (make-instance 'breeder)))

(defun load-evolution (options)
  "load-evolution options => void

Load the evolution file based on command line OPTIONS."
  (let ((path (cdr (assoc 'file options))))
    (if (osicat:regular-file-exists-p path)
        (load (osicat:pathname-as-file path))
      (error 'unloadable-evolution :path path))))

(defun repl ()
  "repl => quit

Top-level function used for the standalone executable created through
bootstrapping evol.
Heads-up: Quits CL after execution."
  (in-package :evol)
  (multiple-value-bind (args opts) (parse-commandline (posix-argv))
    (posix-quit (handler-case
                  (progn
                    (if (cdr (assoc 'help opts))
                        (print-help)
                      (let* ((jobs (parse-integer (cdr (assoc 'jobs opts))))
                             (breeder (jobs-breeder jobs)))
                        (load-evolution opts)
                        (mapc #'(lambda (name)
                                  (breed breeder (getenv name :env *evolvables* :expanded nil)))
                              (evolution-arguments args))))
                    0)
                  (illegal-evolvable (condition)
                    (format *error-output* "evol: Unknown evolvable ~s.  Stop.~%" (illegal-evolvable condition))
                    2)
                  (unemployment ()
                    (format *error-output* "evol: Nothing to do, no evolvable definitions found.  Stop.~%")
                    2)
                  (unloadable-evolution (condition)
                    (format *error-output* "evol: Cannot load evolution definition file ~s.  Stop.~%" (unloadable-evolution-pathname condition))
                    2)
                  (command-failure (condition)
                    (format *error-output* "evol: ~a~&evol: exit ~s  ~a~%"
                            (command-failure-stderr condition)
                            (command-failure-code condition)
                            (command-failure-command condition))
                    (command-failure-code condition))
                  (error (condition)
                    (format *error-output* "evol: Internal error~%evol: ~a~&" condition)
                    2)))))
