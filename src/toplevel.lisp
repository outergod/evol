;;;; evol - toplevel.lisp
;;;; Copyright (C) 2009, 2010  Alexander Kahl <e-user@fsfe.org>
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

(defparameter *options* '((help   :options ("h")                    :default nil
                                  :argument nil    :description "Display this help and exit.")
                          (silent :options ("s" "silent" "quiet")   :default nil
                                  :argument nil    :description "Don't echo commands.")
                          (file   :options ("f" "file" "evolution") :default "Evolution"
                                  :argument "FILE" :description "Use FILE as evolution.")
                          (jobs   :options ("j" "jobs")             :default "1"
                                  :argument "JOBS" :description "Breed JOBS evolvables simultaneously.")))

(defmacro devolution (name (&body dependencies) &rest args &key type &allow-other-keys)
  "devolution name &rest args &key type &allow-other-keys => object

Top-level syntactic sugar macro to create evolvables. Name will be the
environmental hash key, :TYPE must be a valid class name and all other keys will
be proxied to MAKE-INSTANCE."
  `(make-instance ,type :name ,name ,@(remove-from-plist args :type)
                        :deps '(,@dependencies)))

(defmacro default (name)
  "default name => mixed

Top-level syntactic sugar macro to set the default evolvable to name."
  `(setq *default-evolution* ,name))

(defun posix-argv ()
  "posix-argv => list

Return command line argument list. Implementation dependent."
  #+:sbcl sb-ext:*posix-argv*
  #+:ccl *command-line-argument-list*
  #+:clisp ext:*args*
  #+:lispworks system:*line-arguments-list*
  #+:cmu extensions:*command-line-words*
  #-(or sbcl ccl clisp lispworks cmu) nil)

(defun targets (args)
  "targets args => list

List of what to evolve based on list of command line args, default evolution and
- as a last resort - the first defined evolvable."
  (or (cdr args)
      (list
       (or *default-evolution*
           (car (hash-table-keys *environment*))))))

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
    (multiple-value-bind (bool-options parameter-options) (unix-options-options)
      (unix-options:map-parsed-options (cdr argv) bool-options parameter-options
                                      #'(lambda (option value)
                                          (pushnew (cons (car (argument-option option)) value)
                                                   opts :key #'car))
                                      #'(lambda (arg)
                                          (push arg args)))
    (values args opts))))

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

(defun repl ()
  "repl => quit

Top-level function used for the standalone executable created through
bootstrapping evol.
Heads-up: Quits CL after execution."
  (in-package :evol)
  (multiple-value-bind (args opts) (parse-commandline (posix-argv))
    (sb-ext:quit
     :unix-status (handler-case
                   (progn
                     (if (cdr (assoc 'help opts))
                         (print-help)
                       (let* ((jobs (parse-integer (cdr (assoc 'jobs opts))))
                              (breeder
                               (if (> 1 jobs)
                                   (make-instance 'swarm :worker-capacity jobs
                                                         :job-capacity 0
                                                         :worker-timeout-duration 600)
                                 (make-instance 'breeder))))
                         (load (cl-fad:pathname-as-file (cdr (assoc 'file opts))))
                         (mapc #'(lambda (name)
                                   (breed breeder (getenv name :expanded nil)))
                               (targets args))))
                     0)
                   (error (command-failure)
                          (format *error-output* "evol: ~a~&evol: exit ~s  ~a~%"
                                  (command-failure-stderr command-failure)
                                  (command-failure-code command-failure)
                                  (command-failure-command command-failure))
                          (command-failure-code command-failure))
                   (error (condition)
                          (format *error-output* "evol: ~a~&" condition)
                          2)))))
