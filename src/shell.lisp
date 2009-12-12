;;;; evol - shell.lisp
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

;;; conditions
(define-condition command-failure (error)
  ((command :initarg :command
            :reader  command-failure-command
            :type list
            :documentation "The command that failed.")
   (code    :initarg :code
            :reader  command-failure-code
            :type fixnum
            :documentation "Status code of failed invocation.")
   (stdout  :initarg :stdout
            :reader  command-failure-stdout
            :type simple-string
            :documentation "Output of invocation.")
   (stderr  :initarg :stderr
            :reader  command-failure-stderr
            :type simple-string
            :documentation "Error output of invocation."))
  (:documentation "Condition signalled if invocation of an external command
failed."))

(defmethod print-object ((condition command-failure) stream)
  (format stream "~a: exit ~s"
            (command-failure-command condition)
            (command-failure-code condition)))


(defun run-command (cmd &key (verbose t) (fatal nil))
  "run-command cmd &key verbose fatal => (integer string string)

Run command line list CMD (blocking), returning VALUES of exit status of
invocation and strings of stdout and stderr output. If FATAL is non-nil and exit
status is not 0, signal COMMAND-FAILURE instead.
Side-effect: Print CMD prior to invocation and command output if VERBOSE is
non-nil."
  (labels ((conditional-format (destination control-string &rest format-arguments)
             (when verbose
               (let ((*print-pretty* nil))
                 (apply #'format destination control-string format-arguments)))))
    (conditional-format t "~a~%" cmd)
    (multiple-value-bind (code stdout stderr)
        (with-outputs-to-strings (stdout stderr)
          (cadr (multiple-value-list 
                 (external-program:run (car cmd) (cdr cmd) :output stdout :error stderr))))
      (conditional-format *error-output* "~a" stderr)
      (conditional-format t "~a" stdout)
      (if (and (/= 0 code) fatal)
          (error 'command-failure :command cmd :code code :stdout stdout :stderr stderr)
        (values code stdout stderr)))))

(defun interpolate-commandline (cmd &key (target "") (sourcefn #'default-sourcefn) (environment *environment*))
  "interpolate-commandline cmd &key target sourcefn environment => list

Interpolate split arguments of command line string cmd after grouping through
Bourne shell syntax block quoting, see split-commandline for details.
Unquoted quotes are stripped after interpolation, single quotes prevent
interpolation of their contained argument while double quotes don't.
Returns list of split and interpolated arguments."
  (alexandria:flatten
   (mapcar #'(lambda (arg)
               (case (char arg 0)
                 (#\' (string-trim "'" arg))
                 (#\" (string-trim "\"" (interpolate-argument arg target sourcefn environment)))
                 (t   (split-commandline
                       (interpolate-argument arg target sourcefn environment)))))
           (split-commandline cmd))))

(defun default-sourcefn (target modifier)
  "default-sourcefn target modifier => string

Default source computing function that does nothing but ignoring all arguments
and always returns an empty string."
  (declare (ignore target modifier)) "")

(defun split-commandline (cmd)
  "split-commandline cmd => list

Split command line string cmd into arguments accourding to Bourne shell syntax
rules honoring double quotes [\"], single quotes ['] and regular whitespace."
  (cl-ppcre:all-matches-as-strings "'[^']*'|\"[^\"]*\"|\\S+" cmd))

(defun interpolate-argument (argument target sourcefn environment)
  "interpolate-argument argument target sourcefn environment => list

Expand all % and $ matches in string argument in turn."
  (interpolate-$-argument
   (interpolate-%-argument argument target sourcefn environment)))

(defun interpolate-%-argument (argument target sourcefn environment)
  "interpolate-%-argument argument target sourcefn environment => string

Expand all matches of % words in string argument honoring the special target and
sourcefn matches and, for the rest, the environment."
  (cl-ppcre:regex-replace-all "%({[^}%()\"]*}|[^ %()\"]*)" argument
                              (replace-with-region #'expand-%-match target sourcefn environment)))

(defun interpolate-$-argument (argument)
  "interpolate-$-argument argument => string

Expand all matches of $ words in string argument."
  (cl-ppcre:regex-replace-all "\\$({[^}\\$()\"]}|[^ \\$()\"]*)" argument
                              (replace-with-region #'expand-$-match)))

(defun replace-with-region (replacefn &rest args)
  "replace-with-region replacefn &rest args => closure

Create closure that is suitable for use with cl-ppcre replacement forms. Created
closure invokes replacefn against the matched subsequence in the string to be
searched additionally passing args."
  #'(lambda (target-string start end match-start match-end reg-starts reg-ends)
      (declare (ignore start end match-start match-end))
      (apply replacefn (subseq target-string
                               (svref reg-starts 0) (svref reg-ends 0))
             args)))

(defun trim-{} (string)
  "trim-{} string => string

Trims {} brackets strings."
  (string-left-trim "{" (string-right-trim "}" string)))

(defun expand-%-match (match target sourcefn environment)
  "expand-%-match match target sourcefn environment => string

Act depending on string match:
- % returns %
- @ returns target
- < returns result of invoking sourcefn against target and modifier
- Any other sequence will be looked up in evol's environment returning the
  result, defaulting to an empty string
- In case of @ and <, if target respectively sourcefn invocation returns a list,
  it will be auto-deflated to a string with spaces as element seperator. To
  modify the deflation seperator, simply pass any non-whitespace character
  sequence after @ or <, e.g.
  [@,] for target := (foo bar baz) => \"foo,bar,baz\""
  (let* ((stripped-match (trim-{} match))
         (modifier (if (> (length stripped-match) 1)
                       (subseq stripped-match 1) " ")))
    (case (char stripped-match 0)
      (#\% "%")
      (#\@ (deflate-string target modifier))
      (#\< (deflate-string (funcall sourcefn target modifier)))
      (t (or (deflate-string
               (getenv stripped-match :env environment))
             "")))))

(defun expand-$-match (match)
  "expand-$-match match => string

Lookup match in the environment CL was started in returning the result."
  (posix-getenv (trim-{} match)))

(defun deflate-string (list &optional (separator " "))
  "deflate-string list &optional separator => string|object

Splice list of strings into merged string having elements separated with string
seperator."
  (if (listp list)
      (format nil (concatenate 'string "~{~a~^" separator "~}")
              list separator)
    list))
