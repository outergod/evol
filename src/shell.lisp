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

(defun run-bash (&rest args)
  (with-output-to-string (stream)
                         (external-program:run "bash" (list "-c" (apply #'concatenate 'string args))
                                               :output stream)))

(defun run-command (cmd &key (target "") (sourcefn #'default-sourcefn) (verbose t))
  (let ((interpolcmd (interpolate-commandline cmd :target target :sourcefn sourcefn)))
    (when verbose (format t "~a~%" interpolcmd))
    (with-output-to-string (stream)
                           (external-program:run (car interpolcmd) (cdr interpolcmd)
                                                 :output stream))))

(defun interpolate-commandline (cmd &key (target "") (sourcefn #'default-sourcefn))
  (flatten (mapcar #'(lambda (arg)
                       (case (char arg 0)
                         (#\' (string-trim "'" arg))
                         (#\" (string-trim "\"" (interpolate-argument arg target sourcefn)))
                         (t (interpolate-argument arg target sourcefn))))
                   (split-commandline cmd))))

(defun default-sourcefn (target modifier)
  (declare (ignore target modifier)) "")

(defun split-commandline (cmd)
  (cl-ppcre:all-matches-as-strings "'[^']*'|\"[^\"]*\"|\\S+" cmd))

(defun interpolate-argument (argument target sourcefn)
  (split-commandline
   (interpolate-$-argument
    (interpolate-%-argument argument target sourcefn))))

(defun interpolate-%-argument (argument target sourcefn)
  (cl-ppcre:regex-replace-all "%([^ %]*)" argument
                              (replace-with-region #'replace-%-match target sourcefn)))

(defun interpolate-$-argument (argument)
  (cl-ppcre:regex-replace-all "\\$([^ \\$]*)" argument
                              (replace-with-region #'replace-$-match)))

(defun replace-with-region (replacefn &rest args)
  #'(lambda (target-string start end match-start match-end reg-starts reg-ends)
      (declare (ignore start end match-start match-end))
      (apply replacefn (subseq target-string
                               (svref reg-starts 0) (svref reg-ends 0))
             args)))

(defun replace-%-match (match target sourcefn)
  (let ((modifier (when (> (length match) 1)
                    (subseq match 1))))
    (case (char match 0)
      (#\% "%")
      (#\@ (deflate-string target (or modifier " ")))
      (#\< (deflate-string (funcall sourcefn target (or modifier " "))))
      (t (or (getenv match) "")))))

(defun replace-$-match (match)
  (posix-getenv match))

(defun deflate-string (target &optional (modifier " "))
  (if (listp target)
      (join-string-list target modifier)
    target))

(defun join-string-list (string-list &optional (separator " "))
  (format nil (concatenate 'string "~{~a~^" separator "~}")
          string-list separator))

(defun flatten (list)
  (cond ((null list) nil)
        ((atom (car list))
         (cons (car list)
               (flatten (cdr list))))
        (t (nconc (flatten (car list))
                  (flatten (cdr list))))))
