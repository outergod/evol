;;;; evol - shell.lisp
;;;; Copyright (C) 2009  Alexander Kahl <e-user@gmx.net>
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

(defun shell (&rest args)
  (with-output-to-string (stream)
                         (external-program:run "bash" (list "-c" (apply #'concatenate 'string args))
                                               :output stream)))

;; (cl-ppcre:regex-replace-all "%([^ ])" "cc -c -o %@ %<"
;;                                      #'(lambda (target-string start end match-start match-end reg-starts reg-ends)
;;                                          (declare (ignore start end match-start match-end))
;;                                          (let ((match (subseq target-string
;;                                                            (svref reg-starts 0) (svref reg-ends 0))))
;;                                            (or (when (= 1 (length match)) 
;;                                                  (case (char match 0)
;;                                                    (#\@ "foo")
;;                                                    (#\< "bar")
;;                                                    (#\% "%")
;;                                                    (otherwise "")))))))
