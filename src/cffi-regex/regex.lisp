;;;; evol - package.lisp
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

(in-package :cffi-regex)

(defmacro with-pattern-buffer ((var pattern &optional (syntax (list +syntax-emacs+))) &body body)
  `(with-foreign-object (,var 'pattern-buffer)
     (setf (foreign-slot-value ,var 'pattern-buffer 'buffer) (make-pointer 0)
           (foreign-slot-value ,var 'pattern-buffer 'allocated) +regs-unallocated+
           (foreign-slot-value ,var 'pattern-buffer 'fastmap) (make-pointer 0)
           (foreign-slot-value ,var 'pattern-buffer 'translate) (make-pointer 0)
           (foreign-slot-value ,var 'pattern-buffer 'syntax) (logior ,@syntax))
     (compile-pattern ,pattern (length ,pattern) ,var)
     ,@body))

(defun get-register (registers index)
  (values
   (mem-aref
    (mem-aref (foreign-slot-pointer registers 'registers 'start) :pointer)
    'regoff-t index)
   (mem-aref
    (mem-aref (foreign-slot-pointer registers 'registers 'end) :pointer)
    'regoff-t index)))

(defun regex-search (regex target-string &key (start 0) (end (length target-string)))
  (with-pattern-buffer (buffer regex)
    (with-foreign-object (registers 'registers)
      (let ((startpos (%regex-search buffer target-string (length target-string)
                                     start end registers)))
        (if (>= startpos 0)
            (let ((matches (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0)))
              (dotimes (position (1- (foreign-slot-value registers 'registers 'num-regs)))
                (vector-push-extend (multiple-value-list (get-register registers position)) matches))
              (values startpos matches))
          startpos)))))

(defun regex-match (regex target-string &optional (start 0))
  (with-pattern-buffer (buffer regex)
    (with-foreign-object (registers 'registers)
      (%regex-match buffer target-string (length target-string)
                    start registers))))
