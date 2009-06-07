;;;; evol - path.lisp
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

(defun pathname-suffix-p (suffix pathspec)
  (equal suffix (pathname-type (cl-fad:pathname-as-file pathspec))))

(defun pathname-change-suffix (suffix pathspec)
  (let ((pathname (cl-fad:pathname-as-file pathspec)))
    (setf (slot-value pathname 'type) suffix)
    (namestring pathname)))
