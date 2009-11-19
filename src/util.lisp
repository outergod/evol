;;;; evol - util.lisp
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

(defun posix-argv ()
  "posix-argv => list

Return command line argument list. Implementation dependent."
  #+sbcl sb-ext::*posix-argv*
  #-sbcl nil)

;; (defun find-tree (item tree &rest args &key key test test-not)
;;   (if (not (consp tree))
;;       nil
;;     (or (find-tree item (car tree args
