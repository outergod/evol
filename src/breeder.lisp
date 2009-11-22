;;;; evol - breeder.lisp
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

;;; helpers
(defmacro with-dependency-nodes (&body body)
  `(let ((nodes (dependency-nodes-hashtable #'evolvable-p #'name #'dependencies *environment*)))
     ,@body))
  
;;; breeder class
(defclass breeder () ()
  (:documentation "Solitary breeder class."))

(defgeneric breed (breeder evolvable)
  (:documentation "Breed the evolvable so it can hatch")
  (:method ((breeder breeder) (evol evolvable))
    (with-dependency-nodes
      (nreverse (mapcar #'(lambda (name)
                            (evolve (getenv name :expanded nil)))
                        (resolve-queue (find-node (name evol) nodes) nodes))))))
