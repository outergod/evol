;;;; evol - dependency.lisp
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

(shadowing-import
 '(make-dependency-nodes)
 (find-package :evol-test))

(in-package :evol-test)

(in-suite all)
(defsuite dependency)
(in-suite dependency)

(defixture dep-environment-fixture
  (:setup (setq env (make-hash-table))
          (setf (gethash 'a env) 1
                (gethash 'b env) 2
                (gethash 'foo env) 'bar
                (gethash 'baz env) 'bop)))

;; Thanks to lisp-unit
(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))

(deftest dependency-nodes ()
  (with-fixture dep-environment-fixture
    (mapc #'(lambda (expected predicate)
              (is (set-equal expected
                             (make-dependency-nodes env predicate))))
          (list '((1) (2))
                '((bar) (bop)))
          (list #'integerp
                #'symbolp))))

