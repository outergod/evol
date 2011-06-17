;;;; evol - util.lisp
;;;; Copyright (C) 2010 2011  Alexander Kahl <e-user@fsfe.org>
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
 '(mapthread with-outputs-to-strings with-slot-enhanced-environment)
 (find-package :evol-test))

(in-package :evol-test)

(in-suite all)
(defsuite util)
(in-suite util)

(defclass slot-tester ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2)))

(defixture util-environment-fixture
  (setq *environment* (make-hash-table))
  (-body-))

(deftest test-mapthread ()
  (is (equal (list 2 3 4 5 6)
             (mapthread #'1+ (list 1 2 3 4 5)))))

(deftest test-with-outputs-to-strings ()
  (is (equal (list 'result "foo" "bar" "baz")
             (multiple-value-list
              (with-outputs-to-strings (one two three)
               (write-string "foo" one)
               (write-string "bar" two)
               (write-string "baz" three)
               'result)))))

(deftest test-with-slot-enhanced-environment ()
  (with-fixture util-environment-fixture
   (is (equal (list "foo" "bar")
              (with-slot-enhanced-environment '(slot1 slot2)
                  (make-instance 'slot-tester :slot1 "foo" :slot2 "bar")
                (list (gethash 'slot1 *environment*)
                      (gethash 'slot2 *environment*)))))))
