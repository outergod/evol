;;;; evol - environment.lisp
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

(import
 '(*environment* posix-getenv)
 (find-package :evol-test))

(in-package :evol-test)

(in-suite all)
(defsuite environment)
(in-suite environment)

(deftest environment-exists ()
  (is (boundp '*environment*)))

(defixture environment-fixture
  (:setup (setq *environment* (make-hash-table))))

(deftest get-default-nonexist ()
  (is (string= "" (getenv "XXX"))))

(deftest put-string ()
  (defenv "foo" "bar")
  (is (string= "bar" (gethash (intern "foo") *environment* ""))))

(deftest get-string ()
  (put-string)
  (is (string= "bar" (getenv "foo"))))
