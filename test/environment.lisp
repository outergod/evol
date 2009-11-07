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

(shadowing-import
 '(posix-getenv internify symbolize)
 (find-package :evol-test))

(in-package :evol-test)

(in-suite all)
(defsuite environment)
(in-suite environment)

(defparameter *environment* nil)

(defixture env-environment-fixture
  (:setup (setq *environment* (make-hash-table))))

(deftest environment-exists ()
  (is (boundp '*environment*)))

(deftest internification ()
  (is (symbolp (internify "foo")))
  (is (symbolp (internify 'foo)))
  (is (symbolp (internify 4)))
  (is (symbolp (internify #\a))))

(deftest symbolization ()
  (internification)
  (is (equal 'foo (symbolize foo)))
  (is (equal 'foo (symbolize 'foo)))
  (is (equal 'foo (symbolize "foo")))
  (is (equal '|#\\F| (symbolize #\f))))

(deftest getenv-default-nonexist ()
  (with-fixture env-environment-fixture
    (is (stringp (getenv 'XXX *environment*)))
    (is (string= "" (getenv 'XXX *environment*)))))

(deftest put-string ()
  (with-fixture env-environment-fixture
    (defenv foo "bar" *environment*)
    (is (string= "bar" (gethash 'foo *environment* "")))
    (is (string= "bar" (gethash (symbolize foo) *environment* "")))))

(deftest get-string ()
  (with-fixture env-environment-fixture
    (defenv foo "bar" *environment*)
    (is (string= "bar" (getenv 'foo *environment*)))))
