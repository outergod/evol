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
 '(posix-getenv internify)
 (find-package :evol-test))

(in-package :evol-test)

(in-suite all)
(defsuite environment)
(in-suite environment)

(defparameter env nil)

(deftest environment-exists ()
  (is (boundp 'env)))

(defixture env-environment-fixture
  (setq env (make-hash-table))
  (-body-))

(deftest internification ()
  (is (symbolp (internify "foo")))
  (is (symbolp (internify 'foo)))
  (is (symbolp (internify 4)))
  (is (symbolp (internify #\a))))

(deftest getenv-default-nonexist ()
  (with-fixture env-environment-fixture
    (is (stringp (getenv 'XXX :env env)))
    (is (string= "" (getenv 'XXX :env env)))))

(deftest put-string ()
  (with-fixture env-environment-fixture
    (defenv 'foo "bar" env)
    (is (string= "bar" (gethash 'foo env "")))))

(deftest get-string ()
  (with-fixture env-environment-fixture
    (defenv 'foo "bar" env)
    (is (string= "bar" (getenv 'foo :env env)))))
