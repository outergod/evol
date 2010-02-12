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
 '(dependency-nodes-hashtable dependency-node root-nodes leaf-nodes find-node
   resolve-dag resolve-queue resolve-all circular-dependency
   unresolvable-dependency unresolvable-node unresolvable-dependency
   circular-node circular-inter-nodes resolve-roots)
 (find-package :evol-test))

(in-package :evol-test)

(in-suite all)
(defsuite dependency)
(in-suite dependency)

(defparameter env nil)

(defixture dep-environment-fixture-1
  (:setup (setq env (make-hash-table))
          (setf (gethash 'a env) (list 1 'foo 3)
                (gethash 'b env) (list 3)
                (gethash 'foo env) (list 'foo 'bop)
                (gethash 'baz env) (list 'bop 3))))

(defixture dep-environment-fixture-2
  (:setup (setq env (make-hash-table))
          (setf (gethash 'root env) (list "root" "lvl1:1" "lvl1:2")
                (gethash 'lvl1-1 env) (list "lvl1:1" "lvl2:1")
                (gethash 'lvl1-2 env) (list "lvl1:2" "lvl2:1" "lvl4:1")
                (gethash 'lvl2-1 env) (list "lvl2:1" "lvl3:1" "lvl3:2")
                (gethash 'lvl3-1 env) (list "lvl3:1")
                (gethash 'lvl3-2 env) (list "lvl3:2")
                (gethash 'lvl4-1 env) (list "lvl4:1"))))

;; Thanks to lisp-unit
(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))

(defun make-predicate-car (predicate)
  #'(lambda (elt)
      (and (consp elt)
           (funcall predicate (car elt)))))

(deftest test-dependency-node ()
  (with-fixture dep-environment-fixture-1
    (mapc #'(lambda (expected key)
              (is expected
                  (dependency-node #'car #'cdr
                                   (gethash key env))))
          (list '(1 'foo 3)
                '(3)
                '(foo bop)
                '(bop 3))
          (list 'a 'b 'foo 'baz))))

(deftest test-dependency-nodes ()
  (with-fixture dep-environment-fixture-1
    (mapc #'(lambda (expected predicate)
              (is (set-equal expected
                             (dependency-nodes-hashtable predicate #'car #'cdr env))))
          (list '((3) (1 foo 3))
                '((foo bop) (bop 3)))
          (list (make-predicate-car #'integerp)
                (make-predicate-car #'symbolp)))))

(deftest test-root-nodes ()
  (is (set-equal '((1 foo 3))
                 (root-nodes 
                  (dependency-nodes-hashtable #'identity #'car #'cdr env)))))

(deftest test-leaf-nodes ()
  (is (set-equal '((3))
                 (leaf-nodes 
                  (dependency-nodes-hashtable #'identity #'car #'cdr env)))))

(deftest test-find-node ()
  (with-fixture dep-environment-fixture-1
    (let ((nodes (dependency-nodes-hashtable #'identity #'car #'cdr env)))
      (mapc #'(lambda (expected name)
                (is (set-equal expected
                               (find-node name nodes))))
            (list '(1 foo 3)
                  '(3)
                  '(foo bop)
                  '(bop 3))
            (list '1 '3 'foo 'bop)))))

(deftest resolution-dag-1 ()
  (with-fixture dep-environment-fixture-1
    (let ((nodes (dependency-nodes-hashtable #'identity #'car #'cdr env)))
      (mapc #'(lambda (expected root)
                (is (set-equal expected
                               (resolve-dag root nodes))))
            (list '(3)
                  '(bop (3))
                  '(foo (bop (3)))
                  '(1 (foo (bop (3))) (3)))
            (list (gethash 'b env)
                  (gethash 'baz env)
                  (gethash 'foo env)
                  (gethash 'a env))))))

(deftest resolution-dag-2 ()
  (with-fixture dep-environment-fixture-2
    (let ((nodes (dependency-nodes-hashtable #'identity #'car #'cdr env)))
      (is (set-equal '("root" ("lvl1:1" ("lvl2:1" ("lvl3:1") ("lvl3:2")))
                       ("lvl1:2" ("lvl2:1" ("lvl3:1") ("lvl3:2")) ("lvl4:1")))
                     (resolve-dag
                      (car (member "root" nodes :key #'car :test #'equal))
                      nodes))))))

(deftest resolution-queue-2 ()
  (with-fixture dep-environment-fixture-2
    (let ((nodes (dependency-nodes-hashtable #'identity #'car #'cdr env)))
      (is (equal '("lvl3:1" "lvl3:2" "lvl2:1" "lvl1:1" "lvl4:1" "lvl1:2" "root")
                 (resolve-queue
                  (car (member "root" nodes :key #'car :test #'equal))
                  nodes))))))

(defixture dep-environment-fixture-unresolvable-1
  (:setup (setq env (make-hash-table))
          (setf (gethash 'root env) (list "root" "lvl1")
                (gethash 'lvl1 env) (list "lvl1" "foo"))))

(defixture dep-environment-fixture-circular-1
  (:setup (setq env (make-hash-table))
          (setf (gethash 'root env) (list "root" "lvl1")
                (gethash 'lvl1 env) (list "lvl1" "lvl2")
                (gethash 'lvl2 env) (list "lvl2" "lvl3")
                (gethash 'lvl3 env) (list "lvl3" "lvl1"))))

(deftest resolution-unresolvable (nodes)
  (signals unresolvable-dependency
    (resolve-all nodes #'resolve-queue))
  (signals unresolvable-dependency
    (resolve-all nodes #'resolve-dag)))

(deftest resolution-circular (nodes)
  (signals circular-dependency
    (resolve-roots nodes #'resolve-queue))
  (signals circular-dependency
    (resolve-roots nodes #'resolve-dag)))

(deftest resolution-unresolvable-1 ()
  (with-fixture dep-environment-fixture-unresolvable-1
    (let* ((nodes (dependency-nodes-hashtable #'identity #'car #'cdr env))
           (condition (resolution-unresolvable nodes)))
      (is (string= "lvl1"
                   (unresolvable-node condition)))
      (is (string= "foo"
                   (unresolvable-dependency condition))))))

(deftest resolution-circular-1 ()
  (with-fixture dep-environment-fixture-circular-1
    (let* ((nodes (dependency-nodes-hashtable #'identity #'car #'cdr env))
           (condition (resolution-circular nodes)))
      (is (string= "lvl1"
                   (circular-node condition)))
      (is (equal '("lvl2" "lvl3")
                 (circular-inter-nodes condition))))))
