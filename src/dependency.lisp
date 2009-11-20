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

(defun dependency-nodes-hashtable (predicate namefn dependencyfn env)
  "dependency-nodes-hashtable predicate namefn dependencyfn env => node-list

Filter out all values satisfying PREDICATE from hashtable ENV and return result
list of transforming each one into a node list by calling NAMEFN and
DEPENDENCYFN."
  (mapcar #'(lambda (elt)
              (dependency-node namefn dependencyfn elt))
          (remove-if-not predicate
                         (alexandria:hash-table-values env))))

(defun dependency-node (namefn dependencyfn object)
  "dependency-node namefn dependencyfn object => node

Transform OBJECT into a node (list) by appending the results of calling NAMEFN
and DEPENDENCYFN against it."
  (append (list (funcall namefn object))
          (funcall dependencyfn object)))

(defun root-nodes (nodes)
  "root-nodes nodes => node-list

Return all root nodes in NODES.
Root nodes are identified by not being referenced as a dependency by any other
node. This is done by building the difference (complement of intersection) between the
ones having dependencies and the complete set."
  (labels ((acc (rec nodes)
                (if (null nodes)
                      (mapcar #'list rec)
                  (acc (union rec
                              (cdar nodes))
                       (cdr nodes)))))
    (set-difference nodes (acc (list) nodes) :key #'car :test #'equal)))

(defun leaf-nodes (nodes)
  "leaf-nodes nodes => node-list

Return all leaf nodes in NODES.
Leaf nodes are simply the ones with no dependencies at all."
  (remove-if #'cdr nodes))

(defun find-node (name nodes)
  "find-node name nodes => node

Find and return node designated by NAME in NODES."
  (find name nodes :key #'car :test #'equal))

(defun resolve (root nodes)
  "resolve root nodes => dag

Try to resolve dependencies for ROOT node in NODES and return its dependency
dag (directed acyclic graph)."
  (let ((nodes (copy-tree nodes)))
    (labels ((acc (seen branch root)
                  (let ((name (car branch))
                        (rest (cdr branch)))
                    (cond
                     ((consp name) branch)             ; ^= we've already been here
                     ((null name) nil)                 ; ^= end of a branch
                     ((null (find-node name nodes))    ; speaks for itself
                      (error (concatenate 'string "TODO unresolved " name)))
                     ((member name seen :test #'equal) ; so does this
                      (error "TODO circular"))
                     (t (if root                       ; new branch
                            (rplacd branch             ; we want eql nodes, i.e. undirected cycles
                                    (acc (cons name seen) rest nil))
                          (cons (acc seen (find-node name nodes) t)
                                (acc seen rest nil))))))))
      (acc (list) root t))))

(defun resolve-all (nodes &optional (roots nodes))
  "resolve-all nodes &optional (roots nodes) => list of dags

Resolve list of distinct ROOTS (or, by default, everything) in node-list NODES."
  (mapcar #'(lambda (node)
              (resolve node nodes))
          roots))

(defun resolve-roots (nodes)
  "resolve-roots nodes => list of dags

Resolve node-list NODES for all its root-nodes."
  (resolve-all nodes (root-nodes nodes)))
