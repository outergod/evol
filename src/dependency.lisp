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

;;; conditions
(define-condition circular-dependency (error)
  ((circular-node :initarg :circular-node
                  :reader  circular-node)
   (inter-nodes   :initarg :inter-nodes
                  :reader  circular-inter-nodes)))

(define-condition unresolvable-dependency (error)
  ((node       :initarg :node
               :reader  unresolvable-node)
   (dependency :initarg :dependency
               :reader  unresolvable-dependency)))


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

(defmacro resolve-cond (name nodes seen (&body prologue) &body epilogue)
  "resolve-cond name nodes seen (&body prologue) &body epilogue => context

Insert COND clauses between PROLOGUE clauses and EPILOGUE final clause that
check for erroneous conditions between the NAME of a node, the list of all NODES
and nodes SEEN during dependency resolution and signal errors accordingly if
encountered."
  `(cond
    ,@prologue
    ((null (find-node ,name ,nodes))
     (error 'unresolvable-dependency :node (car ,seen)
                                     :dependency ,name))
    ((member ,name ,seen :test #'equal)
     (error 'circular-dependency :circular-node ,name
                                 :inter-nodes (cdr (member ,name (reverse ,seen) :test #'equal))))
    (t ,@epilogue)))

(defun resolve-queue (root nodes)
  "resolve-queue root nodes => queue

Try to resolve dependencies for ROOT node in NODES and return its dependency
queue.

Queues are suitable for sequential building only."
  (labels ((acc (rec seen branch root)
                (let ((name (car branch))
                      (rest (cdr branch)))
                  (resolve-cond name nodes seen
                    (((null name) rec))            ; ^= end of a branch
                    (if root                       ; new branch
                        (if (member name rec)      ; ^= we've already been here
                            rec
                          (nconc (acc rec (cons name seen) rest nil)
                                 (list name)))
                      (acc (acc rec seen (find-node name nodes) t)
                           seen rest nil))))))
    (acc (list) (list) root t)))

(defun resolve-dag (root nodes)
  "resolve root nodes => dag

Try to resolve dependencies for ROOT node in NODES and return its dependency
dag (directed acyclic graph).

Dags are suitable for parallel building."
  (let ((nodes (copy-tree nodes)))
    (labels ((acc (seen branch root)
                  (let ((name (car branch))
                        (rest (cdr branch)))
                    (resolve-cond name nodes seen
                     (((consp name) branch)             ; ^= we've already been here
                      ((null name) nil))                ; ^= end of a branch
                     (if root                       ; new branch
                         (rplacd branch             ; we want eql nodes, i.e. undirected cycles
                                 (acc (cons name seen) rest nil))
                       (cons (acc seen (find-node name nodes) t)
                             (acc seen rest nil)))))))
      (acc (list) root t))))

(defun resolve-all (nodes resfn &optional (roots nodes))
  "resolve-all nodes resfn &optional (roots nodes) => list of dags or queues

Resolve list of distinct ROOTS (or, by default, everything) in node-list NODES
using resfn."
  (mapcar #'(lambda (node)
              (funcall resfn node nodes))
          roots))

(defun resolve-roots (nodes resfn)
  "resolve-roots nodes resfn => list of dags or queues

Resolve node-list NODES using resfn for all its root-nodes."
  (resolve-all nodes resfn (root-nodes nodes)))
