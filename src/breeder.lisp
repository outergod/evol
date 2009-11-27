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
(defmacro with-dependency-nodes (var &body body)
  `(let ((,var (dependency-nodes-hashtable #'evolvable-p #'name #'dependencies *environment*)))
     ,@body))

(defmacro eval-reverse-cons ((&body body1) (&body body2))
  `(let ((result (,@body2)))
     (cons (,@body1)
           result)))

(defmacro with-new-lock-held (var &body body)
  `(let ((,var (bt:make-lock (symbol-name (gensym)))))
     (bt:with-lock-held (,var)
       ,@body)))

(defun safe-getenv (lock var)
  "safe-getenv lock var => mixed

Mutex-protected GETENV."
  (bt:with-lock-held (lock)
    (getenv var :expanded nil)))


;;; breeder class
(defclass breeder () ()
  (:documentation "Solitary breeder class."))

(defgeneric breed (breeder evolvable)
  (:documentation "Breed the evolvable so it can hatch")
  (:method ((breeder breeder) (evol evolvable))
    (with-dependency-nodes nodes
      (nreverse (mapcar #'(lambda (name)
                            (evolve (getenv name :expanded nil)))
                        (resolve-queue (find-node (name evol) nodes) nodes))))))


;;; swarm class
(defclass swarm (breeder patron:patron)
  ((stream :reader   fstream
           :initarg  :stream
           :initform *standard-output*
           :documentation "Stream to use for printing"))
  (:documentation "Swarms are breeders that heavily rely on threading and also
derive from Patron for thread-pooled queue working."))

(defgeneric safe-format (swarm lock control-string &rest format-arguments)
  (:documentation "Mutex lock based FORMAT for swarms.")
  (:method ((swarm swarm) lock control-string &rest format-arguments)
    (bt:with-lock-held (lock)
      (apply #'format (fstream swarm) control-string format-arguments))))

(defmethod breed :around ((swarm swarm) (evol evolvable))
  (patron:start-patron swarm)
  (unwind-protect (call-next-method)
    (patron:stop-patron swarm :wait nil :kill nil)))

(defgeneric enqueue-breeding (swarm evolvable stream-lock)
  (:documentation "Push breeding an evolvable into the worker queue and wait for
the job to finish")
  (:method ((swarm swarm) (evol evolvable) stream-lock)
    (with-new-lock-held job-lock
      (let* ((job-waitqueue (bt:make-condition-variable))
             (finishfn #'(lambda (job)
                           (declare (ignore job))
                           (bt:with-lock-held (job-lock)
                             (bt:condition-notify job-waitqueue))))
             (formatfn #'(lambda (destination control-string &rest format-arguments)
                           (declare (ignore destination))
                           (safe-format swarm stream-lock control-string format-arguments)))
             (job (patron:submit-job
                   swarm
                   (make-instance 'patron:job
                                  :function #'(lambda ()
                                                (evolve evol :formatfn formatfn))
                                  :result-report-function finishfn
                                  :error-report-function  finishfn))))
        (bt:condition-wait job-waitqueue job-lock)
        (if (slot-boundp job 'condition)
            (error (patron:condition-of job))
          (patron:result-of job))))))

(defmethod breed ((swarm swarm) (evol evolvable))
  "breed swarm evolvable => dag

Swarm-based evolution. Works through creating new threads per edge / dag node
while locking the evolvables encountered. Evolution is forwarded to the Patron
queue that works by using a thread pool itself so welcome to deadlock
wonderland!"
  (let ((stream-lock (bt:make-lock "stream"))
        (env-lock    (bt:make-lock "env")))
    (labels ((acc (branch)
                  (if (null branch)
                      nil
                    (let* ((evol (safe-getenv env-lock (car branch)))
                           (evol-lock (mutex evol)))
                      (bt:with-lock-held (evol-lock)
                        (if (hatched evol)
                            t
                          (eval-reverse-cons
                           (enqueue-breeding swarm evol stream-lock)
                           (mapthread #'(lambda (branch)
                                          (acc branch))
                                      (cdr branch)))))))))
     (with-dependency-nodes nodes
       (reinitialize-instance swarm :job-capacity (length nodes))
       (acc (resolve-dag (find-node (name evol) nodes) nodes))))))
