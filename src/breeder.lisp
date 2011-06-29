;;;; evol - breeder.lisp
;;;; Copyright (C) 2009 2011  Alexander Kahl <e-user@fsfe.org>
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
  "with-dependency-nodes var &body body => context

Evaluate BODY in scope of VAR bound to dependency node list from *EVOLVABLES*."
  `(let ((,var (dependency-nodes-hashtable #'evolvable-p #'name #'dependencies *evolvables*)))
     ,@body))

(defmacro eval-reverse-cons ((&body body1) (&body body2))
  "eval-reverse-cons (&body body1) (&body body2) => cons

Evaluate BODY2 and BODY1 in this order and return result cons [BODY1|BODY2]."
  (let ((result (gensym)))
    `(let ((,result (,@body2)))
       (cons (,@body1)
             ,result))))

(defmacro with-new-lock-held (var &body body)
  "with-new-lock-held var &body body => context

Evaluate BODY in locked scope of VAR bound to a new mutex with random name."
  `(let ((,var (bt:make-lock (symbol-name (gensym)))))
     (bt:with-lock-held (,var)
       ,@body)))

(defun safe-getenv (lock var)
  "safe-getenv lock var => result

Mutex-protected GETENV for *EVOLVABLES*."
  (bt:with-lock-held (lock)
    (getenv var :env *evolvables* :expanded nil)))


;;; breeder class
(defclass breeder () ()
  (:documentation "Solitary breeder class."))

(defgeneric breed (breeder evolvable)
  (:documentation "Breed the evolvable so it can hatch")
  (:method ((breeder breeder) (evol evolvable))
    "breed breeder evol => result-list

Breed dependency evolvables of EVOL sequentially depth-first up to and including
EVOL itself. No multithreading, minimal overhead, nil deadlocks."
    (labels ((collect (name)
               (handler-case (list (evolve (getenv name :env *evolvables* :expanded nil)))
                 (hive-burst (condition)
                   (mapcan #'collect (hive-burst-nodes #'resolve-queue condition))))))
      (with-dependency-nodes nodes
        (nreverse (mapcan #'collect (resolve-evol-nodes #'resolve-queue evol nodes)))))))


;;; swarm class
(defclass swarm (breeder patron:patron)
  ((stream      :reader   swarm-stream
                :initarg  :stream
                :initform *standard-output*
                :type     stream
                :documentation "Stream to use for printing")
   (error-stream :reader   swarm-error-stream
                 :initarg  :error-stream
                 :initform *error-output*
                 :type     stream
                :documentation "Stream to use for printing errors")
   (stream-lock :reader        stream-lock
                :initform      (bt:make-lock "swarm-stream-lock")
                :documentation "Mutex to use for printing"))
  (:documentation "Swarms are breeders that heavily rely on threading and also
derive from Patron for thread-pooled queue working."))

(defmethod breed :around ((swarm swarm) (evol evolvable))
  (patron:start-patron swarm)
  (unwind-protect (call-next-method)
    (patron:stop-patron swarm :wait nil :kill nil)))

(defgeneric enqueue-breeding (swarm evolvable)
  (:documentation "Push breeding an evolvable into the worker queue and wait for
the job to finish")
  (:method ((swarm swarm) (evol evolvable))
    "enqueue-breeding swarm evol => result

Create and hold a new mutex, push breeding into the Patron queue and wait for
Patron to call either RESULT-REPORT-FUNCTION or ERROR-REPORT-FUNCTION that are
both set to notify condition against the entry thread of this method which is
waiting. If a condition is found in the finished job, signal it, else return
evolution result."
    (with-new-lock-held job-lock
      (let* ((job-waitqueue (bt:make-condition-variable))
             (finishfn #'(lambda (job)
                           (declare (ignore job))
                           (bt:with-lock-held (job-lock)
                             (bt:condition-notify job-waitqueue))))             
             (job (patron:submit-job
                   swarm
                   (make-instance 'patron:job
                                  :function #'(lambda ()
                                                (evolve evol))
                                  :result-report-function finishfn
                                  :error-report-function  finishfn))))

        (bt:condition-wait job-waitqueue job-lock)

        (if (slot-boundp job 'condition)
            (error (patron:condition-of job))
            (patron:result-of job))))))

(defmethod breed ((swarm swarm) (evol evolvable))
  "breed swarm evolvable => result-dag

Swarm-based evolution. Works through creating new threads per edge / dag node /
branch (equivalent here) while locking the evolvables encountered. Breeding is
forwarded to the Patron queue that works by using a thread pool itself.
The overhead involved caused by using this method should pay off early even for
simple real-life evolutions with mediocre complexity."
  (flet ((format (destination control-string &rest format-arguments)
           (declare (ignore destination))
           (bt:with-lock-held ((stream-lock swarm))
             (apply #'format (swarm-stream swarm) control-string format-arguments))))
    (let ((env-lock (bt:make-lock "env")))
      (labels ((rec (branch)
                 (if (null branch)
                     nil
                     (let* ((evol (safe-getenv env-lock (car branch)))
                            (evol-lock (mutex evol)))
                       (bt:with-lock-held (evol-lock)
                         (locked-breed evol branch)))))
               (locked-breed (evol branch)
                 (or (hatched-p evol)
                     (handler-case (eval-reverse-cons
                                    (bt:with-lock-held (env-lock)
                                      (enqueue-breeding swarm evol))
                                    (mapthread #'rec (cdr branch)))
                       (hive-burst (condition)
                         (locked-breed evol (hive-burst-nodes #'resolve-dag condition)))))))
        (with-dependency-nodes nodes
          (reinitialize-instance swarm :job-capacity (length nodes))
          (rec (resolve-evol-nodes #'resolve-dag evol nodes)))))))
