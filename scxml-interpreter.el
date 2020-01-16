;;; scxml-interpreter --- instantiate and run state machines -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'eieio)
(require 'scxml-element)
(require 'scxml-elements)

(defun scxml--queue ()
  "real quick, build a fifo, stolen from http://irreal.org/blog/?p=40

Usage:
  (let ((queue (scxml--queue)))
    (funcall queue 'push 'my-data)
    (funcall queue 'push 'my-data2)
    (funcall queue 'pop)"
  (let ((front) (back))
    (lambda (cmd &optional data)
      (cond ((eq cmd 'push)
             (push data front))
            ((eq cmd 'pop)
             (when (and (not back) front)
               (setq back (nreverse front))
               (setq front nil))
             (prog1
                 (car back)
               (setq back (cdr back))))))))

(defclass scxml-instance ()
  ((_type :type scxml-scxml
          :documentation "The scxml state machine type to run")
   ;; The rest of these slots are based off the
   ;; "Algorithm for SCXML Interpretation right from the docs.
   (_configuration :type list
                   :initform nil)
   (_states-to-invoke :type list
                      :initform nil)
   (_internal-queue :initform (scxml--queue))
   (_external-queue :initform (scxml--queue))
   (_history-value :type hash-table
                   :initform (make-hash-table))
   (_datamodel :initform nil)
   (_running ; :type boolean?
             :initform nil))
  :documentation "An instance of a state machine.")

(cl-defgeneric scxml-build-instance ((type scxml-scxml))
  "Build an scxml-instance")
(cl-defmethod scxml-build-instance ((type scxml-scxml))
  ;; TODO - validate?
  (let ((instance (scxml-instance)))
    (oset instance _type type)
    instance))

(provide 'scxml-interpreter)
;;; scxml-interpreter.el ends here
