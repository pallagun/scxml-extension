;;; scxml-drawing-connector-rect --- drawing connector for rects -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'scxml-drawing-rect)
(require 'scxml-drawing-connector)


(defclass scxml-drawing-connector-rect (scxml-drawing-connector-connected)
  ((edge :initarg :edge
         :accessor scxml-node-edge
         :type symbol
         :documentation "TODO : bad name, rename this accessor")
   (parametric :initarg :parametric
               :accessor scxml-edge-parametric)))

(cl-defmethod scxml-print ((connector scxml-drawing-connector-rect))
    "Return a stringified version of CONNECTOR for human eyes."
  (with-slots (edge parametric) connector
    (format "ConR(%s@%f/%s)" edge parametric (scxml-print (scxml-connection-point connector)))))

(cl-defgeneric scxml-terminal-direction ((connector scxml-drawing-connector-rect))
  "terminal-direction"
  (scxml-reverse (scxml-node-edge connector)))
(cl-defmethod scxml-exit-direction ((connector scxml-drawing-connector-rect))
  (scxml-node-edge connector))

(cl-defmethod scxml-connection-point ((connector scxml-drawing-connector-rect) &optional offset)
  "Get the point of this connector."
  (with-slots ((rect node) edge parametric) connector
    (let ((raw-point (scxml-parametric (scxml-edge rect edge) parametric)))
      (if (null offset)
          raw-point
        (let ((offset-vec (scxml-vector-from-direction edge)))
          (scxml-add raw-point (scxml-scaled offset-vec offset)))))))
(cl-defmethod scxml-snap ((connector scxml-drawing-connector-rect) &optional allow-partial-snap)
  ;; TODO - should this function be deleted?  It's a snap function.
  "Return a snapped version of CONNECTOR if possible.

Might return the connector right back to you if alreay snapped."
  (let* ((point (scxml-connection-point connector))
         (snapped-point (scxml-snap point)))
    (if (and (equal (scxml-x point) (scxml-x snapped-point))
             (equal (scxml-y point) (scxml-y snapped-point)))
        ;; connector already on snap point
        connector
      (progn
        (let ((parametric (scxml---connector-parametric connector
                                                        snapped-point
                                                        (scxml-node-edge connector)
                                                        (when allow-partial-snap 1.01))))
          (if parametric
              (scxml-drawing-connector-rect :node (scxml-node connector)
                               :edge (scxml-node-edge connector)
                               :parametric parametric)
            (error "Unable to snap connector - is this something that can be ignored? probably not")))))))

(defun scxml---connector-parametric (connector target-point edge &optional tolerance)
  "Return a parametric if CONNECTOR can be moved to satisfy TARGET-POINT on EDGE-ENUMERATOR."
  ;; TODO - is this called directly?  Might be able to make it a letf
  (let* ((edge-segment (scxml-edge (scxml-node connector) edge))
         (edge-parametric (scxml-get-parametric edge-segment target-point tolerance)))
    (when (and edge-parametric
               (<= 0 edge-parametric)
               (<= edge-parametric 1.0))
      edge-parametric)))

(cl-defmethod scxml-build-connector ((connector scxml-drawing-connector-rect) (target-point scxml-point) &optional allow-edge-change)
  "Build a brand new connector based off nudging CONNECTOR over to TARGET-POINT.

Will return nil if the connector can't be built."
  (with-slots ((rect node) edge) connector
    (if (not allow-edge-change)
        (let ((parametric (scxml---connector-parametric connector target-point edge)))
          (when parametric
            (scxml-drawing-connector-rect :node rect :edge edge :parametric parametric)))
      (cl-loop for edge-candidate in (cons edge
                                            (seq-filter (lambda (x) (not (equal x edge)))
                                                        (list 'up
                                                              'down
                                                              'left
                                                              'right)))
               for parametric = (scxml---connector-parametric connector
                                                              target-point
                                                              edge-candidate)
               until parametric
               finally return (when parametric
                                (scxml-drawing-connector-rect :node rect
                                                 :edge edge-candidate
                                                 :parametric parametric))
               ))))

(provide 'scxml-drawing-connector-rect)
;;; scxml-drawing-connector-rect.el ends here
