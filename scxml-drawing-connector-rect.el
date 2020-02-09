;;; scxml-drawing-connector-rect --- drawing connector for rects -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'scxml-drawing-rect)
(require 'scxml-drawing-connector)


(defclass scxml-drawing-connector-rect (scxml-drawing-connector-connected)
  ((edge :initarg :edge
         :type symbol)                  ;todo - validation here sould be chaneg to (member up down left right)
   (parametric :initarg :parametric
               :accessor scxml-edge-parametric)))

(cl-defmethod scxml-print ((connector scxml-drawing-connector-rect))
    "Return a stringified version of CONNECTOR for human eyes."
  (with-slots (edge parametric) connector
    (format "ConR(%s@%f/%s)" edge parametric (scxml-print (scxml-connection-point connector)))))

(cl-defgeneric scxml-to-node-direction ((connector scxml-drawing-connector-rect))
  "terminal-direction"
  (2dg-reverse (oref connector edge)))

(cl-defmethod scxml-from-node-direction ((connector scxml-drawing-connector-rect))
  "Formerly 'scxml-node-edge'"
  (oref connector edge))

(cl-defmethod scxml-connection-point ((connector scxml-drawing-connector-rect) &optional offset)
  "Get the point of this connector."
  (with-slots ((rect node) edge parametric) connector
    (let ((raw-point (2dg-parametric (scxml-edge rect edge) parametric)))
      (if (null offset)
          raw-point
        (let ((offset-vec (2dg-vector-from-direction edge)))
          (2dg-add raw-point (2dg-scaled offset-vec offset)))))))
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
                                                        (oref connector edge)
                                                        (when allow-partial-snap 1.01))))
          (if parametric
              (scxml-drawing-connector-rect :node (scxml-node connector)
                               :edge (oref connector edge)
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

(cl-defmethod scxml-build-connector ((connector scxml-drawing-connector-rect) (target-point 2dg-point))
  "Build a brand new connector based off nudging CONNECTOR over to TARGET-POINT.

This function may return connectors which aren't exactly at
TARGET-POINT.

When told to build a connector at a point which is valid (on the
edges of the rectangle) this function will do so.

When told to build a connector at a point which is invalid (not
on the edges of the rectangle) this function will return a valid
connector which is as close as possible to the TARGET-POINT but
is still valid."
  (with-slots ((rect node) edge) connector
    (cl-loop with best-connection-set = nil
             for edge-candidate in '(up down left right)
             for edge-segment = (scxml-edge rect edge-candidate)
             for edge-parametric = (scxml-get-closest-parametric edge-segment target-point t)
             for edge-point = (2dg-absolute-coordinates edge-segment edge-parametric)
             for distance-sq = (2dg-distance-sq edge-point target-point)
             if (null best-connection-set)
               do (setq best-connection-set (list distance-sq edge-candidate edge-parametric))
             else
               if (or (< distance-sq (first best-connection-set))
                      (and (<= distance-sq (first best-connection-set))
                           (eq edge-candidate edge)))
                 do (setq best-connection-set (list distance-sq edge-candidate edge-parametric))
               end
             end
             finally return (scxml-drawing-connector-rect :node rect
                                                          :edge (second best-connection-set)
                                                          :parametric (third best-connection-set)))))

(cl-defmethod scxml-build-connector-old ((connector scxml-drawing-connector-rect) (target-point 2dg-point))
  "Build a brand new connector based off nudging CONNECTOR over to TARGET-POINT.

Will return nil if the connector can't be built."
  (let ((allow-edge-change t))
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
                 )))))

(provide 'scxml-drawing-connector-rect)
;;; scxml-drawing-connector-rect.el ends here
