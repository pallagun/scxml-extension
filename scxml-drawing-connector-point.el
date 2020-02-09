;;; scxml-drawing-connector-point --- drawing connector for points -*- lexical-binding: t -*-

;;; Commentary:
;; Describes a connector between a parent object and a scxml-point.
;; TODO - I think it should be for a scxml-drawing-point?

;;; Code:
(require 'scxml-drawing-point)
(require 'scxml-drawing-connector)

(defclass scxml-drawing-connector-point (scxml-drawing-connector-connected)
  ((exit-direction :initarg :exit-direction
                   :accessor scxml-exit-direction ;TODO - this shouldn't have an accessor.
                   :type symbol
                   :documentation "May be one of the normal direction integers for up, down, left or right"))
   :documentation "Where something (arrow? \"vertex\"?) connects to a point drawing")
(cl-defmethod scxml-print ((connector scxml-drawing-connector-point))
  "Return a stringified version of CONNECTOR for human eyes."
  (with-slots (exit-direction) connector
    (format "ConP(%s/%s)"
            exit-direction
            (scxml-print (scxml-connection-point connector)))))

(cl-defgeneric scxml-to-node-direction ((connector scxml-drawing-connector-point))
  "Terminal-direction as a symbol"
  (2dg-reverse (scxml-exit-direction connector)))
(cl-defgeneric scxml-from-node-direction ((connector scxml-drawing-connector-point))
  (scxml-exit-direction connector))


(cl-defmethod scxml-build-connector ((connector scxml-drawing-connector-point) (target-point 2dg-point))
  "Build a brand new connector based off nudging CONNECTOR over to TARGET-POINT.

This function will always return a valid connector.  Like the scxml-build-connector function for rectangles this one will 'try' to get as close as possible to the TARGET-POINT.  However, because the connector is constrained to a single point it will always return a connector at that point."
  ;; (when (2dg-almost-equal target-point (scxml-connection-point connector))
  (scxml-drawing-connector-point :exit-direction (scxml-exit-direction connector)
                                 :node (scxml-node connector)))


(cl-defmethod scxml-connection-point ((connector scxml-drawing-connector-point) &optional offset)
  "Get the point of this connector"
  ;; (unless (or (scxml-point-p offset) (numberp offset) (not offset))
  ;;   (error "Offset must be a number of scxml-point"))
  (with-slots ((point node) exit-direction) connector
    (let ((x (2dg-x point))
          (y (2dg-y point)))
      (if (null offset)
          (2dg-point :x x :y y)
        (let* ((offset-unit-vec (2dg-vector-from-direction exit-direction))
               (offset-vec (2dg-scaled offset-unit-vec offset)))
          (2dg-add (2dg-point :x x :y y) offset-vec))))))

(provide 'scxml-drawing-connector-point)
;;; scxml-drawing-connector-point.el ends here
