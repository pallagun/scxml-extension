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
  (scxml-reverse (scxml-exit-direction connector)))
(cl-defgeneric scxml-from-node-direction ((connector scxml-drawing-connector-point))
  (scxml-exit-direction connector))


(cl-defmethod scxml-build-connector ((connector scxml-drawing-connector-point) (target-point scxml-point))
  "Build a brand new connector based off nudging CONNECTOR over to TARGET-POINT.

Will return 'nil if the connector can't be built."
  (when (scxml-almost-equal target-point (scxml-connection-point connector))
    (scxml-drawing-connector-point :exit-direction (scxml-exit-direction connector)
                                   :node (scxml-node connector))))

(cl-defmethod scxml-connection-point ((connector scxml-drawing-connector-point) &optional offset)
  "Get the point of this connector"
  ;; (unless (or (scxml-point-p offset) (numberp offset) (not offset))
  ;;   (error "Offset must be a number of scxml-point"))
  (with-slots ((point node) exit-direction) connector
    (let ((x (scxml-x point))
          (y (scxml-y point)))
      (if (null offset)
          (scxml-point :x x :y y)
        (let* ((offset-unit-vec (scxml-vector-from-direction exit-direction))
               (offset-vec (scxml-scaled offset-unit-vec offset)))
          (scxml-add (scxml-point :x x :y y) offset-vec))))))

(provide 'scxml-drawing-connector-point)
;;; scxml-drawing-connector-point.el ends here
