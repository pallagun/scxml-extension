;;; scxml-drawing-connector --- drawing connector -*- lexical-binding: t -*-

;;; Commentary:
;; A connector is a drawing object that's used to indicate two (or
;; one) other drawing object(s) that are connected and how they are
;; connected.

(require 'scxml-element)
(require 'scxml-drawing)
(require 'scxml-geometry)

;;; Code:
(defclass scxml-drawing-connector ()
  ()
  :documentation "Describes a connection point for a parent drawing and one or zero other drawings.")
(defclass scxml-drawing-connector-unconnected (scxml-drawing-connector)
  ()
  :documentation "A connector for a parent drawing which is not connected to any other drawings.")
(defclass scxml-drawing-connector-connected (scxml-drawing-connector)
  ((node :initarg :node
         :accessor scxml-node
         :type scxml-drawing
         :documentation "TODO: Node seems like a terrible name for this... :("))
  :documentation "A connector for a parent drawing which is connected to another drawing.")

;; Methods that are valid for any connector.
(cl-defgeneric scxml-connection-point ((connector scxml-drawing-connector) &optional offset)
  "Get the point of this CONNECTOR.  Optionally offset by OFFSET to render for human eyes.")
(cl-defgeneric scxml-to-node-direction ((connector scxml-drawing-connector))
  "Get the direction from the CONNECTOR to the connected drawing.

If it's a source connector it'll be the opposite direction of the arrow.
If it's a target connector it'll be the direction of the ending arrowhead.")
(cl-defgeneric scxml-from-node-direction ((connector scxml-drawing-connector))
  "Get the direction this connector exits the drawing as a symbol.

This should be the exact opposite of scxml-to-node-direction.")
;; TODO - there should be a generic for the drawing that the connector connects to.

;; (cl-defmethod scxml-to-node-direction ((connector scxml-drawing-connector-unconnected))
;;   "Returns a symbol"
;;   'up)

;; TODO - break out the dangling connector to a separate file.
(defclass scxml-drawing-connector-dangling (scxml-drawing-connector-unconnected)
  ((dangling-point :initarg :point
                   :accessor scxml-dangling-point
                   :type (or null scxml-point))
   (terminal-direction :initarg :terminal-direction
                       :initform 'up
                       :type symbol))
  :documentation "Describes a floating end point for an arrow")
(cl-defmethod scxml-print ((connector scxml-drawing-connector-dangling))
  "Return a stringified version of CONNECTOR for human eyes."
  (format "ConD(%s/%s)"
          (and (slot-boundp connector 'dangling-point)
               (scxml-print (scxml-dangling-point connector)))
          (and (slot-boundp connector 'terminal-direction)
               (scxml-to-node-direction connector))))
(cl-defmethod scxml-connection-point ((connector scxml-drawing-connector-dangling) &optional offset)
  (scxml-dangling-point connector))
(cl-defmethod scxml-to-node-direction ((connector scxml-drawing-connector-dangling))
  (oref connector terminal-direction))
(cl-defmethod scxml-from-node-direction ((connector scxml-drawing-connector-unconnected))
  "Return an int :(, but is otherwise opposite of terminal-direction."
  (scxml-reverse (oref connector terminal-direction)))
(cl-defmethod scxml-set-point ((connector scxml-drawing-connector-dangling) (point scxml-point))
  "Set the connection point of this dangling connector."
  (oset connector dangling-point point))
(cl-defmethod scxml-set-to-node-direction ((connector scxml-drawing-connector-dangling) (direction symbol))
  "Set the terminal-direction of CONNECTOR to be DIRECTION."
  (unless (memq direction '(up down left right))
    (error "Invalid direction for this connector"))
  (oset connector terminal-direction direction))
(cl-defmethod scxml-build-connector ((connector scxml-drawing-connector-dangling) (target-point scxml-point))
    "Build a brand new connector based off nudging CONNECTOR over to TARGET-POINT.

Will return 'nil if the connector can't be built."
    (scxml-drawing-connector-dangling :point target-point
                                      :terminal-direction (scxml-to-node-direction connector)))


(provide 'scxml-drawing-connector)
;;; scxml-drawing-connector.el ends here
