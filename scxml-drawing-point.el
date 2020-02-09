;;; scxml-drawing-point.el --- scxml drawing "point" functions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'scxml-drawing)
(require 'scxml-geometry-point)

(defclass scxml-drawing-point (2dg-point scxml-drawing)
  ((label :initarg :label
          :accessor scxml-label
          :type string
          :documentation "The label for this 'point' drawing.  Should be a single char"))
  :documentation "Represents point based drawing.  Basically a label floating at a point.")
(cl-defmethod scxml-print ((pt 2dg-point))
  (2dg-pprint pt))
(cl-defmethod scxml-print ((pt scxml-drawing-point))
  (format "[%s]@%s" (scxml-label pt)
          (2dg-pprint pt)))
(cl-defmethod scxml-num-edit-idxs ((pt-drawing scxml-drawing-point))
  "A point drawing has no edit idxs"
  0)
(cl-defmethod scxml-edit-idx-point ((pt-drawing scxml-drawing-point) (idx integer))
  "A point drawing has no edit idxs, this will always error"
  (error "Invalid edit-mode idx"))
(cl-defmethod scxml-build-move-edited ((pt-drawing scxml-drawing-point) (move-vector 2dg-point) (viewport scxml-viewport))
  "Given a PT-DRAWING and a MOVE-VECTOR, apply the movement."
  ;; TODO - this can probabyl just be a clone and incf.
  (let ((new-pt (2dg-add pt-drawing move-vector)))
    (scxml-drawing-point :x (2dg-x new-pt)
                         :y (2dg-y new-pt)
                         :label (scxml-label pt-drawing)
                         :parent (scxml-parent pt-drawing))))
(cl-defmethod scxml-build-idx-edited ((pt-drawing scxml-drawing-point) (edit-idx integer) (move-vector 2dg-point) (viewport scxml-viewport))
  "A point drawing has no edit idxs, this will always error."
  (error "Unable to build an idx-edited scxml-drawing-point object."))

(cl-defmethod scxml-build-hint ((pt 2dg-point) (parent-canvas scxml-inner-canvas))
  "Build a hint for PT inside of PARENT-CANVAS."
  (2dg-relative-coordinates parent-canvas pt))
(cl-defmethod scxml-get-inner-canvas ((pt scxml-drawing-point))
  "It's a point, there's no space inside it."
  nil)
(cl-defmethod scxml-leaving-segment-collision-edge ((source scxml-drawing-point) (dest 2dg-point))
  "If you leave SOURCE headed towards DEST, which edge do you hit?

Returned as one of 4 symbols: 'up, 'down, 'left, 'right."
  (2dg-coarse-direction (2dg-subtract dest source)))

(provide 'scxml-drawing-point)
;;; scxml-drawing-point.el ends here
