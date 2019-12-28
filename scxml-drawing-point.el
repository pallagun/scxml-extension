;;; scxml-drawing-point.el --- scxml drawing "point" functions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'scxml-drawing)
(require 'scxml-geometry-point)

(defclass scxml-drawing-point (scxml-point scxml-drawing)
  ((label :initarg :label
          :accessor scxml-label
          :type string
          :documentation "The label for this 'point' drawing.  Should be a single char"))
  :documentation "Represents point based drawing.  Basically a label floating at a point.")

(cl-defmethod scxml-num-edit-idxs ((pt-drawing scxml-drawing-point))
  "A point drawing has no edit idxs"
  0)
(cl-defmethod scxml-edit-idx-points ((pt-drawing scxml-drawing-point))
  "A point drawing has no edit idxs, this will always return nil"
  nil)
(cl-defmethod scxml-edit-idx-point ((pt-drawing scxml-drawing-point) (idx integer))
  "A point drawing has no edit idxs, this will always error"
  (error "Invalid edit-mode idx"))
(cl-defmethod scxml-build-move-edited ((pt-drawing scxml-drawing-point) (move-vector scxml-point))
  "Given a PT-DRAWING and a MOVE-VECTOR, apply the movement."
  ;; TODO - this can probabyl just be a clone and incf.
  (let ((new-pt (scxml-add pt-drawing move-vector)))
    (scxml-drawing-point :x (scxml-x new-pt)
                         :y (scxml-y new-pt)
                         :label (scxml-label pt-drawing)
                         :parent (scxml-parent pt-drawing))))
(cl-defmethod scxml-build-idx-edited ((pt-drawing scxml-drawing-point) (edit-idx integer) (move-vector scxml-point))
  "A point drawing has no edit idxs, this will always error."
  (error "Unable to build an idx-edited scxml-drawing-point object."))
(cl-defmethod scxml-build-drawing ((initial scxml-initial) (canvas scxml-canvas))
  "Build drawing helper"
  (scxml---drawing-logger "scxml--build-drawing: %s" (scxml-print initial))
  (scxml---drawing-logger "scxml--build-drawing: canvas: %s" (scxml-print canvas))
  (scxml---drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? initial)
                          (if (scxml-element-drawing initial) 't 'nil))
  (let ((hint (scxml--hint initial))
        (highlight (scxml--highlight initial))
        (centroid (scxml-centroid canvas)))
    (if (null hint)
        ;; Generate the drawing (not based on a hint)
        (scxml-drawing-point :x (scxml-x centroid)
                             :y (scxml-y centroid)
                             :label "I" ;label for 'Initial'
                             :highlight highlight
                             :edit-idx nil
                             :parent initial)
      ;; todo - clean up this implementation, it's duplicative :(
      (let* ((parent (scxml-parent initial))
             (parent-drawing (when (object-of-class-p parent 'scxml-drawable-element)
                               (scxml-element-drawing parent)))
             (parent-drawing-canvas (if parent-drawing
                                        (scxml--get-inner-canvas parent-drawing)
                                      canvas)))
        (when (not (scxml-inner-canvas-p parent-drawing-canvas))
          (error "Not sure how to continue here :("))

        (let ((placement (scxml-absolute-coordinates parent-drawing-canvas hint)))
          (scxml-drawing-point :x (scxml-x placement)
                               :y (scxml-y placement)
                               :label "I" ;label for 'Initial'
                               :highlight highlight
                               :edit-idx nil
                               :parent initial))))))
(cl-defmethod scxml-build-hint ((pt scxml-point) (parent-canvas scxml-inner-canvas))
  "Build a hint for PT inside of PARENT-CANVAS."
  (scxml-relative-coordinates parent-canvas pt))
(cl-defmethod scxml--get-inner-canvas ((pt scxml-drawing-point))
  "It's a point, there's no space inside it."
  nil)

(provide 'scxml-drawing-point)
;;; scxml-drawing-point.el ends here
