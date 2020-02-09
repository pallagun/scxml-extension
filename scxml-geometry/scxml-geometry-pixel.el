;;; scxml-geometry-pixel.el --- scxml geometry pixel helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Pixel coordinate system is different from cartesian coordinates.
;;
;; X goes to the right, Y goes down and origin is "top left".

;;; Code:
(require 'eieio)

(defclass scxml-pixel ()
  ((x :initarg :x
      :accessor scxml-x
      :type integer)
   (y :initarg :y
      :accessor scxml-y
      :type integer))
  :documentation "A 2d pixel using standard 'image' coordinate system")
(cl-defmethod scxml-print ((px scxml-pixel))
  "Return a stringified version of PX for human eyes."
  (with-slots (x y) px
    (format "px(%d,%d)" x y)))
(cl-defmethod cl-print-object ((object scxml-pixel) stream)
  "Pretty print the OBJECT to STREAM."
  (princ (scxml-print object) stream))
(cl-defmethod scxml-equal ((A scxml-pixel) (B scxml-pixel))
  "Return non-nil if A and B are the same pixel."
  (and (equal (scxml-x A) (scxml-x B))
       (equal (scxml-y A) (scxml-y B))))
(cl-defmethod 2dg-almost-equal ((A scxml-pixel) (B scxml-pixel))
  "Return non-nil if A nd B are almost the same pixel.

Note: this just calls scxml-equal as pixels are far too coarse to
bother with almost equality checking."
  (scxml-equal A B))
(cl-defmethod 2dg-subtract ((A scxml-pixel) (B scxml-pixel))
  "Return the result of pixel A - pixel B.

Note: the coordinate system is that of pixels."
  (scxml-pixel :x (- (scxml-x A) (scxml-x B))
               :y (- (scxml-y A) (scxml-y B))))
(cl-defmethod 2dg-coarse-direction ((vector scxml-pixel))
  "Return the closest cardinal direction of a vector as a symbol.

Symbols will be one of: 'up, 'down, 'left, 'right.

Note: the coordinate system is that of pixels."
  (with-slots (x y) vector
    (let ((abs-x (abs x))
          (abs-y (abs y)))
      (cond ((> x abs-y) 'right)
            ((> y abs-x) 'down)
            ((>= abs-x abs-y) 'left)
            ((>= abs-y abs-x) 'up)
            ('t (error "Unable to determine coarse direction"))))))

(provide 'scxml-geometry-pixel)
;;; scxml-geometry-pixel.el ends here
