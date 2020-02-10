;;; 2dg-pixel.el --- gpixel helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Pixel coordinate system is different from cartesian coordinates.
;;
;; X goes to the right, Y goes down and origin is "top left".

;;; Code:
(require 'eieio)

(defclass 2dg-pixel ()
  ((x :initarg :x
      :accessor 2dg-x
      :type integer)
   (y :initarg :y
      :accessor 2dg-y
      :type integer))
  :documentation "A 2d pixel using standard 'image' coordinate system")
(cl-defmethod 2dg-pprint ((px 2dg-pixel))
  "Return a stringified version of PX for human eyes."
  (with-slots (x y) px
    (format "px(%d,%d)" x y)))
(cl-defmethod cl-print-object ((object 2dg-pixel) stream)
  "Pretty print the OBJECT to STREAM."
  (princ (2dg-pprint object) stream))
(cl-defmethod 2dg-equal ((A 2dg-pixel) (B 2dg-pixel))
  "Return non-nil if A and B are the same pixel."
  (and (equal (2dg-x A) (2dg-x B))
       (equal (2dg-y A) (2dg-y B))))
(cl-defmethod 2dg-almost-equal ((A 2dg-pixel) (B 2dg-pixel))
  "Return non-nil if A nd B are almost the same pixel.

Note: this just calls 2dg-equal as pixels are far too coarse to
bother with almost equality checking."
  (2dg-equal A B))
(cl-defmethod 2dg-subtract ((A 2dg-pixel) (B 2dg-pixel))
  "Return the result of pixel A - pixel B.

Note: the coordinate system is that of pixels."
  (2dg-pixel :x (- (2dg-x A) (2dg-x B))
               :y (- (2dg-y A) (2dg-y B))))
(cl-defmethod 2dg-coarse-direction ((vector 2dg-pixel))
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

(provide '2dg-pixel)
;;; 2dg-pixel.el ends here
