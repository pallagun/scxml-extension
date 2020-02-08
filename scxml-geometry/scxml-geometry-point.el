;;; scxml-geometry-point.el --- scxml geometry helpers for points -*- lexical-binding: t -*-

;;; Commentary:
;; An scxml-point  represents a coordinate/point in 2d space.

;;; Code:
(require 'eieio)
(require 'scxml-geometry-core)

(defclass scxml-point ()
  ((x :initarg :x
      :accessor scxml-x
      :type float)
   (y :initarg :y
      :accessor scxml-y
      :type float)))

(defun scxml-point- (x y)
  "Build an scxml-point at X Y.

Equivalent of (scxml-point :x X :y Y).  This function is mostly
to spare a bit of typing."
  (scxml-point :x (float x) :y (float y)))
(cl-defmethod scxml-print ((point scxml-point))
  "Return a stringified version of POINT for human eyes."
  (with-slots (x y) point
    (format "p(%g, %g)" x y)))
(cl-defmethod cl-print-object ((object scxml-point) stream)
  "This seems to be used only for edebug sessions."
  (princ (scxml-print object) stream))
(cl-defmethod scxml-cross-prod ((A scxml-point) (B scxml-point))
  "Cross product: A cross B."
  (- (* (scxml-x A) (scxml-y B))
     (* (scxml-y A) (scxml-x B))))
(cl-defmethod scxml-dot-prod ((A scxml-point) (B scxml-point))
  "Dot product: A dot B."
  (+ (* (scxml-x A) (scxml-x B))
     (* (scxml-y A) (scxml-y B))))
(cl-defmethod scxml-magnitude ((vector scxml-point))
  "Vector magnitude of VECTOR."
  (let ((x (scxml-x vector))
        (y (scxml-y vector)))
    (sqrt (+ (* x x) (* y y)))))
(cl-defmethod scxml-box-magnitude ((vector scxml-point))
  "Box magnitude - how big is a square that can hold VECTOR."
  (max (abs (scxml-x vector))
       (abs (scxml-y vector))))
(cl-defmethod scxml-subtract ((A scxml-point) (B scxml-point))
  "Subtraction: A - B."
  (scxml-point :x (- (scxml-x A) (scxml-x B))
               :y (- (scxml-y A) (scxml-y B))))
(cl-defmethod scxml-distance-sq ((A scxml-point) (B scxml-point))
  "Cartesian distance squared between A and B."
  (let ((del-x (- (scxml-x A) (scxml-x B)))
        (del-y (- (scxml-y A) (scxml-y B))))
    (+ (* del-x del-x) (* del-y del-y))))
(cl-defmethod scxml-distance ((A scxml-point) (B scxml-point))
  "Cartesian distance between A and B."
  (let ((del-x (- (scxml-x A) (scxml-x B)))
        (del-y (- (scxml-y A) (scxml-y B))))
    (sqrt (+ (* del-x del-x) (* del-y del-y)))))
(cl-defmethod scxml-normalized ((vector scxml-point))
  "Build a normalized (magnitude = 1) vector from VECTOR."
  (let ((mag (scxml-magnitude vector)))
    (scxml-point :x (/ (scxml-x vector) mag)
                 :y (/ (scxml-y vector) mag))))
(cl-defmethod scxml-rotate-90 ((vec scxml-point) &optional z-rotation-direction)
  "Build a rotated vector 90 degrees in + or - z RHR rotation.

RHR => Right Hand Rule."
  (if (> (if z-rotation-direction z-rotation-direction 1)
         0)
      ;; rotate +1, CCW
      (scxml-point :x (scxml-y vec)
                   :y (* -1.0 (scxml-x vec)))
    (scxml-point :x (* -1.0 (scxml-y vec))
                 :y (scxml-x vec))))
(cl-defmethod scxml-additive-inverse ((vector scxml-point))
  "Get the additive inverse of VECTOR."
  (scxml-point :x (* -1.0 (scxml-x vector))
               :y (* -1.0 (scxml-y vector))))
(cl-defmethod scxml-add ((A scxml-point) (B scxml-point))
  "Summation of points A and B."
  (scxml-point :x (+ (scxml-x A) (scxml-x B))
               :y (+ (scxml-y A) (scxml-y B))))
(cl-defmethod scxml-incf ((A scxml-point) (B scxml-point))
  "In place modification of A to be A + B returning A."
  (oset A x (+ (scxml-x A) (scxml-x B)))
  (oset A y (+ (scxml-y A) (scxml-y B)))
  A)
(cl-defmethod scxml-scaled ((A scxml-point) alpha)
  "Return A scaled by ALPHA."
  (scxml-point :x (* alpha (scxml-x A))
               :y (* alpha (scxml-y A))))
(cl-defmethod scxml-scaled ((A scxml-point) (per-dimension-alpha scxml-point))
  "Scale A by PER-DIMENSION-ALPHA (scale x by x and y by y)."
  (scxml-point :x (* (scxml-x A) (scxml-x per-dimension-alpha))
               :y (* (scxml-y A) (scxml-y per-dimension-alpha))))
(cl-defmethod scxml-almost-equal ((A scxml-point) (B scxml-point) &optional tolerance)
  "Are A and B within TOLERANCE box distance of eachother"
  (and (scxml-almost-equal (scxml-x A) (scxml-x B) tolerance)
       (scxml-almost-equal (scxml-y A) (scxml-y B) tolerance)))
(cl-defmethod scxml-cardinal-direction-vector? ((A scxml-point))
  "Return non-nil if A is vertical or horizontal (but not zero)."
  (with-slots (x y) A
    (or (scxml-almost-zero x)
        (scxml-almost-zero y))))
    ;; (and (equal 0.0 (abs (* x y)))
    ;;      (not (and (equal 0.0 x)
    ;;                (equal 0.0 y))))))
(cl-defmethod scxml-cardinal-displacement? ((A scxml-point) (B scxml-point))
  "Return non-nil if the displacement from A to B is a cardinal
vector.

A cardinal vector is one that has an X component or Y component
of zero but not both. "
  (scxml-cardinal-direction-vector? (scxml-subtract A B)))
(cl-defmethod scxml-centroid ((point scxml-point))
  "Return the centroid of the point which is the point."
  point)
(cl-defmethod scxml-vector-from-direction ((direction symbol))
  "Return a unit vector in the direction specified by DIRECTION."
  (cond ((eq direction 'up)
         (scxml-point :x 0.0 :y 1.0))
        ((eq direction 'down)
         (scxml-point :x 0.0 :y -1.0))
        ((eq direction 'right)
         (scxml-point :x 1.0 :y 0.0))
        ((eq direction 'left)
         (scxml-point :x -1.0 :y 0.0))
        ('t
         (error "Invalid direction enumerator: %s" direction))))
(cl-defmethod scxml-coarse-direction ((vector scxml-point))
  "Get the closest cardinal direction of a vector as a symbol.

Symbols will be one of: 'up, 'down, 'left, 'right."
  (with-slots (x y) vector
    (let ((abs-x (abs x))
          (abs-y (abs y)))
      (cond ((>= y abs-x) 'up)
            ((>= x abs-y) 'right)
            ((>= abs-y abs-x) 'down)
            ((>= abs-x abs-y) 'left)
            (t (error "Unable to determine coarse direction"))))))
(cl-defmethod scxml-coarse-direction-unit-vector ((vector scxml-point))
  "Get a unit vector in a cardinal direction closest to VECTOR.

This can be viewed as:

(scxml-vector-from-diretion
    (scxml-coarse-direction VECTOR))."
  (with-slots (x y) vector
    (let ((abs-x (abs x))
          (abs-y (abs y)))
      (cond ((> x abs-y) (scxml-point :x 1.0 :y 0.0))
            ((> y abs-x) (scxml-point :x 0.0 :y 1.0))
            ((> abs-x abs-y) (scxml-point :x -1.0 :y 0.0))
            ((> abs-y abs-x) (scxml-point :x 0.0 :y -1.0))
            ('t 'nil)))))

(provide 'scxml-geometry-point)
;;; scxml-geometry-point.el ends here
