;;; 2dg-geometry-point.el --- geometry helpers for points -*- lexical-binding: t -*-

;;; Commentary:
;; A 2dg-point represents a coordinate/point in 2d space.

;;; Code:
(require 'eieio)
(require 'scxml-geometry-core)

(defclass 2dg-point ()
  ((x :initarg :x
      :accessor 2dg-x
      :type float)
   (y :initarg :y
      :accessor 2dg-y
      :type float)))

(defun 2dg-point- (x y)
  "Build an 2dg-point at X Y.

Equivalent of (2dg-point :x X :y Y).  This function is mostly
to spare a bit of typing."
  (2dg-point :x (float x) :y (float y)))
(cl-defmethod 2dg-pprint ((point 2dg-point))
  "Return a stringified version of POINT for human eyes."
  (with-slots (x y) point
    (format "p(%g, %g)" x y)))
(cl-defmethod cl-print-object ((object 2dg-point) stream)
  "This seems to be used only for edebug sessions."
  (princ (scxml-print object) stream))
(cl-defmethod 2dg-cross-prod ((A 2dg-point) (B 2dg-point))
  "Cross product: A cross B."
  (- (* (2dg-x A) (2dg-y B))
     (* (2dg-y A) (2dg-x B))))
(cl-defmethod 2dg-dot-prod ((A 2dg-point) (B 2dg-point))
  "Dot product: A dot B."
  (+ (* (2dg-x A) (2dg-x B))
     (* (2dg-y A) (2dg-y B))))
(cl-defmethod 2dg-magnitude ((vector 2dg-point))
  "Vector magnitude of VECTOR."
  (let ((x (2dg-x vector))
        (y (2dg-y vector)))
    (sqrt (+ (* x x) (* y y)))))
(cl-defmethod 2dg-box-magnitude ((vector 2dg-point))
  "Box magnitude - how big is a square that can hold VECTOR."
  (max (abs (2dg-x vector))
       (abs (2dg-y vector))))
(cl-defmethod 2dg-subtract ((A 2dg-point) (B 2dg-point))
  "Subtraction: A - B."
  (2dg-point :x (- (2dg-x A) (2dg-x B))
               :y (- (2dg-y A) (2dg-y B))))
(cl-defmethod 2dg-distance-sq ((A 2dg-point) (B 2dg-point))
  "Cartesian distance squared between A and B."
  (let ((del-x (- (2dg-x A) (2dg-x B)))
        (del-y (- (2dg-y A) (2dg-y B))))
    (+ (* del-x del-x) (* del-y del-y))))
(cl-defmethod 2dg-distance ((A 2dg-point) (B 2dg-point))
  "Cartesian distance between A and B."
  (let ((del-x (- (2dg-x A) (2dg-x B)))
        (del-y (- (2dg-y A) (2dg-y B))))
    (sqrt (+ (* del-x del-x) (* del-y del-y)))))
(cl-defmethod 2dg-normalized ((vector 2dg-point))
  "Build a normalized (magnitude = 1) vector from VECTOR."
  (let ((mag (2dg-magnitude vector)))
    (2dg-point :x (/ (2dg-x vector) mag)
                 :y (/ (2dg-y vector) mag))))
(cl-defmethod 2dg-rotate-90 ((vec 2dg-point) &optional z-rotation-direction)
  "Build a rotated vector 90 degrees in + or - z RHR rotation.

RHR => Right Hand Rule."
  (if (> (if z-rotation-direction z-rotation-direction 1)
         0)
      ;; rotate +1, CCW
      (2dg-point :x (2dg-y vec)
                   :y (* -1.0 (2dg-x vec)))
    (2dg-point :x (* -1.0 (2dg-y vec))
                 :y (2dg-x vec))))
(cl-defmethod 2dg-additive-inverse ((vector 2dg-point))
  "Get the additive inverse of VECTOR."
  (2dg-point :x (* -1.0 (2dg-x vector))
               :y (* -1.0 (2dg-y vector))))
(cl-defmethod 2dg-add ((A 2dg-point) (B 2dg-point))
  "Summation of points A and B."
  (2dg-point :x (+ (2dg-x A) (2dg-x B))
               :y (+ (2dg-y A) (2dg-y B))))
(cl-defmethod 2dg-incf ((A 2dg-point) (B 2dg-point))
  "In place modification of A to be A + B returning A."
  (oset A x (+ (2dg-x A) (2dg-x B)))
  (oset A y (+ (2dg-y A) (2dg-y B)))
  A)
(cl-defmethod 2dg-scaled ((A 2dg-point) alpha)
  "Return A scaled by ALPHA."
  (2dg-point :x (* alpha (2dg-x A))
               :y (* alpha (2dg-y A))))
(cl-defmethod 2dg-scaled ((A 2dg-point) (per-dimension-alpha 2dg-point))
  "Scale A by PER-DIMENSION-ALPHA (scale x by x and y by y)."
  (2dg-point :x (* (2dg-x A) (2dg-x per-dimension-alpha))
               :y (* (2dg-y A) (2dg-y per-dimension-alpha))))
(cl-defmethod 2dg-almost-equal ((A 2dg-point) (B 2dg-point) &optional tolerance)
  "Are A and B within TOLERANCE box distance of eachother"
  (and (2dg-almost-equal (2dg-x A) (2dg-x B) tolerance)
       (2dg-almost-equal (2dg-y A) (2dg-y B) tolerance)))
(cl-defmethod 2dg-cardinal-direction-vector-p ((A 2dg-point))
  "Return non-nil if A is vertical or horizontal (but not zero)."
  (with-slots (x y) A
    (or (2dg-almost-zero x)
        (2dg-almost-zero y)))
  ;; (and (equal 0.0 (abs (* x y)))
  ;;      (not (and (equal 0.0 x)
  ;;                (equal 0.0 y))))))
  )
(cl-defmethod 2dg-cardinal-displacement-p ((A 2dg-point) (B 2dg-point))
  "Return non-nil if the displacement from A to B is a cardinal
vector.

A cardinal vector is one that has an X component or Y component
of zero but not both. "
  (2dg-cardinal-direction-vector-p (2dg-subtract A B)))
(cl-defmethod 2dg-centroid ((point 2dg-point))
  "Return the centroid of the point which is the point."
  point)
(cl-defmethod 2dg-vector-from-direction ((direction symbol))
  "Return a unit vector in the direction specified by DIRECTION."
  (cond ((eq direction 'up)
         (2dg-point :x 0.0 :y 1.0))
        ((eq direction 'down)
         (2dg-point :x 0.0 :y -1.0))
        ((eq direction 'right)
         (2dg-point :x 1.0 :y 0.0))
        ((eq direction 'left)
         (2dg-point :x -1.0 :y 0.0))
        ('t
         (error "Invalid direction enumerator: %s" direction))))
(cl-defmethod 2dg-coarse-direction ((vector 2dg-point))
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
(cl-defmethod 2dg-coarse-direction-unit-vector ((vector 2dg-point))
  "Get a unit vector in a cardinal direction closest to VECTOR.

This can be viewed as:

(scxml-vector-from-diretion
    (2dg-coarse-direction VECTOR))."
  (with-slots (x y) vector
    (let ((abs-x (abs x))
          (abs-y (abs y)))
      (cond ((> x abs-y) (2dg-point :x 1.0 :y 0.0))
            ((> y abs-x) (2dg-point :x 0.0 :y 1.0))
            ((> abs-x abs-y) (2dg-point :x -1.0 :y 0.0))
            ((> abs-y abs-x) (2dg-point :x 0.0 :y -1.0))
            ('t 'nil)))))

(provide 'scxml-geometry-point)
;;; scxml-geometry-point.el ends here
