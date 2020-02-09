;;; 2dg-geometry-core.el --- 2d geometry core/common components -*- lexical-binding: t -*-

;;; Commentary:
;; Coordinate system is a normal cartesian 2d X,Y.
;;
;; The "origin" is in the bottom left and as "x" increases things move
;; to the right and as "y" increases things move up.

;;; Code:
(defconst 2dg--almost-zero 0.00001
  "Used for things like \"Are you close enough to zero?\".")

(defsubst 2dg-reverse (direction)
  "Return the reverse direction of DIRECTION.

DIRECTION should be a symbol in the set [up, down, left, right]."
  (cond ((eq direction 'up) 'down)
        ((eq direction 'down) 'up)
        ((eq direction 'left) 'right)
        ((eq direction 'right) 'left)
        (t (error "Invalid direction symbol: %s" direction))))
(defsubst 2dg-direction-axis (direction)
  "Given a DIRECTION return the axis for the direction.

DIRECTION should be a symbol in the set [up, down, left, right].
Returned value with be one of two symbols [vertical, horizontal]."
  (cond ((or (eq direction 'up)
             (eq direction 'down))
         'vertical)
        ((or (eq direction 'left)
             (eq direction 'right))
         'horizontal)
        ('t
         (error "Unable to provide axis for direction [%s]" direction))))

;; Generic functions applicable to many geometric data types
(cl-defgeneric 2dg-contains (container containee &optional evaluation-mode)
  "Does CONTAINER contain CONTAINEE using with EVALUATION-MODE.

Evaluation mode can be one of:

- 'scrict - container must contain containee and containee may
not touch container's boundary.

- 'stacked - container must contain containee and containee can be at
the 'start' edge(s) of container but not the 'end' edge(s).  This is
used for proper containment checks for a series of touching
containers.

- nil (or anything else) - non strict containment
container contains containee even if containee is part of container's
boundary.")
(cl-defgeneric 2dg-distance (A B)
  "Return the minimum cartesian distance from A to B.")
(cl-defgeneric 2dg-add (A B)
  "Return the addition of A and B.")
(cl-defgeneric 2dg-incf (A B)
  "Modify A to be (2dg-add A B), returning A.")
(cl-defgeneric 2dg-subtract (A B)
  "Return the subtraction of B from A (A - B).")
(cl-defgeneric 2dg-scaled (thing alpha)
  "Scale THING by ALPHA.")
(defsubst 2dg-inverse-scaled (thing alpha)
  "Scale THING by 1/ALPHA."
  (2dg-scaled thing (/ 1.0 alpha)))
(cl-defgeneric 2dg-pprint (thing)
  "Return a string containing the pretty print of THING.")
(cl-defgeneric 2dg-almost-equal (A B &optional tolerance)
  "Return non-nil if A and B almost equal within TOLERANCE.")
(cl-defgeneric 2dg-relative-coordinates (coordinate-reference target)
  "Using COORDINATE-REFERENCE return TARGET in relative coordinates.")
(cl-defgeneric 2dg-absolute-coordinates (coordinate-reference relative)
  "Using COORDINATE-REFERENCE return RELATIVE in absolute coordinates.")
(cl-defgeneric 2dg-parametric (thing parametric-coordinate)
  "Given a THING, return the subset of it at a given PARAMETRIC-COORDINATE.

This can be thought of as removing the first dimension of THING by
locking it at PARAMETRIC-COORDINATE and returning the result.")
(cl-defgeneric 2dg-coarse-direction (direction-indicator)
  "Given a DIRECTION-INDICATOR return the coarse direction.

Coarse direction is one of: 'up, 'down, 'left, 'right")
(cl-defgeneric 2dg-intersection (A B)
  "Return the intersection of A and B.")
(cl-defgeneric 2dg-has-intersection (A B &optional evaluation-mode)
  "Return non-nil if A and B have any intersection.

This might possibly be faster than 2dg-intersection.
Uses the same EVALUATION-MODE spec as 2dg-contains.

Note: evaluation mode only applies to A, the first arg.
Therefore switching the values of arguments A and B may yield
different results.")

;; Concrete implementations
(cl-defmethod 2dg-almost-equal (A B &optional tolerance)
  "Return non-nil if A and B almost equal within a TOLERANCE.

If TOLERANCE is not provided then 2dg--almost-zero will be
used.  This is the default implementation."
  (< (abs (- A B)) (or tolerance 2dg--almost-zero)))
(defsubst 2dg-almost-zero (A &optional tolerance)
  "Return non-nil if A is almost equal to zero within TOLERANCE."
  (2dg-almost-equal A 0.0 tolerance))


(provide 'scxml-geometry-core)
;;; scxml-geometry-core.el ends here
