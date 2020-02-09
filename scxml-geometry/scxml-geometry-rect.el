;;; scxml-geometry-rect.el --- scxml geometry rectangle helpers -*- lexical-binding: t -*-

;;; Commentary:
;; An 2dg-rect represents a rectangle in 2d space.  The rectangle is
;; constrained to be axis aligned

;;; Code:
(require 'eieio)
(require 'scxml-geometry-point)
(require 'scxml-geometry-span)
(require 'scxml-geometry-segment)

(defclass 2dg-rect ()
  ((x-min :initarg :x-min
          :accessor 2dg-x-min
          :type float
          :initform 0.0)
   (y-min :initarg :y-min
          :accessor 2dg-y-min
          :type float
          :initform 0.0)
   (y-max :initarg :y-max
          :type float
          :accessor 2dg-y-max)
   (x-max :initarg :x-max
          :type float
          :accessor 2dg-x-max))
  :documentation "2d rectangle")
(cl-defmethod 2dg-pprint ((rect 2dg-rect))
  "Return a stringified version of RECT for human eyes."
  (with-slots (x-min x-max y-min y-max) rect
    (format "r[x[%f, %f], y[%f, %f]]" x-min x-max y-min y-max)))
(cl-defmethod cl-print-object ((object 2dg-rect) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dg-pprint object) stream))
(cl-defmethod 2dg-add ((A 2dg-rect) (B 2dg-point))
  "Return a rectangle representing A displaced by B."
  (with-slots (x-min x-max y-min y-max) A
    (with-slots ((del-x x) (del-y y)) B
      (2dg-rect :x-min (+ x-min del-x)
                  :x-max (+ x-max del-x)
                  :y-min (+ y-min del-y)
                  :y-max (+ y-max del-y)))))
(cl-defmethod 2dg-incf ((A 2dg-rect) (B 2dg-point))
  "In place modification of A to be A + B, returning A."
  (with-slots (x y) B
    (with-slots (x-min x-max y-min y-max) A
      (oset A x-min (+ x x-min))
      (oset A y-min (+ y y-min))
      (oset A x-max (+ x x-max))
      (oset A y-max (+ y y-max))))
  A)
(cl-defmethod 2dg-TL ((rect 2dg-rect))
  "Top Left point"
  (2dg-point :x (2dg-x-min rect)
             :y (2dg-y-max rect)))
(cl-defmethod 2dg-TR ((rect 2dg-rect))
  "Top Right point"
  (2dg-point :x (2dg-x-max rect)
               :y (2dg-y-max rect)))
(cl-defmethod 2dg-BR ((rect 2dg-rect))
  "Bottom Right point"
  (2dg-point :x (2dg-x-max rect)
               :y (2dg-y-min rect)))
(cl-defmethod 2dg-BL ((rect 2dg-rect))
  "Bottom Left point"
  (2dg-point :x (2dg-x-min rect)
               :y (2dg-y-min rect)))
(cl-defmethod 2dg-edge ((rect 2dg-rect) (edge symbol))
  "Return a segment describing the EDGE of RECT.

EDGE may be one of: 'up, 'down, 'left, 'right.  Edges are
returned with start and end points corresponding to a
counterclockwise traversal of the rectangle's outer edge."
  (cond ((eq edge 'up)
         (2dg-top rect))
        ((eq edge 'down)
         (2dg-bottom rect))
        ((eq edge 'right)
         (2dg-right rect))
        ((eq edge 'left)
         (2dg-left rect))
        ('t
         (error "Invalid edge enumerator"))))
(cl-defmethod 2dg-top ((rect 2dg-rect))
  "Top (a.k.a. up) edge CCW spin"
  (2dg-segment :start (2dg-TR rect)
                 :end (2dg-TL rect)))
(cl-defmethod 2dg-left ((rect 2dg-rect))
  "Left edge CCW spin"
  (2dg-segment :start (2dg-TL rect)
                 :end (2dg-BL rect)))
(cl-defmethod 2dg-bottom ((rect 2dg-rect))
  "Bottom (a.k.a. down) edge CCW spin"
  (2dg-segment :start (2dg-BL rect)
                 :end (2dg-BR rect)))
(cl-defmethod 2dg-right ((rect 2dg-rect))
  "Right edge CCW spin"
  (2dg-segment :start (2dg-BR rect)
                 :end (2dg-TR rect)))
(cl-defmethod 2dg-segments ((rect 2dg-rect))
  "Return a list of 4 segments representing the boundary of RECT.

The segments will be return starting at the bottom left and
proceeding counterclockwise around the rectangle."
  (with-slots (x-min x-max y-min y-max) rect
    (list (2dg-segment :start (2dg-point :x x-min :y y-min)
                         :end (2dg-point :x x-max :y y-min))
          (2dg-segment :start (2dg-point :x x-max :y y-min)
                         :end (2dg-point :x x-max :y y-max))
          (2dg-segment :start (2dg-point :x x-max :y y-max)
                         :end (2dg-point :x x-min :y y-max))
          (2dg-segment :start (2dg-point :x x-min :y y-max)
                         :end (2dg-point :x x-min :y y-min)))))
(cl-defmethod 2dg-width ((rect 2dg-rect))
  "Return the width (x-size) of RECT as a scalar."
  (with-slots (x-min x-max) rect
    (- x-max x-min)))
(cl-defmethod 2dg-height ((rect 2dg-rect))
  "Return the height (y-size) of RECT as a scalar."
  (with-slots (y-min y-max) rect
    (- y-max y-min)))
(cl-defmethod 2dg-centroid ((rect 2dg-rect))
  "Return the centroid of RECT as a point."
  (2dg-point :x (/ (+ (2dg-x-min rect) (2dg-x-max rect)) 2.0)
               :y (/ (+ (2dg-y-min rect) (2dg-y-max rect)) 2.0)))
(cl-defmethod 2dg-almost-equal ((A 2dg-rect) (B 2dg-rect) &optional tolerance)
  "Return non-nil if A and B almost equal within a TOLERANCE"
  (and (2dg-almost-equal (2dg-x-min A) (2dg-x-min B) tolerance)
       (2dg-almost-equal (2dg-x-max A) (2dg-x-max B) tolerance)
       (2dg-almost-equal (2dg-y-min A) (2dg-y-min B) tolerance)
       (2dg-almost-equal (2dg-y-max A) (2dg-y-max B) tolerance)))
;; TODO - this should be 2dg-x but it messes with the setf (2dg-x <2dg-point>)
;; figure that out and address it.  Same for teh y.
(cl-defmethod 2dg-x-span ((rect 2dg-rect))
  "Return the x component of this RECT as a span."
  (with-slots (x-min x-max) rect
    (2dg-span :start x-min :end x-max)))
(cl-defmethod 2dg-y-span ((rect 2dg-rect))
  "Return the y component of this RECT as a span."
  (with-slots (y-min y-max) rect
    (2dg-span :start y-min :end y-max)))
(cl-defmethod 2dg-contains ((container 2dg-rect) (containee 2dg-point) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains CONTAINEE using EVALUATION-MODE."
  (and (2dg-contains (2dg-x-span container) (2dg-x containee) evaluation-mode)
       (2dg-contains (2dg-y-span container) (2dg-y containee) evaluation-mode)))
(cl-defmethod 2dg-contains ((container 2dg-rect) (containee 2dg-rect) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains CONTAINEE using EVALUATION-MODE."
  (and (2dg-contains (2dg-x-span container) (2dg-x-span containee) evaluation-mode)
       (2dg-contains (2dg-y-span container) (2dg-y-span containee) evaluation-mode)))
(cl-defmethod 2dg-relative-coordinates ((base-rect 2dg-rect) (rect 2dg-rect))
  "Return the coordinates of RECT relative to BASE-RECT."
  (let ((x-span (2dg-relative-coordinates (2dg-x-span base-rect) (2dg-x-span rect)))
        (y-span (2dg-relative-coordinates (2dg-y-span base-rect) (2dg-y-span rect))))
    (2dg-rect :x-min (2dg-start x-span)
                :x-max (2dg-end x-span)
                :y-min (2dg-start y-span)
                :y-max (2dg-end y-span))))
(cl-defmethod 2dg-relative-coordinates ((base-rect 2dg-rect) (point 2dg-point))
  "Return the coordinates of POINT relative to BASE-RECT."
  (with-slots (x y) point
    (2dg-point :x (2dg-relative-coordinates (2dg-x-span base-rect) x)
                 :y (2dg-relative-coordinates (2dg-y-span base-rect) y))))
(cl-defmethod 2dg-absolute-coordinates ((base-rect 2dg-rect) (point 2dg-point))
  "Return the absolute coordinates of POINT given relative coordinate base BASE-RECT."
  (with-slots (x y) point
    (2dg-point :x (2dg-absolute-coordinates (2dg-x-span base-rect) x)
                 :y (2dg-absolute-coordinates (2dg-y-span base-rect) y))))
(cl-defmethod 2dg-absolute-coordinates ((base-rect 2dg-rect) (relative-rect 2dg-rect))
    "Return the absolute coordinates of RELATIVE-RECT given relative coordinate base BASE-RECT."
  (let ((x-span (2dg-absolute-coordinates (2dg-x-span base-rect)
                                            (2dg-x-span relative-rect)))
        (y-span (2dg-absolute-coordinates (2dg-y-span base-rect)
                                            (2dg-y-span relative-rect))))
    (2dg-rect :x-min (2dg-start x-span)
                :x-max (2dg-end x-span)
                :y-min (2dg-start y-span)
                :y-max (2dg-end y-span))))
(cl-defmethod 2dg-intersection ((A 2dg-point) (B 2dg-rect))
  "Return the intersection of A and B."
  (when (2dg-contains B A)
    A))
(cl-defmethod 2dg-intersection ((A 2dg-rect) (B 2dg-point))
  "Return the intersection of A and B."
  (when (2dg-contains A B)
    B))
(cl-defmethod 2dg-intersection ((A 2dg-rect) (B 2dg-rect))
  "Return the intersection of A and B."
  (let ((x-range (2dg-intersection (2dg-x-span A)
                                     (2dg-x-span B)))
        (y-range (2dg-intersection (2dg-y-span A)
                                     (2dg-y-span B))))
    (if (and x-range y-range)
        (2dg-rect :x-min (2dg-start x-range)
                    :x-max (2dg-end x-range)
                    :y-min (2dg-start y-range)
                    :y-max (2dg-end y-range))
      'nil)))
(cl-defmethod 2dg-has-intersection ((A 2dg-rect) (B 2dg-point) &optional evaluation-mode)
  "Return non-nil if A and B intersect."
  (2dg-contains A B evaluation-mode))
(cl-defmethod 2dg-has-intersection ((A 2dg-rect) (B 2dg-segment) &optional evaluation-mode)
  "Return non-nil if A and B intersect.

You have an intersection if A contains any end point of B or
if any bounding segment of A intersects B."
  (or (2dg-contains A (2dg-start B) evaluation-mode)
      (2dg-contains A (2dg-end B) evaluation-mode)
      (cond ((eq evaluation-mode 'strict) ;; must hit two segments
             (cl-loop for rect-segment in (2dg-segments A)
                      with num-hits = 0
                      when (2dg-has-intersection rect-segment B 'stacked)
                      do (incf num-hits)
                      when (>= num-hits 2)
                      return t))
            ((eq evaluation-mode 'stacked) ;; must hit two segments OR Left OR bottom segment
             (or (2dg-has-intersection (2dg-bottom A) B 'stacked) ;the bottom edge excluding BR
                 (2dg-has-intersection (2dg-flipped (2dg-left A)) B 'stacked) ;the left edge excluding TL
                 (and (2dg-has-intersection (2dg-right A) B 'strict) ;the right side without the ends
                      (2dg-has-intersection (2dg-top A) B 'stacked)))) ;the top edge excluding TR
            (t                          ;any hit anywhere is ok.
             (cl-loop for rect-segment in (2dg-segments A)
                      when (2dg-has-intersection rect-segment B)
                      return t)))))
(cl-defmethod 2dg-has-intersection ((A 2dg-rect) (B 2dg-rect) &optional evaluation-mode)
  "Return non-nil if A and B intersect."
  (and (2dg-has-intersection (2dg-x-span A) (2dg-x-span B) evaluation-mode)
       (2dg-has-intersection (2dg-y-span A) (2dg-y-span B) evaluation-mode)))
(cl-defmethod 2dg-bounding-pts ((A 2dg-rect))
  "Return a list containing the vertices of the rectangle.

Points are returned in a counterclockwise order starting with the
bottom left."
  (with-slots (x-min x-max y-min y-max) A
    (list (2dg-point :x x-min :y y-min)
          (2dg-point :x x-max :y y-min)
          (2dg-point :x x-max :y y-max)
          (2dg-point :x x-min :y y-max))))

(provide 'scxml-geometry-rect)
;;; scxml-geometry-rect.el ends here
