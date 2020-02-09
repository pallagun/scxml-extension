;;; scxml-geometry-rect.el --- scxml geometry rectangle helpers -*- lexical-binding: t -*-

;;; Commentary:
;; An scxml-rect represents a rectangle in 2d space.  The rectangle is
;; constrained to be axis aligned

;;; Code:
(require 'eieio)
(require 'scxml-geometry-point)
(require 'scxml-geometry-span)
(require 'scxml-geometry-segment)

(defclass scxml-rect ()
  ((x-min :initarg :x-min
          :accessor scxml-x-min
          :type float
          :initform 0.0)
   (y-min :initarg :y-min
          :accessor scxml-y-min
          :type float
          :initform 0.0)
   (y-max :initarg :y-max
          :type float
          :accessor scxml-y-max)
   (x-max :initarg :x-max
          :type float
          :accessor scxml-x-max))
  :documentation "2d rectangle")
(cl-defmethod scxml-print ((rect scxml-rect))
  "Return a stringified version of RECT for human eyes."
  (with-slots (x-min x-max y-min y-max) rect
    (format "r[x[%f, %f], y[%f, %f]]" x-min x-max y-min y-max)))
(cl-defmethod cl-print-object ((object scxml-rect) stream)
  "This seems to be used only for edebug sessions."
  (princ (scxml-print object) stream))
(cl-defmethod 2dg-add ((A scxml-rect) (B 2dg-point))
  "Return a rectangle representing A displaced by B."
  (with-slots (x-min x-max y-min y-max) A
    (with-slots ((del-x x) (del-y y)) B
      (scxml-rect :x-min (+ x-min del-x)
                  :x-max (+ x-max del-x)
                  :y-min (+ y-min del-y)
                  :y-max (+ y-max del-y)))))
(cl-defmethod 2dg-incf ((A scxml-rect) (B 2dg-point))
  "In place modification of A to be A + B, returning A."
  (with-slots (x y) B
    (with-slots (x-min x-max y-min y-max) A
      (oset A x-min (+ x x-min))
      (oset A y-min (+ y y-min))
      (oset A x-max (+ x x-max))
      (oset A y-max (+ y y-max))))
  A)
(cl-defmethod scxml-TL ((rect scxml-rect))
  "Top Left point"
  (2dg-point :x (scxml-x-min rect)
               :y (scxml-y-max rect)))
(cl-defmethod scxml-TR ((rect scxml-rect))
  "Top Right point"
  (2dg-point :x (scxml-x-max rect)
               :y (scxml-y-max rect)))
(cl-defmethod scxml-BR ((rect scxml-rect))
  "Bottom Right point"
  (2dg-point :x (scxml-x-max rect)
               :y (scxml-y-min rect)))
(cl-defmethod scxml-BL ((rect scxml-rect))
  "Bottom Left point"
  (2dg-point :x (scxml-x-min rect)
               :y (scxml-y-min rect)))
(cl-defmethod scxml-edge ((rect scxml-rect) (edge symbol))
  "Return a segment describing the EDGE of RECT.

EDGE may be one of: 'up, 'down, 'left, 'right.  Edges are
returned with start and end points corresponding to a
counterclockwise traversal of the rectangle's outer edge."
  (cond ((eq edge 'up)
         (scxml-top rect))
        ((eq edge 'down)
         (scxml-bottom rect))
        ((eq edge 'right)
         (scxml-right rect))
        ((eq edge 'left)
         (scxml-left rect))
        ('t
         (error "Invalid edge enumerator"))))
(cl-defmethod scxml-top ((rect scxml-rect))
  "Top (a.k.a. up) edge CCW spin"
  (scxml-segment :start (scxml-TR rect)
                 :end (scxml-TL rect)))
(cl-defmethod scxml-left ((rect scxml-rect))
  "Left edge CCW spin"
  (scxml-segment :start (scxml-TL rect)
                 :end (scxml-BL rect)))
(cl-defmethod scxml-bottom ((rect scxml-rect))
  "Bottom (a.k.a. down) edge CCW spin"
  (scxml-segment :start (scxml-BL rect)
                 :end (scxml-BR rect)))
(cl-defmethod scxml-right ((rect scxml-rect))
  "Right edge CCW spin"
  (scxml-segment :start (scxml-BR rect)
                 :end (scxml-TR rect)))
(cl-defmethod scxml-segments ((rect scxml-rect))
  "Return a list of 4 segments representing the boundary of RECT.

The segments will be return starting at the bottom left and
proceeding counterclockwise around the rectangle."
  (with-slots (x-min x-max y-min y-max) rect
    (list (scxml-segment :start (2dg-point :x x-min :y y-min)
                         :end (2dg-point :x x-max :y y-min))
          (scxml-segment :start (2dg-point :x x-max :y y-min)
                         :end (2dg-point :x x-max :y y-max))
          (scxml-segment :start (2dg-point :x x-max :y y-max)
                         :end (2dg-point :x x-min :y y-max))
          (scxml-segment :start (2dg-point :x x-min :y y-max)
                         :end (2dg-point :x x-min :y y-min)))))
(cl-defmethod scxml-width ((rect scxml-rect))
  "Return the width (x-size) of RECT as a scalar."
  (with-slots (x-min x-max) rect
    (- x-max x-min)))
(cl-defmethod scxml-height ((rect scxml-rect))
  "Return the height (y-size) of RECT as a scalar."
  (with-slots (y-min y-max) rect
    (- y-max y-min)))
(cl-defmethod 2dg-centroid ((rect scxml-rect))
  "Return the centroid of RECT as a point."
  (2dg-point :x (/ (+ (scxml-x-min rect) (scxml-x-max rect)) 2.0)
               :y (/ (+ (scxml-y-min rect) (scxml-y-max rect)) 2.0)))
(cl-defmethod 2dg-almost-equal ((A scxml-rect) (B scxml-rect) &optional tolerance)
  "Return non-nil if A and B almost equal within a TOLERANCE"
  (and (2dg-almost-equal (scxml-x-min A) (scxml-x-min B) tolerance)
       (2dg-almost-equal (scxml-x-max A) (scxml-x-max B) tolerance)
       (2dg-almost-equal (scxml-y-min A) (scxml-y-min B) tolerance)
       (2dg-almost-equal (scxml-y-max A) (scxml-y-max B) tolerance)))
;; TODO - this should be scxml-x but it messes with the setf (scxml-x <scxml-point>)
;; figure that out and address it.  Same for teh y.
(cl-defmethod scxml-x-span ((rect scxml-rect))
  "Return the x component of this RECT as a span."
  (with-slots (x-min x-max) rect
    (scxml-span :start x-min :end x-max)))
(cl-defmethod scxml-y-span ((rect scxml-rect))
  "Return the y component of this RECT as a span."
  (with-slots (y-min y-max) rect
    (scxml-span :start y-min :end y-max)))
(cl-defmethod 2dg-contains ((container scxml-rect) (containee 2dg-point) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains CONTAINEE using EVALUATION-MODE."
  (and (2dg-contains (scxml-x-span container) (scxml-x containee) evaluation-mode)
       (2dg-contains (scxml-y-span container) (scxml-y containee) evaluation-mode)))
(cl-defmethod 2dg-contains ((container scxml-rect) (containee scxml-rect) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains CONTAINEE using EVALUATION-MODE."
  (and (2dg-contains (scxml-x-span container) (scxml-x-span containee) evaluation-mode)
       (2dg-contains (scxml-y-span container) (scxml-y-span containee) evaluation-mode)))
(cl-defmethod 2dg-relative-coordinates ((base-rect scxml-rect) (rect scxml-rect))
  "Return the coordinates of RECT relative to BASE-RECT."
  (let ((x-span (2dg-relative-coordinates (scxml-x-span base-rect) (scxml-x-span rect)))
        (y-span (2dg-relative-coordinates (scxml-y-span base-rect) (scxml-y-span rect))))
    (scxml-rect :x-min (scxml-start x-span)
                :x-max (scxml-end x-span)
                :y-min (scxml-start y-span)
                :y-max (scxml-end y-span))))
(cl-defmethod 2dg-relative-coordinates ((base-rect scxml-rect) (point 2dg-point))
  "Return the coordinates of POINT relative to BASE-RECT."
  (with-slots (x y) point
    (2dg-point :x (2dg-relative-coordinates (scxml-x-span base-rect) x)
                 :y (2dg-relative-coordinates (scxml-y-span base-rect) y))))
(cl-defmethod 2dg-absolute-coordinates ((base-rect scxml-rect) (point 2dg-point))
  "Return the absolute coordinates of POINT given relative coordinate base BASE-RECT."
  (with-slots (x y) point
    (2dg-point :x (2dg-absolute-coordinates (scxml-x-span base-rect) x)
                 :y (2dg-absolute-coordinates (scxml-y-span base-rect) y))))
(cl-defmethod 2dg-absolute-coordinates ((base-rect scxml-rect) (relative-rect scxml-rect))
    "Return the absolute coordinates of RELATIVE-RECT given relative coordinate base BASE-RECT."
  (let ((x-span (2dg-absolute-coordinates (scxml-x-span base-rect)
                                            (scxml-x-span relative-rect)))
        (y-span (2dg-absolute-coordinates (scxml-y-span base-rect)
                                            (scxml-y-span relative-rect))))
    (scxml-rect :x-min (scxml-start x-span)
                :x-max (scxml-end x-span)
                :y-min (scxml-start y-span)
                :y-max (scxml-end y-span))))
(cl-defmethod 2dg-intersection ((A 2dg-point) (B scxml-rect))
  "Return the intersection of A and B."
  (when (2dg-contains B A)
    A))
(cl-defmethod 2dg-intersection ((A scxml-rect) (B 2dg-point))
  "Return the intersection of A and B."
  (when (2dg-contains A B)
    B))
(cl-defmethod 2dg-intersection ((A scxml-rect) (B scxml-rect))
  "Return the intersection of A and B."
  (let ((x-range (2dg-intersection (scxml-x-span A)
                                     (scxml-x-span B)))
        (y-range (2dg-intersection (scxml-y-span A)
                                     (scxml-y-span B))))
    (if (and x-range y-range)
        (scxml-rect :x-min (scxml-start x-range)
                    :x-max (scxml-end x-range)
                    :y-min (scxml-start y-range)
                    :y-max (scxml-end y-range))
      'nil)))
(cl-defmethod 2dg-has-intersection ((A scxml-rect) (B 2dg-point) &optional evaluation-mode)
  "Return non-nil if A and B intersect."
  (2dg-contains A B evaluation-mode))
(cl-defmethod 2dg-has-intersection ((A scxml-rect) (B scxml-segment) &optional evaluation-mode)
  "Return non-nil if A and B intersect.

You have an intersection if A contains any end point of B or
if any bounding segment of A intersects B."
  (or (2dg-contains A (scxml-start B) evaluation-mode)
      (2dg-contains A (scxml-end B) evaluation-mode)
      (cond ((eq evaluation-mode 'strict) ;; must hit two segments
             (cl-loop for rect-segment in (scxml-segments A)
                      with num-hits = 0
                      when (2dg-has-intersection rect-segment B 'stacked)
                      do (incf num-hits)
                      when (>= num-hits 2)
                      return t))
            ((eq evaluation-mode 'stacked) ;; must hit two segments OR Left OR bottom segment
             (or (2dg-has-intersection (scxml-bottom A) B 'stacked) ;the bottom edge excluding BR
                 (2dg-has-intersection (scxml-flipped (scxml-left A)) B 'stacked) ;the left edge excluding TL
                 (and (2dg-has-intersection (scxml-right A) B 'strict) ;the right side without the ends
                      (2dg-has-intersection (scxml-top A) B 'stacked)))) ;the top edge excluding TR
            (t                          ;any hit anywhere is ok.
             (cl-loop for rect-segment in (scxml-segments A)
                      when (2dg-has-intersection rect-segment B)
                      return t)))))
(cl-defmethod 2dg-has-intersection ((A scxml-rect) (B scxml-rect) &optional evaluation-mode)
  "Return non-nil if A and B intersect."
  (and (2dg-has-intersection (scxml-x-span A) (scxml-x-span B) evaluation-mode)
       (2dg-has-intersection (scxml-y-span A) (scxml-y-span B) evaluation-mode)))
(cl-defmethod scxml-bounding-pts ((A scxml-rect))
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
