;;; scxml-drawing-rect.el --- scxml drawing rectangle functions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'scxml-drawing)
(require '2dg)

(defclass scxml-drawing-rect (2dg-rect scxml-drawing)
  ((name :initarg :name
         :accessor scxml-name
         :initform nil))
  :documentation "Represents a rectangle which can be drawn on a
canvas.  It can optionally have a name.")
(cl-defmethod scxml-print ((rect scxml-drawing-rect))
  (2dg-pprint rect))

(cl-defmethod scxml-num-edit-idxs ((rect scxml-drawing-rect))
  "How many edit idx points are there for this ARROW"
  8)
(cl-defmethod scxml-edit-idx-points ((rect scxml-drawing-rect))
  "Get the pixel locations of the edit idxs for RECT as a list."
  (with-slots (x-min x-max y-min y-max) rect
    ;; start at BL and go CCW to L
    (list (2dg-point :x x-min :y y-min)
          (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-min)
          (2dg-point :x x-max :y y-min)
          (2dg-point :x x-max :y (/ (+ y-max y-min) 2.0))
          (2dg-point :x x-max :y y-max)
          (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-max)
          (2dg-point :x x-min :y y-max)
          (2dg-point :x x-min :y (/ (+ y-max y-min) 2.0)))))
(cl-defmethod scxml-edit-idx-point ((rect scxml-drawing-rect) (idx integer))
  ;; TODO - figure out how to deduplicate this with points.
  "Get the pixel location of the given edit idx BL is zero, go CCW from there"
  (with-slots (x-min x-max y-min y-max) rect
    (case idx
     ;; BL
     (0 (2dg-point :x x-min :y y-min))
     ;; Bottom
     (1 (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-min))
     ;; BR
     (2 (2dg-point :x x-max :y y-min))
     ;; R
     (3 (2dg-point :x x-max :y (/ (+ y-max y-min) 2.0)))
     ;; TR
     (4 (2dg-point :x x-max :y y-max))
     ;; T
     (5 (2dg-point :x (/ (+ x-min x-max) 2.0) :y y-max))
     ;; TL
     (6 (2dg-point :x x-min :y y-max))
     ;; L
     (7 (2dg-point :x x-min :y (/ (+ y-max y-min) 2.0)))
     ;; err
     (otherwise (error "Invalid edit-mode idx: %s" idx)))))

(cl-defmethod scxml-build-move-edited ((rect scxml-drawing-rect) (move-vector 2dg-point) (viewport scxml-viewport))
  "Given a RECT, and a MOVE-DIRECTION, move in one pixel in that direction."
  (2dg-incf (clone rect) move-vector))
(cl-defmethod scxml-build-idx-edited ((rect scxml-drawing-rect) (edit-idx integer) (move-vector 2dg-point) (viewport scxml-viewport))
  (let ((pts (2dg-bounding-pts rect))
        (horizontal-pts 'nil)
        (vertical-pts 'nil))
    (cond ((equal 0 edit-idx)           ;bottom left
           (setq horizontal-pts (list (first pts) (fourth pts)))
           (setq vertical-pts (list (first pts) (second pts))))
          ((equal 1 edit-idx)           ;bottom edge
           (setq vertical-pts (list (first pts) (second pts))))
          ((equal 2 edit-idx)           ;bottom right
           (setq horizontal-pts (list (second pts) (third pts)))
           (setq vertical-pts (list (first pts) (second pts))))
          ((equal 3 edit-idx)           ;right edge
           (setq horizontal-pts (list (second pts) (third pts))))
          ((equal 4 edit-idx)           ;top right
           (setq horizontal-pts (list (second pts) (third pts)))
           (setq vertical-pts (list (third pts) (fourth pts))))
          ((equal 5 edit-idx)           ;top
           (setq vertical-pts (list (third pts) (fourth pts))))
          ((equal 6 edit-idx)           ;top left
           (setq horizontal-pts (list (first pts) (fourth pts)))
           (setq vertical-pts (list (third pts) (fourth pts))))
          ((equal 7 edit-idx)           ;left edge
           (setq horizontal-pts (list (first pts) (fourth pts))))
          ('t
           (error "invalid edit-idx for scxml-drawing-rect: %s" edit-idx)))
    (with-slots (x y) move-vector
      (when (and horizontal-pts (not (equal x 0.0)))
        (cl-loop for pt in horizontal-pts
                 do (oset pt x (+ (2dg-x pt) x))))
      (when (and vertical-pts (not (equal y 0.0)))
        (cl-loop for pt in vertical-pts
                 do (oset pt y (+ (2dg-y pt) y)))))
    (scxml-drawing-rect :y-min (2dg-y (first pts))
                        :y-max (2dg-y (third pts))
                        :x-min (2dg-x (first pts))
                        :x-max (2dg-x (second pts))
                        :parent (scxml-parent rect)
                        :highlight (scxml-drawing-highlight rect)
                        :edit-idx (scxml-drawing-edit-idx rect)
                        :name (scxml-name rect))))

(cl-defmethod scxml-build-hint ((rect 2dg-rect) (parent-canvas scxml-inner-canvas))
  "Build a hint for RECT inside of PARENT-CANVAS."
  (2dg-relative-coordinates parent-canvas rect))

(cl-defmethod scxml-get-inner-canvas ((rect scxml-drawing-rect))
  "Given a rectangle, pull an inner canvas"
  (with-slots (x-min y-min x-max y-max) rect
    (scxml-inner-canvas :x-min (+ x-min 2.0)
                        :y-min (+ y-min 2.0)
                        :x-max (- x-max 2.0)
                        :y-max (- y-max 3.0)
                        :drawing rect)))

(cl-defmethod scxml-leaving-segment-collision-edge ((rect scxml-drawing-rect) (pt 2dg-point))
  "If you leave centroid of RECT headed towards PT, which edge do you hit?

Returned as one of 4 symbols: 'up, 'down, 'left, 'right."
  (let* ((centroid (2dg-centroid rect))
         (path (2dg-segment :start centroid :end pt))
         (char-vector (2dg-characteristic-vector path))
         (to-tl (2dg-segment :start centroid :end (2dg-TL rect)))
         (to-tr (2dg-segment :start centroid :end (2dg-TR rect)))
         (cross-tl (2dg-cross-prod char-vector (2dg-characteristic-vector to-tl)))
         (cross-tr (2dg-cross-prod char-vector (2dg-characteristic-vector to-tr))))
    (cond ((and (>= cross-tl 0.0) (>= cross-tr 0.0))
           'right)
          ((and (<= cross-tl 0.0) (<= cross-tr 0.0))
           'left)
          ((and (>= cross-tl 0.0) (<= cross-tr 0.0))
           'up)
          ((and (<= cross-tl 0.0) (>= cross-tr 0.0))
           'down)
          ('t (error "Impossible?")))))

(provide 'scxml-drawing-rect)
;;; scxml-drawing-rect.el ends here
