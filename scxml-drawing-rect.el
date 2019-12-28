;;; scxml-drawing-rect.el --- scxml drawing rectangle functions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'scxml-drawing)
(require 'scxml-geometry)

(defclass scxml-drawing-rect (scxml-rect scxml-drawing)
  ((name :initarg :name
         :accessor scxml-rectangle-name
         :initform nil
         ;; TODO - can this name come from the state/element that's the parent of this drawing.
         ))
  :documentation "Represents a rectangle which can be drawn on a canvas.")

(defclass scxml-drawing-noshell-rect (scxml-drawing-rect)
  ()
  ;; TODO - break this out to a separate file.
  :documentation "Exactly the same as a scxml-drawing-rect, but
the outline is never drawn.  This rectangle delegates
responsibility for drawing its bounds to someone else (probably a
parent).  Currently used to draw child <state> elements of a
<parallel>")

(cl-defmethod scxml-num-edit-idxs ((rect scxml-drawing-rect))
  "How many edit idx points are there for this ARROW"
  8)
(cl-defmethod scxml-edit-idx-points ((rect scxml-drawing-rect))
  "Get the pixel locations of the edit idxs for RECT as a list."
  (with-slots (x-min x-max y-min y-max) rect
    ;; start at BL and go CCW to L
    (list (scxml-point :x x-min :y y-min)
          (scxml-point :x (/ (+ x-min x-max) 2.0) :y y-min)
          (scxml-point :x x-max :y y-min)
          (scxml-point :x x-max :y (/ (+ y-max y-min) 2.0))
          (scxml-point :x x-max :y y-max)
          (scxml-point :x (/ (+ x-min x-max) 2.0) :y y-max)
          (scxml-point :x x-min :y y-max)
          (scxml-point :x x-min :y (/ (+ y-max y-min) 2.0)))))
(cl-defmethod scxml-edit-idx-point ((rect scxml-drawing-rect) (idx integer))
  ;; TODO - figure out how to deduplicate this with points.
  "Get the pixel location of the given edit idx BL is zero, go CCW from there"
  (with-slots (x-min x-max y-min y-max) rect
    (case idx
     ;; BL
     (0 (scxml-point :x x-min :y y-min))
     ;; Bottom
     (1 (scxml-point :x (/ (+ x-min x-max) 2.0) :y y-min))
     ;; BR
     (2 (scxml-point :x x-max :y y-min))
     ;; R
     (3 (scxml-point :x x-max :y (/ (+ y-max y-min) 2.0)))
     ;; TR
     (4 (scxml-point :x x-max :y y-max))
     ;; T
     (5 (scxml-point :x (/ (+ x-min x-max) 2.0) :y y-max))
     ;; TL
     (6 (scxml-point :x x-min :y y-max))
     ;; L
     (7 (scxml-point :x x-min :y (/ (+ y-max y-min) 2.0)))
     ;; err
     (otherwise (error "Invalid edit-mode idx: %s" idx)))))

(cl-defmethod scxml-build-move-edited ((rect scxml-drawing-rect) (move-vector scxml-point))
  "Given a RECT, and a MOVE-DIRECTION, move in one pixel in that direction."
  ;; TODO - This may not be correct, may need an object clone.
  (scxml-add rect move-vector))
(cl-defmethod scxml-build-idx-edited ((rect scxml-drawing-rect) (edit-idx integer) (move-vector scxml-point))
  ;; try breaking it out into points that can move vertically given edit-idx
  ;; and points that can move horizontally given edit idx.
  ;; then just fucking move them.
  ;; then reassemble.
  (let ((pts (scxml-bounding-pts rect))
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
                 do (oset pt x (+ (scxml-x pt) x))))
      (when (and vertical-pts (not (equal y 0.0)))
        (cl-loop for pt in vertical-pts
                 do (oset pt y (+ (scxml-y pt) y)))))
    (scxml-drawing-rect :y-min (scxml-y (first pts))
                        :y-max (scxml-y (third pts))
                        :x-min (scxml-x (first pts))
                        :x-max (scxml-x (second pts))
                        :parent (scxml-parent rect))))

(cl-defmethod scxml-build-drawing ((state scxml-state-type) (canvas scxml-canvas))
  "Build drawing helper"
  (scxml---drawing-logger "scxml--build-drawing: %s" (scxml-print state))
  (scxml---drawing-logger "scxml--build-drawing: canvas: %s" (scxml-print canvas))
  (scxml---drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? state)
                          (if (scxml-element-drawing state) 't 'nil))
  (let ((hint (scxml--hint state))
        (state-name (scxml-element-id state))
        (highlight (scxml--highlight state))
        (edit-idx (scxml--edit-idx state))
        ;; if your parent is a <parallel> then this is a noshell rect.
        ;; and the parent <parallel> is responsible for drawing divisions.
        (drawing-factory (if (scxml-parallel-p (scxml-parent state))
                             'scxml-drawing-noshell-rect
                           'scxml-drawing-rect)))
    (if (null hint)
        ;; Generate the drawing (not based on a hint)
        (funcall drawing-factory
                 :x-min (scxml-x-min canvas)
                 :y-min (scxml-y-min canvas)
                 :x-max (scxml-x-max canvas)
                 :y-max (scxml-y-max canvas)
                 :name state-name
                 :highlight highlight
                 :edit-idx edit-idx
                 :parent state)

      ;; build drawing off of the hint and parent drawing canvas
      ;; Todo - this is copypast from scxml-initialze-hint :(
      (let* ((parent (scxml-parent state))
             (parent-drawing (when (object-of-class-p parent 'scxml-drawable-element)
                               (scxml-element-drawing parent)))
             (parent-drawing-canvas (if parent-drawing
                                        (scxml--get-inner-canvas parent-drawing)
                                      canvas)))
        (when (not (scxml-inner-canvas-p parent-drawing-canvas))
          (error "Not sure how to continue here :("))
        (let ((absolute-rect (scxml-absolute-coordinates parent-drawing-canvas
                                                          hint)))
          (with-slots (x-min x-max y-min y-max) absolute-rect
            (funcall drawing-factory
                     :x-min x-min
                     :y-min y-min
                     :x-max x-max
                     :y-max y-max
                     :locked 't
                     :name state-name
                     :highlight highlight
                     :edit-idx edit-idx
                     :parent parent)))))))

(cl-defmethod scxml-build-hint ((rect scxml-rect) (parent-canvas scxml-inner-canvas))
  "Build a hint for RECT inside of PARENT-CANVAS."
  (scxml-relative-coordinates parent-canvas rect))

(cl-defmethod scxml--get-inner-canvas ((rect scxml-drawing-rect))
  "Given a rectangle, pull an inner canvas"
  (with-slots (x-min y-min x-max y-max) rect
    (scxml-inner-canvas :x-min (+ x-min 2.0)
                        :y-min (+ y-min 2.0)
                        :x-max (- x-max 2.0)
                        :y-max (- y-max 3.0)
                        :drawing rect)))
(cl-defmethod scxml--get-inner-canvas-layout ((rect scxml-drawing-rect) (num-elements integer))
  "Given RECT which needs to have NUM-ELEMENTS drawn in it determine canvases for each element"
  ;; TODO - delete this??
  (error "is this even used?"))


(provide 'scxml-drawing-rect)
;;; scxml-drawing-rect.el ends here
