;;; scxml-viewport.el --- scxml viewport object -*- lexical-binding: t -*-

;;; Commentary:
;; A viewport captures a portion of a canvas which should be made visible to the user
;; as well as how it should be made visible to the user (dilation/scaling).

;;; Code:
(require 'scxml-geometry-rect)
(require 'scxml-geometry-span)
(require 'scxml-geometry-pixel)
(require 'scxml-canvas)

(defclass scxml-viewport (scxml-rect)
  ((scaling :initarg :scaling
            :accessor scxml-scaling
            :type 2dg-point
            :initform (2dg-point :x 1.0 :y 1.0)
            :documentation "Scaling is applied as:
X pixel = X coordinate * X scaling;
Y pixel = Y coordinate * Y scalxng;

X coordinate = X pixel / X scaling
Y coordinate = Y pixel / Y scaling

Scaling goes up and you zoom in, scaling does down and you zoom out.
note: 'pixel' here denotes an scxml-scratch-render.el pixel which is lower-left origin.
TODO: don't say pixel, say scratch-coord"))
  :documentation "An object describing what should be visible to a user and how.

The object itself represents a rectangle of area which should be visible to the user.
The scaling slot represents how that space should be scaled before rendering")

(cl-defmethod scxml-reset-scaling ((viewport scxml-viewport))
  "Reset the scaling of the VIEWPORT to be identity"
  (oset viewport scaling (scxml-vector :x 1.0 :y 1.0)))
(cl-defgeneric scxml-build-viewport ((canvas scxml-canvas))
  "Build a viewport to cover the entire CANVAS with identity scaling")
(cl-defmethod scxml-build-viewport ((canvas scxml-canvas))
  "Build a viewport to cover the entire CANVAS with identity scaling"
  ;; TODO - this should really be ceiling, not 1+
  (scxml-viewport :x-min (scxml-x-min canvas)
                  :x-max (1+ (scxml-x-max canvas))
                  :y-min (scxml-y-min canvas)
                  :y-max (1+ (scxml-y-max canvas))))
(cl-defmethod scxml-set-domain ((viewport scxml-viewport) (area scxml-rect))
  "Set the domain of this VIEWPORT to match the CANVAS.

Return the viewport after modification"
  (setf (scxml-x-min viewport) (scxml-x-min area)
        (scxml-x-max viewport) (float (ceiling (+ 2dg--almost-zero (scxml-x-max area))))
        (scxml-y-min viewport) (scxml-y-min area)
        (scxml-y-max viewport) (float (ceiling (+ 2dg--almost-zero (scxml-y-max area))))))
(cl-defmethod scxml-zoom ((viewport scxml-viewport) (alpha number))
  "Zoom the VIEWPORT by ALPHA, modifies viewport, returns viewport.

Alpha > 1 zooms in.  Alpha < 1 zooms out."
  (when (<= alpha 0.0)
    (error "That's a crazy alpha"))
  (let ((centroid (2dg-centroid viewport))
        (x-radius (/ (scxml-width viewport) 2.0 alpha))
        (y-radius (/ (scxml-height viewport) 2.0 alpha)))
    (oset viewport x-min (- (scxml-x centroid) x-radius))
    (oset viewport x-max (+ (scxml-x centroid) x-radius))
    (oset viewport y-min (- (scxml-y centroid) y-radius))
    (oset viewport y-max (+ (scxml-y centroid) y-radius))
    (oset viewport scaling (2dg-scaled (scxml-scaling viewport)
                                         alpha))))
(cl-defmethod scxml-pan ((viewport scxml-viewport) (drawing-coord-delta 2dg-point))
  "Move VIEWPORT by DRAWING-COORD-DELTA.

Uses drawing coordinate system."
  (2dg-incf viewport drawing-coord-delta))
(cl-defmethod scxml-pan-scratch ((viewport scxml-viewport) (scratch-x integer) (scratch-y integer))
  "Move VIEWPORT by SCRATCH-X and SCRATCH-Y scratch pixels"
  (scxml-pan viewport (2dg-scaled (2dg-point :x (float scratch-x)
                                                 :y (float scratch-y))
                                    (scxml-get-point-scaling viewport))))

(cl-defmethod scxml-required-pixel-width ((viewport scxml-viewport))
  "How many pixels of width are required for this VIEWPORT to be fully visible."
  (ceiling (* (scxml-x (scxml-scaling viewport))
              (scxml-length (scxml-x-span viewport)))))
(cl-defmethod scxml-required-pixel-height ((viewport scxml-viewport))
  "How many pixels of height are required for this VIEWPORT to be fully visible."
  (ceiling (* (scxml-y (scxml-scaling viewport))
              (scxml-length (scxml-y-span viewport)))))

;; Coordinate scheme conversions
(cl-defmethod scxml-get-pixel-scaling ((viewport scxml-viewport))
  "Grab the scaling from point to pixel from VIEWPORT"
  (scxml-scaling viewport))
(cl-defmethod scxml-get-point-scaling ((viewport scxml-viewport))
  "Grab the scaling from pixel to point from VIEWPORT"
  (with-slots (scaling) viewport
    (2dg-point :x (/ 1.0 (scxml-x scaling))
                 :y (/ 1.0 (scxml-y scaling)))))

;; Three coordinate schemes are:
;; - drawing points - what all drawings use (and canvases)
;; - scratch coordinates - What is rendered out in the scratch buffer renderer
;; - pixel coordinates - image style coordinates for mouse clicks and what not.

(cl-defgeneric scxml-get-pixel ((viewport scxml-viewport) (drawing-point 2dg-point))
  "Given a drawing coordinate DRAWING-POINT in a VIEWPORT, get the pixel for it")
(cl-defmethod scxml-get-pixel ((viewport scxml-viewport) (drawing-point 2dg-point))
  "Given a drawing coordinate DRAWING-POINT in a VIEWPORT, get the pixel for it"
  (let ((scratch-coord (scxml-get-scratch-coord viewport drawing-point)))
    ;; (let ((fart (scxml-pixel :x (floor (scxml-x scratch-coord))
    ;;                          :y (1- (ceiling (- (scxml-required-pixel-height viewport)
    ;;                                         (scxml-y scratch-coord)))))))
    ;;   ;its h tall, so to get zero it's -h
    ;;   (message "d->s[h:%s] %s -> %s -> %s "
    ;;            (scxml-required-pixel-height viewport)
    ;;            drawing-point
    ;;            scratch-coord
    ;;            fart)
    ;;   fart)
    (scxml-pixel :x (floor (scxml-x scratch-coord))
                 :y (1- (ceiling (- (scxml-required-pixel-height viewport)
                                    (scxml-y scratch-coord)))))
    ))

;; (ert-deftest scxml-pixel-point-pixel-roundtrips ()
;;   "Make sure a pixel, translated to a drawing coordinate and back, is the same"
;;   (let ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
;;                                   :x-min 0.0 :x-max 101.0
;;   test1#s(scxml-viewport 0.0 0.0 41.0 101.0 #s(2dg-point 1.0 1.0))
;;   test2#s(2dg-point 22.5 40.0))))))
(ert-deftest scxml-get-pixel-from-viewport-point ()
  ;; this test is really bad, this has to get fixed.
  (let* ((canvas (scxml-canvas :x-min 0.0 :x-max 4.0
                               :y-min 0.0 :y-max 4.0))
         (viewport (scxml-build-viewport canvas))
         (height (scxml-required-pixel-height viewport))
         (bottom-px-offset (1- height)))
    ;; We'll need to be one pixel taller than the canvas to properly draw all of the canvas
    (should (eq height 5))
    (mapc (lambda (pt)
            (should (scxml-equal
                     (scxml-get-pixel viewport pt)
                     (scxml-pixel :x 0 :y bottom-px-offset))))
          (list (2dg-point :x 0.0 :y 0.0)
                (2dg-point :x 0.1 :y 0.0)
                (2dg-point :x 0.1 :y 0.1)
                (2dg-point :x 0.0 :y 0.1)
                (2dg-point :x 0.9 :y 0.0)
                (2dg-point :x 0.9 :y 0.1)
                (2dg-point :x 0.0 :y 0.9)
                (2dg-point :x 0.1 :y 0.9)
                (2dg-point :x 0.9 :y 0.9)))
    (should (scxml-equal
             (scxml-get-pixel viewport (2dg-point :x 0.0 :y 0.1))
             (scxml-pixel :x 0 :y bottom-px-offset)))
    (should (scxml-equal
             (scxml-get-pixel viewport (2dg-point :x 1.0 :y 0.0))
             (scxml-pixel :x 1 :y bottom-px-offset)))
    (should (scxml-equal
             (scxml-get-pixel viewport (2dg-point :x 1.0 :y 1.0))
             (scxml-pixel :x 1 :y (1- bottom-px-offset))))
    ;; out of bounds coordinates should still be calculable (is that a word?)
    (should (scxml-equal
             (scxml-get-pixel viewport (2dg-point :x -1.0 :y -1.0))
             (scxml-pixel :x -1 :y (1+ bottom-px-offset))))
    ;; (should (scxml-equal
    ;;          (scxml-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
    ;;          (scxml-pixel :x 12 :y -2)))
    )
  ;; Same should work for offset canvases
  (let* ((canvas (scxml-canvas :x-min 0.0 :x-max 10.0
                               :y-min 0.0 :y-max 10.0))
         (viewport (scxml-build-viewport canvas))
         (focus (scxml-rect :x-min 2.0 :x-max 10.0
                            :y-min 2.0 :y-max 10.0)))
    (scxml-set-domain viewport focus)
    (let* ((height (scxml-required-pixel-height viewport))
           (bottom-px-offset (1- height)))
      (should (scxml-equal
               (scxml-get-pixel viewport (2dg-point :x 2.0 :y 2.0))
               (scxml-pixel :x 0 :y bottom-px-offset)))
      (should (scxml-equal
               (scxml-get-pixel viewport (2dg-point :x 3.0 :y 2.0))
               (scxml-pixel :x 1 :y bottom-px-offset)))
      (should (scxml-equal
               (scxml-get-pixel viewport (2dg-point :x 3.0 :y 3.0))
               (scxml-pixel :x 1 :y (1- bottom-px-offset))))
      ;; out of bounds coordinates should still be calculable (is that a word?)
      (should (scxml-equal
               (scxml-get-pixel viewport (2dg-point :x 1.0 :y 1.0))
               (scxml-pixel :x -1 :y (1+ bottom-px-offset))))
      (should (scxml-equal
               (scxml-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
               (scxml-pixel :x 10 :y -2)))))
  ;; Same should work for offset canvases zoomed in.
  ;; (let* ((canvas (scxml-canvas :x-min 0.0 :x-max 10.0
  ;;                              :y-min 0.0 :y-max 10.0))
  ;;        (viewport (scxml-build-viewport canvas))
  ;;        (focus (scxml-rect :x-min 2.0 :x-max 10.0
  ;;                           :y-min 2.0 :y-max 10.0)))
  ;;   (scxml-set-domain viewport focus)
  ;;   (scxml-zoom viewport 2.0)

  ;;   (let* ((height (scxml-required-pixel-height viewport))
  ;;          (bottom-px-offset (1- height)))
  ;;        (should (scxml-equal
  ;;                 (scxml-get-pixel viewport (2dg-point :x 2.0 :y 2.0))
  ;;                 (scxml-pixel :x 0 :y bottom-px-offset)))
  ;;        (should (scxml-equal
  ;;                 (scxml-get-pixel viewport (2dg-point :x 3.0 :y 2.0))
  ;;                 (scxml-pixel :x 2 :y bottom-px-offset)))
  ;;        (should (scxml-equal
  ;;                 (scxml-get-pixel viewport (2dg-point :x 3.0 :y 3.0))
  ;;                 (scxml-pixel :x 2 :y (- bottom-px-offset 2))))
  ;;        (should (scxml-equal
  ;;                 (scxml-get-pixel viewport (2dg-point :x 1.0 :y 1.0))
  ;;                 (scxml-pixel :x -2 :y (+ 2 bottom-px-offset))))
  ;;        (should (scxml-equal
  ;;                 (scxml-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
  ;;                 (scxml-pixel :x 20 :y -4)))))
  ;; Same should work for offset canvases zoomed out.
  ;; (let* ((viewport (scxml-viewport :scaling (2dg-point :x 0.5 :y 0.5)
  ;;                                  :x-min 2.0
  ;;                                  :x-max 10.0
  ;;                                  :y-min 2.0
  ;;                                  :y-max 10.0))
  ;;        (height (scxml-required-pixel-height viewport))
  ;;        (bottom-px-offset (1- height)))
  ;;   (should (scxml-equal
  ;;            (scxml-get-pixel viewport (2dg-point :x 2.0 :y 2.0))
  ;;            (scxml-pixel :x 0 :y bottom-px-offset)))
  ;;   (should (scxml-equal
  ;;            (scxml-get-pixel viewport (2dg-point :x 3.0 :y 2.0))
  ;;            (scxml-pixel :x 0 :y bottom-px-offset)))
  ;;   (should (scxml-equal
  ;;            (scxml-get-pixel viewport (2dg-point :x 3.0 :y 3.0))
  ;;            (scxml-pixel :x 0 :y bottom-px-offset)))
  ;;   (should (scxml-equal
  ;;            (scxml-get-pixel viewport (2dg-point :x 4.0 :y 4.0))
  ;;            (scxml-pixel :x 1 :y (1- bottom-px-offset))))
  ;;   (should (scxml-equal
  ;;            (scxml-get-pixel viewport (2dg-point :x 0.99 :y 0.99))
  ;;            (scxml-pixel :x -1 :y (+ 1 bottom-px-offset))))
  ;;   (should (scxml-equal
  ;;            (scxml-get-pixel viewport (2dg-point :x 12.0 :y 12.0))
  ;;            (scxml-pixel :x 5 :y -1))))
  )

(cl-defmethod scxml-get-scratch-coord ((viewport scxml-viewport) (drawing-point 2dg-point))
  "Given a DRAWING-POINT on VIEWPORT, determine the proper scratch coordinate."
  (with-slots (scaling x-min y-min) viewport
    (2dg-scaled (2dg-point :x (- (scxml-x drawing-point) x-min)
                               :y (- (scxml-y drawing-point) y-min))
                  scaling)))
(ert-deftest scxml-get-scratch-coord-from-viewport-drawing-pt ()
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 0.0 :y 0.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 1.0 :y 0.0))
             (2dg-point :x 1.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x 1.0 :y 1.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x -1.0 :y -1.0))
             (2dg-point :x -1.0 :y -1.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 12.0 :y 12.0))
             (2dg-point :x 12.0 :y 12.0))))
  ;; Same should work for offset canvases
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 2.0 :y 2.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 3.0 :y 2.0))
             (2dg-point :x 1.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 3.0 :y 3.0))
             (2dg-point :x 1.0 :y 1.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x -1.0 :y -1.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 12.0 :y 12.0))
             (2dg-point :x 10.0 :y 10.0))))
  ;; Same should work for offset canvases zoomed in.
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 2.0 :y 2.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 3.0 :y 2.0))
             (2dg-point :x 2.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 3.0 :y 3.0))
             (2dg-point :x 2.0 :y 2.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x -2.0 :y -2.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 12.0 :y 12.0))
             (2dg-point :x 20.0 :y 20.0))))
  ;; Same should work for offset canvases zoomed out.
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 2.0 :y 2.0))
             (2dg-point :x 0.0 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 3.0 :y 2.0))
             (2dg-point :x 0.5 :y 0.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 3.0 :y 3.0))
             (2dg-point :x 0.5 :y 0.5)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 4.0 :y 4.0))
             (2dg-point :x 1.0 :y 1.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 1.0 :y 1.0))
             (2dg-point :x -0.5 :y -0.5)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (2dg-point :x 11.0 :y 11.0))
             (2dg-point :x 4.5 :y 4.5)))))

(cl-defmethod scxml-get-coord-centroid ((viewport scxml-viewport) (pixel scxml-pixel))
  "Given a pixel, return the center of it in drawing coordinates"
  (with-slots (scaling x-min y-min) viewport
    (let ((height (scxml-required-pixel-height viewport))
          (scale-x (/ 1.0 (scxml-x scaling)))
          (scale-y (/ 1.0 (scxml-y scaling))))
      (2dg-point :x (+ x-min (* scale-x (+ 0.5 (scxml-x pixel))))
                   :y (+ y-min (* scale-y (- height (+ 0.5 (scxml-y pixel)))))))))
(ert-deftest scxml-get-coord-centroid-from-viewport ()
  "should fire out rectangles"
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 0 :y 0))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 1 :y 1))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))))))
  ;; offset a bit.
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 0 :y 0))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 1 :y 1))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))))))
  ;; offset a bit and zoom in
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 0 :y 0))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 1 :y 1))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))))))
  ;; offset a bit and zoom out
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 0 :y 0))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 0 :y 0)))))
    (should (2dg-almost-equal
             (scxml-get-coord-centroid viewport (scxml-pixel :x 1 :y 1))
             (2dg-centroid (scxml-get-coord viewport (scxml-pixel :x 1 :y 1)))))))

(cl-defmethod scxml-get-coord ((viewport scxml-viewport) (pixel scxml-pixel))
  "Given a pixel, return the drawing coordinate rectangle for it"
  (with-slots (scaling x-min y-min) viewport
    (let ((height (scxml-required-pixel-height viewport))
          (scale-x (/ 1.0 (scxml-x scaling)))
          (scale-y (/ 1.0 (scxml-y scaling))))
      (scxml-rect :x-min (+ x-min (* scale-x (scxml-x pixel)))
                  :x-max (+ x-min (* scale-x (1+ (scxml-x pixel))))
                  :y-max (+ y-min (* scale-y (- height (scxml-y pixel))))
                  :y-min (+ y-min (* scale-y (- height (1+ (scxml-y pixel)))))))))
(ert-deftest scxml-get-coord-from-viewport ()
  "should fire out rectangles"
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 0 :y 0))
             (scxml-rect :x-min 0.0 :x-max 1.0
                         :y-min 9.0 :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))
             (scxml-rect :x-min 1.0 :x-max 2.0
                         :y-min 8.0 :y-max 9.0))))

  ;; offset a bit.
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 0 :y 0))
             (scxml-rect :x-min 2.0 :x-max 3.0
                         :y-min 9.0 :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))
             (scxml-rect :x-min 3.0 :x-max 4.0
                         :y-min 8.0 :y-max 9.0))))
  ;; offset a bit and zoom in
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 0 :y 0))
             (scxml-rect :x-min 2.0 :x-max 2.5
                         :y-min 9.5 :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))
             (scxml-rect :x-min 2.5 :x-max 3.0
                         :y-min 9.0 :y-max 9.5))))
  ;; offset a bit and zoom out
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 0 :y 0))
             (scxml-rect :x-min 2.0 :x-max 4.0
                         :y-min 8.0 :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-coord viewport (scxml-pixel :x 1 :y 1))
             (scxml-rect :x-min 4.0 :x-max 6.0
                         :y-min 6.0 :y-max 8.0)))))

(cl-defmethod scxml-get-scratch-coord ((viewport scxml-viewport) (pixel scxml-pixel))
  "Given a pixel, return a rectangle describing the area which was clicked in drawing coordinates"
  (with-slots (x-min y-min) viewport
    (let ((height (scxml-required-pixel-height viewport)))
      (2dg-point :x (float (scxml-x pixel))
                   :y (float (- height (1+ (scxml-y pixel))))))))
(ert-deftest scxml-get-scratch-coord-from-viewport-pixel ()
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 0.0
                                   :x-max 10.0
                                   :y-min 0.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 9.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 8.0))))
  ;; offset a bit.
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 7.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 6.0))))
  ;; offset a bit and zoom in
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 15.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 14.0)))
  ;; offset a bit and zoom out
  (let* ((viewport (scxml-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                   :x-min 2.0
                                   :x-max 10.0
                                   :y-min 2.0
                                   :y-max 10.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 0 :y 0))
             (2dg-point :x 0.0 :y 3.0)))
    (should (2dg-almost-equal
             (scxml-get-scratch-coord viewport (scxml-pixel :x 1 :y 1))
             (2dg-point :x 1.0 :y 2.0))))))

(cl-defmethod scxml-get-scratch-transformers ((viewport scxml-viewport))
  "Return a cons cell of X and Y transformers that convert from drawing coordinates to scratch coordinates."
  (cons
   ;; X transformer
   (let ((x-offset (scxml-x-min viewport))
         (x-scale (scxml-x (scxml-scaling viewport))))
     (lambda (x-point) (* (- x-point x-offset) x-scale)))
   ;; Y transformer
   (let ((y-offset (scxml-y-min viewport))
         (y-scale (scxml-y (scxml-scaling viewport))))
     (lambda (y-point) (* (- y-point y-offset) y-scale)))))
(cl-defmethod scxml-get-scratch-int-transformers ((viewport scxml-viewport))
  "Return a cons cell of X and Y transformers that convert from drawing coordinates to scratch INTEGER coordinates.

for reasons that should be investigated, y must be floored.
"
  (cons
   ;; X transformer
   (let ((x-offset (scxml-x-min viewport))
         (x-scale (scxml-x (scxml-scaling viewport))))
     (lambda (x-point) (floor (* (- x-point x-offset) x-scale))))
   ;; Y transformer
   (let ((y-offset (scxml-y-min viewport))
         (y-scale (scxml-y (scxml-scaling viewport))))
     (lambda (y-point) (floor (* (- y-point y-offset) y-scale))))))
(ert-deftest scxml-get-scratch-transformers-match ()
  (let ((viewports (list (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                         :x-min 0.0
                                         :x-max 10.0
                                         :y-min 0.0
                                         :y-max 10.0)
                         (scxml-viewport :scaling (2dg-point :x 1.0 :y 1.0)
                                         :x-min 2.0
                                         :x-max 10.0
                                         :y-min 2.0
                                         :y-max 10.0)
                         (scxml-viewport :scaling (2dg-point :x 2.0 :y 2.0)
                                         :x-min 2.0
                                         :x-max 10.0
                                         :y-min 2.0
                                         :y-max 10.0)
                         (scxml-viewport :scaling (2dg-point :x 0.5 :y 0.5)
                                         :x-min 2.0
                                         :x-max 10.0
                                         :y-min 2.0
                                         :y-max 10.0))))
    (cl-loop for viewport in viewports
             for transformers = (scxml-get-scratch-transformers viewport)
             for transform-x = (car transformers)
             for transform-y = (cdr transformers)
             do (cl-loop for x from -2.0 to 2.0 by 0.1
                         do (cl-loop for y from -2.0 to 2.0 by 0.1
                                     for drawing-point = (2dg-point :x x :y y)
                                     for transformers-scratch = (2dg-point :x (funcall transform-x x)
                                                                             :y (funcall transform-y y))
                                     for one-shot-scratch = (scxml-get-scratch-coord viewport drawing-point)
                                     do (should (2dg-almost-equal transformers-scratch
                                                                    one-shot-scratch)))))))

(provide 'scxml-viewport)
;;; scxml-viewport.el ends here
