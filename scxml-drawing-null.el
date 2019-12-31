;;; scxml-drawing-null.el --- scxml null drawing functions -*- lexical-binding: t -*-

;;; Commentary:
;; scxml-drawing-null is a drawing that is invisible.  It exists to
;; provide other drowings with a reference point or an inner-canvas.
;; Posibly a better name would be scxml-drawing-invisible.

;;; Code:
(require 'scxml-drawing)
(require 'scxml-geometry-rect)

(defclass scxml-drawing-null (scxml-drawing scxml-rect)
  ()
  :documentation "A drawing that can not be drawn but has a
stable inner canvas.  Should serve as reference for other
drawings.")

(cl-defmethod scxml-get-inner-canvas ((container scxml-drawing-null))
  "Return an inner-canvas of CONTAINER."
  (with-slots (x-min y-min x-max y-max) container
    (scxml-inner-canvas :x-min x-min
                        :y-min y-min
                        :x-max x-max
                        :y-max y-max
                        :drawing container)))
(cl-defmethod scxml-build-drawing ((root scxml-scxml) (canvas scxml-canvas))
  "Return a drawing for ROOT within CANVAS.

For an <scxml> element this simply return a null drawing that
consumes the entire canvas."
  (with-slots (x-min y-min x-max y-max) canvas
    (scxml-drawing-null :x-min x-min
                        :y-min y-min
                        :x-max x-max
                        :y-max y-max
                        :parent root)))

(provide 'scxml-drawing-null)
;;; scxml-drawing-null.el ends here
