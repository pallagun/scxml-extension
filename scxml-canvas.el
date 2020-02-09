;;; scxml-canvas.el --- scxml canvas and inner-canvas objects -*- lexical-binding: t -*-

;;; Commentary:
;; A canvas is a rectangular section in space where a drawing is
;; painted/rendered.

;;; Code:
(require 'scxml-geometry)

(defvar scxml-draw--default-canvas-width 100.0
  "Default width of the main canvas.")
(defvar scxml-draw--default-canvas-height 40.0
  "Default height of the main canvas.")

(defclass scxml-canvas (2dg-rect)
  ()
  :documentation "A container for a canvas, a place where scxml things can be drawn")
(defclass scxml-inner-canvas (scxml-canvas)
  ((drawing :initarg :drawing
            :accessor scxml-inner-canvas-drawing
            ;; TODO - rename this to be parent instead of
            ;; scxml-inner-canvas-drawing.
            :type scxml-drawing))
  :documentation "A canvas _inside_ some drawable element.")
(cl-defmethod scxml-print ((canvas scxml-canvas))
  (2dg-pprint canvas))

(defun scxml-canvas--relative (parent-canvas x-size y-size x-offset y-offset)
  "Return a canvas relative to the PARENT-CANVAS.

The X-SIZE and Y-SIZE of the relative canvas can be specified as
well as the X-OFFSET and Y-OFFSET."
  ;; TODO - set parent here.
  (with-slots (x-min x-max y-min y-max) parent-canvas
    (scxml-canvas :x-min (+ x-min x-offset)
                  :y-min (+ y-min y-offset)
                  :x-max (+ x-min x-offset x-size)
                  :y-max (+ y-min y-offset y-size))))
(defun scxml-canvas--default ()
  "Generate a default canvas."
  (scxml-canvas :x-min 0.0
                :y-min 0.0
                :x-max scxml-draw--default-canvas-width
                :y-max scxml-draw--default-canvas-height))
(cl-defmethod scxml--init-buffer ((buffer buffer))
  "Set up a buffer for scxml diagrams"
  ;; TODO - move this function out of here, it's not releated to canvases.
  (when (not (eq (current-buffer) buffer))
    (switch-to-buffer buffer))
  (artist-mode-init)
  (toggle-truncate-lines 't)
  (erase-buffer)
  ;; (insert (s-repeat (+ y-offset y-size 1)
  ;;                   (format "%s\n" (s-repeat (+ x-offset x-size) " "))))
  ;; (whitespace-mode 't)
  (scxml-diagram-mode))
(cl-defgeneric scxml--split-canvas ((canvas scxml-canvas) (rows integer) (columns integer) &optional horizontal-spacing vertical-spacing)
  "Return a list of canvas representing a gridded division of CANVAS.

CANVAS will be devided into a grid having ROWS rows and COLUMNS columns.")
(cl-defmethod scxml--split-canvas ((canvas scxml-canvas) (rows integer) (columns integer) &optional horizontal-spacing vertical-spacing)
  "Return a list of canvas representing a gridded division of CANVAS.

CANVAS will be devided into a grid having ROWS rows and COLUMNS columns."
  (let ((x-spacing (or horizontal-spacing 10.0))
        (y-spacing (or vertical-spacing 4.0))
        (num-rows (float rows))
        (num-columns (float columns)))
    ;; Go from top left to bottom right - english language reading order.
    ;; But I'm going to be pushing them - so opposite that.
    (cl-loop for row-idx from 0 to (1- num-rows)
             with cells = 'nil
             with cell-x-size = (/ (- (2dg-width canvas) (* (1- num-columns) x-spacing))
                                   num-columns)
             with cell-y-size = (/ (- (2dg-height canvas) (* (1- num-rows) y-spacing))
                                   num-rows)
             with cell-x-offset = (+ cell-x-size x-spacing)
             with cell-y-offset = (+ cell-y-size y-spacing)
             do (cl-loop for column-idx from (1- num-columns) downto 0
                         do (push
                             (scxml-canvas--relative canvas
                                                     cell-x-size
                                                     cell-y-size
                                                     (* column-idx cell-x-offset)
                                                     (* row-idx cell-y-offset))
                             cells))
             finally return cells)))

(provide 'scxml-canvas)
;;; scxml-canvas.el ends here
