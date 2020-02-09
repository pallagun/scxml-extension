;;; scxml-drawing.el --- scxml drawing object -*- lexical-binding: t -*-

;;; Commentary:
;; An scxml-drawing is an object that can be drawn on a canvas.  It is
;; usually (always?) belongs to an scxml-drawable-element.

;;; Code:
(require 'eieio)
(require 'scxml-element)
(require 'scxml-canvas)
(require 'scxml-viewport)

;; Logging facilites specific to drawings.
(defvar scxml--debug-drawing nil)
(defun scxml-toggle-debug-drawing-mode ()
  (interactive)
  (setq scxml--debug-drawing (not scxml--debug-drawing))
  (message "Setting scxml--debug-drawing to %s" scxml--debug-drawing))
(defun scxml--drawing-logger (format-string &rest message-args)
  "When scxml--debug-drawing is true, pass FORMAT-STRING and MESSAGE-ARGS to printer."
  (when scxml--debug-drawing
    (apply 'message (cons format-string message-args))))

(defclass scxml-drawing ()
  ((highlight :initarg :highlight
              ;; TODO - rename to just 'scxml-highlightp'?
              :accessor scxml-drawing-highlight
              ; TODO - type should be boolean.
              )
   (edit-idx :initarg :edit-idx
             ;; TODO - should this just be scxml-edit-idx?
             :accessor scxml-drawing-edit-idx
             :type (or null integer)
             :documentation "If edit mode is on this will be non-nil and hold the index of the current edit point")
   (locked :initarg :locked
           :accessor scxml-drawing-locked
           :initform nil
           ;; TODO - I don't think I use this?
           :documentation "Is this drawing locked in place by a user hint or not")
   (parent :initarg :parent
           :accessor scxml-parent
           :type scxml-drawable-element))
  :abstract t
  :documentation "This is a thing which can be drawn.  A rectangle, an arrow, a label, etc.")

(cl-defgeneric scxml-num-edit-idxs ((drawing scxml-drawing))
  "How many edit idx points are there for this DRAWING.

By default, assume zero."
  0)
(cl-defgeneric scxml-edit-idx-point ((drawing scxml-drawing) (idx integer))
  "Get the 2dg-point location of the given edit IDX in DRAWING")
(cl-defgeneric scxml-edit-idx-points ((drawing scxml-drawing))
  "Get a list of all the edit-idx points for this DRAWING in order")
(cl-defmethod scxml-edit-idx-points ((drawing scxml-drawing))
  "By default, none."
  nil)

(cl-defgeneric scxml-build-edited-drawing ((drawing scxml-drawing) edit-idx (move-vector 2dg-point) (viewport scxml-viewport))
  "Derive an edited drawing from DRAWING, EDIT-IDX (nillable) and MOVE-VECTOR for VIEWPORT.

This should only build a new drawing and return it (if possible)
and should not mutate anything.  Note: EDIT-IDX can be nil
meaning move all the edit-idxs (i.e. just move the whole
thing)."
  (if edit-idx
      (scxml-build-idx-edited drawing edit-idx move-vector viewport)
    (scxml-build-move-edited drawing move-vector viewport)))
(cl-defgeneric scxml-build-move-edited ((drawing scxml-drawing) (move-vector 2dg-point) (viewport scxml-viewport))
  "Build a drawing based off moving DRAWING by MOVE-VECTOR.

This should only build a new drawing and return it (if possible)
and should not mutate anything.")
(cl-defgeneric scxml-build-idx-edited ((drawing scxml-drawing) (edit-idx integer) (move-vector 2dg-point) (viewport scxml-viewport))
  "Build a drawing based off moving EDIT-IDX of DRAWING by MOVE-VECTOR.

This should only build a new drawing and return it (if possible)
and should not mutate anything.")
(cl-defgeneric scxml-build-hint ((drawing scxml-drawing) (parent-canvas scxml-inner-canvas))
  "Given a DRAWING and PARENT-CANVAS generate a drawing 'hint'.

A drawing 'hint' is something that captures the intent of the
drawing but not the exact pixels.  Something like box-on-the-left
instead of an exact set of pixels/segments.  It may or may not be
relative to the parent-canvas.")
(cl-defgeneric scxml-build-simplified ((drawing scxml-drawing) (viewport scxml-viewport))
  "Attempt to build a simplified DRAWING as seen by human eyes in VIEWPORT.

VIEWPORT is used to establish how agressive the simplification can be.")

(cl-defgeneric scxml-get-inner-canvas ((drawing scxml-drawing))
  "Return the inner canvas of DRAWING which may be nil.")
(cl-defmethod scxml-get-inner-canvas ((drawing scxml-drawing))
  "By default, drawings will have no inner canvas."
  nil)

(provide 'scxml-drawing)
;;; scxml-drawing.el ends here
