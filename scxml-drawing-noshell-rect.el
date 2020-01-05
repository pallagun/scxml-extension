;;; scxml-drawing-noshell-rect.el --- scxml drawing rectangle without a shell functions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'scxml-drawing-rect)

(defclass scxml-drawing-noshell-rect (scxml-drawing-rect)
  ()
  :documentation "Exactly the same as a scxml-drawing-rect, but
the outline is never drawn.  This rectangle delegates
responsibility for drawing its bounds to someone else (probably a
parent).  Currently used to draw child <state> elements of a
<parallel>")

(cl-defmethod scxml-num-edit-idxs ((rect scxml-drawing-noshell-rect))
  "How many edit idx points are there for this ARROW"
  0)
(cl-defmethod scxml-edit-idx-points ((rect scxml-drawing-noshell-rect))
  "Get the pixel locations of the edit idxs for RECT as a list."
  nil)
(cl-defmethod scxml-edit-idx-point ((rect scxml-drawing-noshell-rect) (idx integer))
  "Get the pixel location of the given edit idx BL is zero, go CCW from there"
  (error "Invalid edit-mode idx"))

(provide 'scxml-drawing-noshell-rect)
;;; scxml-drawing-noshell-rect.el ends here
