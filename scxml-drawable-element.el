;;; scxml-drawable-element.el --- scxml drawable element functions -*- lexical-binding: t -*-

;;; Commentary:
;; Represents a concrete csxml element which can be drawn.

;;; Code:
(require 'eieio)
(require 'scxml-element)
(require 'scxml-drawing)

(defconst scxml---hint-symbol 'scxml---drawing-hint
  "The xml attribute name used to store drawing hints.")

(defclass scxml-drawable-element (scxml-element)
  ((drawing :initarg :drawing
            :accessor scxml-element-drawing
            :type (or scxml-drawing null)
            :initform nil))
  :abstract 't
  :documentation "This is an element that can be drawn.")
(cl-defmethod scxml-print ((element scxml-drawable-element))
  "Pretty print ELEMENT for human eyeballs."
  (format "hasDrawing:%s, %s"
          (and (scxml-element-drawing element) t)
          (cl-call-next-method)))
(cl-defmethod scxml-xml-attributes ((element scxml-drawable-element))
  "Return an xml attribute alist for ELEMENT.

Push in the drawing hint attribute."
  (append (list
           (cons scxml---hint-symbol
                 (scxml-get-attrib element scxml---hint-symbol nil)))
          (cl-call-next-method)))

(cl-defgeneric scxml--drawing-invalid? ((element scxml-drawable-element))
  "Could the drawing for this ELEMENT be invalid? (i.e. needs to be replotted)"
  (scxml-get-attrib element 'scxml---drawing-invalid 't))
(cl-defgeneric scxml--set-drawing-invalid ((element scxml-drawable-element) is-invalid)
  "Note that the drawing for this ELEMENT might not be valid."
  (scxml-put-attrib element 'scxml---drawing-invalid is-invalid))

(cl-defmethod scxml--hint ((element scxml-drawable-element))
  "Get the hint for this drawable ELEMENT"
  (scxml-get-attrib element scxml---hint-symbol))
(cl-defmethod scxml--set-hint ((element scxml-drawable-element) hint)
  "Set the hint for this drawable ELEMENT as HINT"
  (scxml-put-attrib element scxml---hint-symbol hint))
(cl-defmethod scxml--set-hint-from-attrib-list ((element scxml-drawable-element) (attributes list))
  "Set the hint for ELEMENT if a valid hint is found in ATTRIBUTES.

Used for parsing hints out of xml attributes."
  (let ((hint-string (alist-get scxml---hint-symbol attributes nil)))
    (when hint-string
      (let ((drawing-hint (read-from-string hint-string)))
        (when (car drawing-hint)
          (scxml--set-hint element (car drawing-hint)))))))

(cl-defmethod scxml--highlight ((element scxml-drawable-element))
  "Return non-nil if this element should be highlighted."
  (scxml-get-attrib element 'scxml---drawing-highlight))
(cl-defmethod scxml--set-highlight ((element scxml-drawable-element) on-off &optional dont-update-drawing)
  "Highlight this element in any drawings"
  (scxml-put-attrib element 'scxml---drawing-highlight on-off)
  (when (not on-off)
    (scxml--set-edit-idx element))
  (when (not dont-update-drawing)
    (let ((drawing (scxml-element-drawing element)))
      (when drawing (oset drawing highlight (if on-off 't 'nil))))))

(cl-defmethod scxml--edit-idx ((element scxml-drawable-element))
  "Return the current edit-idx of ELEMENT, may be nil."
  (scxml-get-attrib element 'scxml---drawing-edit-idx))
(cl-defmethod scxml--set-edit-idx ((element scxml-drawable-element) &optional idx dont-update-drawing)
  "Set the edit-idx of the ELEMENT's drawing to IDX.

When IDX is not set it will be set to 'nil which sets the drawing
into a state where there is currently _no_ valid edit-idx.
Optionally DONT-UPDATE-DRAWING set to 't will prevent drawing
update"
  (scxml-put-attrib element 'scxml---drawing-edit-idx idx)
  (when (not dont-update-drawing)
    (let ((drawing (scxml-element-drawing element)))
      (when (and drawing
                 (> (scxml-num-edit-idxs drawing) 0))
        (oset drawing edit-idx idx)))))
(cl-defmethod scxml-num-edit-idxs ((element scxml-drawable-element))
  "Determine the number of edit-idxs this ELEMENT has."
  (with-slots (drawing) element
    (if drawing
        (scxml-num-edit-idxs drawing)
      (error "Error scxml-num-edit-idxs: unable to determine edit idx, no drawing"))))
(cl-defmethod scxml--increment-edit-idx ((element scxml-drawable-element) &optional increment dont-update-drawing)
  "Increment the edit-idx for a given ELEMENT's drawing by INCREMENT.

INCREMENT will default to 1.  When DONT-UPDATE-DRAWING is 't the
drawing will not be updated."
  (let* ((delta (or increment 1))
         (current-idx (scxml--edit-idx element))
         (num-idxs (scxml-num-edit-idxs element))
         (new-idx (mod (+ num-idxs delta current-idx) num-idxs)))
    (scxml--set-edit-idx element new-idx dont-update-drawing)))

(cl-defgeneric scxml--update-drawing ((element scxml-drawable-element) (canvas scxml-canvas))
  "If ELEMENT needs to have a drawing regenerated for CANVAS, do so.

Will update ELEMENT accordingly when a new drawing is created.
Will _not_ create a new drawing or modify an existing one if
it's not needed.  Will set drawing and invalid flags on ELEMENT.
Returns the current ELEMENT drawing."
  (when (or (not (scxml-element-drawing element))
            (scxml--drawing-invalid? element))
    (let ((drawing (scxml-build-drawing element canvas)))
      (when drawing
        (oset element drawing drawing)
        (scxml--set-drawing-invalid element 'nil)
        drawing)))
  (scxml-element-drawing element))

(provide 'scxml-drawable-element)
;;; scxml-drawable-element.el ends here
