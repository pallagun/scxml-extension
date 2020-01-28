;;; scxml-drawable-element.el --- scxml drawable element functions -*- lexical-binding: t -*-

;;; Commentary:
;; Represents a concrete csxml element which can be drawn.

;;; Code:
(require 'eieio)
(require 'scxml-element)
(require 'scxml-drawing)
(require 'scxml-canvas)
(require 'scxml-viewport)

(defconst scxml---hint-symbol 'scxml---drawing-hint
  "The xml attribute name used to store drawing hints.")

(defclass scxml-drawable-element (scxml-element)
  ((drawing :initarg :drawing
            :accessor scxml-element-drawing
            :type (or scxml-drawing null)
            :initform nil))
  :abstract 't
  :documentation "This is an element that can be drawn and which
  has a 1 to 1 relationship with an element in a valid scxml
  document.")
(defclass scxml-synthetic-drawing (scxml--core-nil scxml-drawable-element)
  ((_hint-key :allocation :class
              :documentation "Drawing hints for synthetic
              drawings are stored on the first non-synthetic
              ancestor.  To differentiate between the ancestor's
              drawing hint and this elements drawing hint they
              are inserted into the hint alist with the specified
              hint-key."))
  :documentation "This class signifies that the object which is
  drawn does not have a 1 to 1 correspondence with an scxml
  element.")

(cl-defmethod scxml--find-first-non-synthetic-ancestor ((element scxml-element))
  "Find first ancestor that is not synthetic."
  (scxml-parent element))

(cl-defmethod scxml--find-first-non-synthetic-ancestor ((element scxml-synthetic-drawing))
  "Find first ancestor that is not synthetic."
  (scxml-find-ancestor-if element
                          (lambda (e)
                            (not (object-of-class-p e 'scxml-synthetic-drawing)))))

(cl-defmethod scxml-print ((element scxml-drawable-element))
  "Pretty print ELEMENT for human eyeballs."
  (format "hasDrawing:%s, %s"
          (and (scxml-element-drawing element) t)
          (cl-call-next-method)))
(cl-defmethod scxml-xml-attributes ((element scxml-drawable-element))
  "Return an xml attribute alist for ELEMENT.

Push in the drawing hint attribute."
  (let ((hint (scxml--hint element t)))
    (if hint
        (append (list
                 (cons scxml---hint-symbol hint))
                (cl-call-next-method))
      (cl-call-next-method))))

(cl-defgeneric scxml--drawing-invalid? ((element scxml-drawable-element))
  "Could the drawing for this ELEMENT be invalid? (i.e. needs to be replotted)"
  (scxml-get-attrib element 'scxml---drawing-invalid 't))
(cl-defgeneric scxml--set-drawing-invalid ((element scxml-drawable-element) is-invalid)
  "Note that the drawing for this ELEMENT might not be valid."
  (scxml-put-attrib element 'scxml---drawing-invalid is-invalid))

(cl-defmethod scxml--unserialize-drawing-hint (hint-string)
  "Return the drawing hint alist in HINT-STRING.

Convention is that the drawing hint specific to ELEMENT will be
in the association list with key 'self."
  (if hint-string
      (let* ((reader-cons (read-from-string hint-string))
             (all-hints (car reader-cons)))
        all-hints)
    nil))

(cl-defmethod scxml--hint ((element scxml-drawable-element) &optional full-hint)
  "Get the hint for this drawable ELEMENT.

When FULL-HINT is true the entire hint will be returned which may
be an association list containing other drawing information
pertaining to synthetic children."
  (let ((all-hints (scxml-get-attrib element scxml---hint-symbol nil)))
    (if full-hint
        all-hints
      (alist-get 'self all-hints nil))))
(cl-defmethod scxml--hint ((synth scxml-synthetic-drawing) &optional full-hint)
  "Get the hint for this drawable ELEMENT."
  (when full-hint
    (error "Full hint is not a valid request for a synthetic drawing"))
  (let ((hint-key (oref synth _hint-key))
        (hinted-element (scxml--find-first-non-synthetic-ancestor synth)))
    (unless hinted-element
      (error "Unable to find where this hint should be stored"))
    (let ((all-hints (scxml-get-attrib hinted-element scxml---hint-symbol nil)))
      (alist-get hint-key all-hints nil))))
(cl-defmethod scxml--set-hint ((element scxml-drawable-element) hint)
  "Set the hint for this drawable ELEMENT as HINT"
  (let ((all-hints (scxml-get-attrib element scxml---hint-symbol nil)))
    (if hint
        (setf (alist-get 'self all-hints) hint)
      (setf all-hints (assq-delete-all 'self all-hints)))
    (scxml-put-attrib element scxml---hint-symbol all-hints)))
(cl-defmethod scxml--set-hint ((synth scxml-synthetic-drawing) hint)
  "Set the hint for this drawable ELEMENT as HINT"
  ;; todo - this shares a lot with the hint getter scxml--hint
  (let ((hint-key (oref synth _hint-key))
        (hinted-element (scxml--find-first-non-synthetic-ancestor synth)))
    (unless hinted-element
      (error "Unable to find where this hint should be stored"))
    (let ((all-hints (scxml-get-attrib hinted-element scxml---hint-symbol nil)))
      (if hint
          (setf (alist-get hint-key all-hints) hint)
        (setf all-hints (assq-delete-all hint-key all-hints)))
      (scxml-put-attrib hinted-element scxml---hint-symbol all-hints))))

(cl-defmethod scxml--set-hint-from-attrib-list ((element scxml-drawable-element) (attributes list))
  "Set the hint for ELEMENT if a valid hint is found in ATTRIBUTES.

Used for parsing hints out of xml attributes."
  (let* ((hint-string (alist-get scxml---hint-symbol attributes nil))
         (all-hints (scxml--unserialize-drawing-hint hint-string)))
    (scxml-put-attrib element scxml---hint-symbol all-hints)))

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
(cl-defgeneric scxml--set-edit-idx ((element scxml-drawable-element) &optional idx dont-update-drawing)
  "Set the edit-xdi of ELEMENT's drawing to IDX (nillable).")
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
(cl-defgeneric scxml-build-drawing ((element scxml-drawable-element) (canvas scxml-canvas))
  "Return a drawing for ELEMENT within CANVAS.")
(cl-defgeneric scxml-simplify-drawing ((element scxml-drawable-element) (viewport scxml-viewport))
  "Attempt to simplify the drawing for ELEMENT.")
(cl-defmethod scxml-simplify-drawing ((element scxml-drawable-element) (viewport scxml-viewport))
  "Attempt to simplify the drawing for ELEMENT."
  (let ((simplified (scxml-build-simplified (scxml-element-drawing element)
                                            viewport)))
    (when simplified
      (scxml--set-hint element
                       (scxml-build-hint simplified
                                         (scxml-get-parent-drawing-inner-canvas element)))
      (scxml--set-drawing-invalid element t))))

(cl-defgeneric scxml-parent-drawing ((element scxml-drawable-element))
  "Return the drawing of ELEMENT's parent scxml-element.")
(cl-defmethod scxml-parent-drawing ((element scxml-drawable-element))
  "Return the drawing of ELEMENT's parent scxml-element."
  (let ((parent (scxml-parent element)))
    (and parent (scxml-element-drawing parent))))
(cl-defgeneric scxml-get-parent-drawing-inner-canvas ((element scxml-drawable-element))
  "Return the ELEMENT's parent's inner canvas.")
(cl-defmethod scxml-get-parent-drawing-inner-canvas ((element scxml-drawable-element))
  "Return the ELEMENT's parent's inner canvas.

Equivalent to (scxml-inner-canvas (scxml-parent-drawing ELEMENT))
but with some checks."
  (let* ((parent (scxml-parent element))
         (parent-drawing (and parent (scxml-element-drawing parent))))
    (and parent-drawing (scxml-get-inner-canvas parent-drawing))))

(cl-defmethod scxml--build-synthetic-children ((element scxml-drawable-element) (attrib-alist list))
  "Build additional synthethc elements if needed from the attrib-alist.

The only example is for <state> and <scxml> elements which can
have an 'initial=\"...\"' attribute.  When this is found
synthetic <initial> and <transition> elements are made to
graphically display the meanining of the initial attribute."
nil)

(provide 'scxml-drawable-element)
;;; scxml-drawable-element.el ends here
