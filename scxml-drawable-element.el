;;; scxml-drawable-element.el --- scxml drawable element functions -*- lexical-binding: t -*-

;;; Commentary:
;; Enhancements to scxml-drawable-elements
;; note: scxml-drawable-element is actually defined in scxml-element
;; TODO: it shouldn't be though.

;;; Code:
(require 'scxml-element)
(require 'scxml-drawing)

(defclass scxml-drawable-element (scxml-element)
  ((drawing :initarg :drawing
            :accessor scxml-element-drawing
            :type (or scxml-drawing null)
            :initform nil)
   (xml-link :initarg :xml-link
             :accessor scxml-xml-link
             :initform nil
             ;; TODO - is this used?
             ))
  :abstract 't
  :documentation "This is an element that can be drawn on a canvas")
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
;; TODO - these should probably be in another file.
(cl-defmethod scxml--set-drawing-invalid ((initial scxml-initial) is-invalid)
  "Mark INITIAL's drawing as invalid as well as all children.

Note: there should only be one child and it should be a transition."
  (cl-call-next-method initial is-invalid)
  (when is-invalid
    (mapc (lambda (child) (scxml--set-drawing-invalid child 't))
          (scxml-children initial))))
(cl-defmethod scxml--set-drawing-invalid ((final scxml-final) is-invalid)
  "Mark this FINAL's drawing as IS-INVALID.  Will also invalidate any transitions in."
  (cl-call-next-method final is-invalid)
  (when is-invalid
    ;; mark all transitions to or from this state as possibly invalid as well.
    (mapc (lambda (element)
            (scxml--set-drawing-invalid element 't))
          (append
           (seq-filter (lambda (child)
                         (object-of-class-p child 'scxml-drawable-element))
                       (scxml-children final)) ;all from state.
           (scxml-get-all-transitions-to final)))))
(cl-defmethod scxml--set-drawing-invalid ((state scxml-state) is-invalid)
  "Note that the drawing for this STATE might not be valid and also any transition to or from it."
  ;; TODO: maybe the marking of all transitions to and from this state as invalid
  ;; should be handled by a keyword argument of some sort?
  ;; TODO: figure out how to work with keyword arguments.
  (cl-call-next-method state is-invalid)
  (when is-invalid
    ;; mark all transitions to or from this state as possibly invalid as well.
    (mapc (lambda (transition)
            (scxml--set-drawing-invalid transition 't))
          (append
           (seq-filter (lambda (e) (object-of-class-p e 'scxml-transition))
                       (scxml-children state)) ;all from state.
           (scxml-get-all-transitions-to state)))))
(cl-defmethod scxml--set-drawing-invalid ((transition scxml-transition) is-invalid &optional dont-cascade)
  "Note that when a transition goes from hinted to unhinted it cause other transitions to become Invalid.

If there are two transitions touching a single state and one of
them goes from automatic to hinted, it could cause the other
transition to shuffle connector points."
  (cl-call-next-method transition is-invalid)
  (when (and is-invalid (not dont-cascade))
    (let ((touched-states (list (scxml-source transition)
                                (scxml-target transition))))
      (scxml-visit-all transition
                       (lambda (other-transition)
                         (scxml--set-drawing-invalid other-transition 't 't))
                       (lambda (element)
                         (and (object-of-class-p element 'scxml-transition)
                              (or (member (scxml-source element)
                                          touched-states)
                                  (member (scxml-target element)
                                          touched-states))))))))

(defconst scxml---hint-symbol 'scxml---drawing-hint)
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
