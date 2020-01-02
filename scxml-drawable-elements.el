;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(require 'scxml-drawable-element)

(defclass scxml-drawable-scxml (scxml-scxml scxml-drawable-element)
  ())

(defclass scxml-drawable-state (scxml-state scxml-drawable-element)
  ())
(cl-defmethod scxml--set-drawing-invalid ((state scxml-drawable-state) is-invalid)
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

(defclass scxml-drawable-final (scxml-final scxml-drawable-element)
  ())
(cl-defmethod scxml--set-drawing-invalid ((final scxml-drawable-final) is-invalid)
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

(defclass scxml-drawable-initial (scxml-initial scxml-drawable-element)
  ())
(cl-defmethod scxml--set-drawing-invalid ((initial scxml-drawable-initial) is-invalid)
  "Mark INITIAL's drawing as invalid as well as all children.

Note: there should only be one child and it should be a transition."
  (cl-call-next-method initial is-invalid)
  (when is-invalid
    (mapc (lambda (child) (scxml--set-drawing-invalid child 't))
          (scxml-children initial))))

(defclass scxml-drawable-parallel (scxml-parallel scxml-drawable-element)
  ())

(defclass scxml-drawable-transition (scxml-transition scxml-drawable-element)
  ())
(cl-defmethod scxml--set-drawing-invalid ((transition scxml-drawable-transition) is-invalid &optional dont-cascade)
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

(defun scxml--drawable-element-factory (type attrib-alist)
  "Build a drawable element of TYPE and having ATTRIB-ALIST properties."
  (let* ((base-xml-element-name (symbol-name type))
         (base-class (intern (format "scxml-%s" base-xml-element-name)))
         (base-slots (eieio-class-slots base-class))
         (base-slot-symbols (mapcar 'cl--slot-descriptor-name base-slots))
         (drawable-class (intern (format "scxml-drawable-%s" base-xml-element-name)))
         (drawable-slots (eieio-class-slots drawable-class))
         (drawable-slot-symbols (mapcar 'cl--slot-descriptor-name drawable-slots))
         (skip-slots))
    ;; Build a list of all drawable-slots that aren't in teh base slots and
    ;; use them as as a skip-slots list.
    (mapc (lambda (slot-sym)
            (when (not (memq slot-sym base-slot-symbols))
              (push slot-sym skip-slots)))
          drawable-slot-symbols)
    (scxml--element-factory (intern (format "drawable-%s" base-xml-element-name))
                            attrib-alist
                            skip-slots)))

(provide 'scxml-drawable-elements)
