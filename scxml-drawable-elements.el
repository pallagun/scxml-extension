;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(require 'scxml-drawable-element)

(defclass scxml-drawable-scxml (scxml-scxml scxml-drawable-element)
  ())
(cl-defmethod scxml-build-drawing ((root scxml-drawable-scxml) (canvas scxml-canvas))
  "Return a drawing for ROOT within CANVAS.

For an <scxml> element this simply return a null drawing that
consumes the entire canvas."
  ;; TODO - might need to build from a hint someday?
  (with-slots (x-min y-min x-max y-max) canvas
    (scxml-drawing-null :x-min x-min
                        :y-min y-min
                        :x-max x-max
                        :y-max y-max
                        :parent root)))

;; TODO - scxml-state-type is for element, it needs to brought
;; into the drawable elements space.
(cl-defmethod scxml-build-drawing ((state scxml-state-type) (canvas scxml-canvas))
  "Build drawing helper"
  (scxml--drawing-logger "scxml--build-drawing: %s" (scxml-print state))
  (scxml--drawing-logger "scxml--build-drawing: canvas: %s" (scxml-print canvas))
  (scxml--drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? state)
                          (if (scxml-element-drawing state) 't 'nil))
  (let ((hint (scxml--hint state))
        (state-name (scxml-element-id state))
        (highlight (scxml--highlight state))
        (edit-idx (scxml--edit-idx state))
        ;; if your parent is a <parallel> then this is a noshell rect.
        ;; and the parent <parallel> is responsible for drawing divisions.
        (drawing-factory (if (object-of-class-p (scxml-parent state) 'scxml-parallel)
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
                                        (scxml-get-inner-canvas parent-drawing)
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
(cl-defmethod scxml-build-drawing ((initial scxml-drawable-initial) (canvas scxml-canvas))
  "Build drawing helper"
  (scxml--drawing-logger "scxml--build-drawing: %s" (scxml-print initial))
  (scxml--drawing-logger "scxml--build-drawing: canvas: %s" (scxml-print canvas))
  (scxml--drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? initial)
                          (if (scxml-element-drawing initial) 't 'nil))
  (let ((hint (scxml--hint initial))
        (highlight (scxml--highlight initial))
        (centroid (scxml-centroid canvas)))
    (if (null hint)
        ;; Generate the drawing (not based on a hint)
        (scxml-drawing-point :x (scxml-x centroid)
                             :y (scxml-y centroid)
                             :label "I" ;label for 'Initial'
                             :highlight highlight
                             :edit-idx nil
                             :parent initial)
      ;; todo - clean up this implementation, it's duplicative :(
      (let* ((parent (scxml-parent initial))
             (parent-drawing (when (object-of-class-p parent 'scxml-drawable-element)
                               (scxml-element-drawing parent)))
             (parent-drawing-canvas (if parent-drawing
                                        (scxml-get-inner-canvas parent-drawing)
                                      canvas)))
        (when (not (scxml-inner-canvas-p parent-drawing-canvas))
          (error "Not sure how to continue here :("))

        (let ((placement (scxml-absolute-coordinates parent-drawing-canvas hint)))
          (scxml-drawing-point :x (scxml-x placement)
                               :y (scxml-y placement)
                               :label "I" ;label for 'Initial'
                               :highlight highlight
                               :edit-idx nil
                               :parent initial))))))

(defclass scxml-drawable-parallel (scxml-parallel scxml-drawable-element)
  ())
(cl-defmethod scxml-build-drawing ((parallel scxml-drawable-parallel) (canvas scxml-canvas))
  ;; TODO : this 'canvas' argument should be an interior canvas??
  "Build drawing helper"
  (scxml--drawing-logger "scxml--build-drawing: %s" (scxml-print parallel))
  (scxml--drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? parallel)
                          (if (scxml-element-drawing parallel) 't 'nil))
  (let ((hint (scxml--hint parallel))
        (name (scxml-element-id parallel))
        (highlight (scxml--highlight parallel))
        (edit-idx (scxml--edit-idx parallel)))
    (let* ((num-children (length (scxml-children parallel)))
           (num-rows (floor (sqrt num-children)))
           (num-columns (ceiling (/ num-children num-rows))))
      (if (null hint)
          ;; Generate the drawing (not based on a hint)
          (scxml---set-dividers
           (scxml--set-layout
            (scxml-drawing-nest-rect :x-min (scxml-x-min canvas)
                                     :y-min (scxml-y-min canvas)
                                     :x-max (scxml-x-max canvas)
                                     :y-max (scxml-y-max canvas)
                                     :name name
                                     :highlight highlight
                                     :edit-idx edit-idx
                                     :parent parallel)
            num-rows
            num-columns))

        ;; Generate the drawing based on the hint.
        ;; TODO - a lot of this is shared with the scmxl-state routine - share the code??
        (let* ((parent (scxml-parent parallel))
               (parent-drawing (when (object-of-class-p parent 'scxml-drawable-element)
                                 (scxml-element-drawing parent)))
               (parent-drawing-canvas (if parent-drawing
                                          (scxml-get-inner-canvas parent-drawing)
                                        canvas)))
          (when (not (scxml-inner-canvas-p parent-drawing-canvas))
            ;; welp, I'm really hoping you're _in_ a rectangle
            (error "Not sure how to continue here :("))
          (let ((absolute-rect (scxml-absolute-coordinates parent-drawing-canvas
                                                           (scxml-relative-rect hint)))
                (guide-stripe (scxml-stripe hint)))
            (with-slots (x-min x-max y-min y-max) absolute-rect
              (scxml---set-dividers
               (scxml-drawing-nest-rect :x-min x-min
                                        :y-min y-min
                                        :x-max x-max
                                        :y-max y-max
                                        :name name
                                        :highlight highlight
                                        :edit-idx edit-idx
                                        :parent parallel
                                        :axis (scxml-axis guide-stripe)
                                        :cells (scxml-cells guide-stripe)
                                        :divisions (scxml-divisions guide-stripe))))))))))

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
(cl-defmethod scxml-build-drawing ((transition scxml-drawable-transition) (canvas scxml-canvas) &optional source-connector target-connector)
  "?"
  ;; TODO - I'm not sure how this works, but this needs to be looked at.
  (error "TODO: Implementation"))


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
