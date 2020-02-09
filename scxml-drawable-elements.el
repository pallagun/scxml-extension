;;; scxml-drawable-elements.el --- scxml drawable elements -*- lexical-binding: t -*-

;;; Commentary:
;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(eval-when-compile (require 'subr-x))

(require 'scxml-drawable-element)

(require 'scxml-drawing-null)           ;for scxml
(require 'scxml-drawing-rect)           ;for state/final
(require 'scxml-drawing-noshell-rect)   ;for state/final in parallels
(require 'scxml-drawing-point)          ;for initials.
(require 'scxml-drawing-divided-rect)   ;for parallels
(require 'scxml-drawing-arrow)          ;for transitions.

;; Helper function
(defun scxml--build-synthetic-initial (element attrib-alist)
  "If element is found to have a non-nil value of it's
\"initial\" attribute create and add synthetic drawings for the
initial target state."
  (let ((initial-attrib-id (scxml-element-initial element)))
    (when initial-attrib-id
      (let ((initial (scxml-drawable-synthetic-initial))
            (transition (scxml-drawable-synthetic-transition :target initial-attrib-id)))
        (scxml-add-child initial transition)
        (scxml-add-child element initial)))))

;; Drawable elements and synthetic elements.
(defclass scxml-drawable-scxml (scxml-scxml scxml-drawable-element)
  ())
(cl-defmethod scxml-build-drawing ((root scxml-drawable-scxml) (canvas scxml-canvas))
  "Return a drawing for ROOT within CANVAS.

For an <scxml> element this simply return a null drawing that
consumes the entire canvas."
  (scxml-build-drawing-null root canvas))
(cl-defmethod scxml--build-synthetic-children ((element scxml-drawable-scxml) (attrib-alist list))
  ;; TODO - I should not need this function, I should be able to specify this with a class type.
  ;;  see the same named function but for scxml-drawable-state.
  (scxml--build-synthetic-initial element attrib-alist))

(defclass scxml-drawable-state-type (scxml-drawable-element)
  ()
  :documentation "A drawable layer equivalent to scxml-state-type."
  :abstract t)
(cl-defmethod scxml--set-drawing-invalid ((element scxml-drawable-state-type) is-invalid)
  "Mark this ELEMENT's drawing as IS-INVALID.  Will also invalidate any transitions in."
  (cl-call-next-method element is-invalid)
  (when is-invalid
    ;; mark all transitions to or from this state as possibly invalid as well.
    (mapc (lambda (element)
            (scxml--set-drawing-invalid element 't))
          (nconc
           (seq-filter (lambda (child)
                         (object-of-class-p child 'scxml-drawable-element))
                       (scxml-children element)) ;all from state.
           (scxml-get-all-transitions-to element)))))
(cl-defmethod scxml-build-drawing ((state scxml-drawable-state-type) (canvas scxml-canvas))
  "Build a drawing for STATE within CANVAS."
  (scxml--drawing-logger "scxml--build-drawing: %s" (scxml-print state))
  (scxml--drawing-logger "scxml--build-drawing: canvas: %s" (scxml-print canvas))
  (scxml--drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? state)
                          (if (scxml-element-drawing state) 't 'nil))
  (let ((hint (scxml--hint state))
        (label (scxml-element-id state))
        (highlight (scxml--highlight state))
        (edit-idx (scxml--edit-idx state))
        ;; if your parent is a <parallel> then this is a noshell rect.
        ;; and the parent <parallel> is responsible for drawing divisions.
        (drawing-factory (if (object-of-class-p (scxml-parent state) 'scxml-parallel)
                             'scxml-drawing-noshell-rect
                           'scxml-drawing-rect)))
    (if (null hint)
        (funcall drawing-factory
                 :x-min (scxml-x-min canvas)
                 :y-min (scxml-y-min canvas)
                 :x-max (scxml-x-max canvas)
                 :y-max (scxml-y-max canvas)
                 :name label
                 :highlight highlight
                 :edit-idx edit-idx
                 :parent state)

      (let ((parent-drawing-canvas (scxml-get-parent-drawing-inner-canvas state)))
        (unless parent-drawing-canvas
          (error "Unable to build drawing without an already drawn parent."))
        (let ((absolute-rect (2dg-absolute-coordinates parent-drawing-canvas hint)))
          (with-slots (x-min x-max y-min y-max) absolute-rect
            (funcall drawing-factory
                     :x-min x-min
                     :y-min y-min
                     :x-max x-max
                     :y-max y-max
                     :locked 't
                     :name label
                     :highlight highlight
                     :edit-idx edit-idx
                     :parent state)))))))

(defclass scxml-drawable-state (scxml-state scxml-drawable-state-type)
  ())
(cl-defmethod scxml--build-synthetic-children ((element scxml-drawable-state) (attrib-alist list))
  (scxml--build-synthetic-initial element attrib-alist))

(defclass scxml-drawable-final (scxml-final scxml-drawable-state-type)
  ())

(defconst scxml--drawable-initial-label "I"
  "What text label to use for rendering <initial> elements, should be a single character.")
(defconst scxml--drawable-synthetic-initial-label "i"
  "What text label to use for rendering initial=\"...\" attributes, should be a single character.")
(defclass scxml-drawable-initial (scxml-initial scxml-drawable-element)
  ()
  :documentation "A drawable <initial> element")
(cl-defmethod scxml--set-drawing-invalid ((initial scxml-drawable-initial) is-invalid)
  "Mark INITIAL's drawing as invalid as well as all children.

Note: there should only be one child and it should be a transition."
  (cl-call-next-method initial is-invalid)
  (when is-invalid
    (mapc (lambda (child) (scxml--set-drawing-invalid child 't))
          (scxml-children initial))))
(cl-defmethod scxml-build-drawing ((initial scxml-drawable-initial) (canvas scxml-canvas))
  "Build a drawing for INITIAL within CANVAS."
  (scxml--drawing-logger "scxml--build-drawing: %s" (scxml-print initial))
  (scxml--drawing-logger "scxml--build-drawing: canvas: %s" (scxml-print canvas))
  (scxml--drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? initial)
                          (if (scxml-element-drawing initial) 't 'nil))
  (let ((hint (scxml--hint initial))
        (highlight (scxml--highlight initial))
        (label (if (object-of-class-p initial 'scxml-drawable-synthetic-initial)
                   scxml--drawable-synthetic-initial-label
                 scxml--drawable-initial-label))
        (centroid (2dg-centroid canvas)))
    (if (null hint)
        ;; Generate the drawing (not based on a hint)
        (scxml-drawing-point :x (2dg-x centroid)
                             :y (2dg-y centroid)
                             :label label
                             :highlight highlight
                             :edit-idx nil
                             :parent initial)
      (let ((parent-drawing-canvas (scxml-get-parent-drawing-inner-canvas initial)))
        (unless parent-drawing-canvas
          (error "Unable to build drawing without an already drawn parent."))
        (let ((placement (2dg-absolute-coordinates parent-drawing-canvas hint)))
          (scxml-drawing-point :x (2dg-x placement)
                               :y (2dg-y placement)
                               :label label
                               :highlight highlight
                               :edit-idx nil
                               :parent initial))))))
(defclass scxml-drawable-synthetic-initial (scxml-synthetic-drawing scxml-drawable-initial)
  ((_hint-key :allocation :class
              ;; TODO- this should have a type to restrict it to being specifically 'synth-initial
              :initform synth-initial))
  :documentation "A drawable representation of an element's
  'initial=\"...\"' attribute.")
(cl-defmethod scxml-make-orphan ((synth-initial scxml-drawable-synthetic-initial))
  "Remove the synthetic initial attribute drawings from the parent state/scxml."
  (let ((parent (scxml-parent synth-initial)))
    (when parent
      (setf (scxml-element-initial parent) nil))
    (cl-call-next-method)))

(defclass scxml-drawable-parallel (scxml-parallel scxml-drawable-element)
  ())
(cl-defmethod scxml-add-child ((parent scxml-drawable-parallel) (new-child scxml-element) &optional append)
  "Modify PARENT adding NEW-CHILD as a child returning PARENT.

In the case of a parallel PARENT element, the parent's drawing
must be invalidated to ensure it replots with enough containers
for all the children.

When APPEND is non-nil NEW-CHILD will become the last child.
When APPEND is nil NEW-CHILD will become the first child."
  (cl-call-next-method)
  (when-let ((hint (scxml--hint parent)))
    ;; invalidate only the inner division portions of the hint.
    (setf (scxml-stripe-invalid hint) t))
  (scxml--set-drawing-invalid parent 't))

(cl-defmethod scxml-build-drawing ((parallel scxml-drawable-parallel) (canvas scxml-canvas))
  "Build drawing helper"
  ;; TODO - this needs clean up.
  (scxml--drawing-logger "scxml--build-drawing: %s" (scxml-print parallel))
  (scxml--drawing-logger "scxml--build-draiwng:- drawingInvalid?: %s, drawingExists %s"
                          (scxml--drawing-invalid? parallel)
                          (if (scxml-element-drawing parallel) 't 'nil))
  (let ((hint (scxml--hint parallel))
        (name (scxml-element-id parallel))
        (highlight (scxml--highlight parallel))
        (edit-idx (scxml--edit-idx parallel)))
    (let* ((num-children (length (scxml-children parallel)))
           (num-rows (max 1 (floor (sqrt num-children))))
           (num-columns (max 1 (ceiling (/ num-children num-rows)))))
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
        ;; If the hint is just a simple rectangle hint, fine.  Upcast it.
        (when (and (not (scxml---drawing-nest-rect-hint-p hint))
                   (object-of-class-p hint 'scxml-rect))
          (setq hint (scxml--build-empty-nest-rect-hint hint)))
        (let ((parent-drawing-canvas (scxml-get-parent-drawing-inner-canvas parallel)))
          (unless parent-drawing-canvas
            (error "Unable to build drawing without an already drawn parent."))
          (let ((absolute-rect (2dg-absolute-coordinates parent-drawing-canvas
                                                           (scxml-relative-rect hint)))
                (guide-stripe (scxml-stripe hint)))
            (when (scxml-stripe-invalid hint)
              (scxml--set-layout guide-stripe num-rows num-columns))
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
(cl-defmethod scxml--set-drawing-invalid ((parallel scxml-drawable-parallel) is-invalid)
  "Mark this PARALLEL's drawing as IS-INVALID.  Will also invalidate any transitions in."
  ;; TODO - this is very similar to the state-type set-drawing-invalid function.
  (cl-call-next-method parallel is-invalid)
  (when is-invalid
    ;; mark all transitions to or from this state as possibly invalid as well.
    (mapc (lambda (e) (scxml--set-drawing-invalid e 't))
          (nconc
           (seq-filter (lambda (child) (object-of-class-p child 'scxml-drawable-element))
                       (scxml-children parallel)) ;all from state.
           (scxml-get-all-transitions-to parallel)))))

(defclass scxml-drawable-transition (scxml-transition scxml-drawable-element)
  ()
  :documentation "Note there is no scxml-build-drawing function
  for a transition as they are all build as a collection, not
  individually.")
(cl-defmethod scxml--set-drawing-invalid ((transition scxml-drawable-transition) is-invalid)
  "Note that when a transition goes from hinted to unhinted it cause other transitions to become Invalid.

If there are two transitions touching a single state and one of
them goes from automatic to hinted, it could cause the other
transition to shuffle connector points."

  ;; This doesn't quite work perfectly.  It's invalidating based on
  ;; the edge that the arrow is leaving.  But when going to auto
  ;; drawing mode the arrow could go from one edge to another.
  ;; Really, I want to invalidate both of those edges.  Or one of
  ;; them?  This is a bit strange.  Possibly automatic routing mode is
  ;; asking for trouble in this case

  ;; TODO - this currently doesn't take into account the edges.  I had
  ;; to take that out as it's possible for a transition to not yet
  ;; have a drawing.  In that case you can't even tell what edge is
  ;; going to be involved.

  (cl-call-next-method transition is-invalid)
  (cl-flet ((build-node-list
             (transition)
             (let ((source-node (scxml-source transition))
                   (target-node (scxml-target transition)))
               (list source-node target-node))))
    (when (and is-invalid)
      (let ((node-edges (build-node-list transition)))
        (scxml-visit-all transition
                         (lambda (other-transition)
                           (when (intersection node-edges
                                               (build-node-list other-transition)
                                               :test 'equal)
                             (scxml--set-drawing-invalid-raw other-transition t)))
                         #'scxml-transition-class-p)))))
(cl-defmethod scxml-build-drawing ((transition scxml-drawable-transition) (canvas scxml-canvas))
  "Warning method: transitions/arrows are not build individually, they're built as a group."
  (error "Invalid operation for an scxml-transition type element"))
(defclass scxml-drawable-synthetic-transition (scxml-synthetic-drawing scxml-drawable-transition)
  ((_hint-key :allocation :class
              ;; TODO- this sholud have a type ot restrict it to being specificiall 'synth-transition.
              :initform synth-transition))
  :documentation "Note there is no scxml-build-drawing function
  for a transition as they are all build as a collection, not
  individually.")
(cl-defmethod scxml-make-orphan ((synth-transition scxml-drawable-synthetic-transition))
  "Remove the synthetic initial attribute drawings from the grand parent state/scxml."
  (let ((parent (scxml-parent synth-transition)))
    (when parent
      (scxml-make-orphan parent))
    (cl-call-next-method)))



(defun scxml--drawable-element-factory (type attrib-alist)
  "Build a drawable element of TYPE and having ATTRIB-ALIST properties."
  (let* ((base-xml-element-name (symbol-name type))
         (base-class (intern (format "scxml-%s" base-xml-element-name)))
         (base-slots (eieio-class-slots base-class))
         ;; TODO - Is there something more appropriate than a cl--* function?
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
    (let ((element (scxml--element-factory (intern (format "drawable-%s" base-xml-element-name))
                                           attrib-alist
                                           skip-slots)))
      (scxml--set-hint-from-attrib-list element attrib-alist)
      (scxml--build-synthetic-children element attrib-alist)
      element)))


(provide 'scxml-drawable-elements)
;;; scxml-drawable-elements.el ends here
