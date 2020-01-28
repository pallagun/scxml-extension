;;; scxml-drawable-elements.el --- scxml drawable elements -*- lexical-binding: t -*-

;;; Commentary:
;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
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
        (let ((absolute-rect (scxml-absolute-coordinates parent-drawing-canvas hint)))
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
(defclass scxml-drawable-synthetic-initial (scxml-synthetic-drawing scxml-drawable-initial)
  ((_hint-key :allocation :class
              ;; TODO- this should have a type to restrict it to being specifically 'synth-initial
              :initform synth-initial))
  :documentation "A drawable representation of an element's
  'initial=\"...\"' attribute.")
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
        (centroid (scxml-centroid canvas)))
    (if (null hint)
        ;; Generate the drawing (not based on a hint)
        (scxml-drawing-point :x (scxml-x centroid)
                             :y (scxml-y centroid)
                             :label label
                             :highlight highlight
                             :edit-idx nil
                             :parent initial)
      (let ((parent-drawing-canvas (scxml-get-parent-drawing-inner-canvas initial)))
        (unless parent-drawing-canvas
          (error "Unable to build drawing without an already drawn parent."))
        (let ((placement (scxml-absolute-coordinates parent-drawing-canvas hint)))
          (scxml-drawing-point :x (scxml-x placement)
                               :y (scxml-y placement)
                               :label label
                               :highlight highlight
                               :edit-idx nil
                               :parent initial))))))

(defclass scxml-drawable-parallel (scxml-parallel scxml-drawable-element)
  ())
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

        (let ((parent-drawing-canvas (scxml-get-parent-drawing-inner-canvas parallel)))
          (unless parent-drawing-canvas
            (error "Unable to build drawing without an already drawn parent."))
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
(defclass scxml-drawable-synthetic-transition (scxml-synthetic-drawing scxml-transition)
  ((_hint-key :allocation :class
              ;; TODO- this sholud have a type ot restrict it to being specificiall 'synth-transition.
              :initform synth-transition))
  :documentation "Note there is no scxml-build-drawing function
  for a transition as they are all build as a collection, not
  individually.")
(cl-defmethod scxml--set-drawing-invalid ((transition scxml-drawable-transition) is-invalid)
  "Note that when a transition goes from hinted to unhinted it cause other transitions to become Invalid.

If there are two transitions touching a single state and one of
them goes from automatic to hinted, it could cause the other
transition to shuffle connector points."
  ;; TODO - this needs to invalidate properly.  It'll have to invalidate all transitions
  ;; which touch any states which are touched by any transitions which touch any states
  ;; which .... possibly all transitions?
  (cl-call-next-method transition is-invalid)
  (when (and is-invalid)
    (let ((touched-states (list (scxml-source transition)
                                (scxml-target transition))))
      ;; (scxml-visit-all transition
      ;;                  (lambda (other-transition)
      ;;                    (scxml--set-drawing-invalid other-transition t))
      ;;                  (lambda (element)
      ;;                    (and (object-of-class-p element 'scxml-transition)
      ;;                         (or (member (scxml-source element)
      ;;                                     touched-states)
      ;;                             (member (scxml-target element)
      ;;                                     touched-states)))))
      )))
(cl-defmethod scxml-build-drawing ((transition scxml-drawable-transition) (canvas scxml-canvas))
  "Warning method: transitions/arrows are not build individually, they're built as a group."
  (error "Invalid operation for an scxml-transition type element"))

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
