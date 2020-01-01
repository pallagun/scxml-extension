;; The non-abstract scxml objects representing scxml elements in xml

(require 'scxml-element)

;; This is not actually an element, but needs to be here for code
;; structure sanity reasons.
;; TODO - don't make all elements drawable.
;; Have it as a mixin type thing in the future.
(defclass scxml-drawing ()
  ((highlight :initarg :highlight
              :accessor scxml-drawing-highlight)
   (edit-idx :initarg :edit-idx
             :accessor scxml-drawing-edit-idx
             :type (or null integer)
             :documentation "If edit mode is on this will be non-nil and hold the index of the current edit point")
   (locked :initarg :locked
           :accessor scxml-drawing-locked
           :initform nil
           :documentation "Is this drawing locked in place by a user hint or not")
   (parent :initarg :parent
           :accessor scxml-parent
           :type scxml-drawable-element))
  :abstract t
  :documentation "This is a thing which can be rendered on a canvas.  A rectangle, an arrow, etc.")
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


(defclass scxml-scxml (scxml-element scxml-element-with-initial)
  ((name :initarg :name
         :accessor scxml-element-name
         :initform nil
         :type (or string null)))
  :documentation "The main <scxml /> element.

Recognized attributes: initial, name, datamodel, binding
Locked attributes: xmlns, version,")
(defclass scxml-drawable-scxml (scxml-scxml scxml-drawable-element)
  ())
(cl-defmethod scxml-print ((scxml scxml-scxml))
  "Pretty print SCXML for human eyeballs."
  (format "scxml(name:%s, %s)"
          (scxml-element-name scxml)
          (cl-call-next-method)))
(defun scxml---scxml-factory (attrib-alist)
  "Build an scxml-scxml element from the ATTRIBUTES alist."
  (let ((default-attribs (list (cons 'xmlns "http://www.w3.org/2005/07/scxml")
                               (cons 'version "1.0")))
        (element (scxml-drawable-scxml :initial (alist-get 'initial attrib-alist))))
    (mapc (lambda (attrib)
            (unless (assoc (car attrib) attrib-alist)
              (push attrib attrib-alist)))
          default-attribs)
    (scxml---append-extra-properties element attrib-alist '(id initial))))
(cl-defmethod scxml-xml-attributes ((element scxml-scxml))
  "attributes: initial, name, xmlns, version, datamodel, binding.

Only doing xmlnns and version here."
  (let ((attributes (list ;; (cons 'xmlns "http://www.w3.org/2005/07/scxml")
                          ;; (cons 'version "1.0")
                          (cons 'name (scxml-element-name element))
                          (cons 'initial (scxml-element-initial element)))))
    (append attributes
            (cl-call-next-method))))

(defclass scxml-state-type (scxml-element scxml-element-with-id)
  ()
  :abstract t
  :documentation "Abstract parent class for <state> and <final>, both of which are state-ish")

(defclass scxml-state (scxml-state-type scxml-element-with-initial)
  ()
  :documentation "Scxml <state> element.
Recognized attributes: id, initial")
(defclass scxml-drawable-state (scxml-state scxml-drawable-element)
  ())
(cl-defmethod scxml-print ((state scxml-state))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "state(%s)" (cl-call-next-method)))
(cl-defmethod scxml-xml-attributes ((element scxml-state))
  "attributes: id, initial"
  (append
   (list (cons 'id (scxml-element-id element))
         (cons 'initial (scxml-element-initial element)))
   (cl-call-next-method)))
(defun scxml---state-factory (attrib-alist)
  "Build an scxml-state element from the ATTRIBUTES alist."
  (let ((element (scxml-drawable-state :id (alist-get 'id attrib-alist)
                              :initial (alist-get 'initial attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(id initial))))

(defclass scxml-final (scxml-state-type)
  ()
  :documentation "Scxml <final> element.
Recognized attributes: id
Children:
  <onentry>, <onexit>, <donedata>"
  )
(defclass scxml-drawable-final (scxml-final scxml-drawable-element)
  ())
(cl-defmethod scxml-xml-attributes ((element scxml-final))
  "attributes: id"
  (append
   (list (cons 'id (scxml-element-id element)))
   (cl-call-next-method)))
(defun scxml---final-factory (attrib-alist)
  "Build an scxml-final element from the ATTRIBUTES alist."
  (let ((element (scxml-drawable-final :id (alist-get 'id attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(id))))

(defclass scxml-initial (scxml-element)
  ()
  :documentation "Scxml <initial> element.
No attributes required.
No attributes recognized.
Must contain a single child <transition> element indicating initial state.
Child <transition> element may not have 'cond' or 'event' attributes and must be a valid state.")
(defclass scxml-drawable-initial (scxml-initial scxml-drawable-element)
  ())
(cl-defmethod scxml-print ((initial scxml-initial))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "initial(%s)" (cl-call-next-method)))
(defun scxml---initial-factory (&optional attrib-alist)
  "Build an scxml-initial element from the ATTRIBUTES alist."
  (let ((element (scxml-drawable-initial)))
    (scxml---append-extra-properties element attrib-alist)))

(defclass scxml-parallel (scxml-element scxml-element-with-id)
  ()
  ;; TODO - should this inherit from scxml-state-type - yes, probably.
  :documentation "Scxml <parallel> element.
Recognized attributes: id
No attrubtes required.
Children:
  <onentry>, <onexit>, <transition>, <start>, <parallel>, <history>, <datamodel>, <invoke>")
(defclass scxml-drawable-parallel (scxml-parallel scxml-drawable-element)
  ())
(cl-defmethod scxml-print ((parallel scxml-parallel))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "parallel(%s)" (cl-call-next-method)))
(cl-defmethod scxml-xml-attributes ((element scxml-parallel))
  "attributes: id, initial"
  (append
   (list (cons 'id (scxml-element-id element)))
   (cl-call-next-method)))
(defun scxml---parallel-factory (attrib-alist)
  "Build an scxml-parallel element from the ATTRIBUTES alist."
  (let ((element (scxml-drawable-parallel :id (alist-get 'id attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(id initial))))

(defclass scxml-transition (scxml-element)
  ((target :initarg :target
           :accessor scxml-target-id
           :type string
           :documentation "This is actually the target ID value from scxml, not the target scxml-element"))
  :documentation "Scxml <transition> element.

No attributes are required.
Recognized attributes: event, cond, target, type
  (note: one of 'event', 'cond' or 'target' must be present)
Children must be executable content.")
(defclass scxml-drawable-transition (scxml-transition scxml-drawable-element)
  ())
(cl-defmethod scxml-print ((transition scxml-transition))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "transition(targetId:%s, %s)"
          (scxml-target-id transition)
          (cl-call-next-method)))
(cl-defmethod scxml-xml-attributes ((element scxml-transition))
  "attributes: target"
  (append (list (cons 'target (scxml-target-id element)))
          (cl-call-next-method)))

(cl-defmethod scxml-get-all-transitions-to ((element scxml-state-type))
  "Collect all transition elements which target STATE"
  (let ((target-id (scxml-element-id element)))
    (scxml-collect (scxml-root-element element)
                   (lambda (other)
                     (and (object-of-class-p other 'scxml-transition)
                          (equal target-id (scxml-target-id other)))))))
(cl-defmethod scxml-target ((transition scxml-transition))
  "Return the target element for TRANSITIONs target."
  (scxml-element-find-by-id (scxml-root-element transition)
                            (scxml-target-id transition)))
(cl-defmethod scxml-source ((transition scxml-transition))
  "Return the source element for TRANSITION."
  (scxml-parent transition))
(defun scxml---transition-factory (attrib-alist)
  "Build an scxml-transitione element from the ATTRIBUTES alist."
  (let ((element (scxml-drawable-transition :target (alist-get 'target attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(target))))

(defclass scxml-onentry ()
  ()
  ;; TODO - Implementation
  :documentation "Scxml <onentry> element.

No attributes are required.
No attributes are recognized.
Children must be executable content.")
(defclass scxml-onexit ()
  ()
  ;; TODO - Implementation
  :documentation "Scxml <onexit> element.

No attributes are required.
No attributes are recognized.
Children must be executable content.")
(defclass scxml-history ()
  ()
  ;; TODO - Implementation
  :documentation "Scxml <history> element.

No attributes are required.
Attributes recognized: id, type['shallow' or 'deep', default: 'shallow']
Children: must contain exactly one unconditional <transition>
indicating default history.")

(defconst scxml--default-factories
  '((scxml . scxml---scxml-factory)
    (state . scxml---state-factory)
    (transition . scxml---transition-factory)
    (parallel . scxml---parallel-factory)
    (final . scxml---final-factory)
    (initial . scxml---initial-factory))
  "Default methods to create concrete scxml document elements
  based on their type (as a symbol).")

(provide 'scxml-elements)
