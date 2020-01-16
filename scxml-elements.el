;;; scxml-elements --- scxml element objects -*- lexical-binding: t -*-

;;; Commentary:
;; Concrete and abstract scxml-element classes.  These classes represent elements that might be found in an scxml document

;;; Code:
(require 'eieio)
(require 'seq)
(require 'scxml-element)

(defclass scxml-scxml (scxml-element scxml-element-with-initial)
  ((name :initarg :name
         :accessor scxml-element-name
         :initform nil
         :type (or string null)))
  :documentation "The main <scxml /> element.

Recognized attributes: initial, name, datamodel, binding
Locked attributes: xmlns, version,")
(cl-defmethod scxml-print ((scxml scxml-scxml))
  "Pretty print SCXML for human eyeballs."
  (format "scxml(name:%s, initial:%s, %s)"
          (scxml-element-name scxml)
          (scxml-element-initial scxml)
          (cl-call-next-method)))
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
(cl-defmethod scxml-print ((state scxml-state))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "state(id: %s, %s)"
          (scxml-element-id state)
          (cl-call-next-method state)))
(cl-defmethod scxml-xml-attributes ((element scxml-state))
  "attributes: id, initial"
  (append
   (list (cons 'id (scxml-element-id element))
         (cons 'initial (scxml-element-initial element)))
   (cl-call-next-method)))

(defclass scxml-final (scxml-state-type)
  ()
  :documentation "Scxml <final> element.
Recognized attributes: id
Children:
  <onentry>, <onexit>, <donedata>"
  )
(cl-defmethod scxml-xml-attributes ((element scxml-final))
  "attributes: id"
  (append
   (list (cons 'id (scxml-element-id element)))
   (cl-call-next-method)))

(defclass scxml-initial (scxml-element)
  ()
  :documentation "Scxml <initial> element.
No attributes required.
No attributes recognized.
Must contain a single child <transition> element indicating initial state.
Child <transition> element may not have 'cond' or 'event' attributes and must be a valid state.")
(cl-defmethod scxml-print ((initial scxml-initial))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "initial(%s)" (cl-call-next-method initial)))
(cl-defmethod scxml-add-child :before ((initial scxml-initial) (any-child scxml-element))
  "Ensure this child is valid"
  (when (not (object-of-class-p any-child 'scxml-transition))
    (error "The child of an <initial> element must be a transition"))
  (when (null (scxml-target-id any-child))
    (error "The child on an <initial> element must be a <transition> with the id set"))
  (when (> (scxml-num-children initial) 0)
    (error "An <initial> element may only have a single child element")))

(defclass scxml-parallel (scxml-element scxml-element-with-id)
  ()
  ;; TODO - should this inherit from scxml-state-type - yes, probably.
  :documentation "Scxml <parallel> element.
Recognized attributes: id
No attrubtes required.
Children:
  <onentry>, <onexit>, <transition>, <start>, <parallel>, <history>, <datamodel>, <invoke>")
(cl-defmethod scxml-print ((parallel scxml-parallel))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "parallel(%s)" (cl-call-next-method parallel)))
(cl-defmethod scxml-xml-attributes ((element scxml-parallel))
  "attributes: id, initial"
  (append
   (list (cons 'id (scxml-element-id element)))
   (cl-call-next-method)))

(defclass scxml-transition (scxml-element)
  ((target :initarg :target
           :accessor scxml-target-id
           :type (or null string)
           :initform nil
           :documentation "Attribute: \"target\".  This is actually the target ID value from scxml, not the target scxml-element")
   (events :initarg :events
           :accessor scxml-events
           :type (or null list)
           :initform nil
           :documentation "Attribute: \"event\".  A list of event discriptors though in xml it is a space separated string.")
   (cond-expr :initarg :cond
              :accessor scxml-cond-expr
              :type (or null string)
              :initform nil
              :documentation "Attribute: \"cond\". Condition for the transition which must evaluate to a boolean.")
   ;; TODO - 'type'
   )
  :documentation "Scxml <transition> element.

No attributes are required.
Recognized attributes: event, cond, target, type
  (note: one of 'event', 'cond' or 'target' must be present)
Children must be executable content.")
(cl-defmethod scxml-print ((transition scxml-transition))
  "Spit out a string representing ELEMENT for human eyeballs"
  (with-slots (target events cond-expr) transition
    (format "transition(targetId:%s, event:%s, cond:%s, %s)"
            target
            events
            cond-expr
            (cl-call-next-method))))
(cl-defmethod scxml-xml-attributes ((element scxml-transition))
  "attributes: target"
  (append (seq-filter (lambda (key-value) (cdr key-value))
                      (list (cons 'target (scxml-target-id element))
                            (cons 'event (mapconcat #'identity (scxml-events element) " "))
                            (cons 'cond-expr (scxml-cond-expr element))))
          (cl-call-next-method)))
(cl-defmethod scxml-get-all-transitions-to ((element scxml-element-with-id))
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

;; interactions
(cl-defmethod scxml-add-child :before ((parent scxml-element) (child scxml-element) &optional append)
  "Add CHILD element to PARENT ensuring id attribute is unique in the entire tree.

Validate based on element id attribute - the ids must not be duplicated."
  ;; if the child is an scxml-element-with-initial
  (let* ((new-idables (scxml-collect child
                                     (lambda (e)
                                       (object-of-class-p e 'scxml-element-with-id))))
         (new-ids (filter #'identity (mapcar 'scxml-element-id new-idables))))
    (scxml-visit-all parent
                     (lambda (idable-element)
                       (let ((id (scxml-element-id idable-element)))
                         (when (and id
                                    (member id new-ids))
                           (error "Added child (or decendent) has a conflicting id: \"%s\"" id))))
                     (lambda (element)
                       (object-of-class-p element 'scxml-element-with-id)))))
(cl-defmethod scxml-add-child :before ((parent scxml-element) (initial scxml-initial))
  "Ensure it's valid to add an scxml-initial to this state"
  ;; <initial> elements are only allowed to be added if this state has
  ;; child states. additionally if an <initial> element is added it must
  ;; already have a valid target.
  (when (not (object-of-class-p parent 'scxml-state))
    (error "<initial> elements are only valid as children of a <state>"))

  (cl-flet ((validate-and-retrieve-initial-target
             (lambda (initial-element)
               (let ((children (scxml-children initial-element)))
                 (when (not (= (length children) 1))
                   (error "<initial> elements must have exactly one child, a <transition>"))
                 (let ((transition (first children)))
                   (when (not (object-of-class-p transition 'scxml-transition))
                     (error "<initial> elements must have exactly one child, a <transition>"))
                   (let ((target-id (scxml-target-id transition)))
                     (when (<= (length target-id) 0)
                       (error "<initial> elements must have a child <transition> with a valid target"))
                     target-id))))))
    (let ((initial-target (validate-and-retrieve-initial-target initial))
          (found-target-element))
      (cl-loop for sibling in (scxml-children parent)
               when (object-of-class-p sibling 'scxml-initial)
                 do (error "An element may only have a single <initial> child")
               when (and (object-of-class-p sibling 'scxml-element-with-id)
                         (equal (scxml-element-id sibling) initial-target))
                 do (setq found-target-element t))
      (when (not found-target-element)
        (error "<initial> elements must have a <transition> which targets a sibling")))))

(defun scxml--element-factory (type attrib-alist &optional skip-slots)
  "Build a childless element by TYPE and their ATTRIB-ALIST.

Optionally, if the slot name is in skip-slots (as a symbol) then
forcefully put it in t he element's attribute hash table, not in
the slot (even if a proper slot is found.

Does not build recursively."
  (unless (symbolp type)
    (error "Type must be a symbol"))
  (let* ((class-name (format "scxml-%s" (symbol-name type)))
         (class (intern-soft class-name))
         (slots (eieio-class-slots class))
         (slot-names (mapcar (lambda (slot)
                               ;; TODO - probably shouldn't use a cl--* function.
                               (let ((slot-symbol (cl--slot-descriptor-name slot)))
                                 (symbol-name slot-symbol)))
                             slots))
         (attribute-slots (seq-filter
                           (lambda (slot-name)
                             (not (eq (aref slot-name 0) (aref "_" 0))))
                           slot-names))
         (attribute-slot-symbols (mapcar 'intern attribute-slots)))
    ;; TODO - this will only work if the slot name and the eieio
    ;; initarg are the same.
    ;; split up everything in attrib-list
    (let ((constructor-params nil)
          (attribute-params nil))
      (cl-loop for cell in attrib-alist
               for attrib-name-symbol = (car cell)
               when (and (memq attrib-name-symbol attribute-slot-symbols)
                         (not (memq attrib-name-symbol skip-slots)))
               do (let ((initarg-sym (intern
                                      (format ":%s"
                                              (symbol-name attrib-name-symbol)))))
                    (setq constructor-params
                          (plist-put constructor-params initarg-sym (cdr cell))))
               else
               when (scxml---visible-xml-attribute-name attrib-name-symbol)
               do (push cell attribute-params))
      (let ((element (apply class constructor-params)))
        (mapc (lambda (cell)
                (scxml-put-attrib element (car cell) (cdr cell)))
              attribute-params)
        ;; TODO - THIS PART IS A HACK!
        (when (object-of-class-p element 'scxml-drawable-element)
          (scxml--set-hint-from-attrib-list element attrib-alist))
        element))))

(provide 'scxml-elements)
;;; scxml-elements.el ends here
