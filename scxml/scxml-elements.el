;;; scxml-elements --- scxml element objects -*- lexical-binding: t -*-

;;; Commentary:
;; Concrete and abstract scxml-element classes.  These classes represent elements that might be found in an scxml document

;;; Code:
(require 'eieio)
(require 'seq)
(require 'scxml-element-core)
(require 'scxml-element)

(defclass scxml-scxml (scxml--core-scxml scxml-element scxml-element-with-initial)
  ((name :initarg :name
         :accessor scxml-element-name
         :initform nil
         :type (or string null))
   (datamodel :initarg :datamodel
              :accessor scxml-datamodel-type
              :type (or string null))
   (binding :initarg :binding
            :accessor scxml-binding
            :initform nil            ;must be one of 'early or 'late.  defaults to early.
            :type (or symbol null)))
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
(defclass scxml-state (scxml--core-state scxml-state-type scxml-element-with-initial)
  ()
  :documentation "Scxml <state> element.
Recognized attributes: id, initial
Children:
  <onentry>, <onexit>, <transition>, <initial>, <state>, <parallel>")
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

(defclass scxml-final (scxml--core-final scxml-state-type)
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

(defclass scxml-initial (scxml--core-initial scxml-element)
  ()
  :documentation "Scxml <initial> element.
No attributes required.
No attributes recognized.
Must contain a single child <transition> element indicating initial state.
Child <transition> element may not have 'cond' or 'event' attributes and must be a valid state.")
(defun scxml-initial-class-p (any)
  "Equivalent of (object-of-class-p ANY-OBJECT 'scxml-initial)"
  ;; todo - make this a defsubst
  (object-of-class-p any 'scxml-initial))
(cl-defmethod scxml-print ((initial scxml-initial))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "initial(%s)" (cl-call-next-method initial)))

(defclass scxml-parallel (scxml--core-parallel scxml-element scxml-element-with-id)
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

(defclass scxml-transition (scxml--core-transition scxml-element)
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
   (type :initarg :type
         :accessor scxml-type
         :initform nil
         :type (or null string)
         :documentation "Attribute: \"type\".  Transition type to determine if the parent state is exited when transitioning to a child state.  Must be one of 'external or 'internal"
         ;; todo - setf for this slot to protect against incorrect value.
         ))
  :documentation "Scxml <transition> element.

No attributes are required.
Recognized attributes: event, cond, target, type
  (note: one of 'event', 'cond' or 'target' must be present)
Children must be executable content.")
(defun scxml-transition-class-p (any)
  "Equivalent of (object-of-class-p ANY-OBJECT 'scxml-transition)"
  ;; todo - make this a defsubst
  (object-of-class-p any 'scxml-transition))
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
  (with-slots (target events cond-expr type) element
    (let ((stringified-events (if events
                                  (mapconcat #'identity events " ")
                                nil)))
    (append (seq-filter #'cdr
                        `((target . ,target)
                          (event . ,stringified-events)
                          (cond . ,cond-expr)
                          (type . ,type)))
          (cl-call-next-method)))))
(cl-defmethod scxml-get-all-transitions-to ((element scxml-element-with-id))
  "Collect all transition elements which target STATE"
  (let ((target-id (scxml-element-id element)))
    (scxml-collect (scxml-root-element element)
                   (lambda (other)
                     (and (object-of-class-p other 'scxml-transition)
                          (equal target-id (scxml-target-id other)))))))
(cl-defmethod scxml-target ((transition scxml-transition))
  "Return the target element for TRANSITIONs target."
  ;; TODO - should this null coalescing be done here? it's null ?? ""
  (with-slots ((target-id target)) transition
    (if target-id
        (scxml-element-find-by-id (scxml-root-element transition) target-id)
      nil)))
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

;; validation functions
(cl-defmethod scxml--validate-add-child ((parent scxml-element) (child scxml-element))
  "Validate addition of CHILD to PARENT throwing an error when invalid.

The starting assumption is that PARENT belongs to an scxml
document that is already known to be valid."
  (scxml--validate-child-given-parent parent child))
(cl-defmethod scxml--validate ((element scxml-element))
  "Validate the structure of ELEMENT and all child elements throwing an error when invalid."
  (scxml--validate-child-given-parent nil element))
(defun scxml--validate-parent-child-types (parent child)
  "Throw an error if CHILD is not a valid element type for PARENT."
  (let ((parent-core-type (scxml--core-type parent))
        (child-core-type (scxml--core-type child)))
    (unless (memq child-core-type
                  (alist-get parent-core-type
                             scxml--valid-child-types
                             nil))
      (error "Invalid child of type <%s> for parent of type <%s>"
             child-core-type
             parent-core-type))))
(defun scxml--validate-child-given-parent (parent child)
  "Return non-nil if CHILD is a valid child of PARENT.

PARENT can be nil and child will be validated as the full
document.

When PARNET is non-nil the starting assumption is that PARENT
belongs to an scxml document that is already known to be valid."
  ;; ensure there are no id collisions.
  ;; ensure transition targets are valid state ids.
  ;; ensure scxml-initial elements have exactly one child that is a transition
  ;; - and that the child transition is a valid <initial> transition.
  ;; - and that the target of the initial's transition is a sibling of the initial.
  ;; ensure all elements which can have an <initial> child have 0 or 1 of them, not 2+

  ;; TODO - validate that an element does not have it's initial
  ;; attribute set at the same time as having an initial child
  ;; element.

  ;; TODO - there is a lot of duplicated code here for checking if an
  ;; element is an element which has an id and then if it is
  ;; extracting the id and then if that id is non-nil returning that
  ;; id.  Subfun that out
  (cl-labels ((get-all-non-nil-ids
               (starting-element)
               (seq-filter #'identity
                           (mapcar #'scxml-element-id
                                   (scxml-collect-all starting-element
                                                      #'scxml-element-with-id-class-p))))
              (get-all-transition-targets
               (starting-element)
               (seq-filter #'identity
                           (mapcar #'scxml-target-id
                                   (scxml-collect-all starting-element
                                                      (lambda (element)
                                                        (object-of-class-p element 'scxml-transition))))))
              (validate-initial-element
               (initial-element parent-element)
               ;; make sure the initial has a single child
               ;; make sure that single child is a transition
               ;; make sure that transition does not contain a cond or event
               ;; make sure that transition's target references a sibling of initial-element.
               ;; make sure that there is only one <initial> as a direct child of the parent
               (let* ((initial-children (scxml-children initial-element))
                      (initial-transition (first initial-children)))
                 (unless (eq 1 (length initial-children))
                   (error "An <initial> element must contain exactly 1 child, found %d children" (length initial-children)))
                 (unless (object-of-class-p initial-transition 'scxml-transition)
                   (error "An <initial> element must have a child of type <transition>, found %s" (eieio-object-class-name initial-transition)))
                 (when (scxml-events initial-transition)
                   (error "The <transition> in an <initial> element must not have any events, found events: %s" (scxml-events initial-transition)))
                 (when (scxml-cond-expr initial-transition)
                   (error "The <transition> in an <initial> element must not have a condition, found condition: %s" (scxml-cond-expr initial-transition)))
                 (let* ((initial-target-id (scxml-target-id initial-transition))
                        (siblings (seq-filter (lambda (sibling) (not (eq sibling initial-element)))
                                              (scxml-children parent-element)))
                        (sibling-ids (seq-filter #'identity
                                                 (mapcar #'scxml-element-id
                                                         (seq-filter #'scxml-element-with-id-class-p siblings)))))
                   (when (some #'scxml-initial-class-p siblings)
                     (error "An element may only have one <initial> child element, found more than one."))
                   (unless (some (lambda (sibling-id)
                                   (equal sibling-id initial-target-id))
                                 sibling-ids)
                     (error "The <transition> child of an <initial> must target a sibling of the <initial> (stated target: %s, valid targets: (%s))"
                            initial-target-id
                            (mapconcat #'identity sibling-ids " "))))))
              (validate-initial-attribute
               (element-with-initial)
               ;; one child must have the id indicated by element's initial attribute
               (let ((initial-state-id (scxml-element-initial element-with-initial)))
                 (when initial-state-id
                   (let ((child-ids (seq-filter #'identity
                                                (mapcar (lambda (child)
                                                          (when (scxml-element-with-id-class-p child)
                                                            (scxml-element-id child)))
                                                        (scxml-children element-with-initial)))))
                     (unless (member initial-state-id child-ids)
                       (error "Unsatisfied initial attribute of '%s'"
                              initial-state-id)))))))
    (let ((existing-ids (if parent
                            (get-all-non-nil-ids parent)
                          nil))
          (additional-ids (get-all-non-nil-ids child)))
      (let ((intersection-ids (intersection existing-ids additional-ids :test 'equal)))
        (when intersection-ids
          (error "Unable to add due to colliding 'id' attributes: %s"
                 (mapconcat #'identity intersection-ids ", "))))
      (let ((all-ids (union existing-ids additional-ids :test 'equal))
            (additional-transition-target-ids (get-all-transition-targets child)))
        (mapc (lambda (target-id)
                (unless (member target-id all-ids)
                  (error "Unable to add transition with unknown target state id: %s"
                         target-id)))
              additional-transition-target-ids))
      (scxml-visit-all child
                       #'validate-initial-attribute
                       (lambda (element)
                         (object-of-class-p element 'scxml-element-with-initial)))
      (scxml-visit-all child
                       (lambda (initial-element)
                         ;; If the initial element is the element to
                         ;; be added you'll need to get the parent
                         ;; from the existing document.  Otherwise you
                         ;; can simply trust the element's parent.
                         (if (eq initial-element child)
                             (validate-initial-element initial-element parent)
                           (validate-initial-element initial-element (scxml-parent initial-element))))
                       (lambda (element)
                         (object-of-class-p element 'scxml-initial)))
      (when parent
        (scxml--validate-parent-child-types parent child))
      (scxml-visit child
                   (lambda (sub-parent)
                     (mapc (lambda (sub-child)
                             (scxml--validate-parent-child-types sub-parent sub-child))
                           (scxml-children sub-parent))))))

  ;; (let* ((new-idables (scxml-collect child
  ;;                                    (lambda (e)
  ;;                                      (object-of-class-p e 'scxml-element-with-id))))
  ;;        (new-ids (seq-filter #'identity (mapcar 'scxml-element-id new-idables))))
  ;;   (scxml-visit-all parent
  ;;                    (lambda (idable-element)
  ;;                      (let ((id (scxml-element-id idable-element)))
  ;;                        (when (and id
  ;;                                   (member id new-ids))
  ;;                          (error "Added child (or decendent) has a conflicting id: \"%s\"" id))))
  ;;                    (lambda (element)
  ;;                      (object-of-class-p element 'scxml-element-with-id)))))
;; (cl-defmethod scxml-add-child :before ((parent scxml-element) (initial scxml-initial))
;;   "Ensure it's valid to add an scxml-initial to this state"
;;   ;; <initial> elements are only allowed to be added if this state has
;;   ;; child states. additionally if an <initial> element is added it must
;;   ;; already have a valid target.
;;   (when (not (object-of-class-p parent 'scxml-element-with-child-initial))
;;     (error "<initial> elements are only valid as children of a <state>"))

;;   (cl-flet ((validate-and-retrieve-initial-target
;;              (lambda (initial-element)
;;                (let ((children (scxml-children initial-element)))
;;                  (when (not (= (length children) 1))
;;                    (error "<initial> elements must have exactly one child, a <transition>"))
;;                  (let ((transition (first children)))
;;                    (when (not (object-of-class-p transition 'scxml-transition))
;;                      (error "<initial> elements must have exactly one child, a <transition>"))
;;                    (let ((target-id (scxml-target-id transition)))
;;                      (when (<= (length target-id) 0)
;;                        (error "<initial> elements must have a child <transition> with a valid target"))
;;                      target-id))))))
;;     (let ((initial-target (validate-and-retrieve-initial-target initial))
;;           (found-target-element))
;;       (cl-loop for sibling in (scxml-children parent)
;;                when (object-of-class-p sibling 'scxml-initial)
;;                  do (error "An element may only have a single <initial> child")
;;                when (and (object-of-class-p sibling 'scxml-element-with-id)
;;                          (equal (scxml-element-id sibling) initial-target))
;;                  do (setq found-target-element t))
;;       (when (not found-target-element)
;;         (error "<initial> elements must have a <transition> which targets a sibling")))))
)

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
        element))))

(provide 'scxml-elements)
;;; scxml-elements.el ends here
