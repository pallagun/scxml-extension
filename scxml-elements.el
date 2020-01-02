;;; scxml-elements --- scxml element objects -*- lexical-binding: t -*-

;;; Commentary:
;; Concrete and abstract scxml-element classes.  These classes represent elements that might be found in an scxml document

;;; Code:
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
  (format "scxml(name:%s, %s)"
          (scxml-element-name scxml)
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
  (format "state(%s)" (cl-call-next-method)))
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
  (format "initial(%s)" (cl-call-next-method)))

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
  (format "parallel(%s)" (cl-call-next-method)))
(cl-defmethod scxml-xml-attributes ((element scxml-parallel))
  "attributes: id, initial"
  (append
   (list (cons 'id (scxml-element-id element)))
   (cl-call-next-method)))

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
