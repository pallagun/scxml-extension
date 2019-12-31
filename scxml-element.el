;;; scxml-element --- scxml element level functions -*- lexical-binding: t -*-

;;; Commentary:
;; scxml elements

;;; Code:

;; The scxml-element is used as the base class for any <element
;; with="attributes">or child</element> in a valid <scxml> document.
(defclass scxml-element ()
  ((attributes :initarg :attributes
               :accessor scxml-element-attributes
               :initform nil
               :type (or hash-table null))
   ;; _children and _parent are both "private" slots.
   (_children :initform nil
              :type (or list null))
   (_parent :initform nil
            :type (or null scxml-element)))
  :abstract 't)

(cl-defgeneric scxml-print ((element scxml-element))
  "Return a string representing ELEMENT for human eyes"
  (with-slots (_parent) element
    (format "parent:%s children:%s attributes:[%s]"
            (and _parent (scxml-xml-element-name _parent))
            (scxml-num-children element)
            (let ((hash (scxml-element-attributes element)))
              (when hash
                (let ((parts 'nil))
                  (maphash (lambda (k v) (push (format "%s=%.10s" k v) parts)) hash)
                  (mapconcat 'identity parts ", ")))))))
(cl-defmethod cl-print-object ((object scxml-element) stream)
  "Pretty print the OBJECT to STREAM."
  (princ (scxml-print object) stream))
(cl-defmethod scxml-xml-element-name ((element scxml-element))
  "return what the xml element name would be for this ELEMENT.

Doesn't check to ensure the ELEMENT is actually valid for rendering to xml.
Assumes everyone follows a nice naming scheme."
  (substring
   (symbol-name (eieio-object-class element))
   (length "scxml-")))
(cl-defmethod scxml-xml-attributes ((element scxml-element))
  "Return an alist of ELEMENT's attributes for XML rendering."
  (let ((attribs nil))
    (scxml-map-attrib element
                      (lambda (key val)
                        (when (scxml---visible-xml-attribute-name key)
                          (push (cons key val) attribs))))
    attribs))
(defun scxml---visible-xml-attribute-name (key)
  "Return non-nil if KEY (a symbol) is an attribute that should be put in XML.

Generally filters out symbols that start with 'scxml---'."
  ;; TODO - this 'scxml---' prefix should be a constant somewhere.
  (let ((prop-name (downcase (symbol-name key))))
    (or (< (length prop-name) (1+ (length "scxml---")))
        (not (equal (substring prop-name 0 8) "scxml---")))))
(cl-defmethod scxml-xml-string ((element scxml-element))
  "Get a string holding the XML for ELEMENT and any/all children."
  (let ((xml-name (scxml-xml-element-name element))
        (children (scxml-children element))
        (attribute-list (mapcar (lambda (name-value)
                                  (format "%s=\"%s\"" (car name-value) (cdr name-value)))
                                (seq-filter 'cdr
                                        (scxml-xml-attributes element)))))
    (let ((start-tag (format "<%s" xml-name))
          (attribute-string (if attribute-list
                                (format " %s" (mapconcat 'identity attribute-list " "))
                              ""))
          (start-tag-ending (if children ">" " />"))
          (children-xml (mapconcat 'scxml-xml-string children ""))
          (end-tag (when children (format "</%s>" xml-name))))
      (mapconcat 'identity
                 (list start-tag
                       attribute-string
                       start-tag-ending
                       children-xml
                       end-tag)
                 ""))))
(cl-defmethod scxml-children ((element scxml-element))
  "Return the children of ELEMENT."
  (oref element _children))
(cl-defmethod scxml-num-children ((element scxml-element))
  "Return the number of child elements for ELEMENT."
  (length (oref element _children)))
(cl-defmethod scxml-parent ((element scxml-element))
  "Return the parent of ELEMENT."
  (oref element _parent))
(cl-defmethod scxml-make-orphan ((element scxml-element))
  "Break ELEMENT away from any parent elements."
  (with-slots (_parent) element
    ;; element has a parent, break the link from parent->child
    (when _parent
      (oset _parent _children
            (cl-remove-if (lambda (child) (eq child element))
                          (scxml-children _parent)
                          :count 1)))
    (setf _parent 'nil)))
(cl-defgeneric scxml-add-child ((parent scxml-element) (new-child scxml-element) &optional append)
  "Make NEW-CHILD a child element of PARENT, returning PARENT.

When APPEND is non-nil NEW-CHILD will become the last child.  When APPEND is nil NEW-CHILD will become the first child.")
(cl-defmethod scxml-add-child ((parent scxml-element) (new-child scxml-element) &optional append)
  "Make NEW-CHILD a child element of PARENT, returning PARENT.

When APPEND is non-nil NEW-CHILD will become the last child.  When APPEND is nil NEW-CHILD will become the first child."
  (scxml-make-orphan new-child)   ;; make sure new-child isn't connected someplace else.
  (oset new-child _parent parent)
  (if append
      (oset parent _children (nconc (oref parent _children) (list new-child)))
    (push new-child (oref parent _children)))
  parent)
(cl-defmethod scxml-add-children ((parent scxml-element) &rest new-children)
  "Make NEW-CHILDREN into child elements of PARENT, returning PARENT."
  (mapc (lambda (child) (scxml-add-child parent child))
        (reverse new-children))
  parent)
(defun scxml---ensure-attributes (element)
  "If ELEMENT doesn't have an attribute hash table, put on there."
  (when (null (scxml-element-attributes element))
    (setf (scxml-element-attributes element)
          (make-hash-table :size 10 :test 'equal))))
(cl-defmethod scxml-map-attrib ((element scxml-element) function)
  "Map over attributes in ELEMENT calling ('FUNCTION key value).

Return is unspecified."
  (scxml---ensure-attributes element)
  (maphash function (oref element attributes)))
(cl-defgeneric scxml-put-attrib ((element scxml-element) key value)
  "Put VALUE into ELEMENT's attributes with a name of KEY.

Return is unspecified.")
(cl-defmethod scxml-put-attrib ((element scxml-element) key value)
  "Put VALUE into ELEMENT's attributes with a name of KEY.

Return is unspecified."
  (scxml---ensure-attributes element)
  (puthash key value (oref element attributes)))
(cl-defmethod scxml-get-attrib ((element scxml-element) key &optional default)
  "Return ELEMENT's attribute value for KEY, defaulting to DEFAULT"
  (if (scxml-element-attributes element)
      (gethash key (oref element attributes) default)
    default))
(cl-defmethod scxml-root-element ((element scxml-element))
  "Given any ELEMENT in an scxml-element tree, find the root of the tree."
  (let ((last element)
        (parent (scxml-parent element)))
    (while parent
      (setq last parent)
      (setq parent (scxml-parent last)))
    last))
(cl-defmethod scxml-visit ((element scxml-element) visitor &optional filter)
  "Visit all children of ELEMENT with VISITOR and optionally FILTER first.

Visitation starts at element and descends per child.  Other than
visiting ELEMENT first the order of visitation is undefined.

VISITOR must be of form (lambda (element) ...)
FILTER must be of form (lambda (element) ...)

Return value is undefined."
  (if (or (not filter) (funcall filter element))
      (funcall visitor element))
  (mapc (lambda (child)
          (scxml-visit child visitor filter))
        (scxml-children element)))
(cl-defmethod scxml-visit-all ((element scxml-element) visitor &optional filter)
  "Visit all elements (parent or child, recursively) starting at the root element"
  (scxml-visit (scxml-root-element element) visitor filter))
(cl-defmethod scxml-collect ((element scxml-element) filter)
  "Return a list of ELEMENT and ELEMENT's children filtered by FILTER.

May include ELEMENT or children at any depth if they satisfy
FILTER."
  (let ((matches nil))
    (scxml-visit element
                 (lambda (x) (push x matches))
                 filter)
    matches))
(cl-defmethod scxml-find-nearest-mutual-parent ((A scxml-element) (B scxml-element))
  "Given elements A and B return their closest mutual parent element."
  (cl-labels ((build-parent-list
               (element)
               (if element
                   (let ((parent (scxml-parent element)))
                     (cons element (build-parent-list parent)))
                 'nil)))
    (let ((A-parents (build-parent-list A))
          (B-parents (build-parent-list B)))
      (block scxml-find-parent
        (mapc (lambda (A-element)
                (when (cl-member A-element B-parents :test 'eq)
                  (return-from scxml-find-parent A-element)))
              A-parents)))))

;; not actually an element, but needs to be here
;; for code structure sanity reasons.
;; TODO - don't make all elements drawable.  Have it
;; as a mixin type thing in the future.
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

(defclass scxml-element-with-id ()
  ((id :initarg :id
       :accessor scxml-element-id
       :initform nil
       :type (or string null)))
  :abstract t
  :documentation "Apply to an scxml element if it's capable of holding an 'id' child")
(cl-defmethod scxml-print ((idable-element scxml-element-with-id))
  (format "id:%s, %s"
          (scxml-element-id idable-element)
          (cl-call-next-method)))
(cl-defmethod scxml-element-find-by-id ((search-root scxml-element) (id-to-find string))
  "Find element with ID-TO-FIND starting at SEARCH-ROOT and going down.

Function will not traverse the whole tree, only the portion at or
below SEARCH-ROOT"
  ;; TODO - this implementation might not be the best.
  (if (and (object-of-class-p search-root 'scxml-element-with-id)
           (string-equal (scxml-element-id search-root) id-to-find))
      search-root
    (let ((children (scxml-children search-root)))
      (if children
          (cl-reduce (lambda (accumulator child)
                    (if accumulator accumulator
                      (scxml-element-find-by-id child id-to-find)))
                  children
                  :initial-value 'nil)
        'nil))))

(defclass scxml-element-with-initial ()
  ((initial :initarg :initial
            :accessor scxml-element-initial
            :initform nil
            :type (or string null)))
  :abstract t
  :documentation "Apply to an scxml element if it's capable of holding an 'initial' xml attribute.")

(defclass scxml-scxml (scxml-drawable-element scxml-element-with-initial)
  ((name :initarg :name
         :accessor scxml-name
         :type (or string null)
         :initform nil))
  :documentation "The main <scxml /> element.

Recognized attributes: initial, name, xmlns, version, datamodel,
binding")
(cl-defmethod scxml-print ((scxml scxml-scxml))
  "Pretty print SCXML for human eyeballs."
  (format "scxml(name:%s, %s)"
          (scxml-name scxml)
          (cl-call-next-method)))
(defun scxml---scxml-factory (attrib-alist)
  "Build an scxml-scxml element from the ATTRIBUTES alist."
  (let ((default-attribs (list (cons 'xmlns "http://www.w3.org/2005/07/scxml")
                               (cons 'version "1.0")))
        (element (scxml-scxml :initial (alist-get 'initial attrib-alist))))
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

(defclass scxml-state-type (scxml-drawable-element scxml-element-with-id)
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
(defun scxml---state-factory (attrib-alist)
  "Build an scxml-state element from the ATTRIBUTES alist."
  (let ((element (scxml-state :id (alist-get 'id attrib-alist)
                              :initial (alist-get 'initial attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(id initial))))

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
(defun scxml---final-factory (attrib-alist)
  "Build an scxml-final element from the ATTRIBUTES alist."
  (let ((element (scxml-final :id (alist-get 'id attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(id))))

(defclass scxml-initial (scxml-drawable-element)
  ()
  :documentation "Scxml <initial> element.
No attributes required.
No attributes recognized.
Must contain a single child <transition> element indicating initial state.
Child <transition> element may not have 'cond' or 'event' attributes and must be a valid state.")
(cl-defmethod scxml-print ((initial scxml-initial))
  "Spit out a string representing ELEMENT for human eyeballs"
  (format "initial(%s)" (cl-call-next-method)))
(defun scxml---initial-factory (&optional attrib-alist)
  "Build an scxml-initial element from the ATTRIBUTES alist."
  (let ((element (scxml-initial)))
    (scxml---append-extra-properties element attrib-alist)))

(defclass scxml-parallel (scxml-drawable-element scxml-element-with-id)
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
(defun scxml---parallel-factory (attrib-alist)
  "Build an scxml-parallel element from the ATTRIBUTES alist."
  (let ((element (scxml-parallel :id (alist-get 'id attrib-alist))))
    (scxml---append-extra-properties element attrib-alist '(id initial))))

(defclass scxml-transition (scxml-drawable-element)
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
                     (and (scxml-transition-p other)
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
  (let ((element (scxml-transition :target (alist-get 'target attrib-alist))))
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

(provide 'scxml-element)
;;; scxml-element.el ends here
