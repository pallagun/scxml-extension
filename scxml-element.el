;;; scxml-element --- scxml element level functions -*- lexical-binding: t -*-

;;; Commentary:
;; The scxml-element is used as the base class for any <element
;; with="attributes">or child</element> in a valid <scxml> document.

;;; Code:
(require 'eieio)
(require 'seq)
(require 'cl-macs)

(defclass scxml-element ()
  ((_attributes :initarg :attributes
                ;; :accessor scxml-element-attributes
                :initform nil
                :type (or hash-table null))
   (_children :initform nil
              :type (or list null))
   (_parent :initform nil
            :type (or null scxml-element)))
  :abstract 't)
(cl-defgeneric scxml-print ((element scxml-element))
  "Return a string representing ELEMENT for human eyes"
  (with-slots (_attributes _parent) element
    (format "parent:%s children:%s attributes:[%s]"
            (and _parent (scxml-xml-element-name _parent))
            (scxml-num-children element)
            (let ((parts))
              (scxml-map-attrib element (lambda (k v)
                                          (push (format "%s=%.10s" k v) parts)))
              (mapconcat 'identity parts ", ")))))
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
(cl-defmethod scxml-xml-string ((element scxml-element) &optional exclude-children)
  "Get a string holding the XML for ELEMENT.

Normally all child elements will be rendered to xml and output as
well.  When EXCLUDE-CHILDREN is true then no child elements will
be included in the output."
  (let ((xml-name (scxml-xml-element-name element))
        (children (and (not exclude-children) (scxml-children element)))
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
  "Modify PARENT adding NEW-CHILD as a child returning PARENT.

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
  (when (null (oref element _attributes))
    (oset element _attributes
          (make-hash-table :size 10 :test 'equal))))
(cl-defgeneric scxml-map-attrib ((element scxml-element) function)
  "Map over attributes in ELEMENT calling ('FUNCTION key value).

Return is unspecified.")
(cl-defmethod scxml-map-attrib ((element scxml-element) function)
  "Map over attributes in ELEMENT calling ('FUNCTION key value).

Return is unspecified."
  (with-slots (_attributes) element
    (when _attributes
      (maphash function _attributes))))
(cl-defmethod scxml-num-attrib ((element scxml-element))
  "Return the number of attributes in ELEMENT's hashtable."
  (with-slots (_attributes) element
    (if _attributes
        (hash-table-count _attributes)
      0)))
(cl-defgeneric scxml-put-attrib ((element scxml-element) key value)
  "Put VALUE into ELEMENT's attributes with a name of KEY.

Return is unspecified.")
(cl-defmethod scxml-put-attrib ((element scxml-element) key value)
  "Put VALUE into ELEMENT's attributes with a name of KEY.

Return is unspecified."
  (scxml---ensure-attributes element)
  (puthash key value (oref element _attributes)))
(cl-defgeneric scxml-get-attrib ((element scxml-element) key &optional default)
  "Return ELEMENT's attribute value for KEY, defaulting to DEFAULT")
(cl-defmethod scxml-get-attrib ((element scxml-element) key &optional default)
  "Return ELEMENT's attribute value for KEY, defaulting to DEFAULT"
  (with-slots (_attributes) element
    (if _attributes
        (gethash key _attributes default)
      default)))
(cl-defmethod scxml-root-element ((element scxml-element))
  "Given any ELEMENT in an scxml-element tree, find the root of the tree."
  (let ((last element)
        (parent (scxml-parent element)))
    (while parent
      (setq last parent)
      (setq parent (scxml-parent last)))
    last))
(cl-defgeneric scxml-visit ((element scxml-element) visitor &optional filter)
  "Visit all children of ELEMENT with VISITOR and optionally FILTER first.

Visitation starts at ELEMENT and descends per child.  Other than
visiting ELEMENT first the order of visitation is undefined.

VISITOR must be of form (lambda (element) ...)
FILTER must be of form (lambda (element) ...)

Return value is undefined.")
(cl-defmethod scxml-visit ((element scxml-element) visitor &optional filter)
  "Visit all children of ELEMENT with VISITOR and optionally FILTER first.

Visitation starts at ELEMENT and descends per child.  Other than
visiting ELEMENT first the order of visitation is undefined.

VISITOR must be of form (lambda (element) ...)
FILTER must be of form (lambda (element) ...)

Return value is undefined."
  (if (or (not filter) (funcall filter element))
      (funcall visitor element))
  (mapc (lambda (child)
          (scxml-visit child visitor filter))
        (scxml-children element)))

(cl-defgeneric scxml-visit-all ((element scxml-element) visitor &optional filter)
  "Visit all elements (parent or child, recursively) starting at the root element.")
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
      (cl-block scxml-find-parent
        (mapc (lambda (A-element)
                (when (cl-member A-element B-parents :test 'eq)
                  (cl-return-from scxml-find-parent A-element)))
              A-parents)))))

(defclass scxml-element-with-id ()
  ((id :initarg :id
       :accessor scxml-element-id
       :initform nil
       :type (or string null)
       ;; - todo - get this working? :writer scxml-set-element-id
       ))
  :abstract t
  :documentation "Apply to an scxml element if it has an 'id'
  attribute that's significant.")
(cl-defmethod scxml-set-element-id ((element scxml-element-with-id) (id string))
  "Set the id of an element with protections."
  (when (scxml-element-find-by-id (scxml-root-element element) id)
    (error "An element with id of %s already exists" id))
  (oset element id id))

(cl-defmethod scxml-print ((idable-element scxml-element-with-id))
  (format "id:%s, %s"
          (scxml-element-id idable-element)
          (cl-call-next-method)))
(cl-defgeneric scxml-element-find-by-id ((search-root scxml-element) (id-to-find string))
  "Find element with ID-TO-FIND starting at SEARCH-ROOT and going down.

Function will not traverse the whole tree, only the portion at or
below SEARCH-ROOT")
(cl-defmethod scxml-element-find-by-id ((search-root scxml-element) (id-to-find string))
  "Find element with ID-TO-FIND at or below SEARCH-ROOT."
  (cl-block scxml-element-find-by-id-block
    (scxml-visit search-root
                 (lambda (element)
                   (when (string-equal (scxml-element-id element) id-to-find)
                     (cl-return-from scxml-element-find-by-id-block element)))
                 (lambda (element)
                   (object-of-class-p element 'scxml-element-with-id)))
    nil))

(cl-defmethod scxml-add-child :before ((parent scxml-element) (child-with-id scxml-element-with-id) &optional append)
  "Add child element to parent ensuring id is unique in the entire tree."
  (let ((proposed-id (scxml-element-id child-with-id)))
    (when (and proposed-id
               (> (length proposed-id) 0)
               (scxml-element-find-by-id (scxml-root-element parent) proposed-id))
      (error "Unable to add element with id %s, an element with that id already exists." proposed-id))))

(defclass scxml-element-with-initial ()
  ((initial :initarg :initial
            :accessor scxml-element-initial
            :initform nil
            :type (or string null)))
  :abstract t
  :documentation "Apply to an scxml element if it has an
  'initial' attribute that's significant.")

(provide 'scxml-element)
;;; scxml-element.el ends here
