;;; scxml-element-serialization --- scxml I/O functions -*- lexical-binding: t -*-

;;; Commentary:
;; Getting scxml-elements to and from xml.

;;; Code:
(require 'scxml-element)
(require 'scxml-elements)

;; XML reading/writing stuff
(defun scxml--trim-xml (xml-list)
  (seq-filter (lambda (thing)
                (not (stringp thing)))
              xml-list))
(defun scxml---append-extra-properties (element &optional attrib-alist exclude-list)
  "Add attributes from ATTRIB-ALIST to ELEMENT as attributes, excluding EXCLUDE-LIST."
  ;; TODO - fix this, this is a parent-knowing-about-children situation.
  ;; TODO - I think this can be done entirely with reflection when/if all the attributes
  ;;        that are important to a specific class are noted somehow.
  (when (object-of-class-p element 'scxml-drawable-element)
    (scxml--set-hint-from-attrib-list element attrib-alist))

  (let ((filtered-alist (seq-filter (lambda (cell)
                                      (scxml---visible-xml-attribute-name (car cell)))
                                    attrib-alist)))
    (cl-loop for prop-cell in filtered-alist
             for prop-name = (car prop-cell)
             do (unless (member prop-name exclude-list)
                  (scxml-put-attrib element prop-name (cdr prop-cell)))
             finally return element)))

(cl-defmethod scxml-xml-string ((element scxml-element) &optional exclude-children)
  "Get a string holding the XML for ELEMENT.

Normally all child elements will be rendered to xml and output as
well.  When EXCLUDE-CHILDREN is true then no child elements will
be included in the output."
  (let ((xml-name (scxml-xml-element-name element)))
    (if (null xml-name)
        ""
      (let ((children (and (not exclude-children) (scxml-children element)))
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
                     ""))))))

(defun scxml--factory (xml-element &optional element-factory)
  "Build scxml elements based off parsed xml XML-ELEMENT data.

Optionally use ELEMENT-FACTORY to build elements.
ELEMENT-FACTORY is called with a type and attributes alist to
build a childless element.  Children are then appended.  When
element factory is not specified the default
'scxml--element-factory is used."
  (let ((element-factory (or element-factory 'scxml--element-factory))
        (type (first xml-element))
        (attributes (second xml-element))
        (children (cddr xml-element)))
      (let ((element (funcall element-factory type attributes)))
        (mapc (lambda (child)
                ;; TODO - this is a lot of appending, possibly there is a better way to do this.
                ;; the appending is needed because the synthetic children are built by
                ;; the factory function and are alredy set on the parent.
                (scxml-add-child element
                                 (scxml--factory child element-factory)
                                 t))
              (scxml--trim-xml children))
        element)))
(defun scxml-read-string (xml-string &optional element-factory)
  "Return the scxml-element tree of XML-STRING.

Optionally use ELEMENT-FACTORY to build elements.
ELEMENT-FACTORY is called with a type and attributes alist to
build a childless element.  Children are then appended.  When
element factory is not specified the default
scxml--element-factory is used."
  (interactive "sXml string:")
  (with-temp-buffer
    (insert xml-string)
    (scxml-read-buffer nil element-factory)))

(defun scxml-read-buffer (&optional buffer-to-read element-factory)
  "Return the scxml-element tree of 'current-buffer' or BUFFER-TO-READ.

Optionally use ELEMENT-FACTORY to build elements.
ELEMENT-FACTORY is called with a type and attributes alist to
build a childless element.  Children are then appended.  When
element factory is not specified the default
scxml--element-factory is used."
  (interactive)
  (let* ((xml-data (progn (if buffer-to-read
                              (with-current-buffer buffer-to-read
                                (xml-parse-region))
                            (xml-parse-region))))
         (root-xml (first xml-data))
         (root-xml-element (first root-xml)))
    (unless (and (eq root-xml-element 'scxml)
                 (eq (length xml-data) 1))
      (error "Unable to read non-<scxml> documents"))
    (scxml--factory root-xml element-factory)))
(defun scxml-write-buffer ()
  "fire out some XML to a random buffer"
  (interactive)
  (let ((buffer (generate-new-buffer "scxml-xml-output"))
        (diagram scxml-draw--diagram))
    (with-current-buffer buffer
      (insert (scxml-xml-string (scxml-diagram-root diagram)))
      (xml-mode))
    (switch-to-buffer buffer)))

(provide 'scxml-element-serialization)
