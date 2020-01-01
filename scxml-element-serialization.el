;;; scxml-element-serialization --- scxml I/O functions -*- lexical-binding: t -*-

;;; Commentary:
;; Getting scxml-elements to and from xml.

;;; Code:
(require 'scxml-element)

;; XML reading/writing stuff
(defun scxml--trim-xml (xml-list)
  (seq-filter (lambda (thing)
                (not (stringp thing)))
              xml-list))
(defun scxml---append-extra-properties (element &optional attrib-alist exclude-list)
  "Add attributes from ATTRIB-ALIST to ELEMENT as attributes, excluding EXCLUDE-LIST."
  ;; TODO - fix this, this is a parent-knowing-about-children situation.
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
(defun scxml--factory (element &optional factory-methods)
  (let ((type (first element))
        (attributes (second element))
        (children (cddr element))
        (factory-methods (or factory-methods scxml--default-factories)))
    (let ((factory-method (alist-get type factory-methods)))
      (unless factory-method
        (error "No factory to make element of type: %s" type))
      (let ((element (funcall factory-method attributes)))
        (mapc (lambda (child)
                (scxml-add-child element
                               (scxml--factory child factory-methods)))
              ;; possibly I can do this without the reverse?
              (reverse (scxml--trim-xml children)))
        element))))
(defun scxml-read-buffer (&optional buffer-to-read)
  "Return the scxml-element tree of 'current-buffer' or BUFFER-TO-READ."
  (interactive)
  (let* ((xml-data (progn (if buffer-to-read
                              (with-current-buffer buffer-to-read
                                (xml-parse-region))
                            (xml-parse-region))))
         (root-xml (first xml-data))
         (root-element (first root-xml)))
    (unless (and (eq root-element 'scxml)
                 (eq (length xml-data) 1))
      (error "Unable to read non-<scxml> documents"))
    (scxml--factory root-xml)))
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
