;;; scxml-element-serialization --- scxml I/O functions -*- lexical-binding: t -*-

;;; Commentary:
;; Getting scxml-elements to and from xml.

;;; Code:
(require 'scxml-element)

;; XML reading/writing stuff
(defun scxml---trim-xml (xml-list)
  (seq-filter (lambda (thing)
                (not (stringp thing)))
              xml-list))
(defun scxml---append-extra-properties (element &optional attrib-alist exclude-list)
  "Add attributes from ATTRIB-ALIST to ELEMENT as attributes, excluding EXCLUDE-LIST."
  ;; TODO - fix this, this is a parent-knowing-about-children situation.
  (when (object-of-class-p element 'scxml-drawable-element)
    (scxml--set-hint-from-attrib-list element attrib-alist))

  (let ((filtered-alist (filter (lambda (cell)
                                  (scxml---visible-xml-attribute-name (car cell)))
                                attrib-alist)))
    (cl-loop for prop-cell in filtered-alist
             for prop-name = (car prop-cell)
             do (unless (member prop-name exclude-list)
                  (scxml-put-attrib element prop-name (cdr prop-cell)))
             finally return element)))
(defun scxml---factory (element)
  "no clue"
  (let ((type (first element))
        (attributes (second element))
        (children (cddr element)))
    ;; TODO: probably shouldn't be using a case statement here.
    (let ((element (cond ((eq type 'scxml)
                          (scxml---scxml-factory attributes))
                         ((eq type 'state)
                          (scxml---state-factory attributes))
                         ((eq type 'transition)
                          (scxml---transition-factory attributes))
                         ((eq type 'parallel)
                          (scxml---parallel-factory attributes))
                         ((eq type 'final)
                          (scxml---final-factory attributes))
                         ((eq type 'initial)
                          (scxml---initial-factory attributes))
                         (t (error "scxml---factory: What is this element? %s" type)))))
      (mapc (lambda (child)
              (scxml-add-child element
                               (scxml---factory child)))
            ;; possibly I can do this without the reverse?
            (reverse (scxml---trim-xml children)))
      element)))
(defun scxml-read-buffer (&optional buffer-to-read)
  "Whatever buffer you're in, eat it and fire out an SCXML."
  (interactive)
  (cl-flet ((read-current-buffer
             ()
             (let* ((xml-data (xml-parse-region))
                    (root-xml (first xml-data))
                    (root-element (first root-xml)))
               (unless (and (eq root-element 'scxml)
                            (eq (length xml-data) 1))
                 (error "Unable to read non-<scxml> documents"))
                 (scxml---factory root-xml))))
    (if buffer-to-read
        (with-current-buffer buffer-to-read
          (read-current-buffer))
      (read-current-buffer))))
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
