;;; scxml-xml.el --- scxml helper functions for dealing with xml -*- lexical-binding: t -*-

;;; Commentary:
;; Some wrappers around xmltok and other nxml-mode functions

;;; Code:

(require 'nxml-rap)
(require 'nxml-mode)                    ;for nxml-forward-element
(require 'eieio)
(require 'cl)

(defclass scxml-xmltok ()
  ((type :accessor scxml-type)
   (start :accessor scxml-start)
   (name-end :accessor scxml-name-end)
   (name-colon :accessor scxml-name-colon)
   (attributes :accessor scxml-attributes)
   (namespace-attributes :accessor scxml-namespace-attributes)
   (next-token-pos :accessor scxml-next-token-pos))
  :documentation "Wrap up the results from xmltok operations")
(cl-defmethod scxml-print ((tag scxml-xmltok))
  "xml[%s]: %s"
  (scxml-type tag)
  (buffer-substring (scxml-start tag) (scxml-next-token-pos tag)))
(defun scxml---attribute-key-value (attribute-marker)
  (cons (buffer-substring (elt attribute-marker 0)
                          (elt attribute-marker 2))
        (buffer-substring (elt attribute-marker 3)
                          (elt attribute-marker 4))))
(cl-defmethod scxml--refresh ((tag scxml-xmltok))
  "Attempt to refresh TAG if it was changed.

This function is only valid when you've modified attributes
of the TAG and not modified *anything* else in the document."
  (goto-char (scxml-start tag))
  (scxml-xmltok-after))

(cl-defmethod scxml-tag-name ((xml-tag scxml-xmltok))
  "Get the tag name of this tag."
  (buffer-substring (1+ (scxml-start xml-tag))
                    (scxml-name-end xml-tag)))
(cl-defmethod scxml-get-text-prop ((xml-tag scxml-xmltok) &optional property-name)
  "Yank out the first character having an text property of 'scxml-element"
  (let ((property-name (or property-name 'scxml-element)))
    (cl-loop for char-idx from (scxml-start xml-tag) to (scxml-next-token-pos xml-tag)
             for text-prop = (get-text-property char-idx property-name)
             when text-prop
             return text-prop)))
(cl-defmethod scxml-attributes-alist ((xml-tag scxml-xmltok))
  "Fire out an alist of all the attributes of XML-TAG."
  (with-slots (attributes) xml-tag
    (when attributes
      (cl-loop for attrib in attributes
               with attrib-alist = nil
               do (push (cons
                         (buffer-substring (elt attrib 0)
                                           (elt attrib 2))
                         (buffer-substring (elt attrib 3)
                                           (elt attrib 4)))
                        attrib-alist)
               finally return attrib-alist))))
(cl-defmethod scxml-find-end ((xml-tag scxml-xmltok))
  "Get the beginning of the end tag for this xml-tag if there is one"
  (when (eq 'start-tag (scxml-type xml-tag))
    (goto-char (scxml-start xml-tag))
    (nxml-forward-element 1)
    (scxml-xmltok-before)))
(cl-defmethod scxml-insert-new-child ((parent-start-tag scxml-xmltok) (child string) &optional tracking-property-value)
  "Insert the XML of CHILD as the last child of PARENT-RANGE returning the new scxml-xmltok."
  (let* ((parent-end-tag (scxml-find-end parent-start-tag))
         (insert-start (scxml-start parent-end-tag)))
    (goto-char insert-start)
    (insert child "\n")
    (let ((insert-end (point)))
      (when tracking-property-value
        (put-text-property insert-start
                           insert-end
                           'scxml-element
                           tracking-property-value))
      (indent-region insert-start (1+ (point))))
    (goto-char insert-start)
    (scxml-xmltok-after)))
(cl-defmethod scxml-children ((xml-tag scxml-xmltok))
  "Get all the directi children tags of XML-TAG."
  (let ((end-tag (and (eq (scxml-type xml-tag) 'start-tag)
                      (scxml-find-end xml-tag))))
    (when end-tag
      (goto-char (scxml-next-token-pos xml-tag))
      (let ((children nil)
            (child-tag (scxml-xmltok-after)))
        (while (and child-tag
                    (not (eq child-tag end-tag)))
          (when (and (memq (scxml-type child-tag)
                           (list 'start-tag 'empty-element))
                     (scxml-get-text-prop child-tag))
            (push child-tag children))
          (goto-char (scxml-next-token-pos
                      (if (eq (scxml-type child-tag) 'start-tag)
                          (scxml-find-end child-tag)
                        child-tag)))
          (setq child-tag (scxml-xmltok-after)))
        (nreverse children)))))

(defun scxml-xmltok-before ()
  "Wrap up nxml-token-before."
  (when (> (point) (point-min))
    (scxml---gather-xmltok (nxml-token-before))))
(defun scxml-xmltok-after ()
  "Wrap up nxml-token-after."
  (when (< (point) (point-max))
    (scxml---gather-xmltok (nxml-token-after))))
(defun scxml---xmltok-prune (xml-tag)
  "Prune this tag from the current buffer.
This will modify the buffer and possibly invalidate other scxml-xmltok objects!"
  (cond ((eq (scxml-type xml-tag) 'empty-element)
         (delete-region (scxml-start xml-tag)
                        (scxml-next-token-pos xml-tag)))
        ((eq (scxml-type xml-tag) 'start-tag)
         (let ((end-tag (scxml-find-end xml-tag)))
           (unless end-tag
             (error "Unable to prune tag[%s], unable to find end" xml-tag))
           (delete-region (scxml-start xml-tag)
                          (scxml-next-token-pos end-tag))))
        (t
         (error "Currently unable to delete this type of tag: %s" xml-tag))))
(defun scxml---gather-xmltok (next-token-pos)
  "Create an scxml-xmltok object capturing the current state of xmltok processing"
  ;; TODO - capture if you're scanning forwards or backwards.
  ;; that has an impact on what next-token-pos means.
  (let ((parser-state (scxml-xmltok)))
    (oset parser-state type xmltok-type)
    (oset parser-state start xmltok-start)
    (oset parser-state name-end xmltok-name-end)
    (oset parser-state name-colon xmltok-name-colon)
    (oset parser-state attributes xmltok-attributes)
    (oset parser-state namespace-attributes xmltok-namespace-attributes)
    (oset parser-state next-token-pos next-token-pos)
    parser-state))
(defun scxml---xmltok-init ()
  "Initialize the xmltok processing in current-buffer.

I'm not sure why I have to do this, but I think it's because I'm
hot wiring the whole thing."
  (when (or (not (boundp 'nxml-prolog-end))
            (null nxml-prolog-end))
    (setq-local nxml-scan-end
                (set-marker (make-marker) (point-min)))
    (nxml-scan-prolog)))
(defun scxml---xmltok-debug-at-point ()
  (interactive)
  (scxml---xmltok-init)
  (let ((tag (scxml-xmltok-after)))
    ;; (message (scxml-print tag))
    (message "%s:%s: %s"
             (scxml-type tag)
             (scxml-tag-name tag)
             (scxml-attributes-alist tag))
    (goto-char (scxml-next-token-pos tag))))


(provide 'scxml-xml)
;;; scxml-xml.el ends here
