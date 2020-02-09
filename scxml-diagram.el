;;; scxml-diagram.el --- scxml diagram container -*- lexical-binding: t -*-

;;; Commentary:
;; A diagram holds all pieces of data needed render and interact with
;; an scxml data set.

;;; Code:

(require 'scxml-canvas)
(require 'scxml-element)
(require 'scxml-viewport)
;; TODO - break out the xml dependencies into another file.
(require 'nxml-rap)
(require 'nxml-mode)                    ;for nxml-forward-element
(require 'scxml-xml)

(defvar scxml--diagram 'nil
  "Buffer local variable which holds the scxml-diagram being drawn.")
(make-variable-buffer-local 'scxml--diagram)

(defclass scxml-diagram ()
  ((canvas :initarg :canvas
           :accessor scxml-diagram-canvas
           :type scxml-canvas
           :documentation "The canvas here serves as the bounding box for the display-element of the diagram.  In drawing coordinates, not pixel.")
   (viewport :initarg :viewport
             :accessor scxml-diagram-viewport
             :type scxml-viewport
             :documentation "The viewport to the canvas for this diagram")
   (root :initarg :root
         :accessor scxml-diagram-root
         :type scxml-drawable-scxml
         :documentation "Root of the document containing the display-element")
   (buffer :initarg :buffer
           :accessor scxml-buffer
           :type buffer
           :documentation "The buffer this diagram is bound to for diagram rendering.")
   (xml-buffer :initarg :xml-buffer
               :accessor scxml-xml-buffer
               :initform nil
               :type (or null buffer)
               :documentation "The buffer this diagram is bound to for XML rendering.")
   (display-element :initarg :display-element
                    :accessor scxml-diagram-display-element
                    :type scxml-element
                    :documentation "Element to display (can be the root if you want)."))
  :documentation "Contains everything you'll need to render the diagram")
(cl-defgeneric scxml-find-element-selection ((diagram scxml-diagram) (selection-rect scxml-rect))
  "Find the element in DIAGRAM in the SELECTION-RECT.")
(cl-defmethod scxml-find-element-selection ((diagram scxml-diagram) (selection-rect scxml-rect))
  "Find the element in DIAGRAM that is inside the SELECTION-RECT.

Used to return and element when the user wants to do something like
click on an area.

Selection preference order:
- <initial>
- <transition>
- anything else (that isn't <scxml>)
- the root <scxml> element."
  (let ((start-element (scxml-diagram-root diagram)))
    (or (scxml---find-initial selection-rect start-element)
        (scxml---find-transition selection-rect start-element)
        (scxml---find-other selection-rect start-element)
        (scxml-diagram-display-element diagram))))
(defun scxml---find-other (selection-rect search-parent)
  "Get first non-transition/non-initial element in the SELECTION-RECT.
Start searching at SEARCH-PARENT.  When nothing is found return
search-parent."
  (block scxml---find-selection
    (mapc (lambda (child)
            (let ((drawing (scxml-element-drawing child)))
              (when (and drawing
                         (2dg-has-intersection selection-rect drawing 'stacked))
                (return-from scxml---find-selection
                  (scxml---find-other selection-rect child)))))
          (seq-filter (lambda (element) (and (not (object-of-class-p element 'scxml-transition))
                                             (not (object-of-class-p element 'scxml-initial))))
                      (scxml-children search-parent)))
    search-parent))
(defun scxml---find-transition (selection-rect search-parent)
  "Get first transition element in SELECTION-RECT.

Start searching at SEARCH-PARENT.  When nothing is found return
nil."
  (block scxml---find-selection
    (scxml-visit search-parent
                 (lambda (element)
                   (let ((drawing (scxml-element-drawing element)))
                     (when (and drawing
                                (2dg-has-intersection selection-rect drawing 'stacked))
                       (return-from scxml---find-selection element))))
                 (lambda (element)
                   (object-of-class-p element 'scxml-transition)))
    nil))
(defun scxml---find-initial (selection-rect search-parent)
  "Get the first initial element in SELECTION-RECT.
Start searching at SEARCH-PARENT.  When nothing is found return
nil."
  (block scxml---find-selection
    (scxml-visit search-parent
                 (lambda (element)
                   (let ((drawing (scxml-element-drawing element)))
                     (when (and drawing
                                (2dg-has-intersection selection-rect drawing 'stacked)
                                (return-from scxml---find-selection element)))))
                 (lambda (element)
                   (object-of-class-p element 'scxml-initial)))
    nil))

(cl-defmethod scxml-link-xml-buffer ((diagram scxml-diagram))
  "If the diagram has an XML buffer associated with it, link the buffer."
  (let ((root (scxml-diagram-root diagram))
        (buffer (scxml-xml-buffer diagram)))
    (when (and root buffer)
      (with-current-buffer buffer
        (when (and (boundp 'scxml--diagram) scxml--diagram)
          (error "This xml document already has a linked diagram"))
        (setq-local scxml--diagram diagram)
        (goto-char (point-min))
        (scxml---xmltok-init)
        ;; scxml-visit is depth first and the order of the document is depth first
        ;; so let's just pretend this will work for now.
        (scxml-visit
         root
         (lambda (element)
           (let ((xml-tag (scxml---xmltok-find-next-by-name
                             (scxml-xml-element-name element))))
             (unless xml-tag
               (error "Can't find the element I'm looking for"))
             (let ((end-pos (scxml-next-token-pos xml-tag)))
               (put-text-property (scxml-start xml-tag)
                                  end-pos
                                  'scxml-element
                                  element)
               (goto-char end-pos)))))))))

;; XML stuff that shouldn't be in here.
(cl-defmethod scxml-insert-new-child ((parent-start-tag scxml-xmltok) (child scxml-element) &optional exclude-children)
  "Insert the XML of CHILD as the last child of PARENT-RANGE."
  (scxml-insert-new-child parent-start-tag
                          (scxml-xml-string child exclude-children)
                          child))
(cl-defmethod scxml-update-xml-attributes ((xml-tag scxml-xmltok) (element scxml-drawable-element))
  "Get the attributes from ELEMENT and place them into XML-TAG's element"
  ;; TODO: This needs some attention
  (let ((attribute-markers (scxml-attributes xml-tag))
        ;; Apparently when parsing xml, the 'xmlns' attribute is
        ;; somehow special and not a real attribute.  Because of that
        ;; I'll have to pretend it doesn't exist here as well.
        (proper-attributes (seq-filter (lambda (attrib)
                                         (not (eq (car attrib) 'xmlns)))
                                       (scxml-xml-attributes element)))
        (valid-markers nil)
        (found-properties nil)
        (addition-point (- (scxml-next-token-pos xml-tag)
                           (if (eq (scxml-type xml-tag) 'empty-element)
                               2
                             1)))
        (additive-operations nil)
        (mutating-operations nil))
    ;; Go through every attribute in proper-attributes and make sure they're
    ;; in attribute-markers.
    ;; then anything that's in attribute-markers that's not in proper attributes
    ;; and remove it.
    (cl-loop for marker in attribute-markers
             for marker-keyval = (scxml---attribute-key-value marker)
             for marker-key = (car marker-keyval)
             for proper-keyval = (assoc marker-key
                                        proper-attributes
                                        (lambda (a b) ;this seems risky
                                          (equal (symbol-name a)
                                                 b)))
             for proper-val = (if proper-keyval
                                  (cdr proper-keyval)
                                nil)
             do (if (and proper-keyval proper-val)
                    ;; this marker matches up to a proper attribute
                    ;; TODO: for now assume the attribute is wrong and clobber it
                    ;; in the future, do a real check.
                    (let ((proper-val (cdr proper-keyval)))
                      (push proper-keyval found-properties)
                      (push (list 'insert (format "%s" proper-val))
                            mutating-operations)
                      (push (list 'goto-char (elt marker 3))
                            mutating-operations)
                      (push (list 'delete-region (elt marker 3) (elt marker 4))
                            mutating-operations))
                  ;; this proper attribute is *not* found, removed this attribute
                  (push (list 'delete-region (1- (elt marker 0)) (1+ (elt marker 4)))
                        mutating-operations)))
    (mapc (lambda (missing-attribute)
            (push (list 'insert (format " %s=\"%s\""
                                        (car missing-attribute)
                                        (cdr missing-attribute)))
                  additive-operations)
            (push (list 'goto-char addition-point)
                  additive-operations))
          (seq-filter (lambda (attribute-keyval)
                    (and (not (memq attribute-keyval found-properties))
                         ;; Additionally, only add in missing attributes
                         ;; if their values are non-nil
                         (cdr attribute-keyval)))
                  proper-attributes))

    (mapc (lambda (thunk-list)
            (apply (car thunk-list) (cdr thunk-list)))
          additive-operations)
    (mapc (lambda (thunk-list)
            (apply (car thunk-list) (cdr thunk-list)))
          mutating-operations)

    ;; when finished, reproperty this whole range.
    (goto-char (scxml-start xml-tag))
    (let ((updated-xml-tag (scxml-xmltok-after)))
      (message "putting my prop on %d -> %d"
               (scxml-start updated-xml-tag)
               (scxml-next-token-pos updated-xml-tag))
      (put-text-property (scxml-start updated-xml-tag)
                         (scxml-next-token-pos updated-xml-tag)
                         'scxml-element
                         element))))

(defun scxml---xmltok-find-next-by-name (name)
  ;; TODO - this should be in another file.
  "Wherever point is in a buffer, get the range of the next xml element with a name match."
  (let ((found-tag nil)
        (search-end nil)
        (last-point (point-max)))
    (while (not search-end)
      (let ((xml-tag (scxml-xmltok-after)))
        (if (null xml-tag)              ;no more tags found
            (setq search-end t)
          ;; todo - what do I do, if anything when I see 'end-tag?
          (if (and (memq (scxml-type xml-tag) (list 'start-tag 'empty-element))
                   (equal name (scxml-tag-name xml-tag)))
              (setf found-tag xml-tag
                    search-end t)
            (goto-char (scxml-next-token-pos xml-tag))))))
    found-tag))
(defun scxml---xmltok-find-element (element &optional start-at-point)
  ;; TODO - this should be in another file.
  "Use xmltok to find the ELEMENT using nxml-token-after."
  (when (not start-at-point)
    (goto-char (point-min)))
  (let ((element-name (scxml-xml-element-name element))
        (search-done nil)
        (result nil))
    (while (not search-done)
      (let ((candidate (scxml---xmltok-find-next-by-name element-name)))
        (if candidate
            (if (eq (scxml-get-text-prop candidate) element)
                (setf search-done t
                      result candidate)
              (goto-char (scxml-next-token-pos candidate)))
          (setf search-done t))))
    result))

(defun scxml---debug-xml-at-point ()
  "Debugging function for humans looking at xml. display whatever is known about text @ point."
  (interactive)
  (let ((scxml-prop (get-text-property (point) 'scxml-element)))
    (message "%s" scxml-prop)))

(cl-defmethod scxml-xml-update-element ((diagram scxml-diagram) (changed-element scxml-element) &optional include-children start-at-point)
  "Update the xml document if it exists"
  ;; must have a core type that is non-nil or the element shouldn't make it to the document.
  (when (scxml--core-type changed-element)
    (when (eq (scxml--core-type changed-element) 'compound)
      (error "Currently unable to update xml from compound elements"))
    (let ((buffer (scxml-xml-buffer diagram)))
      (when buffer
        (with-current-buffer buffer
          (unless (eq scxml--diagram diagram)
            (error "It appears the link between diagram and XML is bad"))
          (let ((xml-tag (scxml---xmltok-find-element changed-element)))
            (if xml-tag
                (progn
                  (scxml-update-xml-attributes xml-tag changed-element)
                  ;; have to update the tag as I may have modified it.
                  (setq xml-tag (scxml--refresh xml-tag)))
              ;; unable to find element, must add it.
              (let ((parent-range (scxml---xmltok-find-element
                                   (scxml-parent changed-element))))
                ;; Insert just this parent tag, I'll recurse to add children.
                (setq xml-tag
                      (scxml-insert-new-child parent-range changed-element t))))

            ;; xml-tag can now be trusted - handle children
            (when include-children
              ;; scan for all child xml-tags and delete any that are missing.
              (let ((child-tags (scxml-children xml-tag))
                    (child-elements (scxml-children changed-element))
                    (tags-to-prune nil))
                (cl-loop for child in child-tags
                         for linked-element = (scxml-get-text-prop child)
                         when (not (memq linked-element child-elements))
                         do (push child tags-to-prune))
                (when tags-to-prune
                  (sort tags-to-prune (lambda (a b)
                                        (> (scxml-start a) (scxml-start b))))
                  (mapc 'scxml---xmltok-prune tags-to-prune))

                ;; update all the children or create them if they're new.
                (cl-loop for child in child-elements
                         do (scxml-xml-update-element diagram child t t))))
            ))))))

(defun scxml-update-drawing ()
  "Update the diagram from what is in xml."
  (interactive)
  (error "Implement me."))

(defun scxml---xml-debug-next-point ()
  (interactive)
  (cl-loop with old-prop = (get-text-property (point) 'scxml-element)
           for idx from (1+ (point)) to (point-max)
           for prop-element = (get-text-property idx 'scxml-element)
           with forward-count = 0
           do (incf forward-count)
           when (not (eq old-prop prop-element))
           do (progn (message "ProN[%d]: %s"
                              forward-count
                              (and prop-element
                                   (scxml-print prop-element)))
                     (goto-char idx)
                     (cl-return))
           finally do (message "No change found forward.")))
(defun scxml---xml-debug-at-point ()
  (interactive)
  (cl-loop for idx from (point) to (point-max)
           for prop-element = (get-text-property idx 'scxml-element)
           with forward-count = -1
           do (incf forward-count)
           when prop-element
           do (progn (message "Prop[%d]: %s"
                              forward-count
                              (and prop-element
                                   (scxml-print prop-element)))
                     (cl-return))
           finally do (message "None found forward.")))

(provide 'scxml-diagram)
