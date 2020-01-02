;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(require 'scxml-drawable-element)

(defclass scxml-drawable-scxml (scxml-scxml scxml-drawable-element)
  ())
(defclass scxml-drawable-state (scxml-state scxml-drawable-element)
  ())
(defclass scxml-drawable-final (scxml-final scxml-drawable-element)
  ())
(defclass scxml-drawable-initial (scxml-initial scxml-drawable-element)
  ())
(defclass scxml-drawable-parallel (scxml-parallel scxml-drawable-element)
  ())
(defclass scxml-drawable-transition (scxml-transition scxml-drawable-element)
  ())
(defun scxml--drawable-element-factory (type attrib-alist)
  (let* ((base-xml-element-name (symbol-name type))
         (base-class (intern (format "scxml-%s" base-xml-element-name)))
         (base-slots (eieio-class-slots base-class))
         (base-slot-symbols (mapcar 'cl--slot-descriptor-name base-slots))
         (drawable-class (intern (format "scxml-drawable-%s" base-xml-element-name)))
         (drawable-slots (eieio-class-slots drawable-class))
         (drawable-slot-symbols (mapcar 'cl--slot-descriptor-name drawable-slots))
         (skip-slots))
    ;; Build a list of all drawable-slots that aren't in teh base slots and
    ;; use them as as a skip-slots list.
    (mapc (lambda (slot-sym)
            (when (not (memq slot-sym base-slot-symbols))
              (push slot-sym skip-slots)))
          drawable-slot-symbols)
    (scxml--element-factory (intern (format "drawable-%s" base-xml-element-name))
                            attrib-alist
                            skip-slots)))

(provide 'scxml-drawable-elements)
