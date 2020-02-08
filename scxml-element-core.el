;;; scxml-element-core --- scxml concepts  -*- lexical-binding: t -*-

;;; commentary:
;; These classes serve as markers.

;;; Code:
(require 'eieio)

;; Marker classes -
(defclass scxml--core ()
  (
   ;; TODO - I'm unable to specify the slot here and make it stricter for the child classes.  I think this is a bug in eieio maybe?  It appears CLOS allows this? check on that.
   ;; TODO - the rest of these are in scxml-elements.el - maybe break them out into their own file.
   ;; (-core-type :allocation :class
   ;;             :type symbol)
   )
  :abstract t
  :documentation "Indicates how this object relates to an scxml element.")

;; Marker classes - never apply more than one to an object.
(defun scxml--core-type (anything)
  "Return the scxml core type of ANYTHING."
  (if (object-of-class-p anything 'scxml--core)
      (oref anything -core-type)        ;this will throw if it's an actual scxml--core
    nil))
(defclass scxml--core-scxml (scxml--core)
  ((-core-type :initform 'scxml
               :allocation :class
               :type (and symbol (member scxml))))
  :abstract t
  :documentation "Indicates that this object represents a top level scxml document entity.")
(defclass scxml--core-state (scxml--core)
  ((-core-type :initform 'state
               :allocation :class
               :type (and symbol (member state))))
  :abstract t
  :documentation "Indicates that this object represents an scxml state.")
(defclass scxml--core-final (scxml--core)
  ((-core-type :initform 'final
               :allocation :class
               :type (and symbol (member final))))
  :abstract t
  :documentation "Indicates that this object represents an scxml final state.")
(defclass scxml--core-parallel (scxml--core)
  ((-core-type :initform 'parallel
               :allocation :class
               :type (and symbol (member parallel))))
  :abstract t
  :documentation "Indicates that this object represents an scxml parallel state.")
(defclass scxml--core-initial (scxml--core)
  ((-core-type :initform 'initial
               :allocation :class
               :type (and symbol (member initial))))
  :abstract t
  :documentation "Indicates that this object represents an scxml initial configuration.")
(defclass scxml--core-transition (scxml--core)
  ((-core-type :initform 'transition
               :allocation :class
               :type (and symbol (member transition))))
  :abstract t
  :documentation "Indicates that this object represents an scxml transition.")
(defclass scxml--core-nil (scxml--core)
  ((-core-type :initform 'nil
               :allocation :class
               :type (and symbol
                          (satisfies 'not))))
  :abstract t
  :documentation "Indicates that this object is not related to a
  defined scxml element and should be ignored.  Examples: drawing
  artifacts, shims, garnishes.")
(defclass scxml--core-compound (scxml--core)
  ((-core-type :initform 'compound
               :allocation :class
               :type (and symbol (member 'compound))))
  :abstract t
  :documentation "Indicates that this object is possibly related
  to a formal scxml element but not in a one to one or one to
  zero way.  This is a wildcard object.")

(defconst scxml--valid-child-types
  '((scxml . (state parallel final datamodel script))
    (state . (onentry onexit transition initial state parallel final history datamodel invoke))
    (parallel . (onentry onexit transition state parallel history datamodel invoke))
    (transition . nil)
    (initial . (transition))
    (final . (onentry onexit donedata))
    (onentry . nil)
    (onexit . nil)
    (history . transition))
  "Mapping of what child element types are valid for what parent
element types.  Constructed as an association list per parent
element type."
  ;; TODO - add counts?  e.g. there can be at most one <initial> as a child of a <state>
  )

(cl-defgeneric scxml-xml-element-name ((element scxml--core))
  "Return what the xml element name would be for this ELEMENT.")
(cl-defmethod scxml-xml-element-name ((element scxml--core))
  "return what the xml element name would be for this ELEMENT.

Doesn't check to ensure the ELEMENT is actually valid for rendering to xml.
Assumes everyone follows a nice naming scheme."
  (let ((core-type (scxml--core-type element)))
    (if core-type
        (symbol-name core-type)
      nil)))

(provide 'scxml-element-core)
;;; scxml-element-core.el ends here
