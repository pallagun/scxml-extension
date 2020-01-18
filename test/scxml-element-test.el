(require 'ert)
(require 'scxml-element)

(ert-deftest scxml-element-root-element-test ()
  (let* ((transition-to-b (scxml-transition :target "subB"))
         (transition-to-a (scxml-transition :target "subA"))
         (state-a (scxml-state :id "subA"))
         (state-b (scxml-state :id "subB"))
         (my-state (scxml-state :id "main"))
         (my-scxml (scxml-scxml))
         (element-list 'nil))

    ;; initialize tree
    (scxml-add-child state-a transition-to-b)
    (scxml-add-child state-b transition-to-a)
    (scxml-add-children my-state state-a state-b)
    (scxml-add-children my-scxml my-state)

    (scxml-visit my-scxml
                 (lambda (element)
                   (push element element-list)))

    (mapc (lambda (starting-element)
            (should (eq my-scxml
                        (scxml-root-element starting-element))))
          element-list)))

(ert-deftest scxml-element-visit-test ()
  (let* ((transition-to-b (scxml-transition :target "subB"))
         (transition-to-a (scxml-transition :target "subA"))
         (state-a (scxml-state :id "subA"))
         (state-b (scxml-state :id "subB"))
         (my-state (scxml-state :id "main"))
         (my-scxml (scxml-scxml))
         (id-list 'nil)
         (count 0)
         (targets 'nil))

    ;; initialize tree
    (scxml-add-child state-a transition-to-b)
    (scxml-add-child state-b transition-to-a)
    (scxml-add-children my-state state-a state-b)
    (scxml-add-children my-scxml my-state)

    ;; traverse all elements
    (progn
      (setq id-list 'nil)
      (setq count 0)
      (scxml-visit my-scxml
                   (lambda (element)
                     (incf count)
                     (let ((id (and (object-of-class-p element 'scxml-element-with-id)
                                    (scxml-element-id element))))
                       (when id
                         (push id id-list)))))
      (setq id-list (nreverse id-list))
      (should (equal (first id-list) "main")) ; should have hit main first
      (should (eql (length id-list) 3))       ; should hit all three states
      (should (member "subA" id-list))
      (should (member "subB" id-list))
      (should (equal count 6)))

    (progn
      (setq count 0)
      (setq targets '())
      (scxml-visit my-scxml
                   (lambda (element)
                     (should (scxml-transition-p element))
                     (push (scxml-target-id element) targets)
                     (incf count))
                   'scxml-transition-p)
      (should (eql (length targets) 2))
      (should (member "subA" targets))
      (should (member "subB" targets)))
    (progn
      (setq count 0)
      (setq id-list 'nil)
      (scxml-visit state-a
                   (lambda (element)
                     (incf count)
                     (let ((id (and (object-of-class-p element 'scxml-element-with-id)
                                    (scxml-element-id element))))
                       (when id
                         (push id id-list)))))
      (should (eql (length id-list) 1))
      (should (equal (first id-list) "subA"))
      (should (eql count 2)))))

(ert-deftest scxml-element-print-test ()
  (let* ((final (scxml-final :id "myFinalId"))
         (state (scxml-state :id "myStateId" :initial "stateInitial"))
         (scxml (scxml-scxml :name "myName" :initial "scxmlInitial"))
         (parallel (scxml-parallel :id "myParallelId"))
         (transition (scxml-transition :target "transitionTarget"))
         (initial (scxml-add-child (scxml-initial)
                                   transition)))
    (scxml-print final)
    (scxml-print state)
    (scxml-print scxml)
    (scxml-print parallel)
    (scxml-print transition)
    (scxml-print initial)))

(ert-deftest scxml-element-xml-string-test ()
  (let ((state (scxml-state :id "test-state")))
    (should (equal (scxml-xml-string state)
                   "<state id=\"test-state\" />"))
    (scxml-put-attrib state 'anything "really, anything")
    (should (equal (scxml-xml-string state)
                   "<state id=\"test-state\" anything=\"really, anything\" />"))
    (scxml-add-child state (scxml-state :id "child-id"))
    (should (equal (scxml-xml-string state)
                   "<state id=\"test-state\" anything=\"really, anything\"><state id=\"child-id\" /></state>"))))

(ert-deftest scxml-element-find-by-id-test ()
  (let* ((parent (scxml-state :id "parent"))
         (child-a (scxml-state :id "child-a"))
         (child-b (scxml-state :id "child-b"))
         (child-a-a (scxml-state :id "child-a-a")))
    (scxml-add-child child-a child-a-a)
    (scxml-add-child parent child-a)
    (scxml-add-child parent child-b)
    (scxml-add-child child-a (scxml-transition :target "child-b"))

    (should (eq (scxml-element-find-by-id parent "parent")
                parent))
    (should (eq (scxml-element-find-by-id parent "child-a")
                child-a))
    (should (eq (scxml-element-find-by-id parent "child-b")
                child-b))
    (should (eq (scxml-element-find-by-id parent "child-a-a")
                child-a-a))))

(ert-deftest scxml-element-find-nearest-mutual-parent ()
  (let ((root (scxml-scxml))
        (left (scxml-state))
        (right (scxml-state))
        (left-left (scxml-state))
        (left-right (scxml-state))
        (right-right (scxml-state))
        (right-left (scxml-state)))
    aoeuaoeuaou

(cl-defmethod scxml-find-nearest-mutual-parent-variadic (&rest elements)
(provide 'scxml-element-test)
