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
                     (let ((id (scxml-element-id element)))
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
                     (push (scxml-transition-target element) targets)
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
                     (let ((id (scxml-element-id element)))
                       (when id
                         (push id id-list)))))
      (should (eql (length id-list) 1))
      (should (equal (first id-list) "subA"))
      (should (eql count 2)))))

(provide 'scxml-element-test)
