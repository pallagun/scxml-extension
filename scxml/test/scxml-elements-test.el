(require 'ert)

(require 'scxml-elements)

(ert-deftest scxml--element-factory-test-scxml ()
  (let* ((attribs '((initial . "test-initial")
                    (test-property . "anything")))
         (element (scxml--element-factory 'scxml attribs)))
    (should (equal (scxml-element-name element)
                   nil))
    (should (equal (scxml-element-initial element)
                   "test-initial"))
    (should (equal (scxml-get-attrib element 'test-property)
                   "anything"))
    (should (eq (scxml-num-attrib element)
                1))))

(ert-deftest scxml--element-factory-test-state ()
  (let* ((attribs '((id . "test-id")
                    (anything . "else")))
         (element (scxml--element-factory 'state attribs)))
    (should (equal (scxml-element-id element)
                   "test-id"))
    (should (equal (scxml-element-initial element)
                   nil))
    (should (equal (scxml-get-attrib element 'anything)
                   "else"))
    (should (eq (scxml-num-attrib element)
                1))))

(ert-deftest scxml--element-factory-test-final ()
  )
(ert-deftest scxml--element-factory-test-initial ()
  )
(ert-deftest scxml--element-factory-test-parallel ()
  )
(ert-deftest scxml--element-factory-test-transition ()
  )

(ert-deftest scxml--element-validate-add-child-initial-test ()
  (let ((my-parent (scxml-state :id "parent")))
    ;; Empty <initial> elements are never valid to add.
    (should-error
     (scxml--validate-add-child my-parent (scxml-initial)))
    (let ((my-child (scxml-state :id "child"))
          (another-child (scxml-state :id "another-child")))
      (scxml-add-child my-parent my-child)
      (scxml-add-child my-parent another-child)

      ;; should not be able to add an empty <initial>
      (should-error
       (scxml--validate-add-child my-parent (scxml-initial)))

      ;; should not be able to add an initial with a an invalid transition.
      (should-error
       (scxml--validate-add-child my-parent
                                  (scxml-add-child (scxml-initial)
                                                   (scxml-transition :target "invalid"))))
      ;; parent is not valid either.
      (should-error
       (scxml--validate-add-child my-parent
                                  (scxml-add-child (scxml-initial)
                                                   (scxml-transition :target "parent"))))

      ;; Only possible valid <initial>
      (scxml--validate-add-child my-parent
                                 (scxml-add-child (scxml-initial)
                                                  (scxml-transition :target "child")))
      ;; actually add it.
      (scxml-add-child my-parent
                       (scxml-add-child (scxml-initial)
                                        (scxml-transition :target "child")))

      ;; should not allow a second initial, even if it is valid as a first <initial>
      (should-error
       (scxml--validate-add-child my-parent
                                  (scxml-add-child (scxml-initial)
                                                   (scxml-transition :target "another-child")))))))

(provide 'scxml-elements-test)
