(require 'ert)
(require 'scxml-element-serialization)


(ert-deftest scxml-read-string-test1 ()
  "Empty scxml"
  (let ((element (scxml-read-string
                  "<scxml />")))
    (should (eq 0 (scxml-num-children element)))
    (should (scxml-scxml-p element))
    (should (eq nil (scxml-element-name element)))
    (should (eq 0 (scxml-num-attrib element)))))
(ert-deftest scxml-read-string-test2 ()
  "Empty scxml with name"
  (let ((element (scxml-read-string
                  "<scxml name=\"test\" />")))
    (should (eq 0 (scxml-num-children element)))
    (should (scxml-scxml-p element))
    (should (equal "test" (scxml-element-name element)))
    (should (eq 0 (scxml-num-attrib element)))))
(ert-deftest scxml-read-string-test3 ()
  "Empty scxml with name and attribute"
  (let ((element (scxml-read-string
                  "<scxml name=\"test\" extra-attributes=\"sure\" />")))
    (should (eq 0 (scxml-num-children element)))
    (should (scxml-scxml-p element))
    (should (equal "test" (scxml-element-name element)))
    (should (eq 1 (scxml-num-attrib element)))
    (should (equal "sure"
                   (scxml-get-attrib element 'extra-attributes)))))
(ert-deftest scxml-read-string-test4 ()
  "Scxml element with single child state"
  (let ((element (scxml-read-string
                  "<scxml name=\"test\"> \n\t<state id=\"A\" extra=\"attributes\" /> \n </scxml>")))
    (should (scxml-scxml-p element))
    (should (equal "test" (scxml-element-name element)))
    (should (equal 1 (scxml-num-children element)))
    (should (eq nil (scxml-element-initial element)))
    (let* ((children (scxml-children element))
           (child (first children)))
      (should (eq 1 (length children)))
      (should (scxml-state-p child))
      (should (equal "A" (scxml-element-id child)))
      (should (eq 1 (scxml-num-attrib child)))
      (should (equal "attributes" (scxml-get-attrib child 'extra)))
      (should (eq 0 (scxml-num-children child)))
      (should (eq nil (scxml-element-initial element))))))
(ert-deftest scxml-read-string-test5 ()
  "Scxml element with parallel child and 2 state grandchildren"
  (let ((element (scxml-read-string
                  "<scxml initial=\"P\">
                    <!-- comment should be fine. -->
                    <parallel id=\"P\" extra=\"attributes\">
                     <state missing=\"id\" />
                     <state id=\"has-id\" />
                    </parallel> \n </scxml>")))
    (should (scxml-scxml-p element))
    (should (eq nil (scxml-element-name element)))
    (should (equal 1 (scxml-num-children element)))
    (should (equal "P" (scxml-element-initial element)))
    (let* ((children (scxml-children element))
           (child (first children)))
      (should (eq 1 (length children)))
      (should (scxml-parallel-p child))
      (should (equal "P" (scxml-element-id child)))
      (should (eq 1 (scxml-num-attrib child)))
      (should (equal "attributes" (scxml-get-attrib child 'extra)))
      (should (eq 2 (scxml-num-children child)))
      (let* ((grandchildren (scxml-children child))
             (first-grand-child (first grandchildren))
             (second-grand-child (second grandchildren)))
        (should (eq 2 (length grandchildren)))

        (should (scxml-state-p first-grand-child))
        (should (eq nil (scxml-element-id first-grand-child)))
        (should (eq 1 (scxml-num-attrib first-grand-child)))
        (should (equal "id" (scxml-get-attrib first-grand-child 'missing)))
        (should (eq 0 (scxml-num-children first-grand-child)))

        (should (scxml-state-p second-grand-child))
        (should (equal "has-id" (scxml-element-id second-grand-child)))
        (should (eq 0 (scxml-num-attrib second-grand-child)))
        (should (eq 0 (scxml-num-children second-grand-child)))))))


(provide 'scxml-element-serialization-test)
