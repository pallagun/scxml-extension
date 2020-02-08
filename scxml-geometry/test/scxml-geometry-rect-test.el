(require 'ert)

(require 'scxml-geometry-rect)

(ert-deftest scxml-geometry-rect-segments-matches ()
  (let* ((rect (scxml-rect :x-min 0.0 :x-max 5.0
                           :y-min 2.0 :y-max 7.0))
         (segments (scxml-segments rect)))
    (should (scxml-almost-equal (first segments)
                                (scxml-bottom rect)))
    (should (scxml-almost-equal (second segments)
                                (scxml-right rect)))
    (should (scxml-almost-equal (third segments)
                                (scxml-top rect)))
    (should (scxml-almost-equal (fourth segments)
                                (scxml-left rect)))))

(ert-deftest scxml-geometry-rect-contains-rect-point ()
  (let ((rect (scxml-rect :x-min 44.0 :x-max 45.0
                          :y-min 19.0 :y-max 20.0))
        (test-LL (scxml-point :x 44.0 :y 19.0))
        (test-LR (scxml-point :x 45.0 :y 19.0))
        (test-TR (scxml-point :x 45.0 :y 20.0))
        (test-TL (scxml-point :x 44.0 :y 20.0)))
    (should (scxml-contains rect test-LL))
    (should (scxml-contains rect test-LL nil))
    (should (scxml-contains rect test-LL 'stacked))
    (should-not (scxml-contains rect test-LL 'strict))

    (should (scxml-contains rect test-LR))
    (should (scxml-contains rect test-LR nil))
    (should-not (scxml-contains rect test-LR 'stacked))
    (should-not (scxml-contains rect test-LR 'strict))

    (should (scxml-contains rect test-TR))
    (should (scxml-contains rect test-TR nil))
    (should-not (scxml-contains rect test-TR 'stacked))
    (should-not (scxml-contains rect test-TR 'strict))

    (should (scxml-contains rect test-TL))
    (should (scxml-contains rect test-TL nil))
    (should-not (scxml-contains rect test-TL 'stacked))
    (should-not (scxml-contains rect test-TL 'strict))))

(provide 'scxml-geometry-rect-test)
