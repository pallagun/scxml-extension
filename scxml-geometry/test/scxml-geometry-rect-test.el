(require 'ert)

(require 'scxml-geometry-rect)

(ert-deftest scxml-geometry-rect-segments-matches ()
  (let* ((rect (scxml-rect :x-min 0.0 :x-max 5.0
                           :y-min 2.0 :y-max 7.0))
         (segments (scxml-segments rect)))
    (should (2dg-almost-equal (first segments)
                                (scxml-bottom rect)))
    (should (2dg-almost-equal (second segments)
                                (scxml-right rect)))
    (should (2dg-almost-equal (third segments)
                                (scxml-top rect)))
    (should (2dg-almost-equal (fourth segments)
                                (scxml-left rect)))))

(ert-deftest scxml-geometry-rect-contains-rect-point ()
  (let ((rect (scxml-rect :x-min 44.0 :x-max 45.0
                          :y-min 19.0 :y-max 20.0))
        (test-LL (2dg-point :x 44.0 :y 19.0))
        (test-LR (2dg-point :x 45.0 :y 19.0))
        (test-TR (2dg-point :x 45.0 :y 20.0))
        (test-TL (2dg-point :x 44.0 :y 20.0)))
    (should (2dg-contains rect test-LL))
    (should (2dg-contains rect test-LL nil))
    (should (2dg-contains rect test-LL 'stacked))
    (should-not (2dg-contains rect test-LL 'strict))

    (should (2dg-contains rect test-LR))
    (should (2dg-contains rect test-LR nil))
    (should-not (2dg-contains rect test-LR 'stacked))
    (should-not (2dg-contains rect test-LR 'strict))

    (should (2dg-contains rect test-TR))
    (should (2dg-contains rect test-TR nil))
    (should-not (2dg-contains rect test-TR 'stacked))
    (should-not (2dg-contains rect test-TR 'strict))

    (should (2dg-contains rect test-TL))
    (should (2dg-contains rect test-TL nil))
    (should-not (2dg-contains rect test-TL 'stacked))
    (should-not (2dg-contains rect test-TL 'strict))))

(provide 'scxml-geometry-rect-test)
