(require 'ert)

(require '2dg-rect)

(ert-deftest 2dg-rect-segments-matches ()
  (let* ((rect (2dg-rect :x-min 0.0 :x-max 5.0
                           :y-min 2.0 :y-max 7.0))
         (segments (2dg-segments rect)))
    (should (2dg-almost-equal (first segments)
                                (2dg-bottom rect)))
    (should (2dg-almost-equal (second segments)
                                (2dg-right rect)))
    (should (2dg-almost-equal (third segments)
                                (2dg-top rect)))
    (should (2dg-almost-equal (fourth segments)
                                (2dg-left rect)))))

(ert-deftest 2dg-rect-contains-rect-point ()
  (let ((rect (2dg-rect :x-min 44.0 :x-max 45.0
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

(provide '2dg-rect-test)
