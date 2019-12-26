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

(ert-deftest scxml-geometry-rect-leaving-segment-collision-edge ()
  "test distance between two segments given they are parallel"
  (let* ((rect (scxml-rect :x-min 2.0 :x-max 6.0
                           :y-min 2.0 :y-max 4.0)))
    ;; clearly right
    (should (eq (scxml-leaving-segment-collision-edge
                 rect
                 (scxml-point :x 20.0 :y 5.0))
                'right))
    ;; clearly the top
    (should (eq (scxml-leaving-segment-collision-edge
                 rect
                 (scxml-point :x 5.0 :y 100.0))
                'up))
    ;; clearly the left
    (should (eq (scxml-leaving-segment-collision-edge
                 rect
                 (scxml-point :x -100.0 :y 6.0))
                'left))
    ;; clearly the bottom
    (should (eq (scxml-leaving-segment-collision-edge
                 rect
                 (scxml-point :x 5.0 :y -100.0))
                'down))

    ;; less picky tests
    ;; point between right and top
    (should (member (scxml-leaving-segment-collision-edge
                     rect
                     (scxml-point :x 8.0 :y 5.0))
                    (list 'right 'up)))
    ;;point between top and left
    (should (member (scxml-leaving-segment-collision-edge
                     rect
                     (scxml-point :x -2.0 :y 6.0))
                    (list 'left 'up)))
    ;; point between left and bottom
    (should (member (scxml-leaving-segment-collision-edge
                     rect
                     (scxml-point :x 0.0 :y 1.0))
                    (list 'left 'down)))
    ;; point between bottom and right
    (should (member (scxml-leaving-segment-collision-edge
                     rect
                     (scxml-point :x 8.0 :y 1.0))
                    (list 'down 'right)))))

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
