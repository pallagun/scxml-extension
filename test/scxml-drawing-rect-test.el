(require 'ert)
(require 'scxml-drawing-rect)

(ert-deftest scxml-drawing-rect-leaving-segment-collision-edge ()
  "test distance between two segments given they are parallel"
  (let* ((state (scxml-drawable-state :id "test-state"))
         (rect (scxml-drawing-rect :parent state
                                   :x-min 2.0
                                   :x-max 6.0
                                   :y-min 2.0
                                   :y-max 4.0)))
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

(provide 'scxml-drawing-rect-test)
