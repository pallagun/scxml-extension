(require 'ert)
(require 'scxml-drawing-connector-rect)


(ert-deftest scxml-build-conector ()
  (let ((drawing (scxml-drawing-rect :x-min 0.0 :y-min 0.0
                                     :x-max 10.0 :y-max 10.0)))

    ;; should find a point on a given edge without moving edges
    ;; failure to find a point on this edge is a nil return.
    (let* ((connector (scxml-drawing-connector-rect :node drawing :edge 'left :parametric 0.1))
           (derived (scxml-build-connector connector (scxml-point :x 0.0 :y 5.0)))
           (shouldnt-work-1 (scxml-build-connector connector (scxml-point :x 2.0 :y 2.0) 'nil))
           (shouldnt-work-2 (scxml-build-connector connector (scxml-point :x 1.0 :y 0.0) 'nil))
           (shouldnt-work-3 (scxml-build-connector connector (scxml-point :x 1.0 :y 10.0) 'nil)))
      (should (and (eq (scxml-node derived) drawing)
                   (eql (scxml-node-edge derived) 'left)
                   (equal (scxml-edge-parametric derived) 0.5)))
      (should (not shouldnt-work-1))
      (should (not shouldnt-work-2))
      (should (not shouldnt-work-3)))

    ;; should find a point on a given edge (prefer current edge)
    ;; failure to find a point on any edge is a nil return
    (let* ((connector (scxml-drawing-connector-rect :node drawing :edge 'left :parametric 0.1))
           (derived (scxml-build-connector connector (scxml-point :x 0.0 :y 5.0) 't))
           (derived-left-1 (scxml-build-connector connector (scxml-point :x 0.0 :y 10.0) 't))
           (derived-left-2 (scxml-build-connector connector (scxml-point :x 0.0 :y 0.0) 't))
           (derived-right (scxml-build-connector connector (scxml-point :x 10.0 :y 5.0) 't))
           (shouldnt-work (scxml-build-connector connector (scxml-point :x 2.0 :y 2.0) 't)))
      (should (and (eq (scxml-node derived) drawing)
                   (eql (scxml-node-edge derived) 'left)
                   (equal (scxml-edge-parametric derived) 0.5)))
      (should (and (eq (scxml-node derived-left-1) drawing)
                   (eql (scxml-node-edge derived-left-1) 'left)
                   (equal (scxml-edge-parametric derived-left-1) 0.0)))
      (should (and (eq (scxml-node derived-left-2) drawing)
                   (eql (scxml-node-edge derived-left-2) 'left)
                   (equal (scxml-edge-parametric derived-left-2) 1.0)))
      (should (and (eq (scxml-node derived-right) drawing)
                   (eql (scxml-node-edge derived-right) 'right)
                   (equal (scxml-edge-parametric derived-right) 0.5)))
      (should (not shouldnt-work)))))

(provide 'scxml-drawing-connector-rect-test)
