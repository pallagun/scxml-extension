(require 'ert)
(require 'scxml-draw)


(ert-deftest scxml-draw-get-canvas-divisions ()
  (let* ((rect (scxml-drawing-rect :x-min 0.0 :y-min 0.0 :x-max 100.0 :y-max 50.0))
         (num-children 3)
         (divisions (scxml---get-canvas-divisions rect num-children)))
    (should (>= (length divisions) num-children))))


(provide 'scxml-draw-test)
