(require 'cl-lib)


(cl-pushnew default-directory load-path :test 'equal)

(require 'scxml)
(require 'scxml-drawing-connector-rect-test)
(require 'scxml-drawing-divided-rect-test)
(require 'scxml-draw-test)
(require 'scxml-element-test)
(require 'scxml-geometry-core-test)
(require 'scxml-geometry-path-test)
(require 'scxml-geometry-point-test)
(require 'scxml-geometry-rect-test)
(require 'scxml-geometry-segment-test)
(require 'scxml-geometry-span-test)
(require 'test-recorder)
(require 'scxml-run-test-recordings)
