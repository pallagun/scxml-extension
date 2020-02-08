;; bootstrap for this folder
(cl-pushnew (format "%s/test/" default-directory)
            load-path
            :test 'equal)

(require 'scxml-geometry-core-test)
(require 'scxml-geometry-path-test)
(require 'scxml-geometry-point-test)
(require 'scxml-geometry-rect-test)
(require 'scxml-geometry-segment-test)
(require 'scxml-geometry-span-test)
