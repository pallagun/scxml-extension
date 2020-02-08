;; bootstrap for this folder
(cl-pushnew (format "%s/test/" default-directory)
            load-path
            :test 'equal)

(require 'scxml-element-test)
(require 'scxml-elements-test)
