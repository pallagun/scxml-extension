;; bootstrap for this folder
(cl-pushnew default-directory load-path :test 'equal)
(require 'scxml)
