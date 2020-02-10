;; bootstrap for this folder
(cl-pushnew (format "%s/test/" default-directory)
            load-path
            :test 'equal)

(require '2dg-core-test)
(require '2dg-path-test)
(require '2dg-point-test)
(require '2dg-rect-test)
(require '2dg-segment-test)
(require '2dg-span-test)
