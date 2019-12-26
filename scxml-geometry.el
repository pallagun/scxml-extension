;;; scxml-geometry.el --- scxml geometry helper -*- lexical-binding: t -*-

;;; Commentary:
;; This file serves as a collection point for all the geometry helper
;; files.

;; (pushnew default-directory load-path :test 'equal)

;;; Code:
(require 'scxml-geometry-core)
(require 'scxml-geometry-point)
(require 'scxml-geometry-span)
(require 'scxml-geometry-segment)
(require 'scxml-geometry-rect)
(require 'scxml-geometry-path)

(provide 'scxml-geometry)
;;; scxml-geometry.el ends here
