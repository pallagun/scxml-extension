;;; -*- mode: lisp; lexical-binding: t; -*-

(require 'cl-lib)

;; runwith
;; $> emacs -batch -l ert -l test/run-me.el -f ert-run-tests-batch-and-exit

;; This could be run with a batch command or in a session.  I'm not
;; sure how to properly load files in that case, but determining the
;; project root and going from there seems ok for now.
;; (defvar scxml-test-root nil
;;   "project root, used to resolve tests and test files.")

(let ((root (locate-dominating-file default-directory "scxml.el")))
  (setq scxml-test-root root)
  (cl-pushnew default-directory load-path :test 'equal)
  (cl-pushnew (format "%s/test" default-directory) load-path :test 'equal))

(defun scxml-resolve-file (file-path)
  "Resolve file for testing."
  (if (file-exists-p file-path)
      file-path
    (format "%s/%s" scxml-test-root file-path)))

(require 'scxml)
(require 'scxml-drawing-rect-test)
(require 'scxml-drawing-connector-rect-test)
(require 'scxml-drawing-divided-rect-test)
(require 'scxml-draw-test)
(require 'scxml-element-test)

(require 'test-recorder)
(require 'scxml-run-test-recordings)
