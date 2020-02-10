;;; -*- mode: lisp; lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl)

;; runwith
;; $> emacs -batch -l ert -l test/run-me.el -f ert-run-tests-batch-and-exit

(defvar scxml-test-root nil
   "project root, used to resolve tests and test files.")
(eval-when ('load 'eval 'compile)
  (setq scxml-test-root default-directory))

(setq scxml-test-root default-directory)

(let ((original-dir default-directory))
  (cd (format "%sscxml" original-dir))
  (load (format "%sscxml/bootstrap.el" original-dir))
  (load (format "%sscxml/bootstrap-test.el" original-dir))
  (cd original-dir))

(let ((original-dir default-directory))
  (cl-pushnew default-directory load-path :test 'equal)
  (cl-pushnew (format "%s/test" default-directory) load-path :test 'equal)
  (cd (format "%stest" original-dir))
  (load (format "run-me.el" default-directory))
  (cd original-dir))

(require 'eieio)
(require 'scxml-drawable-elements)
(require 'scxml-draw)
(require 'scxml-diagram-mode)
