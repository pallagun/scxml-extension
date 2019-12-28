(require 'ert)
(require 'scxml)

;; Overall tests.

(defun test-get-viewport-string (buffer viewport)
  "Grab the contents of canvas from buffer"
  (let ((width (scxml-required-pixel-width viewport))
        (height (1- (scxml-required-pixel-height viewport))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line height)
      (backward-char)
      (split-string (buffer-substring (point-min) (point))
                    "\n"))))
(defun test--draw-root (root x-max y-max)
  (let* ((buffer (get-buffer-create (format "%s-scxml-test-series" (random))))
         (canvas (scxml-canvas :x-min 0.0 :y-min 0.0
                               :x-max (float x-max) :y-max (float y-max)))
         (viewport (scxml-build-viewport canvas))
         (diagram (scxml-diagram :canvas canvas
                                 :viewport viewport
                                 :root root
                                 :display-element root
                                 :buffer buffer)))
    (scxml--init-buffer buffer)
    (scxml-draw diagram)
    (switch-to-buffer (scxml-buffer diagram))
    diagram))

(ert-deftest scxml-overall-001-empty ()
  "Plot a single state on a canvas"
  (let* ((buffer (get-buffer-create (format "%s-scxml-test-series" (random))))
         (root-element (scxml-scxml :id "scxml-mode-test"))
         (canvas (scxml-canvas :x-min 0.0 :y-min 0.0
                               :x-max 10.0 :y-max 10.0))
         (viewport (scxml-build-viewport canvas))
         (diagram (scxml-diagram :canvas canvas
                                  :viewport viewport
                                  :root root-element
                                  :display-element root-element
                                  :buffer buffer)))
    (scxml--init-buffer buffer)
    (scxml-draw diagram)
    (switch-to-buffer (scxml-buffer diagram))
    (let ((expected-viewport (cl-loop for i from 0 to (- (scxml-required-pixel-height viewport) 2)
                                      collect (make-string (scxml-required-pixel-width viewport) (string-to-char " "))))
          (actual-viewport (test-get-viewport-string buffer viewport)))

      (should (equal actual-viewport expected-viewport)))))

(ert-deftest scxml-overall-002-shrink ()
  "Shrinking something a great deal should not cause error"
  (let* ((test-file "test_document_001.xml")
         (xml-buffer (progn (find-file test-file)
                            (current-buffer)))
         (scxml-root (prog2
                       (find-file test-file)
                       (scxml-read-buffer)
                       (kill-this-buffer))))
    (test--draw-root scxml-root 100 40)
    ;; select the bottom right corner of A (via fake move and fake double-click)
    (goto-char (point-min))
    (forward-line 40)
    (forward-char 45)
    (scxml-diagram-mode--mark-at-point t)
    ;; now move it up and left so the box becomes too small
    (scxml-diagram-mode--modify (scxml-point :x -29.0 :y 30.0))

    ;; now this last shrinking move will break *something*
    ;; the cardinal path plotter is eating shit.
    (scxml-diagram-mode--modify-left)
    (scxml-diagram-mode--modify-left)
    (scxml-diagram-mode--modify-left)
    (kill-buffer)))
