(require 'scxml)

(defun scxml-capture-screen (viewport)
  "Grab the contents of canvas from buffer"
  (save-excursion
    (let ((width (scxml-required-pixel-width viewport))
          (height (scxml-required-pixel-height viewport)))
      (goto-char (point-min))
      (forward-line height)
      (backward-char)
      (split-string (buffer-substring (point-min) (point))
                    "\n"))))

(defun scxml-begin-replay-session (test-file &optional x-size y-size)
  "Given an scxml TEST-FILE open it up in a drawing buffer."
  (let* ((x-size (or x-size 100))
         (y-size (or y-size 40))
         (root (prog2
                   (find-file test-file)
                   (scxml-read-buffer)
                 (kill-this-buffer)))
         (buffer (get-buffer-create (format "%s-scxml-replay" (abs (random)))))
         (canvas (scxml-canvas :x-min 0.0 :y-min 0.0
                               :x-max (float x-size) :y-max (float y-size)))
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

(defun scxml-begin-diagram-recording-session (test-file)
  "Supply a file with valid scxml data to start a recording session"
  (interactive "fFile: ")
  (let ((x-size 100)
        (y-size 40))
    (scxml-begin-replay-session test-file x-size y-size)
    (setq scxml-recording
          (list (list 'open-with-canvas-size test-file x-size y-size)))
    (scxml-diagram-mode--redraw)))

;; (defun scxml-assert-screen (expected-screen)
;;   (let* ((viewport (scxml-diagram-viewport scxml--diagram))
;;          (actual-screen (scxml-capture-screen viewport)))
;;     (should (equal expected-screen actual-screen))))

(defun scxml-record-assert-screen ()
  (interactive)
  (let ((buffer (scxml-buffer scxml--diagram))
        (viewport (scxml-diagram-viewport scxml--diagram)))
    (when scxml-recording
      (save-excursion
        (push (list 'assert-screen (scxml-capture-screen viewport))
              scxml-recording)
        (scxml-diagram-mode--redraw)))))

(defun scxml-replay-test (recording-file &optional perform-asserts)
  (interactive "fOpen Recording File: ")
  ;; TODO - this shouldn't be an exists check, it should be an exists
  ;; and is readable check
  (unless (file-exists-p recording-file)
    (error "Unable to open file: %s" recording-file))

  (with-temp-buffer
    (find-file recording-file)
    (scxml-replay-test-in-buffer perform-asserts)))

(defun scxml-replay-test-in-buffer (&optional perform-asserts)
  "Replay a test in the current buffer"
  (interactive)
  ;; disable global recording, no need to record if you're replaying.
  (setq scxml-recording nil)
  (let ((test-data (buffer-string))
        (current-point 0)
        (diagram))
    (cl-loop for line-data = (condition-case nil
                                 (read-from-string test-data current-point)
                               (error nil))
             when (null line-data)      ;If this is true there are no more instructions
               return diagram

             ;; Pull the instruction out and prepare for the next instruction read.
             for instruction = (car line-data)
             do (setq current-point (cdr line-data))
             ;; Execute the instruction
             do (let ((function-name (first instruction)))
                  ;; Instruction router, separate the special
                  ;; instructions from the normal ones.
                  (cond ((eq function-name 'open-with-canvas-size)
                         (setq diagram
                               (apply #'scxml-begin-replay-session (cdr instruction))))
                        ((eq function-name 'assert-screen)
                         (when perform-asserts
                           (should (equal (cadr instruction)
                                          (scxml-capture-screen (scxml-diagram-viewport diagram))))))
                        (t
                         ;; normal instruction router.
                         (eval instruction)))))))
