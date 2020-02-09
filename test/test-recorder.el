;; TODO - clean this up a bit.  It's a test recorder and replayer with  -*- lexical-binding: t; -*-

;; asserts on what the screen looks like.

;; TODO - rename this scxml-test-recorder?  I'm not sure if it's important if it's only used in tests.

;;; Code:
(require 'ert)
(require 'scxml)

(defun scxml-capture-screen (viewport)
  "Return the VIEWPORT content as a list of strings, by row."
  (save-excursion
    (let ((width (scxml-required-pixel-width viewport))
          (height (scxml-required-pixel-height viewport)))
      (goto-char (point-min))
      (forward-line height)
      (backward-char)
      (split-string (buffer-substring (point-min) (point))
                    "\n"))))

(defun scxml-begin-replay-session (test-file &optional x-size y-size)
  "Open TEST-FILE in a diagram session, returning the diagram.

Optionally set the root canvas size to X-SIZE by Y-SIZE."
  (let* ((x-size (or x-size 100))
         (y-size (or y-size 40))
         (root (prog2
                   (find-file (scxml-resolve-file test-file))
                   (scxml-read-buffer nil #'scxml--drawable-element-factory)
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
  "Begin a recording session started by loading TEST-FILE.

Recordinng sessions are initialized with a default main canavas
size of 100x40.  Returns the diagram."
  (interactive "fFile: ")
  (let* ((x-size 100)
         (y-size 40)
         (diagram (scxml-begin-replay-session test-file x-size y-size)))
    (setq scxml-recording
          (list (list 'open-with-canvas-size test-file x-size y-size)))
    (scxml-diagram-mode--redraw)
    diagram))

(defun scxml-record-assert-screen ()
  "Insert the current viewport data as an assert into the current recording.

This will only insert the assertion if you are already recording.
The intention of this function is for recording tests."
  (interactive)
  (let ((buffer (scxml-buffer scxml--diagram))
        (viewport (scxml-diagram-viewport scxml--diagram)))
    (when scxml-recording
      (save-excursion
        (push (list 'assert-screen (scxml-capture-screen viewport))
              scxml-recording)
        (scxml-diagram-mode--redraw)))))

(defun scxml-replay-test (recording-file &optional perform-asserts)
  "Replay the scxml-recording file: RECORDING-FILE.

When PERFORM-ASSERTS is non-nil the replay will use the ert
should macro to enforce recorded screen asserts.

If recording-file isn't found it'll try to be resolevd
using (scxml-resolve-file)."
  (interactive "fOpen Recording File: ")
  ;; TODO - this shouldn't be an exists check, it should be an exists
  ;; and is readable check
  (unless (file-exists-p recording-file)
    (error "Unable to open file: %s" recording-file))

  (with-temp-buffer
    (find-file recording-file)
    (scxml-replay-test-in-buffer perform-asserts)))

(defun scxml-replay-test-in-buffer (&optional perform-asserts)
  "Replay the scxml-recording file in the current buffer.

When PERFORM-ASSERTS is non-nil the replay will use the ert
should macro to enforce recorded screen asserts."
  (interactive)
  ;; disable global recording, no need to record if you're replaying.
  (setq scxml-recording nil)
  (let ((test-data (buffer-string))
        (human-mode (called-interactively-p))
        (current-point 0)
        (diagram)
        (step-count 0))
    (cl-loop with last-instruction = nil
             for line-data = (condition-case nil
                                 (read-from-string test-data current-point)
                               (error nil))
             when (null line-data)      ;If this is true there are no more instructions
               return diagram

             ;; Pull the instruction out and prepare for the next instruction read.
             for instruction = (car line-data)
             do (incf step-count)
             do (setq current-point (cdr line-data))
             ;; Execute the instruction
             do (let ((function-name (first instruction)))
                  ;; Instruction router, separate the special
                  ;; instructions from the normal ones.
                  (cond ((eq function-name 'open-with-canvas-size)
                         (setq diagram
                               (apply #'scxml-begin-replay-session (cdr instruction))))
                        ((eq function-name 'assert-screen)
                         (if perform-asserts
                             (message "last instr: %s" last-instruction)
                             (should (equal (cadr instruction)
                                            (scxml-capture-screen (scxml-diagram-viewport diagram))))
                           ;; otherwise, if you're in human-mode, yell about it.
                           (when (and human-mode
                                      (not (equal (cadr instruction)
                                                  (scxml-capture-screen (scxml-diagram-viewport diagram)))))
                             (read-string (format "Failed assert, last instruction: %s" last-instruction)))))
                        (t
                         ;; normal instruction router.
                         (eval instruction)
                         (when human-mode
                           (read-string (format "Command %d complete: %s" step-count instruction))))))
             do (setq last-instruction instruction))))

(provide 'test-recorder)
