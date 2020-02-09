;;; scxml-scratch-render.el --- Rendering to a scratch array  -*- lexical-binding: t -*-

;;; Commentary:
;; welp, this is going to be bad.
;; Scratch coordinates are *not* pixel coordinates
;; Pixel coordinates are integers with origin top left.
;; scratch cordinates are integers with origin bottom left.
;;  - and i'll even let you use floats as scratch coordinates
;;    cause I'm gonna round 'em for you.

;;; Code:

(require 'scxml-geometry)

(defface scxml-highlight
  '((t :foreground "yellow"))
  "scxml highlight color."
  :group 'scxml-faces)
(defface scxml-drawing-edit
  '((t :foreground "light green"))
  "scxml drawing edit point/feature face"
  :group 'scxml-faces)
(defface scxml-state-outline
  '((t :foreground "red"))
  "scxml-state outlines default."
  :group 'scxml-faces)
(defface scxml-arrow
  '((t :foreground "blue"))
  "it's the face for scxml-arrows?"
  :group 'scxml-faces)
(defface scxml-arrow-head
  '((t :foreground "light blue"))
  "it's the face for the arrowhhead of an scxml arrow"
  :group 'scxml-faces)

(defconst scxml---vertical (get-byte 0 "|"))
(defconst scxml---horizontal (get-byte 0 "-"))
(defconst scxml---cross (get-byte 0 "+"))
(defconst scxml---x (get-byte 0 "X"))
(defconst scxml---arrow-up (get-byte 0 "^"))
(defconst scxml---arrow-down (get-byte 0 "v"))
(defconst scxml---arrow-left (get-byte 0 "<"))
(defconst scxml---arrow-right (get-byte 0 ">"))
(defconst scxml---divider (get-byte 0 "."))

;; scratch is y-major
(defun scxml---scratch-debug (scratch &optional x y)
  "Human visible debug message."
  (let* ((y-size (length scratch))
         (x-size (length (elt scratch 0)))
         (prefix (format "Scratch dim[%s, %s]" x-size y-size)))
    (if (and x y)
        (format "%s coord[%s, %s]" prefix x y)
      prefix)))
(defun scxml---scratch-coord-valid-p (scratch x y &optional build-message)
  "return nil if it's a valid coordinate, return a debug string if it's not."
  (let* ((y-size (length scratch))
         (x-size (length (elt scratch 0))))
    (when (and (<= 0 x (1- x-size))
             (<= 0 y (1- y-size)))
        t)))
(defun scxml---scratch-size-x (scratch)
  (length (elt scratch 0)))
(defun scxml---scratch-size-y (scratch)
  (length scratch))

(defun scxml---scratch-factory (x-size y-size)
  "Given an X-SIZE and Y-SIZE build a new scratch."
  (let ((scratch (make-vector y-size 'nil)))
    (cl-loop for y-idx from 0 to (1- y-size)
             do (setf (elt scratch y-idx) (make-vector x-size 'nil))
             finally return scratch)))
(defun scxml---scratch-elt (scratch x y)
  "Get the element from SCRATCH at X and Y coordinates."
  (elt (elt scratch y) x))
(defun scxml---scratch-label (scratch x y string &optional style)
  "Write a label STRING to SCRATCH starting at X Y with STYLE."
  ;; this When is the check, remove to get back to normal
  (when (and string
             (<= 0 y (1- (scxml---scratch-size-y scratch))))
    (let ((x-pos x)
          (x-max (1- (scxml---scratch-size-x scratch))))
      (mapc (lambda (char)
              ;; Here's the old unchecked version
              ;; (scxml---scratch-set scratch (incf x-pos) y char style)
              (let ((x x-pos))
                (when (<= 0 x x-max)
                  (scxml---scratch-set scratch x y char style)))
              (incf x-pos))
            string))))
(defun scxml---scratch-set (scratch x y char &optional style)
  "Set scratch-pixel in SCRATCH at X Y to be CHAR optionally with STYLE."
  ;; TODO - should I use aset here?
  (setf (elt (elt scratch y) x)
        (cons char style)))
(defun scxml---scratch-overlay (scratch x y char &optional style)
  "Overlay CHAR onto SCRATCH at X Y with optional STYLE."
  (let ((extant (car (elt (elt scratch y) x))))
    (setf (elt (elt scratch y) x)
          (cons (cond
                 ;; if it's empty just put it there
                 ;; if it's an arrow or an X it's top priority
                 ((or (eq extant 'nil)
                      (eq char scxml---x)
                      (eq char scxml---arrow-up)
                      (eq char scxml---arrow-down)
                      (eq char scxml---arrow-left)
                      (eq char scxml---arrow-right))
                  char)
                 ;; two perpendicular lines .
                 ((or (and (or (eq extant scxml---vertical)
                               (eq extant scxml---cross))
                           (eq char scxml---horizontal))
                      (and (or (eq extant scxml---horizontal)
                               (eq extant scxml---cross))
                           (eq char scxml---vertical)))
                  scxml---cross)
                 ;; I guess just slap it on there?
                 ('t char))
                style))))
(defun scxml---scratch-line (scratch x-start y-start x-end y-end &optional char style)
  "Place a line from X-START/Y-START to X-END/Y-END on SCRATCH with optional STYLE."
  (cond ((equal x-start x-end)
         ;; Vertical!
         (if (<= y-start y-end)
             (scxml---scratch-line-vert scratch
                                        x-start
                                        y-start
                                        y-end
                                        (or char scxml---vertical)
                                        style)
           (scxml---scratch-line-vert scratch
                                      x-start
                                      y-end
                                      y-start
                                      (or char scxml---vertical)
                                      style)))
        ((equal y-start y-end)
         ;; horizontal
         (if (<= x-start x-end)
             (scxml---scratch-line-hori scratch
                                        x-start
                                        x-end
                                        y-start
                                        (or char scxml---horizontal)
                                        style)
           (scxml---scratch-line-hori scratch
                                      x-end
                                      x-start
                                      y-start
                                      (or char scxml---horizontal)
                                      style)))
        ('t
         (error "Non cardinal direction line in scxml---scratch-line (%s, %s) -> (%s, %s)"
                x-start y-start x-end y-end))))
(defun scxml---scratch-line-vert (scratch x y-min y-max char &optional style)
  "Place a vertical line at X (from Y-MIN to Y-MAX) on SCRATCH with optional STYLE."
  (if (<= 0 x (1- (scxml---scratch-size-x scratch)))
      (let ((y-start (max 0 y-min))
            (y-end (min y-max (1- (scxml---scratch-size-y scratch)))))
        (cl-loop for y from y-start to y-end
                 do (scxml---scratch-overlay scratch x y char style))))
  ;; Here's the old unchecked version
  ;; (cl-loop for y from y-min to y-max
  ;;          do (scxml---scratch-overlay scratch x y char style))
  )
(defun scxml---scratch-line-hori (scratch x-min x-max y char &optional style)
  "Place a horizontal line at Y (from X-MIN to X-MAX) on SCRATCH with optional STYLE."
  (if (<= 0 y (1- (scxml---scratch-size-y scratch)))
      (let ((x-start (max 0 x-min))
            (x-end (min x-max (1- (scxml---scratch-size-x scratch)))))
        (cl-loop for x from x-start to x-end
                 do (scxml---scratch-overlay scratch x y char style))))
  ;; here's the old uncheckd version
  ;; (cl-loop for x from x-min to x-max
  ;;          do (scxml---scratch-overlay scratch x y char style))
  )

(defconst scxml---scratch-rect-edit-point-bytes
  (list scxml---x
        scxml---arrow-down
        scxml---x
        scxml---arrow-right
        scxml---x
        scxml---arrow-up
        scxml---x
        scxml---arrow-left
        scxml---x)
  "If you're rendering edit idxs for a rect, this is the order you'll need to do that in.")
(defun scxml---scratch-fit-string (string max-chars)
  "Given a STRING, shorten it so it is at most MAX-CHARS"
  (if (<= max-chars 0)
    nil
    (let ((strlen (length string)))
      (if (<= strlen max-chars)
          string
        (substring string 0 max-chars)))))

(defun scxml--get-scratch (viewport)
  "Given a VIEWPORT - fire out a scratch."
  (scxml---scratch-factory (ceiling (scxml-required-pixel-width viewport))
                           (ceiling (scxml-required-pixel-height viewport))))
(defun scxml--scratch-dividers (scratch viewport divider-list)
  "Place DIVIDER-LIST of 2dg-segments on to SCRATCH for VIEWPORT."
  (let* ((transformers (scxml-get-scratch-int-transformers viewport))
         (x-transformer (car transformers))
         (y-transformer (cdr transformers)))
    (mapc (lambda (divider-segment)
            (with-slots (start end) divider-segment
              (scxml---scratch-line scratch
                                    (funcall x-transformer (2dg-x start))
                                    (funcall y-transformer (2dg-y start))
                                    (funcall x-transformer (2dg-x end))
                                    (funcall y-transformer (2dg-y end))
                                    scxml---divider)))
          divider-list)))
(defun scxml--scratch-rect (scratch viewport rect &optional exclude-lines)
  "Place RECT (an scxml-drawing-rect) on to SCRATCH for VIEWPORT, optionally EXCLUDE-LINES."
  (scxml--drawing-logger "scratch rendering id: %s" (scxml-name rect))
  (let* ((transformers (scxml-get-scratch-int-transformers viewport))
         (x-transformer (car transformers))
         (y-transformer (cdr transformers)))
    (let ((x-min (funcall x-transformer (2dg-x-min rect)))
          (x-max (funcall x-transformer (2dg-x-max rect)))
          (y-min (funcall y-transformer (2dg-y-min rect)))
          (y-max (funcall y-transformer (2dg-y-max rect)))
          (highlight (scxml-drawing-highlight rect)))
      (when (or (null exclude-lines) highlight)
        (let ((style (if (scxml-drawing-highlight rect)
                         'scxml-highlight
                       'scxml-state-outline)))
          (scxml---scratch-line-vert scratch x-min y-min y-max scxml---vertical style)
          (scxml---scratch-line-vert scratch x-max y-min y-max scxml---vertical style)
          (scxml---scratch-line-hori scratch x-min x-max y-min scxml---horizontal style)
          (scxml---scratch-line-hori scratch x-min x-max y-max scxml---horizontal style))
        (when (scxml-drawing-edit-idx rect)
          (cl-loop for point in (scxml-edit-idx-points rect)
                   for char in scxml---scratch-rect-edit-point-bytes
                   for x = (funcall x-transformer (2dg-x point))
                   for y = (funcall y-transformer (2dg-y point))
                   do (scxml---scratch-set scratch
                                           x
                                           y
                                           char
                                           'scxml-drawing-edit))))
      ;; always draw the rect name
      (scxml---scratch-label scratch
                             (1+ x-min)
                             (1- y-max)
                             (scxml---scratch-fit-string (scxml-name rect)
                                                         (- x-max x-min 2))
                             'nil))))
(defun scxml--scratch-point-label (scratch viewport pt-label)
  "Place PT-LABEL (an scxml-drawing-point) on to SCRATCH for VIEWPORT.

Right now this only properly works with single character labels."
  (scxml--drawing-logger "scratch rendering id: %s" (scxml-print pt-label))
  (scxml--drawing-logger "\tLabel (should not length === 1): %s" (scxml-label pt-label))
  (let ((style (if (scxml-drawing-highlight pt-label)
                   'scxml-highlight
                 nil))
        (transformers (scxml-get-scratch-int-transformers viewport)))
    (let ((x-transformer (car transformers))
          (y-transformer (cdr transformers)))
      (scxml---scratch-label scratch
                             (funcall x-transformer (2dg-x pt-label))
                             (funcall y-transformer (2dg-y pt-label))
                             (scxml-label pt-label)
                             style))))
(defun scxml--scratch-arrow (scratch viewport arrow)
  "Place ARROW (an scxml-drawing-arrow) on to SCRATCH for VIEWPORT with optional STYLE."
  (let ((points (scxml--full-path arrow (scxml-get-point-scaling viewport)))
        (line-style (if (scxml-drawing-highlight arrow)
                        'scxml-highlight
                      'scxml-arrow))
        (head-style (if (scxml-drawing-highlight arrow)
                        'scxml-highlight
                      'scxml-arrow-head))
        (transformers (scxml-get-scratch-int-transformers viewport)))
    (let ((x-transformer (car transformers))
          (y-transformer (cdr transformers)))
      (let ((last-pt-x (funcall x-transformer (2dg-x (first points))))
            (last-pt-y (funcall y-transformer (2dg-y (first points))))
            (double-pt-x 'nil)
            (double-pt-y 'nil))
        (cl-loop for pt in (cdr points)
                 do (let ((pt-x (funcall x-transformer (2dg-x pt)))
                          (pt-y (funcall y-transformer (2dg-y pt))))
                      (scxml---scratch-line scratch
                                            last-pt-x
                                            last-pt-y
                                            pt-x
                                            pt-y
                                            nil
                                            line-style)
                      (setf double-pt-x last-pt-x
                            double-pt-y last-pt-y)
                      (setf last-pt-x pt-x
                            last-pt-y pt-y)))
        ;; now do the arrow.
        (let ((terminal-direction (scxml-to-node-direction
                                   (scxml-arrow-target arrow))))
          (when (scxml---scratch-coord-valid-p scratch last-pt-x last-pt-y)
            (scxml---scratch-set scratch
                                 last-pt-x
                                 last-pt-y
                                 (cond
                                  ((eq terminal-direction 'up)
                                   scxml---arrow-up)
                                  ((eq terminal-direction 'down)
                                   scxml---arrow-down)
                                  ((eq terminal-direction 'left)
                                   scxml---arrow-left)
                                  ((eq terminal-direction 'right)
                                   scxml---arrow-right)
                                  (t
                                   (error "Unknown terminal direction: %s"
                                          terminal-direction)))
                                 head-style)))
        (when (and nil (scxml---scratch-coord-valid-p scratch last-pt-x last-pt-y))
          ;; old head rendering code
          (if (equal last-pt-x double-pt-x)
              ;; x is equal, this is a vertical line.
              (scxml---scratch-set scratch
                                   last-pt-x
                                   last-pt-y
                                   (if (>= last-pt-y double-pt-y)
                                       scxml---arrow-up
                                     scxml---arrow-down)
                                   head-style)
            ;; y is equal (probably), this is a horizontal line.
            (scxml---scratch-set scratch
                                 last-pt-x
                                 last-pt-y
                                 (if (>= last-pt-x double-pt-x)
                                     scxml---arrow-right
                                   scxml---arrow-left)
                                 head-style)))
        ;; now handle the edit-points if they're supposed to be visible
        (when (scxml-drawing-edit-idx arrow)
          (cl-loop for point in (scxml-edit-idx-points arrow)
                   for x = (funcall x-transformer (2dg-x point))
                   for y = (funcall y-transformer (2dg-y point))
                   when (scxml---scratch-coord-valid-p scratch x y)
                   do (scxml---scratch-set scratch
                                           x
                                           y
                                           scxml---x
                                           'scxml-drawing-edit)))
        ))))
(defun scxml--scratch-write (scratch)
  "Write out SCRATCH contents to current point."
  (let ((size-y (length scratch))
        (size-x (length (elt scratch 0)))
        (default-char (get-byte 0 " ")))
    (cl-loop for y from (1- size-y) downto 0
             for y-row = (elt scratch y)
             do (cl-loop for x from 0 to (1- size-x)
                         for pixel-data = (elt y-row x)
                         do (insert (or (car pixel-data) default-char))
                         do (when (cdr pixel-data)
                              (let ((pos (point)))
                                (put-text-property (1- pos)
                                                   pos
                                                   'face
                                                   (cdr pixel-data))))
                         finally (insert "\n")))))

(provide 'scxml-scratch-render)
;;; scxml-scratch-render.el ends here
