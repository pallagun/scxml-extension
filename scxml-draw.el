;;; scxml-draw.el --- scxml drawing functions -*- lexical-binding: t -*-

;;; Commentary:
;; Draw elements to a screen for human eyes.
;;
;; Drawing consists of plotting and rendering, so plot + render = draw
;; In this case, plotting is done - states first, all of them recursing to children.
;; Then it's done for all transitions so
;;  draw:
;;   plot-nodes
;;   plot-links
;;   render

;;; Code:
(require 'artist)
(require 's)

(require 'scxml-element)
(require 'scxml-drawable-element)
(require 'scxml-geometry)
(require 'scxml-canvas)
(require 'scxml-drawing-rect)
(require 'scxml-drawing-arrow)
(require 'scxml-drawing-noshell-rect)
(require 'scxml-drawing-divided-rect)
(require 'scxml-drawing-point)
(require 'scxml-diagram)
(require 'scxml-scratch-render)

(defvar scxml-render-mode 'scxml-scratch
  "The mode you want to use for drawing.  Can be 'scxml-buffer-and-point or 'scxml-scratch")
(defun scxml-toggle-render-mode ()
  ;; TODO - remove buffer-and-point mode entirel.
  "Toggle scxml-render-mode."
  (interactive)
  (setq scxml-render-mode
        (if (eq scxml-render-mode 'scxml-buffer-and-point)
            'scxml-scratch
          'scxml-buffer-and-point))
  (message "SCXML render mode set to %s" (symbol-name scxml-render-mode)))
(defvar scxml-snap-states 't
  "Do or do not snap states whenever they're in automatic mode.

I think this should always be on.  Sometimes you'll get annoying
rounding errors and the arrows and rectangles won't line up
exactly.  There's probably a better way to fix this, but for now
I'm just going to round everything to ints")

(defvar scxml-draw--diagram nil
  "Where the diagram is stored for the diagram buffer.")
(make-variable-buffer-local 'scxml-draw--diagram)

(defvar scxml-draw--buffer-name "*scxml-diagram*"
  ;; TODO - I don't think this is used anymore.
  "Buffer name for scxml drawing.")

;; TODO - I don't think these faces are used anymore?
;; If they are they should be moved out to scratch-render
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

(defun scxml---is-renderable-as-node (element)
  "Return non-nil if ELEMENT can be rendered as a node (non-transition)"
  (or (scxml---is-renderable-as-rect element)
      (scxml-initial-p element)))
(defun scxml---is-renderable-as-rect (element)
  "Return non-nil if ELEMENT can be rendered as a rectangle (state, parallel, final)."
  (or (object-of-class-p element 'scxml-state-type)
      (scxml-parallel-p element)))

;; enhancements to scxml-element& friends to support drawing
(cl-defgeneric scxml--modify-drawing-hint ((element scxml-drawable-element) (move-vector scxml-point))
  ;; TODO - this should move do scxml-drawable-element.el
  "Modify the drawing hint for ELEMENT (within ROOT) by moving it or it's edit-idx by MOVE-VECTOR")
(cl-defmethod scxml--modify-drawing-hint ((scxml scxml-scxml) (move-vector scxml-point))
  "Always returns nil, currently you can't modify these"
  nil)
(cl-defmethod scxml--modify-drawing-hint ((element scxml-drawable-element) (move-vector scxml-point))
  "Move or change ELEMENT by MOVE-VECTOR.

When ELEMENT has an edit-idx highlighted the drawing will be
modified.  When ELEMENT has no edit-idx highlighted the entire
drawing will be moved/displaced.

Will throw if it can't move it."
  (unless (scxml---is-renderable-as-node element)
    (error "Scxml--modify-drawing-hint not tested for this type yet, only state+parallel+final"))
  (let ((rect (scxml-element-drawing element)))
    (when (not (scxml-drawing-noshell-rect-p rect)) ;noshell rects are hintless, they strictly obey their parent.
      (let* ((edit-idx (scxml--edit-idx element))
             (edited-rect (scxml-build-edited-drawing rect
                                                      edit-idx
                                                      move-vector))
             (parent (scxml-parent element))
             (parent-drawing (scxml-element-drawing parent))
             (parent-drawing-canvas (scxml--get-inner-canvas parent-drawing)))
        ;; any element inside of this one must be marked as possibly invalid as well.
        (when (and
               ;; Ensure your edit didn't make you extend outside of your parent.
               (scxml-contains parent-drawing-canvas edited-rect)
               ;; Ensure your edit didn't make you collide with any sibling rectangles
               (not (find-if (lambda (other-child-drawing)
                               (scxml-intersection edited-rect other-child-drawing))
                             (mapcar 'scxml-element-drawing
                                     (seq-filter (lambda (sibling)
                                                   (and (scxml---is-renderable-as-node sibling)
                                                        (not (eq sibling element))))
                                                 (scxml-children (scxml-parent element)))))))

          ;; mark all your children as possibly invalid.
          (scxml-visit element
                       (lambda (child-element)
                         (scxml--set-drawing-invalid child-element 't))
                       'scxml---is-renderable-as-node)

          ;; mark this element as invalid - (should mark all transitions which touch
          ;; this as possibly invalid as well).
          (scxml--set-drawing-invalid element 't)
          (scxml--set-hint element (scxml-build-hint edited-rect parent-drawing-canvas)))))))
(cl-defmethod scxml--modify-drawing-hint ((transition scxml-transition) (move-vector scxml-point))
  "Given some TRANSITION in edit-mode, move the current index by MOVE-VECTOR.

Will throw if it can't move it. will not render!!"
  (let* ((arrow (scxml-element-drawing transition))
         (edit-idx (scxml--edit-idx transition))
         (edited-arrow (scxml-build-edited-drawing arrow
                                                   edit-idx
                                                   move-vector))
         (parent (scxml-parent transition))
         (parent-drawing (scxml-element-drawing parent))
         (parent-drawing-canvas (or (scxml--get-inner-canvas parent-drawing)
                                    (scxml-inner-canvas)))) ;note - injects a totally bogus canvas when the parent is <initial>
    ;; TODO - fix the above note.
    (if edited-arrow
        (with-slots (source target path) edited-arrow
          (setf (scxml-arrow-source arrow) source
                (scxml-arrow-target arrow) target
                (scxml-arrow-path arrow) path)
          (scxml--set-hint transition
                           (scxml-build-hint arrow parent-drawing-canvas))
          (scxml--set-drawing-invalid transition 't)
          't)
      'nil)))
(cl-defgeneric scxml--simplify-drawing-hint ((element scxml-drawable-element))
  ;; TODO - this should be moved out to the scxml-drawable-element.el file.
  "Given a drawable ELEMENT, simplify the drawing hint if it exists.")
(cl-defmethod scxml--simplify-drawing-hint ((state scxml-state))
  "Given a STATE element, simplyfiy the drawing hint (create if needed)."
  (let* ((drawing (scxml-element-drawing state))
         (snapped (scxml-snap drawing))
         (parent (scxml-parent state))
         (parent-drawing (scxml-element-drawing parent))
         (parent-drawing-canvas (scxml--get-inner-canvas parent-drawing)))
    (scxml--set-drawing-invalid state 't)
    (scxml--set-hint state (scxml-build-hint snapped parent-drawing-canvas))))
(cl-defmethod scxml--simplify-drawing-hint ((transition scxml-transition))
  "Given a TRANSITION, simplify the drawing hint if it exists."
  (if (scxml--hint transition)
      (let* ((arrow (scxml-element-drawing transition))
             (parent (scxml-parent transition))
             (parent-drawing (scxml-element-drawing parent))
             (parent-drawing-canvas (scxml--get-inner-canvas parent-drawing)))
        (when (null arrow)
          (error "No drawing set for this transition element"))
        (let ((snapped-arrow (scxml-snap arrow t)))
          (when snapped-arrow
            (let ((new-path (scxml-simplify-path snapped-arrow)))
              (oset arrow source (scxml-arrow-source snapped-arrow))
              (oset arrow target (scxml-arrow-target snapped-arrow))
              (oset arrow path new-path)
              (scxml--set-hint transition
                               (scxml-build-hint arrow parent-drawing-canvas))
              (scxml--set-drawing-invalid transition 't))
            't)))
    (scxml---drawing-logger "Not simplified, no hint exists")
    'nil))

;; primitive functions for pixel related movement/info
(cl-defmethod scxml-draw--goto-pixel ((pixel scxml-pixel))
  "Go to PIXEL location on the screen."
  (goto-char (point-min))
  (with-slots (x y) pixel
    (let ((additional-lines (forward-line y)))
      (when (> additional-lines 0)
        (insert (s-repeat additional-lines "\n"))))
    (move-to-column x 't)))
(cl-defmethod scxml-draw--point-at-pixel ((pixel scxml-pixel))
  "Go to X Y and return buffer point."
  (scxml-draw--goto-pixel pixel)
  (point))
(defun scxml-draw--get-pixel-at-point ()
  "Return the pixel at the current point in the buffer."
  (scxml-pixel :x (current-column)
               :y (- (line-number-at-pos) 1)))
(defun scxml-draw--get-buffer (&optional source-name)
  "Get the drawing buffer, if it doesn't exist then make it."
  (or (get-buffer scxml-draw--buffer-name)
      (let ((buffer (generate-new-buffer (if source-name
                                             (format "<diagram>%s" source-name)
                                           "<diagram>*scxml*"))))
        buffer)))
(cl-defmethod scxml--place-string ((character string) (pixel scxml-pixel) &optional face)
  ;; TODO: I think this is only related to artist mode rendering... so.. move it.
  ;; TODO - delete this entirely.
  (when (and face (not (facep face)))
    (error "Error scxml--place-string: face must be a valid face"))
  (scxml-draw--goto-pixel pixel)
  (delete-char 1)
  (insert character)
  (when face
    (let ((pt (point)))
      (put-text-property (- pt 1) pt 'face face))))
(defun scxml--arrow-head-char (direction)
  ;; I think this in only related to artist mode
  ;; TODO - delete this entirely.
  "Return an arrow string (with a single char in it) for the DIRECTION provided."
  (cond ((eq direction 'left) "<")
        ((eq direction 'right) ">")
        ;; these two are backwards because coord systems flip between
        ;; normal cartesian and image coordinates
        ((eq direction 'up) "v")
        ((eq direction 'down) "^")
        ('t (error "Invalid direction for arrow head"))))

;; face functions
(defun scxml-draw--set-sline-face (artist-sline face)
  "Set the FACE for for the given ARTIST-SLINE."
  ;; TODO - delete this.
  ;; todo - apparently this function is super slow?
  ;; will be of the form  '([46 33] [50 33] [46 33 5 0 32 32 32 32 32]))
  ;; or (first point, last point, [...firstpoint, char length, ??, ...chars replaced])
  (let* ((start (car artist-sline))
         (start-x (elt start 0))
         (start-y (elt start 1))
         (end (cadr artist-sline))
         (end-x (elt end 0))
         (end-y (elt end 1)))
    (cond
     ;; horizontal line
     ((eq start-y end-y)
      (let ((start-pos (scxml-draw--point-at-pixel
                        (scxml-pixel :x (min start-x end-x) :y start-y)))
            (end-pos (scxml-draw--point-at-pixel
                      (scxml-pixel :x (max start-x end-x) :y end-y))))
        (put-text-property start-pos (+ 1 end-pos) 'face face)))
     ;; vertical line
     ((eq start-x end-x)
      (mapc (lambda (y)
              (let ((pt (scxml-draw--point-at-pixel (scxml-pixel :x start-x :y y))))
                (put-text-property pt (+ 1 pt) 'face face)))
            (number-sequence (min start-y end-y) (max start-y end-y))))
     ('t
      (error "Unable to set face on a non-cardinal line")))))
(defun scxml-draw--set-rect-face (artist-rect outline-face)
  ;; TODO - delete this.
  "set the face(s ?) for an artist rectangle."
  ;; will be of the form ( [top-left xy] [bottom-rigth xy] ( (edge-sline) (edge-sline) ...))
  (mapc (lambda (edge-sline)
          (scxml-draw--set-sline-face edge-sline outline-face))
        (elt artist-rect 2)))

;
;; rectangle helper
(cl-defmethod scxml-artist-render ((rect scxml-drawing-rect) (canvas scxml-canvas))
  "Draw this rectangle on the canvas."
  ;; TODO - delet this outright.
  (when (not (scxml-canvas-p canvas))
    (error "Unable to render to an interior canvas"))
  (let ((name (scxml-rectangle-name rect))
        ;; (edit-idx (scxml-drawing-edit-idx rect))
        (face (if (scxml-drawing-highlight rect)
                  'scxml-highlight
                'scxml-state-outline)))
    (let* ((pixel-TL (scxml-get-pixel canvas (scxml-TL rect)))
           (pixel-BR (scxml-get-pixel canvas (scxml-BR rect)))
           (x-min (scxml-x pixel-TL))
           (y-min (scxml-y pixel-TL))
           (x-max (scxml-x pixel-BR))
           (y-max (scxml-y pixel-BR)))
      (when (not (scxml-drawing-noshell-rect-p rect))
        (oset rect artist-data (artist-draw-rect x-min y-min x-max y-max))
        (scxml-draw--set-rect-face (scxml-drawing-artist-data rect) face))
      (when (scxml-drawing-edit-idx rect)
        ;; draw X's on the corners, draw arrow heads at the segment midpoints
        (scxml--place-string "X" (scxml-pixel :x x-min :y y-min) 'scxml-drawing-edit)
        (scxml--place-string "X" (scxml-pixel :x x-min :y y-max) 'scxml-drawing-edit)
        (scxml--place-string "X" (scxml-pixel :x x-max :y y-min) 'scxml-drawing-edit)
        (scxml--place-string "X" (scxml-pixel :x x-max :y y-max) 'scxml-drawing-edit)
        (let* ((pixel-centroid (scxml-get-pixel canvas (scxml-centroid rect)))
               (x-avg (scxml-x pixel-centroid))
               (y-avg (scxml-y pixel-centroid)))
          (scxml--place-string (scxml--arrow-head-char 'up)
                               (scxml-pixel :x x-avg :y y-max) 'scxml-drawing-edit)
          (scxml--place-string (scxml--arrow-head-char 'down)
                               (scxml-pixel :x x-avg :y y-min) 'scxml-drawing-edit)
          (scxml--place-string (scxml--arrow-head-char 'right)
                               (scxml-pixel :x x-max :y y-avg) 'scxml-drawing-edit)
          (scxml--place-string (scxml--arrow-head-char 'left)
                               (scxml-pixel :x x-min :y y-avg) 'scxml-drawing-edit)))
      (if name
          (progn (goto-line (+ 2 y-min))
                 (forward-char (+ 2 x-min))
                 (insert name)
                 (delete-char (length name)))))))
(cl-defmethod scxml-artist-render ((arrow scxml-arrow) (canvas scxml-canvas))
  ;; TODO - delete this entirely.
  "Draw this arrow on the canvas."
  (when (not (scxml-canvas-p canvas))
    (error "Unable to render to an interior canvas"))
  (let* ((highlight (scxml-drawing-highlight arrow))
         (face (if highlight 'scxml-highlight 'scxml-arrow))
         (face-head (if highlight 'scxml-highlight 'scxml-arrow-head))
         (artist-slines 'nil)
         (last-pixel 'nil))
    (mapc (lambda (pt)
            (let ((pixel (scxml-get-pixel canvas pt)))
              (when last-pixel
                (let ((sline (artist-draw-sline
                              (scxml-x last-pixel)
                              (scxml-y last-pixel)
                              (scxml-x pixel)
                              (scxml-y pixel))))
                  (push sline artist-slines)
                  (scxml-draw--set-sline-face sline face)))
              (setq last-pixel pixel)))
          (scxml--full-path arrow scxml-arrow-connector-offset))
    (oset arrow artist-data artist-slines)
    ;; place an arrow head on there too
    (let* ((edge (scxml-node-edge (scxml-arrow-target arrow))))
      ;; TODO - replace with scxml--arrow-head
      (scxml--place-string (scxml--arrow-head-char (scxml--direction-inverse edge))
                           last-pixel
                           face-head))
    (when (scxml-drawing-edit-idx arrow)
      (seq-do (lambda (pix)
                (scxml--place-string "X"
                                     pix
                                     'scxml-drawing-edit))
              (mapcar (lambda (point)
                        (scxml-get-pixel canvas point))
                      (scxml--full-path arrow))))))

(cl-defmethod scxml---get-canvas-divisions ((rectangle scxml-rect) (num-child-nodes integer))
  "First arg is scxml-rect type to catch scxml-drawing-rect as well as scxml-drawing-null"
  ;; TODO - this should probably be moved to the rect drawing file?
  (let* ((num-columns (ceiling (sqrt num-child-nodes)))
         (num-rows (ceiling (/ (float num-child-nodes) (float num-columns)))))
    (scxml--split-canvas (scxml--get-inner-canvas rectangle)
                         num-rows
                         num-columns
                         10.0
                         4.0)))
(cl-defmethod scxml---get-canvas-divisions ((rectangle scxml-drawing-divided-rect) (num-child-nodes integer))
  ;; TODO - this should be moved out into the drawing file.
  (mapcar (lambda (division)
            (let ((sub-rect (cdr division)))
              (with-slots (x-min x-max y-min y-max) sub-rect
                (scxml-inner-canvas :x-min x-min
                                    :y-min y-min
                                    :x-max x-max
                                    :y-max y-max
                                    :drawing rectangle))))
          (scxml-get-divisions rectangle)))

(cl-defmethod scxml--plot-node ((element scxml-drawable-element) (canvas scxml-canvas))
  "Plot rectangular elements (and any child elements), phase 1 of plotting."
  (when (not (or (scxml---is-renderable-as-node element) (scxml-scxml-p element)))
    (error "Wat?  shouldn't be calling this with thtat")) ;TODO - remove this check at some point?
  (scxml---drawing-logger "scxml--plot-node: type/id: %s/%s"
                          (scxml-xml-element-name element)
                          (scxml-element-id element))
  (scxml---drawing-logger "scxml--plot-node: canvas: %s" (scxml-print canvas))
  (let ((child-nodes (seq-filter 'scxml---is-renderable-as-node (scxml-children element)))
        (node (scxml--update-drawing element canvas)))
    (scxml---drawing-logger "scxml--plot-node: has-hint: %s" (when (scxml--hint element) t))
    (when (and scxml-snap-states             ;TODO - if this is going to be here it should probably be called snap-rectangles.
               (not (scxml-drawing-noshell-rect-p node))
               (not (scxml-drawing-point-p node))
               (not (scxml-drawing-null-p node))
               (null (scxml--hint element))) ;apparently don't snap for hinted stuff, that will cause issues.
      (let ((snapped (scxml-snap-shrink node)))
        (scxml---drawing-logger "scxml--plot-node: snapping %s\nscxml--plot-node: snapped  %s" (scxml-print node) (scxml-print snapped))
        (with-slots (x-min x-max y-min y-max) snapped
          (setf (scxml-x-min node) x-min
                (scxml-x-max node) x-max
                (scxml-y-min node) y-min
                (scxml-y-max node) y-max))))
    (scxml---drawing-logger "\tDrawing: %s" (scxml-print node))
    (when child-nodes
      (let ((divided-canvases (scxml---get-canvas-divisions node
                                                            (length child-nodes))))
      ;; (cond ((scxml-parallel-p element)
      ;;        (error "Error????"))
      ;;       ('t                         ; scxml-state and scxml-scxml
      ;;        (let* ((num-child-nodes (length child-nodes))
      ;;               (num-columns (ceiling (sqrt num-child-nodes)))
      ;;               (num-rows (ceiling (/ num-child-nodes num-columns)))
      ;;               (divided-canvases (scxml--split-canvas (scxml--get-inner-canvas node)
      ;;                                                      num-rows
      ;;                                                      num-columns
      ;;                                                      10.0
        ;;                                                      4.0)))
        ;; TODO: remove this when-error check once you can trust scmxl---get-canvas-divisions.
        (cl-loop for child in child-nodes
                 while child
                 for sub-canvas in divided-canvases
                 do (scxml--plot-node child sub-canvas))))))
(cl-defmethod scxml--plot-links ((start scxml-element) (canvas scxml-canvas))
  "Plot all links for the start element on canvas"

  (cl-flet ((make-connector-for
             (transition drawing destination-drawing)
             (if (null drawing)
                 ;; this connector won't be connected, make a dangling connector.
                 (scxml-drawing-connector-dangling :point) ;; (scxml-centroid (scxml-element-drawing (scxml-parent transition))))

               (let* ((target-point (scxml-centroid (or destination-drawing (scxml-element-drawing (scxml-parent transition)))))

                      (edge-enumerator (scxml-leaving-segment-collision-edge drawing target-point)))
                 (cond ((object-of-class-p drawing 'scxml-drawing-rect)
                        (scxml-drawing-connector-rect :node drawing :edge edge-enumerator))
                       ((scxml-drawing-point-p drawing)
                        (scxml-drawing-connector-point :node drawing :exit-direction edge-enumerator))
                       ('t
                        (error "Unknown drawing type, unable to make connector"))))))
            (set-dangling-connector-positions
             ;; this function will set the location of a dangling connector for target connections only.
             (arrow-drawing)
             (let ((target-connector (scxml-arrow-target arrow-drawing)))
               (when (object-of-class-p target-connector 'scxml-drawing-connector-unconnected)
                 (let* ((source-connector (scxml-arrow-source arrow-drawing))
                        (exit-direction (scxml-exit-direction source-connector)))
                   ;; always set the direction.
                   (scxml-set-terminal-direction target-connector exit-direction)
                   (when (null (scxml-dangling-point target-connector))
                     ;; no point is set, must set it.
                     (let ((exit-vector (scxml-vector-from-direction exit-direction)))
                       (scxml-set-point target-connector (scxml-add
                                                      (scxml-connection-point source-connector)
                                                      (scxml-scaled exit-vector 2.0))))))))))
    (let* ((all-transitions (scxml-collect start 'scxml-transition-p))
           (need-drawings (seq-filter (lambda (transition)
                                        (or (not (scxml-element-drawing transition))
                                            (scxml--drawing-invalid? transition)))
                                      all-transitions))
           ;; (by-drawing-and-edge nil)
           (by-state-and-edge nil)
           ;; (by-state (make-hash-table))
           )

      ;; this mapc will build all arrows but will _not_ fill in the path.
      ;; and the connector parametrics won't be set for automatic arrows
      ;; (hinted arrows will still use their hinted connector parametrics)
      (mapc (lambda (transition)
              (cl-flet ((make-arrow
                         (source-connector target-connector locked)
                         (scxml-arrow :source source-connector
                                      :target target-connector
                                      :parent transition
                                      :highlight (scxml--highlight transition)
                                      :edit-idx (scxml--edit-idx transition)
                                      :locked locked))
                        ;; (collect-unparameterized-connectors2
                        ;;  (connector transition drawing)
                        ;;  ;; If this connector is connected to a rectangle it must have an
                        ;;  ;; edge and a parametric for that edge set.  Parametrics are set
                        ;;  ;; later on so I'll need to collect all unparameterized connectors
                        ;;  ;; for later assignment.  I'm collecting them by state(element)
                        ;;  ;; and edge
                        ;;  (when (scxml-drawing-connector-rect-p connector)
                        ;;    (let* ((edge (scxml-node-edge connector))
                        ;;           (key (cons drawing edge))
                        ;;           (cell (assoc key by-state-and-edge)))
                        ;;      (if cell
                        ;;          (setcdr cell (cons transition (cdr cell)))
                        ;;        (setq by-drawing-and-edge
                        ;;              (cons (cons key (list transition)) by-drawing-and-edge))))))
                        (collect-unparameterized-connectors
                         (connector transition element)
                         ;; If this connector is connected to a rectangle it must have an
                         ;; edge and a parametric for that edge set.  Parametrics are set
                         ;; later on so I'll need to collect all unparameterized connectors
                         ;; for later assignment.  I'm collecting them by state(element)
                         ;; and edge
                         (when (scxml-drawing-connector-rect-p connector)
                           (let* ((edge (scxml-node-edge connector))
                                  (key (cons element edge))
                                  (cell (assoc key by-state-and-edge)))
                             (if cell
                                 (setcdr cell (cons transition (cdr cell)))
                               (setq by-state-and-edge
                                     (cons (cons key (list transition)) by-state-and-edge)))))))
                ;; group transitions by the scxml-states they touch.
                (let ((source (scxml-source transition))
                      (target (scxml-target transition)))
                  (let ((source-drawing (scxml-element-drawing source))
                        (target-drawing (and target (scxml-element-drawing target)))
                        (hint (scxml--hint transition)))
                    (if hint
                        ;; when there's a hint just set up all parts of the connectors
                        (oset transition
                              drawing
                              (make-arrow (scxml-build-arrow-connector (scxml-source hint) source-drawing)
                                          (scxml-build-arrow-connector (scxml-target hint) target-drawing)
                                          't))
                      ;; when there's no hint do not set parametric part of the connectors,
                      ;; that'll get handled in a subsequent foreach (played by a maphash)
                      ;; place a partially filled drawing here.
                      ;; still needs connector parametrics and a path
                      (let ((source-connector (make-connector-for transition source-drawing target-drawing))
                            (target-connector (make-connector-for transition target-drawing source-drawing)))
                        (oset transition
                              drawing
                              (make-arrow source-connector target-connector 'nil))
                        ;; (collect-unparameterized-connectors2 source-connector transition source-drawing)
                        ;; (collect-unparameterized-connectors2 target-connector transition target-drawing)
                        (collect-unparameterized-connectors source-connector transition source) ;; source-edge)
                        (collect-unparameterized-connectors target-connector transition target) ;; target-edge)g
                        ))))))
            need-drawings)
      ;; now set the connector parametrics for unhinted transitions that you collected previously
      (mapc (lambda (cell)
              (let ((state (caar cell))
                    ;; (edge (cdar cell))
                    (transitions (cdr cell)))
                (let ((num-transitions (length transitions))
                      (idx 0.0))
                  ;; set each transitions connector sequetially PER EDGE and in proportion
                  ;; this feels like it's leading to an optimization problem.
                  (mapc (lambda (transition)
                          (incf idx 1.0)
                          (let* ((drawing (scxml-element-drawing transition))
                                 (connector (if (eq (scxml-source transition) state)
                                                (scxml-arrow-source drawing)
                                              (scxml-arrow-target drawing))))
                            (oset connector
                                  parametric
                                  (/ idx (+ 1.0 num-transitions)))))
                        transitions))))
            by-state-and-edge)
      ;; now you should be able to flesh out the paths
      ;; and then mark them as valid drawings
      (mapc (lambda (transition)
              (let ((hint (scxml--hint transition)))
                (set-dangling-connector-positions (oref transition drawing))
                (if hint
                    ;; handle hinted paths
                    (scxml--set-path-from-hint (oref transition drawing) hint)
                  ;; handle automatic paths
                  (scxml--arrow-set-default-path (oref transition drawing)))
                (scxml--set-drawing-invalid transition 'nil)))
            need-drawings))))
(cl-defmethod scxml-plot ((diagram scxml-diagram))
  "Plot out DIAGRAM (ensure all drawable elements have valid drawings"
  (let ((start-node (float-time))
        (start-link 'nil)
        (end-time 'nil))
    (with-slots (canvas root (element display-element)) diagram
      (scxml--plot-node element canvas)
      (setq start-link (float-time))
      (scxml--plot-links element canvas)
      (setq end-time (float-time))
      (let ((node-time (- start-link start-node))
            (link-time (- end-time start-link)))
        (scxml---drawing-logger "scxml-plot (node: %.5f s, link: %.5f s, total: %.5f)"
                                node-time
                                link-time
                                (+ node-time link-time)))))
    diagram)
(cl-defmethod scxml-draw ((diagram scxml-diagram))
  "Render a DIAGRAM in some buffer"
  (scxml-plot diagram)
  (let ((start-time (float-time)))
    (with-slots (canvas root buffer viewport (element display-element)) diagram
      (switch-to-buffer buffer)
      (setq-local scxml-draw--diagram diagram)
      (cond ((eq scxml-render-mode 'scxml-buffer-and-point)
             ;; TODO - completely remove this drawing mode.
             (scxml-canvas-clear canvas)
             (scxml-visit root
                          (lambda (e) (scxml-artist-render (scxml-element-drawing e) canvas))
                          (lambda (e) (scxml---is-renderable-as-node e)))

             ;; draw non-highlighted transitons first so they aren't obscured by other
             ;; non-highlighted transitions
             (scxml-visit root
                          (lambda (e) (scxml-artist-render (scxml-element-drawing e) canvas))
                          (lambda (e) (and (scxml-transition-p e)
                                           (not (scxml--highlight e)))))
             (scxml-visit root
                          (lambda (e) (scxml-artist-render (scxml-element-drawing e) canvas))
                          (lambda (e) (and (scxml-transition-p e)
                                           (scxml--highlight e))))
             )
            ((eq scxml-render-mode 'scxml-scratch)
             (let ((scratch (scxml--get-scratch viewport)))
               ;; erase the buffer
               (goto-char (point-min))
               (delete-char (- (point-max) (point-min)) nil)

               ;; draw states, finals, parallels
               (scxml-visit root
                            (lambda (e)
                              (let ((drawing (scxml-element-drawing e)))
                                (scxml---drawing-logger "scxml-draw element:%s\n\tdrawing: %s\n"
                                                        (when e (scxml-print e))
                                                        (when drawing (scxml-print drawing)))
                                (progn
                                  (when (object-of-class-p drawing 'scxml-drawing-divided-rect)
                                    (scxml--scratch-dividers scratch
                                                             viewport
                                                             (scxml-dividers drawing)))
                                  (scxml--scratch-rect scratch
                                                       viewport
                                                       drawing
                                                       ;; if it's a noshell-rect don't draw the actual rectangle
                                                       (scxml-drawing-noshell-rect-p drawing)))))
                            'scxml---is-renderable-as-rect)

               ;; draw non-highlighted transitons first so they aren't obscured by other
               ;; non-highlighted transitions
               (scxml-visit root
                            (lambda (e)
                              (scxml--scratch-arrow scratch
                                                    viewport
                                                    (scxml-element-drawing e)))
                            (lambda (e) (and (scxml-transition-p e)
                                             (not (scxml--highlight e)))))
               (scxml-visit root
                            (lambda (e)
                              (scxml--scratch-point-label scratch
                                                          viewport
                                                          (scxml-element-drawing e)))
                            'scxml-initial-p)
               (scxml-visit root
                            (lambda (e)
                              (scxml--scratch-arrow scratch
                                                     viewport
                                                     (scxml-element-drawing e)))
                            (lambda (e) (and (scxml-transition-p e)
                                             (scxml--highlight e))))

               (scxml--scratch-write scratch)))
            ('t
             (error "Invalid scxml-render-mode")))
      (scxml---drawing-logger "scxml-draw %.5f ms" (- (float-time) start-time))
      diagram)))

(provide 'scxml-draw)
;;; scxml-draw.el ends here
