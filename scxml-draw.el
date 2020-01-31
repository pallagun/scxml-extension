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
(require 'scxml-element)
(require 'scxml-drawable-element)
(require 'scxml-geometry)
(require 'scxml-canvas)
(require 'scxml-drawing-null)
(require 'scxml-drawing-rect)
(require 'scxml-drawing-arrow)
(require 'scxml-drawing-noshell-rect)
(require 'scxml-drawing-divided-rect)
(require 'scxml-drawing-point)
(require 'scxml-diagram)
(require 'scxml-scratch-render)

(defvar scxml-draw--diagram nil
  "Where the diagram is stored for the diagram buffer.")
(make-variable-buffer-local 'scxml-draw--diagram)

(defvar scxml-draw--buffer-name "*scxml-diagram*"
  ;; TODO - I don't think this is used anymore.
  "Buffer name for scxml drawing.")

(defun scxml---is-renderable-as-node (element)
  "Return non-nil if ELEMENT can be rendered as a node (non-transition)"
  (or (scxml---is-renderable-as-rect element)
      (object-of-class-p element 'scxml-initial)))
(defun scxml---is-renderable-as-rect (element)
  "Return non-nil if ELEMENT can be rendered as a rectangle (state, parallel, final)."
  (or (object-of-class-p element 'scxml-state-type)
      (object-of-class-p element 'scxml-parallel)))

;; enhancements to scxml-element& friends to support drawing
(cl-defgeneric scxml--modify-drawing-hint ((element scxml-drawable-element) (move-vector scxml-point) (viewport scxml-viewport))
  ;; TODO - this should move do scxml-drawable-element.el
  "Modify the drawing hint for ELEMENT (within ROOT) by moving it or it's edit-idx by MOVE-VECTOR, perform modification so it looks correct in VIEWPORT.")
(cl-defmethod scxml--modify-drawing-hint ((scxml scxml-scxml) (move-vector scxml-point) (viewport scxml-viewport))
  "Always returns nil, currently you can't modify these"
  nil)
(cl-defmethod scxml--modify-drawing-hint ((element scxml-drawable-element) (move-vector scxml-point) (viewport scxml-viewport))
  "Move or change ELEMENT by MOVE-VECTOR.

When ELEMENT has an edit-idx highlighted the drawing will be
modified.  When ELEMENT has no edit-idx highlighted the entire
drawing will be moved/displaced.

Will throw if it can't move it."
  (unless (scxml---is-renderable-as-node element)
    (error "Scxml--modify-drawing-hint not tested for this type(%s) yet, only state+parallel+final"
           (eieio-object-class-name element)))
  (let ((rect (scxml-element-drawing element)))
    (when (not (scxml-drawing-noshell-rect-p rect)) ;noshell rects are hintless, they strictly obey their parent.
      (let* ((edit-idx (scxml--edit-idx element))
             (edited-rect (scxml-build-edited-drawing rect
                                                      edit-idx
                                                      move-vector
                                                      viewport))
             ;; TODO - use the parent drawing canvas function here.
             (parent (scxml-parent element))
             (parent-drawing (scxml-element-drawing parent))
             (parent-drawing-canvas (scxml-get-inner-canvas parent-drawing)))
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
(cl-defmethod scxml--modify-drawing-hint ((transition scxml-transition) (move-vector scxml-point) (viewport scxml-viewport))
  "Given some TRANSITION in edit-mode, move the current index by MOVE-VECTOR.

Will throw if it can't move it. will not render!!"
  (let* ((arrow (scxml-element-drawing transition))
         (edit-idx (scxml--edit-idx transition))
         (edited-arrow (scxml-build-edited-drawing arrow
                                                   edit-idx
                                                   move-vector
                                                   viewport))
         (parent (scxml-parent transition))
         (parent-drawing (scxml-element-drawing parent))
         (parent-drawing-canvas (or (scxml-get-inner-canvas parent-drawing)
                                    (scxml-inner-canvas)))) ;note - injects a totally bogus canvas when the parent is <initial>
    ;; TODO - fix the above note.
    (if edited-arrow
        (with-slots (source target path) edited-arrow
          (setf (scxml-arrow-source arrow) source
                (scxml-arrow-target arrow) target
                (scxml-arrow-path arrow) path)
          ;; In some situations the number of edit idxs may change.
          ;; In these situations the edit-idx must be kept valid.
          (let ((num-edit-idxs (scxml-num-edit-idxs edited-arrow))
                (current-idx (scxml--edit-idx transition)))
            (when (and current-idx
                       (>= current-idx num-edit-idxs))
              (scxml--set-edit-idx transition (1- num-edit-idxs))))
          (scxml--set-hint transition
                           (scxml-build-hint arrow parent-drawing-canvas))
          (scxml--set-drawing-invalid transition 't)
          't)
      'nil)))

(cl-defgeneric scxml--simplify-drawing-hint ((element scxml-drawable-element) (viewport scxml-viewport))
  ;; TODO - this should be moved out to the scxml-drawable-element.el file.
  "Given a drawable ELEMENT, simplify the drawing hint if it exists.")
(cl-defmethod scxml--simplify-drawing-hint ((state scxml-state))
  "Given a STATE element, simplyfiy the drawing hint (create if needed)."
  (let* ((drawing (scxml-element-drawing state))
         (snapped (scxml-snap drawing))
         (parent (scxml-parent state))
         (parent-drawing (scxml-element-drawing parent))
         (parent-drawing-canvas (scxml-get-inner-canvas parent-drawing)))
    (scxml--set-drawing-invalid state 't)
    (scxml--set-hint state (scxml-build-hint snapped parent-drawing-canvas))))
(cl-defmethod scxml--simplify-drawing-hint ((transition scxml-transition))
  "Given a TRANSITION, simplify the drawing hint if it exists."
  (if (scxml--hint transition)
      (let* ((arrow (scxml-element-drawing transition))
             (parent (scxml-parent transition))
             (parent-drawing (scxml-element-drawing parent))
             (parent-drawing-canvas (scxml-get-inner-canvas parent-drawing)))
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
    (scxml--drawing-logger "Not simplified, no hint exists")
    'nil))

;; primitive functions for pixel related movement/info
(cl-defmethod scxml-draw--goto-pixel ((pixel scxml-pixel))
  "Go to PIXEL location on the screen."
  (goto-char (point-min))
  (with-slots (x y) pixel
    (let ((additional-lines (forward-line y)))
      (when (> additional-lines 0)
        (insert (mapconcat 'identity (make-list additional-lines "\n") ""))))
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

(cl-defmethod scxml---get-canvas-divisions ((rectangle scxml-rect) (num-child-nodes integer))
  "First arg is scxml-rect type to catch scxml-drawing-rect as well as scxml-drawing-null"
  ;; TODO - this should probably be moved to the rect drawing file?
  (let* ((num-columns (ceiling (sqrt num-child-nodes)))
         (num-rows (ceiling (/ (float num-child-nodes) (float num-columns)))))
    (scxml--split-canvas (scxml-get-inner-canvas rectangle)
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
  (when (not (or (scxml---is-renderable-as-node element)
                 (object-of-class-p element 'scxml-scxml)))
    (error "Wat?  shouldn't be calling this with thtat")) ;TODO - remove this check at some point?
  (scxml--drawing-logger "scxml--plot-node: type:%s"
                          (scxml-xml-element-name element))
  (scxml--drawing-logger "scxml--plot-node: canvas: %s" (scxml-print canvas))
  (let ((child-nodes (seq-filter 'scxml---is-renderable-as-node (scxml-children element)))
        (node (scxml--update-drawing element canvas)))
    (scxml--drawing-logger "scxml--plot-node: has-hint: %s" (when (scxml--hint element) t))
    (scxml--drawing-logger "\tDrawing: %s" (scxml-print node))
    (when child-nodes
      (let ((divided-canvases (scxml---get-canvas-divisions node
                                                            (length child-nodes))))
        ;; (cond ((scxml-parallel-p element)
        ;;        (error "Error????"))
        ;;       ('t                         ; scxml-state and scxml-scxml
        ;;        (let* ((num-child-nodes (length child-nodes))
        ;;               (num-columns (ceiling (sqrt num-child-nodes)))
        ;;               (num-rows (ceiling (/ num-child-nodes num-columns)))
        ;;               (divided-canvases (scxml--split-canvas (scxml-get-inner-canvas node)
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
                        (exit-direction (scxml-from-node-direction source-connector)))
                   ;; always set the direction.
                   (scxml-set-terminal-direction target-connector exit-direction)
                   (when (null (scxml-dangling-point target-connector))
                     ;; no point is set, must set it.
                     (let ((exit-vector (scxml-vector-from-direction exit-direction)))
                       (scxml-set-point target-connector (scxml-add
                                                          (scxml-connection-point source-connector)
                                                          (scxml-scaled exit-vector 2.0))))))))))
    (let* ((all-transitions (scxml-collect start (lambda (e) (object-of-class-p e 'scxml-transition))))
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
                           (let* ((edge (scxml-from-node-direction connector))
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
        (scxml--drawing-logger "scxml-plot (node: %.5f s, link: %.5f s, total: %.5f)"
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
      ;; TODO - don't need both of these, use the scxml--diagram.
      (setq-local scxml-draw--diagram diagram)
      (setq-local scxml--diagram diagram)
      (let ((scratch (scxml--get-scratch viewport)))
        ;; erase the buffer
        (goto-char (point-min))
        (delete-char (- (point-max) (point-min)) nil)

        ;; draw states, finals, parallels
        (scxml-visit root
                     (lambda (e)
                       (let ((drawing (scxml-element-drawing e)))
                         (scxml--drawing-logger "scxml-draw element:%s\n\tdrawing: %s\n"
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
                     (lambda (e) (and (object-of-class-p e 'scxml-transition)
                                      (not (scxml--highlight e)))))
        (scxml-visit root
                     (lambda (e)
                       (scxml--scratch-point-label scratch
                                                   viewport
                                                   (scxml-element-drawing e)))
                     (lambda (e) (object-of-class-p e 'scxml-initial)))
        (scxml-visit root
                     (lambda (e)
                       (scxml--scratch-arrow scratch
                                             viewport
                                             (scxml-element-drawing e)))
                     (lambda (e) (and (object-of-class-p e 'scxml-transition)
                                      (scxml--highlight e))))

        (scxml--scratch-write scratch))

      (scxml--drawing-logger "scxml-draw %.5f ms" (- (float-time) start-time))
      diagram)))

(provide 'scxml-draw)
;;; scxml-draw.el ends here
