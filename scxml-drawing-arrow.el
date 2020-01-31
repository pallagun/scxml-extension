;; scxml-drawing-arrow.el --- scxml drawing rectangle functions -*- lexical-binding: t -*-

;;; Commentary:
;; This file is an absolute mess and needs to be broken out into many different files.
;; possibly one exclusively for hints.

;;; Code:
(defconst scxml-arrow-connector-offset 1.0
  "How far connectors are offset (for display only) from their actual connection point.")

(require 'scxml-element)
(require 'scxml-viewport)
(require 'scxml-drawing)
(require 'scxml-drawing-rect)
(require 'scxml-geometry)
(require 'scxml-drawing-connector-rect)
(require 'scxml-drawing-connector-point)

(defclass scxml-arrow-connector-hint ()
  ()
  :abstract t)
(defclass scxml-arrow-connector-point-hint (scxml-arrow-connector-hint)
  ((exit-direction :initarg :exit-direction
                   :accessor scxml-exit-direction
                   :type symbol)))
(cl-defmethod scxml-print ((hint scxml-arrow-connector-point-hint))
  "Return a stringified version of POINT for human eyes."
  (format "[Ex:%s]"
          (scxml-exit-direction hint)))
(defclass scxml-arrow-connector-rect-hint (scxml-arrow-connector-hint)
  ((edge :initarg :edge
         :accessor scxml-hint-edge
         :type symbol)
   (parametric :initarg :parametric
               :accessor scxml-parametric
               :type number)))
(cl-defmethod scxml-print ((hint scxml-arrow-connector-rect-hint))
  "Return a stringified version of POINT for human eyes."
  (format "[E:%s, P:%f]"
          (scxml-hint-edge hint)
          (scxml-parametric hint)))
(defclass scxml-arrow-connector-dangling-hint (scxml-arrow-connector-hint)
  ((point :initarg :point
          :accessor scxml-connector-hint-point
          :type scxml-point)))
(cl-defmethod scxml-print ((hint scxml-arrow-connector-dangling-hint))
  "Return a stringified version of POINT for human eyes."
  (format "[P:%s]"
          (scxml-print (scxml-connector-hint-point hint))))

(cl-defgeneric scxml-build-arrow-connector ((hint scxml-arrow-connector-hint) &optional (drawing scxml-drawing))
  ;; TODO - is this duplicated someplace??
  "Make a connector based off a hint.")
(cl-defmethod scxml-build-arrow-connector ((hint scxml-arrow-connector-point-hint) &optional (drawing scxml-drawing))
  "Build a connector from a connector hint"
  (if (object-of-class-p drawing 'scxml-drawing)
      (scxml-drawing-connector-point :node drawing
                                     :exit-direction (scxml-exit-direction hint))
    (scxml-drawing-connector-dangling)))
(cl-defmethod scxml-build-arrow-connector ((hint scxml-arrow-connector-rect-hint) &optional (drawing scxml-drawing))
  "Build a connector from a connector hint"
  (if (object-of-class-p drawing 'scxml-drawing)
      (scxml-drawing-connector-rect :node drawing
                                    :edge (scxml-hint-edge hint)
                                    :parametric (scxml-parametric hint))
    (scxml-drawing-connector-dangling)))
(cl-defmethod scxml-build-arrow-connector ((hint scxml-arrow-connector-dangling-hint) &optional (drawing scxml-drawing))
  (scxml-drawing-connector-dangling :point (scxml-connector-hint-point hint)))
(cl-defmethod scxml-build-connector-hint ((connector scxml-drawing-connector-rect))
  "Build a connector hint for this connector"
  (scxml-arrow-connector-rect-hint :edge (scxml-from-node-direction connector)
                                   :parametric (scxml-edge-parametric connector)))
(cl-defmethod scxml-build-connector-hint ((connector scxml-drawing-connector-point))
  "Build a connector hint for this connector"
  (scxml-arrow-connector-point-hint :exit-direction (scxml-from-node-direction connector)))
(cl-defmethod scxml-build-connector-hint ((connector scxml-drawing-connector-dangling))
  (scxml-arrow-connector-dangling-hint :point (scxml-connection-point connector)))

(defclass scxml-arrow-hint ()
  ((source :initarg :source
           :accessor scxml-source
           :type scxml-arrow-connector-hint)
   (target :initarg :target
           :accessor scxml-target
           :type scxml-arrow-connector-hint)
   ;; TODO - Most of these slots don't appear to be used.  Reap those that are not.
   (original-path :initarg :original-path
                  :accessor scxml-original-path
                  :initform 'nil
                  :type (or null scxml-cardinal-path))
   (original-points :initarg :original-points
                  :accessor scxml-original-points
                  :initform 'nil
                  :type (or null list))
   (relative-points :initarg :relative-points
                    :accessor scxml-relative-points
                    :initform 'nil
                    :type (or null list))
   (last-move-direction :initarg :last-move-direction
                        :accessor scxml-last-move-direction
                        :initform 'nil
                        :type (or null integer)
                        :documentation "If the source or target drawing has changed, indicate which direction it moved here")
   (last-move-connector :initarg :last-move-connector
                        :accessor scxml-last-move-connector
                        :initform 'nil
                        ;; TODO - is this type a normal up/down/left/right type?
                        :type (or null symbol)
                        :documentation "If the source or target drawing has changed, indicate which one here."))
  :documentation "scxml-arrow hint structure")
(cl-defmethod scxml--possibly-invalid? ((arrow-hint scxml-arrow-hint))
  (or (scxml-last-move-connector arrow-hint)
      (scxml-last-move-direction arrow-hint)))
(cl-defmethod scxml-print ((arrow-hint scxml-arrow-hint))
  "Return a stringified version of ARROW-HINT for human eyes."
  (with-slots (source-edge source-parametric target-edge target-parametric relative-points original-path original-points last-move-direction last-move-connector) arrow-hint
    (message "Hint[Src:%s, Tar:%s, RelPts[%s], OrigPts[%s], last[D:%s, C:%s]]"
             (scxml-print (scxml-source arrow-hint))
             (scxml-print (scxml-target arrow-hint))
             (mapconcat 'scxml-print relative-points ", ")
             (mapconcat 'scxml-print original-points ", ")
             last-move-direction
             last-move-connector)))

(defclass scxml-arrow (scxml-drawing)
  ;; TODO - consider creating an scxml-arrow builder to allow for
  ;; nullable slots during construction and promised set slots after
  ;; construction
  ((source :initarg :source
           :accessor scxml-arrow-source
           :type (or null scxml-drawing-connector))
   (target :initarg :target
           :accessor scxml-arrow-target
           :type (or null scxml-drawing-connector))
   (path :initarg :path
         :accessor scxml-arrow-path
         :initform nil
         :type (or null scxml-cardinal-path)
         :documentation
         "These are the points that make up the _middle_ of the
path. The full path uses the start and end points from the
connectors.  So the full path would be (append (source-point)
path (target-point))."))
  :documentation "Connect two drawings (rectangles) with an arrow along a path")
(cl-defmethod scxml-print ((arrow scxml-arrow))
  "Return a stringified version of ARROW for human eyes."
  (format "Arrow([%s]->%s->[%s])"
          (scxml-print (scxml-arrow-source arrow))
          (scxml-print (scxml-arrow-path arrow))
          (scxml-print (scxml-arrow-target arrow))))
(defun scxml--arrow-source-locked (arrow)
  "Return t if the ARROW's source connector can not be move-edited."
  (object-of-class-p (scxml-arrow-source arrow) 'scxml-drawing-connector-point))
(defun scxml--arrow-target-locked (arrow)
  "Return t if the ARROW's target connector can not be move-edited."
  (object-of-class-p (scxml-arrow-target arrow) 'scxml-drawing-connector-point))
(cl-defmethod scxml-edit-idx-point ((arrow scxml-arrow) (idx integer))
  "Return the scxml-point of ARROW's edit-idx IDX."
  ;; 0 is the start connector, N is the end connector
  ;; 1 -> N-1 are the path points
  (when (< idx 0)
    (error "Arg: scxml--edit-idx-point: index must be >= 0"))
  (with-slots (source target path) arrow
    ;; If the source connector isn't editable, effectively bump up the idx.
    (when (scxml--arrow-source-locked arrow)
      (incf idx))
     (if (eq idx 0)                      ;start connector
         (scxml-connection-point source)
       (let ((num-path-pts (scxml-num-points path)))
         ;; [0 1 2 3] -> #2
         (if (< (1- idx) num-path-pts)
             ;; return the (- idx 2)'th path point
             (scxml-nth path (1- idx))
           ;; return the last point if it's returnable.
           (if (and (eq idx (1+ num-path-pts))
                    (not (scxml--arrow-target-locked arrow)))
               (scxml-connection-point target)
             (error "Arg: scxml--edit-idx-points: idx must be <= num path points")))))))
(cl-defmethod scxml-edit-idx-points ((arrow scxml-arrow))
  "Return all edit-idx points for ARROW in order"
  (let ((all-points (scxml--full-path arrow 0.0)))
    (when (scxml--arrow-source-locked arrow)
      (setq all-points (cdr all-points)))
    (if (scxml--arrow-target-locked arrow)
        (nbutlast all-points)
      all-points)))
(cl-defmethod scxml-num-edit-idxs ((arrow scxml-arrow))
  "Return the number of edit-idxs in ARROW."
  ;; connectors to points are not editable.
  (+ (scxml-num-points (scxml-arrow-path arrow))
     (if (scxml--arrow-source-locked arrow) 0 1)
     (if (scxml--arrow-target-locked arrow) 0 1)))

(cl-defmethod scxml-build-hint ((arrow scxml-arrow) (parent-canvas scxml-canvas))
  "?"
  ;; TODO - I'm not sure how this works, but this needs to be looked at.
  (error "TODO: Implementation"))
(defun scxml--build-closest-path (arrow new-pts connector-offset is-endpoint-move)
  "Given an ARROW and a desired set of NEW-PTS, get as close as you can."
  (with-slots ((current-source source) (current-target target)) arrow
    (let* ((current-source-point (scxml-connection-point current-source))
           (current-target-point (scxml-connection-point current-target))
           (first-path-point (first new-pts))
           (last-path-links (last new-pts 2))
           (last-path-point (cadr last-path-links))
           (new-source (scxml-build-connector current-source first-path-point))
           (new-target (scxml-build-connector current-target last-path-point)))
      ;; Both the source and target connectors must exist or this is not possible.
      (if (and new-source new-target)
          (let* (;; TODO - note that the 2.0 below here is a 'looks-nice-fudge-factor'
                 (connector-offset (scxml-scaled connector-offset 2.0))
                 (new-source-point (scxml-connection-point new-source))
                 (new-target-point (scxml-connection-point new-target))
                 (source-point-moved (not (scxml-almost-equal current-source-point new-source-point)))
                 (target-point-moved (not (scxml-almost-equal current-target-point new-target-point)))
                 (any-end-point-moved (or source-point-moved target-point-moved))
                 (source-point-match (scxml-almost-equal new-source-point first-path-point))
                 (target-point-match (scxml-almost-equal new-target-point last-path-point)))

            (cond ((and is-endpoint-move any-end-point-moved)
                   ;; this is an end point move and one of the end points has moved.
                   (when (not (eq (scxml-from-node-direction current-source)
                                  (scxml-from-node-direction new-source)))
                     ;; this is an end poind move and the source jumped edges.
                     ;; Source connect has jumped edges, ensure there is a perpendicular start.
                     (let ((first-segment (scxml-segment :start (first new-pts)
                                                         :end (second new-pts)))
                           (required-vector (scxml-vector-from-direction (scxml-from-node-direction new-source))))
                       (when (or (scxml-almost-zero (scxml-length first-segment))
                                 (<= (scxml-dot-prod
                                      (scxml-characteristic-vector first-segment)
                                      required-vector)
                                     scxml--almost-zero))
                         (push (scxml-subtract first-path-point
                                               (scxml-scaled required-vector
                                                             connector-offset))
                               new-pts))))
                   (when (not (eq (scxml-from-node-direction current-target)
                                  (scxml-from-node-direction new-target)))
                     ;; this is an end point move and the target jumped edges
                     ;; target connect has jumped edges, ensure there is a perpendicular end.
                     (let ((last-segment (scxml-segment :start (car last-path-links)
                                                        :end last-path-point))
                           (required-vector (scxml-vector-from-direction
                                             (scxml-from-node-direction new-target))))
                       (when (or (scxml-almost-zero (scxml-length last-segment))
                                 (<=  (scxml-dot-prod
                                       (scxml-characteristic-vector last-segment)
                                       required-vector)
                                      scxml--almost-zero))
                         (setq new-pts (append
                                        new-pts
                                        (list (scxml-subtract last-path-point
                                                              (scxml-scaled required-vector
                                                                            connector-offset))))))))
                   ;; Build the path by stretching.
                   (let* ((full-path (scxml---path-stretch new-pts
                                                           new-source-point
                                                           new-target-point)))
                     (scxml-arrow :source new-source
                                  :target new-target
                                  :parent (scxml-parent arrow)
                                  :path (scxml-cardinal-path :points
                                                             (nbutlast (cdr full-path))))))
                  ((and is-endpoint-move (not any-end-point-moved))
                   ;; this is specifically and end point move but neither end point moved.
                   nil)
                  ;; at this point there are no specific end point moves made.
                  ((not (and (eq (scxml-from-node-direction current-source)
                                 (scxml-from-node-direction new-source))
                             (eq (scxml-from-node-direction current-target)
                                 (scxml-from-node-direction new-target))))
                   ;; this is not an end point move, but an end point edge changed, invalid.
                   nil)
                  ((and source-point-match target-point-match)
                   ;; Whatever happened the source and target connectors were able to comply
                   ;; so this path is entirely valid.
                   (scxml-arrow :source new-source
                                :target new-target
                                :parent (scxml-parent arrow)
                                :path (scxml-cardinal-path :points
                                                           (nbutlast (cdr new-pts)))))
                  ((or (and (not source-point-moved) (not source-point-match))
                       (and (not target-point-moved) (not target-point-match)))
                   ;; at least one of the end point connectors was supposed to move
                   ;; but did not move at all, this is invalid.
                   nil)
                  (t
                   ;; At least one of your end points moved, but not enough.  neither
                   ;; end point changed edges.
                   (let* ((full-path (scxml---path-stretch new-pts
                                                           new-source-point
                                                           new-target-point)))
                     (scxml-arrow :source new-source
                                  :target new-target
                            :parent (scxml-parent arrow)
                            :path (scxml-cardinal-path :points
                                                       (nbutlast (cdr full-path))))))))
        ;; Unable to get new-source or new-target, fail.
        nil))))
;; (defun scxml---build-path-if-valid (arrow new-pts &optional iterations)
;;   "If ARROW is valid to represend by new-pts, return a new scxml-arrow with those points"
;;   ;; TODO - rename this 'build-arrow-if-valid', it's making an arrow, not a path.

;;   ;; TODO - examine why this is needed.
;;   ;; TODO - don't bother building a new connecto if it didn't move.
;;   ;; Actually, I think I should.

;;   ;; TODO - don't bother calling build-connector if the point doesn't change?
;;   (let ((new-source-connector (scxml-build-connector (scxml-arrow-source arrow)
;;                                                      (first new-pts)))
;;         (new-target-connector (scxml-build-connector (scxml-arrow-target arrow)
;;                                                      (car (last new-pts)))))
;;     (if (and new-source-connector new-target-connector)
;;         (let ((new-arrow (scxml-arrow :source new-source-connector
;;                                       :target new-target-connector
;;                                       :path (scxml-cardinal-path :points
;;                                                                  (nbutlast (cdr new-pts)))
;;                                       :parent (scxml-parent arrow))))

;;           ;; TODO - if either connector is a rect connector which jumped edges,
;;           ;; Ensure you aren't creating a strangely locked arrow.
;;           (when (and (scxml-drawing-connector-rect-p new-source-connector)
;;                      (scxml-drawing-connector-rect-p (scxml-arrow-source arrow))
;;                      (not (eq (scxml-node-edge new-source-connector)
;;                               (scxml-node-edge (scxml-arrow-source arrow)))))
;;             ;; Both connectors are rectangles and the edge has changed,
;;             ;; check that path is sane.  TODO - fix this intelligently,
;;             ;; currently I'm just reverting to auto-path
;;             (scxml--arrow-set-default-path new-arrow))

;;           (when (and (scxml-drawing-connector-rect-p new-target-connector)
;;                      (scxml-drawing-connector-rect-p (scxml-arrow-target arrow))
;;                      (not (eq (scxml-node-edge new-target-connector)
;;                               (scxml-node-edge (scxml-arrow-target arrow)))))
;;             ;; Both connectors are rectangles and the edge has changed,
;;             ;; check that path is sane.
;;             (scxml--arrow-set-default-path new-arrow))
;;           new-arrow)
;;       nil)))
(cl-defmethod scxml-build-move-edited ((arrow scxml-arrow) (move-vector scxml-point) (viewport scxml-viewport))
  "Return a new arrow representing ARROW moved by MOVE-VECTOR.

This may not be possible due to constraint violation and in those
cases this function may return nil."
  (let ((desired-points (mapcar (lambda (pt)
                                  (scxml-add pt move-vector))
                                (scxml--full-path arrow))))
    ;; (scxml---build-path-if-valid arrow desired-points)
    (scxml--build-closest-path arrow desired-points (scxml-get-point-scaling viewport) nil)
    ))
(cl-defmethod scxml-build-idx-edited ((arrow scxml-arrow) (edit-idx integer) (move-vector scxml-point) (viewport scxml-viewport))
  "Return a new arrow representing ARROW's EDIT-IDX moved by MOVE-VECTOR.

This may not be possible due to constraint violation and in those
cases this function may return nil."
  (let* ((full-pts (scxml--full-path arrow))
         ;; Sometimes the source connector is locked entirely.  In those
         ;; cases the edit idx 0 is locked so the drawing layer considers
         ;; edit idx "0" to be what is normally edit idx 1.
         ;; ... this does not seem like a great way to handle that but...
         (unbiased-edit-idx (+ (if (scxml--arrow-source-locked arrow) 1 0)
                                       edit-idx))
         (new-pts (scxml-nudge-path full-pts
                                    unbiased-edit-idx
                                    move-vector)))
    ;; (scxml---build-path-if-valid arrow new-pts)
    (scxml--build-closest-path arrow
                               new-pts
                               (scxml-get-point-scaling viewport)
                               (or (eq unbiased-edit-idx 0)
                                   (eq unbiased-edit-idx (1- (length full-pts)))))))

(cl-defmethod scxml-has-intersection ((rect scxml-rect) (arrow scxml-arrow) &optional evaluation-mode)
  "Return non-nil if RECT intersect with ARROW's path at any point."
  (scxml-has-intersection rect (scxml-path :points (scxml--full-path arrow)) evaluation-mode))
(cl-defgeneric scxml--full-path ((arrow scxml-arrow) &optional offset)
  "Get the full path of the ARROW with optional start/end OFFSET from ends.

TODO- this shouldn't really be called full path because it's not
recturning a path, just a list of points.  maybe change that.")
(cl-defmethod scxml--full-path ((arrow scxml-arrow) &optional offset)
  "Get the full path of the ARROW with optional start/end OFFSET from ends.

TODO- this shouldn't really be called full path because it's not
recturning a path, just a list of points.  maybe change that.

NOTE: when you give a non-nil offset all bets are off, this function just
'trys-real-hard' at that point.  Don't use non-nil offset for anything except
presentation to human eyeballs.

When offset is non-nil path stretching will be done to ensure the path returned
is always cardinal."
  ;; TODO: I'm using clone below because I'm not sure if I'll be
  ;; messing with the connector parameters or not.
  ;; establish if I nede clone and if there should be some convention
  ;; for a function's return.
  (let* ((path (scxml-arrow-path arrow))
         (path-pts (when path (scxml-points path))))
    (if (null offset)
        (append (cons (scxml--start-point arrow) path-pts)
                (list (scxml--end-point arrow)))
      (let ((start (clone (scxml--start-point arrow offset)))
            (start-original (scxml--start-point arrow))
            (end (clone (scxml--end-point arrow offset)))
            (end-original (scxml--end-point arrow)))

        ;; TODO: - a check that should be removed.  check things.
        ;; (unless (scxml-is-cardinal-path? (append (cons start-original path-pts)
        ;;                                          (list end-original)))
        ;;   (error "Not a cardinal path?!"))
        ;; stitch in path stretch routes here.
        (if path-pts
            ;; do I need to shape up the beginning?
            (let ((path-start (first path-pts))
                  (path-end (car (last path-pts))))
              (if (scxml-cardinal-displacement? start path-start)
                  (setq path-pts (cons start path-pts))
                (let ((start-path (scxml---path-stretch
                                   (list start-original path-start)
                                   start
                                   path-start)))
                  (mapc (lambda (pt) (push pt path-pts))
                        (reverse start-path))))
              (if (scxml-cardinal-displacement? path-end end)
                  (append path-pts (list end))
                (append path-pts
                        (scxml---path-stretch
                         (list path-end end-original)
                         path-end
                         end))))
          ;; no inner points.  stretch the originals if needed
          (if (scxml-cardinal-displacement? start end)
              (list start end)
            (scxml---path-stretch (list start-original end-original)
                                    start
                                    end)))))))
(cl-defmethod scxml--start-point ((arrow scxml-arrow) &optional offset)
  "Return the starting point of ARROW, optionally OFFSET."
  (with-slots (source) arrow
    (scxml-connection-point source offset)))
(cl-defmethod scxml--end-point ((arrow scxml-arrow) &optional offset)
  "Return the ending point of ARROW, optionally OFFSET."
  (with-slots (target) arrow
    (scxml-connection-point target offset)))
(cl-defmethod scxml--arrow-set-default-path ((arrow scxml-arrow) &optional offset)
  "Destructively modifies path! - Normal, not smart path routing.

OFFSET is how much to offset from the _actual_ start and end
points."
  ;; TODO - the offset parameter here seems to be misnamed, I think
  ;; it's actually minimum start/end segment length?
  (let ((start-time (float-time)))
    (let* ((source (scxml-arrow-source arrow))
           (target (scxml-arrow-target arrow))
           (full-path (scxml-build-path-cardinal (scxml-connection-point source)
                                                 (scxml-connection-point target)
                                                 (scxml-vector-from-direction (scxml-from-node-direction source))
                                                 (scxml-vector-from-direction (scxml-reverse (scxml-from-node-direction target)))
                                                 ;; TODO: this shouldn't be 1.0 - it should be a defconst
                                                 (or offset scxml-arrow-connector-offset)))
           (middle-path (if (> (scxml-num-points full-path) 2)
                            (cdr (nbutlast (scxml-points full-path)))
                          'nil)))
      (oset arrow path (scxml-cardinal-path :points middle-path)))
    (scxml--drawing-logger "scxml--arrow-set-default-path: %s s"
                            (- (float-time) start-time ))))
(cl-defgeneric scxml--set-path-from-hint ((arrow scxml-arrow) (hint scxml-arrow-hint))
  "Modify ARROW by setting the path from HINT.

This function assumes that the arrow's connectors (source and target) have already.
been correctly set."
  (oset arrow
        path
        (scxml-cardinal-path :points
                             (nbutlast
                              (cdr
                               (scxml---path-stretch (scxml-original-points hint)
                                                     (scxml--start-point arrow)
                                                     (scxml--end-point arrow))))))
  ;; (error "fix me scxml--set-path-from-hint")
  ;; (let* ((characteristic-full-path (scxml-relative-points hint))
  ;;        (full-path (scxml--path-stretch characteristic-full-path
  ;;                                        (scxml-connection-point (scxml-arrow-source arrow))
  ;;                                        (scxml-connection-point (scxml-arrow-target arrow)))))
  ;;   ;; trim the start and end points off and you're done.
  ;;   ;; note - this _won't_ take into account jogs you'll have to insert
  ;;   ;;        if/when a perfectly horizontal or vertical line needs to go
  ;;   ;;        diagonal
  ;;   (when (eql 2 (length full-path))
  ;;     (error "Start handling jogs"))

  ;;   (oset arrow
  ;;         path
  ;;         (scxml-path :points (cdr (nbutlast full-path)))))
  )
(cl-defmethod scxml--build-drawing-from-hint ((hint scxml-arrow-hint) (source scxml-drawing-rect) (target scxml-drawing-rect))
  "Given a moving SOURCE or TARGET drawing, correct any errors in HINT and return a valid drawing."
  ;; TODO - is this used?
  (error "fix me scxml--build-drawing-from-hint")
  (let* ((source-connector (scxml-build-arrow-connector (scxml-source hint) source)
          ;; (scxml-drawing-connector-rect :node source
                           ;;                  :edge (scxml-source-edge hint)
                           ;;                  :parametric (scxml-source-parametric hint))
                           )
         (target-connector (scxml-build-arrow-connector (scxml-target hint) target)
          ;; (scxml-drawing-connector-rect :node target
                           ;;                  :edge (scxml-target-edge hint)
                           ;;                  :parametric (scxml-target-parametric hint))
                           )
         (relative-rect (scxml--generate-hint-rect source target))
         (points (mapcar (lambda (pt) (scxml-absolute-coordinates relative-rect pt))
                         (scxml-relative-points hint))))
    (when (scxml--possibly-invalid? hint)
      (let* ((last-move-direction (scxml-last-move-direction hint))
             (last-move-connector (scxml-last-move-connector hint))
             (last-move-axis (scxml--direction-axis last-move-direction))
             (source-point (scxml-connection-point source-connector))
             (target-point (scxml-connection-point target-connector))
             (delta (scxml-subtract target-point source-point)))
        (when (not (scxml--cardinal-direction-vector? delta))
          (cl-flet ((build-connection-path
                     (source-pt target-pt connection-axis)
                     (if (eq connection-axis 'scxml--vertical)
                         (let ((mid-x (/ (+ (scxml-x source-pt) (scxml-x target-pt)) 2.0)))
                           (list (scxml-point :x mid-x :y (scxml-y source-pt))
                                 (scxml-point :x mid-x :y (scxml-y target-pt))))
                       (let ((mid-y (/ (+ (scxml-y source-pt) (scxml-y target-pt)) 2.0)))
                         (list (scxml-point :x (scxml-x source-pt) :y mid-y)
                               (scxml-point :x (scxml-x target-pt) :y mid-y))))))
            ;; this is no longer a cardinal direction vector, adjust the end points if possible
            ;; and if not, insert a jog to the middle of the points.
            (let* ((corrected-delta (if (eq last-move-axis 'scxml--vertical)
                                        (scxml-vector :x (scxml-x delta)
                                                      :y 0.0)
                                      (scxml-vector :x 0.0
                                                    :y (scxml-y delta)))))
              (if (eq last-move-connector 'source)
                  ;; source was last to move, so move the target connector to satisfy
                  (let* ((correct-target-point (scxml-add source-point corrected-delta))
                         (correct-target-connector (scxml-draw--move-connector-if-possble
                                                    target-connector
                                                    (scxml-subtract corrected-delta
                                                                    delta))))
                    (if correct-target-connector
                        ;; you can make the connector handle this change.
                        (with-slots (edge parametric) correct-target-connector
                          (setf (scxml-from-node-direction target-connector) edge
                                (scxml-edge-parametric target-connector) parametric))
                      ;; you _can't_ make the connector handle this change
                      ;; you must insert a jog.
                      ;; TODO: make the connector handle as much of the change
                      ;; as possible before you insert a jog.
                      (setf points (build-connection-path source-point target-point last-move-axis))))

                ;; target was last to move, so move the source connector to satisfy
                (let* ((correct-source-point (scxml-add target-point corrected-delta))
                       (correct-source-connector (scxml-draw--move-connector-if-possble
                                                  source-connector
                                                  (scxml-add corrected-delta
                                                             delta))))
                  (if correct-source-connector
                      ;; you can make the connector handle thisp change.
                      (with-slots (edge parametric) correct-source-connector
                        (setf (scxml-from-node-direction source-connector) edge
                              (scxml-edge-parametric source-connector) parametric))
                    ;; you _can't_ make the connector handle this change
                    ;; you must insert a jog.
                    ;; TODO: make the connector handle as much of the change
                    ;; as possible before you insert a jog.
                    (setf points (build-connection-path source-point target-point last-move-axis))))))))))
    (scxml-arrow :source source-connector
                 :target target-connector
                 :path (scxml-path :points points)))
  )
(cl-defmethod scxml--build-move-edited ((arrow scxml-arrow) move-direction)
  "Given a rect, and a move direction, move in one pixel in that direction"
  ;; TODO - delete this if it's not used.
  (error "Is this even used???")
  ;; so, whatever edit idx you're moving, you're also going to be
  ;; moving idx-1 and idx+1 (possibly) to handle the linkages.
  ;; define: 'doesMyNeigborHaveToMove' as -
  (cl-flet
      ((new-free-neighbor-pt
        (idx-pt neighbor-pt move-vec)
        (let ((to-neighbor (scxml-subtract neighbor-pt idx-pt)))
          (if (scxml-almost-equal (scxml-dot-prod to-neighbor move-vec) 0)
              neighbor-pt          ; no movement needed
            (scxml-add neighbor-pt move-vec))))
       (new-connector-neighbor-pt
        (idx-pt neighbor-connector move-vec)
        (let ((should-move (scxml-dot-prod
                            move-vec
                            (scxml-cardinal-exit-vector (scxml-from-node-direction neighbor-connector)))))
          (if (not should-move)
              neighbor-connector        ; no movement desired
            ;; must move, but _can_ you move on this edge?
            ;; how much do you need to move in parameter space to move
            ;; move-vec units in pixel space?
            (error "implement me")
            )))))




  (error "implement me"))
(cl-defgeneric scxml--generate-hint-rect ((source scxml-drawing) (target scxml-drawing))
  "Get a reference rectangle for the hint to be based on."
  ;; TODO - is this still used?
  (let* ((source-element (scxml-parent source))
         (target-element (scxml-parent target))
         (parent (scxml-find-nearest-mutual-parent source-element target-element))
         (parent-drawing (scxml-element-drawing parent)))
    (scxml-get--inner-canvas parent-drawing)))
(cl-defmethod scxml--generate-hint-less-old ((arrow scxml-arrow) (parent-canvas scxml-inner-canvas))
  ;; TODO - is this still used?
  (with-slots (source target path) arrow
    (scxml-arrow-hint :source-edge (scxml-from-node-direction source)
                      :source-parametric (scxml-edge-parametric source)
                      :target-edge (scxml-from-node-direction target)
                      :target-parametric (scxml-edge-parametric target)
                      :relative-points (scxml--full-path arrow))))
(cl-defmethod scxml--generate-hint-old ((arrow scxml-arrow) (parent-canvas scxml-inner-canvas))
  "relative deltas as a plist"
  ;; TODO - is this still used?
  (with-slots (source target path) arrow
    (let* ((relative-rect (scxml--generate-hint-rect (scxml-node source)
                                                     (scxml-node target))))
      (scxml-arrow-hint :source-edge (scxml-from-node-direction source)
                        :source-parametric (scxml-edge-parametric source)
                        :target-edge (scxml-from-node-direction target)
                        :target-parametric (scxml-edge-parametric target)
                        :relative-points
                        (mapcar (lambda (pt)
                                  (scxml-relative-coordinates relative-rect pt))
                                (scxml-points path))))))

(cl-defmethod scxml-build-hint ((arrow scxml-arrow) (parent-canvas scxml-inner-canvas))
  "Right now it's just the original points.

The arrow factory when building from a hint is smart enough to sort it all out."
  ;; TODO - is *this* one used?!
  (with-slots (source target) arrow
    (scxml-arrow-hint :source (scxml-build-connector-hint source)
                      :target (scxml-build-connector-hint target)
                      :original-points (scxml--full-path arrow))))

(cl-defmethod scxml-snap ((arrow scxml-arrow) &optional allow-partial-snap)
  "Return an arrow but with all coordinates snapped to the nearest pixel"
  ;; TODO - remove all snapping functions?
  (error "This function should no longer be called")
  (with-slots (source target path) arrow
    (let ((snap-source (scxml-snap source allow-partial-snap))
          (snap-target (scxml-snap target allow-partial-snap))
          (snap-path (scxml-snap path)))
      ;; TODO - do I need to make sure that the snap path is still a cardinal
      ;; direction to each end point connector?  I don't think I do beacuse
      ;; they're all getting snapped the same way... but.... ehh, meh.
      (when (and snap-source snap-target snap-path)
        (scxml-arrow :source snap-source
                     :target snap-target
                     :path snap-path
                     :parent (scxml-parent arrow))))))
(defun scxml--arrow-path-simplify (full-path-pts slack-allowance-x slack-allowance-y)
  "actual simplifier - returns the first argument when it fails."
  ;; find any segments smaller than viewport's pixel size.
  ;;    so this won't work. I have to do these comparisons absolutely.
  (let ((last-path-pt)
        (num-compacted-pts 1)
        (num-pts 0)
        (last-compacted-pt (first full-path-pts))
        (compacted-pts (list (first full-path-pts))))
    (cl-loop for real-pt in (cdr full-path-pts)
             for delta = (scxml-subtract real-pt last-compacted-pt)
             for abs-delta-x = (abs (scxml-x delta))
             for abs-delta-y = (abs (scxml-y delta))
             ;; if you went through either slack add a new point with the slack
             ;; that went over.
             do (cond ((>= abs-delta-x slack-allowance-x)
                       ;; X went over allowance.
                       (if (>= abs-delta-y slack-allowance-y)
                           ;; X and Y went over allowance.
                           (push real-pt compacted-pts)
                         ;; X went over but Y did not.
                         (push (scxml-point :x (scxml-x real-pt) :y (scxml-y last-compacted-pt))
                               compacted-pts))
                       (incf num-compacted-pts)
                       (setq last-compacted-pt (first compacted-pts)))
                      ((>= abs-delta-y slack-allowance-y)
                       ;; Y went over but X did not
                       (push (scxml-point :x (scxml-x last-compacted-pt) :y (scxml-y real-pt))
                             compacted-pts)
                       (setq last-compacted-pt (first compacted-pts))
                       (incf num-compacted-pts)))
             do (setq last-path-pt real-pt
                      num-pts (1+ num-pts)))
    (if (scxml-almost-equal last-path-pt last-compacted-pt)
        ;; end point matches, no additional work needed.
        (nreverse compacted-pts)
      ;; End point does not match, amend the first N points to handle this.
      ;; note: if the list is only 2 elements long this will be impossible and
      ;;       the path router needs to be called.
      (if (>= num-compacted-pts 3)
          (let* ((required-delta (scxml-subtract last-path-pt last-compacted-pt))
                 (required-unit-vec (scxml-normalized required-delta))
                 (failure))
            ;; go through compacted-pts, adding this delta until you hit a segment
            ;; wich you don't need to.
            ;; TODO - this only takes direction into account, it could produce paths
            ;;        that overlap themselves.  - handle that.
            (cl-loop for reverse-path on compacted-pts
                     for start-pt = (first reverse-path)
                     for end-pt = (second reverse-path)
                     ;; if you got to the end point and you're still trying
                     ;; to correct things then this algorithm failed.
                     unless (and end-pt)
                     do (progn (setq failure t)
                               (cl-return))
                     for segment-char-vec = (scxml-subtract end-pt start-pt)
                     for segment-unit-vec = (scxml-normalized segment-char-vec)

                     ;; the start point *must* be moved.
                     do (scxml-incf start-pt required-delta)

                     ;; if the dot product is zero, then you have to move this point, it won't have freedom
                     ;; in the required direction.
                     do (unless (scxml-almost-zero (scxml-dot-prod required-unit-vec segment-unit-vec))
                          (cl-return)))
            (scxml-simplified (if failure
                                full-path-pts
                              (nreverse compacted-pts))))
        ;; Only 2 points or less in the path. do not attepmt correction
        ;; just give up
        full-path-pts))))

(cl-defmethod scxml-build-simplified ((arrow scxml-arrow) (viewport scxml-viewport))
  "Attempt to simplify (modify) ARROW as seen on VIEWPORT."
  (with-slots (source target) arrow
    (let* ((full-path-pts (scxml--full-path arrow))
           (slack (scxml-get-point-scaling viewport))
           (simplified (scxml--arrow-path-simplify full-path-pts
                                                   (scxml-x slack)
                                                   (scxml-y slack)))
           ;; TODO - the path-simplify should do this for me,
           ;; modify it do do so on each pt evaluation.
           (simplified-reduced (and simplified (scxml-simplified simplified))))
      (when simplified-reduced
        (scxml-arrow :source source
                     :target target
                     :path (scxml-cardinal-path :points
                                                (nbutlast (cdr simplified-reduced))))))))

(provide 'scxml-drawing-arrow)
