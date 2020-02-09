;;; scxml-drawing-divided-rect.el --- scxml drawing divided rectangle functions -*- lexical-binding: t -*-

;;; Commentary:
;; This needs some attention

;;; Code:
(require 'scxml-drawing)
(require 'scxml-geometry)
(require 'scxml-drawing-rect)
(require 'scxml-element)

(defconst scxml---divided-rect-header-height 2.0)
(defclass scxml-drawing-divided-rect (scxml-drawing-rect)
  ((dividers :initarg :dividers
             :accessor scxml-dividers
             :initform nil
             :type list
             :documentation "A list of all dividers in this rect as scxml-segments.  This is really only used for rendering."))
  :abstract 't
  :documentation "Represents a rectangle which can be drawn with divisions")
(defun scxml-drawing-divided-rect-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-drawing-divided-rect)."
  (object-of-class-p any 'scxml-drawing-divided-rect))
(cl-defmethod scxml-get-inner-canvas ((rect scxml-drawing-divided-rect))
  "Given a rectangle, pull an inner canvas"
  (with-slots (x-min y-min x-max y-max) rect
    (scxml-inner-canvas :x-min x-min
                        :y-min y-min
                        :x-max x-max
                        :y-max (- y-max scxml---divided-rect-header-height)
                        :drawing rect)))

(defclass scxml---nest-stripe ()
  ((axis :initarg :axis
         :accessor scxml-axis
         :initform 'scxml--horizontal
         :type symbol)
   (cells :initarg :cells
          :accessor scxml-cells
          :initform (list 'undivided)
          :type list)
   (divisions :initarg :divisions
              :initform nil
              :accessor scxml-divisions
              :type list))
  :documentation "Child of nest-rect")
(cl-defmethod scxml-set ((nest-rect scxml---nest-stripe) (coordinate list) thing)
  (let ((parent-coord (butlast coordinate))
        (last-coord (car (last coordinate)))
        (actual-thing (or thing 'undivided)))
    (let ((parent-cell (scxml-get-cell nest-rect parent-coord)))
      (with-slots (cells divisions) parent-cell
        (when (>= last-coord (length cells))
          (error "Coordinate out of bounds"))
        (setf (nth last-coord cells) thing)))))
(cl-defmethod scxml-add ((nest-rect scxml---nest-stripe) (coordinate list) &optional addition)
  (let ((cell (scxml-get-cell nest-rect coordinate))
        (actual-addition (or addition 'undivided)))
    (with-slots (cells divisions) cell
      (setf cells (append cells (list actual-addition)))
      (let* ((size (/ 1.0 (float (length cells)))))
        (setf divisions (number-sequence size 0.99999 size))))))
(cl-defmethod scxml-get-cell ((nest-rect scxml---nest-stripe) (coordinate list))
  (if coordinate
      (let ((cell (nth (car coordinate) (scxml-cells nest-rect))))
        (if (eq cell 'undivided)
            (progn (when (cdr coordinate)
                     (error "Unable to find cell at coordinate"))
                   cell)
          (scxml-get-cell cell (cdr coordinate))))
    nest-rect))
(cl-defmethod scxml---get-cell-range ((nest-rect scxml---nest-stripe) (idx integer))
  "return the start and end parametrics of the cell @ IDX location"
  (with-slots (divisions cells) nest-rect
      (cons (if (eq idx 0) 0.0 (nth (1- idx) divisions))
            (if (eq idx (1- (length cells))) 1.0 (nth idx divisions)))))
(cl-defmethod scxml---get-sub-rect ((stripe scxml---nest-stripe) (coordinate list) (rect scxml-rect))
  "Get the rectangle describing the sub division @ coordinate."
  (if (null coordinate)
      rect
    (let ((idx (car coordinate))
          (remaining-coord (cdr coordinate)))
      (with-slots (axis cells divisions) stripe
        (let* ((cons-range (scxml---get-cell-range stripe idx))
               (start-parametric (car cons-range))
               (end-parametric (cdr cons-range))
               ;; (start-parametric (if (eq idx 0) 0.0 (nth (1- idx) divisions)))
               ;; (end-parametric (if (eq idx (1- (length cells))) 1.0 (nth idx divisions)))
               (sub-rect (if (eq axis 'scxml--vertical)
                             (let ((vertical-span (- (scxml-y-max rect) (scxml-y-min rect))))
                               (scxml-rect :x-min (scxml-x-min rect)
                                           :x-max (scxml-x-max rect)
                                           :y-min (+ (scxml-y-min rect) (* start-parametric vertical-span))
                                           :y-max (+ (scxml-y-min rect) (* end-parametric vertical-span))))
                           (let ((horizontal-span (- (scxml-x-max rect) (scxml-x-min rect))))
                             (scxml-rect :y-min (scxml-y-min rect)
                                         :y-max (scxml-y-max rect)
                                         :x-min (+ (scxml-x-min rect) (* start-parametric horizontal-span))
                                         :x-max (+ (scxml-x-min rect) (* end-parametric horizontal-span))))))
               (cell (nth idx cells)))
          (when (and (eq cell 'undivided)
                     (not (null remaining-coord)))
            (error "Unable to find cell by coordinate, tree terminated early"))
          (if (null remaining-coord)
              sub-rect
            (scxml---get-sub-rect cell remaining-coord sub-rect)))))))
(cl-defmethod scxml---get-divisions ((stripe scxml---nest-stripe) (rect scxml-rect) &optional prepend-coordinates)
  "Get out all the divisons in a list of (coordinate . rect) entries."
  (cl-loop for cell in (scxml-cells stripe)
           while cell
           do (when (eq cell nil)
                (error "Someone made a bad cell"))
           with sub-divisions = 'nil
           with cell-idx = 0
           for sub-rect = (scxml---get-sub-rect stripe (list cell-idx) rect)
           for sub-coordinate = (append prepend-coordinates (list cell-idx))
           do (if (eq cell 'undivided)
                  (push (cons sub-coordinate sub-rect) sub-divisions)
                (mapc (lambda (sub-division)
                        (push sub-division sub-divisions))
                      (scxml---get-divisions cell sub-rect sub-coordinate)))
           do (incf cell-idx)
           finally return sub-divisions))

(defun scxml---get-dividers (stripe rect)
  "This doesn't seem ideal, seems like it could be done at the same time as
the scxml---get-divisions call."
  (when (or (not (object-of-class-p stripe 'scxml---nest-stripe))
            (not (object-of-class-p rect 'scxml-rect)))
    (error "Bleh"))
  ;; TODO: roll this into scxml---get-divisions?
  (cl-loop for cell in (scxml-cells stripe)
           with segment-fn = (if (eq (scxml-axis stripe) 'scxml--horizontal)
                                 'scxml-left
                               'scxml-bottom)
           with divider-segments = 'nil
           with cell-idx = 0
           for sub-rect = (scxml---get-sub-rect stripe (list cell-idx) rect)
           for get-divider = (> cell-idx 0)
           do (when get-divider
                (push (funcall segment-fn sub-rect) divider-segments))
           do (when (not (eq cell 'undivided))
                (mapc (lambda (divider) (push divider divider-segments))
                      (scxml---get-dividers cell sub-rect)))
           do (incf cell-idx)
           finally return divider-segments))
(cl-defmethod scxml---get-edit-points ((stripe scxml---nest-stripe) (rect scxml-rect))
  "Get the relative edit idxs of this stripe and all its children."
  (mapcar 'car
        (scxml---get-edit-points-and-coordinates stripe rect)))
(defun scxml---get-edit-points-and-coordinates (stripe rect &optional prepend-coordinates)
  "Get the relative edit idxs of this stripe and all its children.

Should return a list of (idx-point . coordinate)

The coordinate returned is for the cell right before the division."
  (let ((idxs nil))
    (cl-loop for cell in (scxml-cells stripe)
             with cell-idx = 0
             do (when (not (eq cell 'undivided))
                  (let ((sub-rect (scxml---get-sub-rect stripe (list cell-idx) rect)))
                    (mapc (lambda (idx) (push idx idxs))
                          (scxml---get-edit-points-and-coordinates cell
                                                                   sub-rect
                                                                   (append prepend-coordinates (list cell-idx))))))
             do (incf cell-idx))

    (let ((point-producer (if (eq (scxml-axis stripe) 'scxml--horizontal)
                              (lambda (division-param)
                                (2dg-point :x (2dg-parametric (scxml-x-span rect) division-param)
                                             :y (/ (+ (scxml-y-min rect) (scxml-y-max rect)) 2.0)))
                            (lambda (division-param)
                              (2dg-point :x (/ (+ (scxml-x-min rect) (scxml-x-max rect)) 2.0)
                                           :y (2dg-parametric (scxml-y-span rect) division-param))))))
      (cl-loop for division-param in (scxml-divisions stripe)
               for cell-idx = 0
               do (push (cons (funcall point-producer division-param)
                              (list (append prepend-coordinates (list cell-idx))))
                        idxs)
               do (incf cell-idx)))
    idxs))

(defun scxml---stripe-build-division-offsets (num-cells)
  (let ((size (/ 1.0 (float num-cells))))
    (number-sequence size 0.99999 size)))
(defun scxml---stripe-build-cells (num-cells)
  (cl-loop for cell from 1 to num-cells
           with cells = 'nil
           do (push 'undivided cells)
           finally return cells))
(defun scxml---stripe-build (num-cells &optional axis)
  (unless (and (integerp num-cells) (> num-cells 0))
    (error "Unable to build a stripe with 0 cells"))
  (scxml---nest-stripe :axis (or axis 'scxml--horizontal)
                       :cells (scxml---stripe-build-cells num-cells)
                       :divisions (scxml---stripe-build-division-offsets num-cells)))
(defun scxml---stripe-initialize (container num-cells &optional axis)
  (unless (and (integerp num-cells) (> num-cells 0))
    (error "Unable to build a stripe with 0 cells"))
  (oset container axis (or axis 'scxml--horizontal))
  (oset container cells (scxml---stripe-build-cells num-cells))
  (oset container divisions (scxml---stripe-build-division-offsets num-cells))
  container)

(cl-defmethod scxml--set-layout ((container scxml---nest-stripe) (num-rows integer) (num-columns integer))
  ;; crush and reset -  build a row major grid of cells from nest-stripes
  "Set the layout of this CONTAINER to be NUM-ROWS by NUM-COLUMNS, row major."
  (if (equal num-rows 0)
      (scxml---stripe-initialize container num-columns 'scxml--horizontal)
    (scxml---stripe-initialize container num-rows 'scxml--vertical)
    (cl-loop for row-idx from 0 to (1- num-rows)
             do (scxml-set container
                           (list row-idx)
                           (scxml---stripe-build num-columns 'scxml--horizontal))
             finally return container)))
(defun scxml---stripe-clone (stripe)
  "Get a deep clone of this stripe."
  (scxml---nest-stripe :axis (scxml-axis stripe)
                       :divisions (mapcar 'identity (scxml-divisions stripe))
                       :cells (scxml---stripe-clone-cells stripe)))
(defun scxml---stripe-clone-cells (stripe)
  "Get a deep clone of the STRIPE's cells *only*.

usage: (scxml---nest-stripe :axis (scxml-axis thing)
                            :divisions (scxml-divisions thing)
                            :cells (scxml---strip-clone-cells thing))"
  (mapcar (lambda (cell)
            (if (eq cell 'undivided)
                'undivided
              (scxml---stripe-clone cell)))
          (scxml-cells stripe)))

(defclass scxml-drawing-nest-rect (scxml-drawing-divided-rect scxml---nest-stripe)
  ()
  :documentation "A nested grid approach to handling many divisions of a rectangle."
  ;; I should be able to identify any specific division of the divided-rect
  ;; a divided rect should be a tree of stripes (container)
  ;; - stripes should be nodes in this tree.
  ;; I should be able to add a stripe anywhere
  ;; I should be able to add a cell to a stripe anywhere.
  ;; ---
  ;; automatic plotting - can just be drawn in order
  ;;  - it's automatic so whatever
  ;;  - for hinted
  ;;    - the hint can contain all the divisions and their layout
  ;;    - there's nothing stopping the hint from holding *some* identifier for what state goes in what cell of the nested stripes.
  ;;    - the plot routine can handle the toggling.
  )
(defclass scxml---drawing-nest-rect-hint ()
  ((relative-rect :initarg :relative-rect
                  :accessor scxml-relative-rect
                  :type scxml-rect)
   (stripe-invalid :accessor scxml-stripe-invalid
                   :type (member t nil)
                   :initarg nil
                   :documentation "It's possible for only the stripe to be invalid (e.g. a child was added or removed.")
   (stripe :initarg :stripe
           :accessor scxml-stripe
           :type scxml---nest-stripe)))

(cl-defmethod scxml--build-empty-nest-rect-hint ((relative-rect scxml-rect))
  "Build an empty parallel hint from only a rectangle."
  (scxml---drawing-nest-rect-hint :relative-rect relative-rect
                                  :stripe (scxml---stripe-build 1)))

(cl-defmethod scxml-get-divisions ((divided-rect scxml-drawing-nest-rect))
  (scxml---get-divisions divided-rect (scxml-get-inner-canvas divided-rect)))
(cl-defmethod scxml---set-dividers ((divided-rect scxml-drawing-nest-rect))
  (let ((inner-rect (scxml-get-inner-canvas divided-rect)))
    (oset divided-rect
          dividers
          (cons (scxml-top inner-rect)
                (scxml---get-dividers divided-rect inner-rect)))
    divided-rect))
(cl-defmethod scxml-num-edit-idxs ((divided-rect scxml-drawing-nest-rect))
  "How many edit idx points are there for this DIVIDED-RECT"
  (+ 8 (length (scxml---get-edit-points divided-rect divided-rect))))
(cl-defmethod scxml-edit-idx-point ((divided-rect scxml-drawing-nest-rect) (idx integer))
  "Get the pixel location of the given edit idx BL is zero, go CCW from there"
  (if (< idx 8)
      (cl-call-next-method)
    (nth (- idx 8) (scxml---get-edit-points divided-rect divided-rect))))
(cl-defmethod scxml-edit-idx-points ((divided-rect scxml-drawing-nest-rect))
  "Get the pixel locations of the edit idxs for DIVIDED-RECT as a list."
  (append (cl-call-next-method)
          (scxml---get-edit-points divided-rect divided-rect)))
(cl-defmethod scxml-build-idx-edited ((divided-rect scxml-drawing-nest-rect) (edit-idx integer) (move-vector 2dg-point) (viewport scxml-viewport))
  ;; This needs to be documented.
  (if (< edit-idx 8)
      (let ((rect-shell (cl-call-next-method)))
        (scxml-drawing-nest-rect :x-min (scxml-x-min rect-shell)
                                 :x-max (scxml-x-max rect-shell)
                                 :y-min (scxml-y-min rect-shell)
                                 :y-max (scxml-y-max rect-shell)
                                 :parent (scxml-parent rect-shell)
                                 :axis (scxml-axis divided-rect)
                                 :cells (scxml-cells divided-rect)
                                 :divisions (scxml-divisions divided-rect)))
    ;; Determine which division is being edited.
    (let ((deep-clone (scxml---stripe-clone divided-rect)))
      (let* ((edit-pts-and-coords (scxml---get-edit-points-and-coordinates deep-clone divided-rect))
             (edit-coord (cadr (nth (- edit-idx 8) edit-pts-and-coords))) ;this is packed in a weird list configuration.
             (child-coord (car (last edit-coord)))
             (parent-coord (nbutlast edit-coord))
             (parent-cell (scxml-get-cell deep-clone parent-coord))
             (parent-rect (scxml---get-sub-rect deep-clone parent-coord divided-rect))
             (axis-vector (if (eq (scxml-axis parent-cell) 'scxml--horizontal)
                              (2dg-point :x 1.0 :y 0.0)
                            (2dg-point :x 0.0 :y 1.0)))
             (parent-cell-span (if (eq (scxml-axis parent-cell) 'scxml--horizontal)
                                   (scxml-x-span parent-rect)
                                 (scxml-y-span parent-rect)))
             (allowed-movement (2dg-dot-prod axis-vector move-vector))
             (relative-movement (/ allowed-movement (2dg-length parent-cell-span))))
        ;; determine the cell axis and bump (nth child-coord (scxml-divisions parent-cell))
        ;; by a tiny amount.
        ;; Not sure if this should clone or modify, cloning now.
        (setf (nth child-coord (scxml-divisions parent-cell))
              (+ relative-movement (nth child-coord (scxml-divisions parent-cell))))

        (scxml-drawing-nest-rect :x-min (scxml-x-min divided-rect)
                                 :x-max (scxml-x-max divided-rect)
                                 :y-min (scxml-y-min divided-rect)
                                 :y-max (scxml-y-max divided-rect)
                                 :parent (scxml-parent divided-rect)
                                 :axis (scxml-axis deep-clone)
                                 :cells (scxml-cells deep-clone)
                                 :divisions (scxml-divisions deep-clone))))))
(cl-defmethod scxml-build-move-edited ((divided-rect scxml-drawing-nest-rect) (move-vector 2dg-point) (viewport scxml-viewport))
  ;; TODO- possibly this would work for *all* scxml-drawings?
  (2dg-incf (clone divided-rect) move-vector))
(cl-defmethod scxml-build-hint ((divided-rect scxml-drawing-nest-rect) (parent-canvas scxml-inner-canvas))
  "Build a hint for DIVIDED-RECT inside of PARENT-CANVAS."
  (scxml---drawing-nest-rect-hint
   :relative-rect (2dg-relative-coordinates parent-canvas divided-rect)
   :stripe (scxml---stripe-clone divided-rect)))
(defun scxml---divided-rect-division-segments (divided-rect)
  "Get a list of segments to draw inside of DIVIDED-RECT."
  ;; first line in the one right below the header
  (with-slots (x-min x-max y-min (real-y-max y-max)) divided-rect
    (let ((y-max (- real-y-max scxml---divided-rect-header-height)))
      (list (scxml-segment :start (2dg-point :x x-min :y y-max)
                           :end (2dg-point :x x-max :y y-max))))))

(provide 'scxml-drawing-divided-rect)
;;; scxml-drawing-divided-rect.el ends here
