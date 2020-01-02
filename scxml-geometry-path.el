;;; scxml-geometry-path.el --- scxml geometry path helpers -*- lexical-binding: t -*-

;;; Commentary:
;; An scxml-path represents an ordered collection of contiguous
;; segments stored internally as a collection of points in a
;; connect-the-dots style system.  A path may contain no
;; points/segments when it is empty.
;;
;; An scxml-cardinal-path represents an scxml-path where each segment
;; of the path must be a purely horizontal or vertical line (a line in
;; a cardinal direction).

;;; Code:
(require 'eieio)
(require 'scxml-geometry-point)
(require 'scxml-geometry-segment)

(defclass scxml-path ()
  ((points :initarg :points
           :accessor scxml-points
           :initform '()
           :type (or null list)))
  ;; TODO - Should there be a make-instance for this class which
  ;; validates that all things in the points slot are actually points?
  ;; If so, would there be some way to circumvent that check when it's
  ;; called by a trustworthy source such as the make-instance for
  ;; scxml-cardinal-path?
)
(defclass scxml-cardinal-path (scxml-path)
  ())
(defun scxml---is-cardinal-path? (points-list)
  "Return non-nil if POINTS-LIST contains a list of cardinal path points."
  (cl-loop with last-pt = (first points-list)
           with is-cardinal = 't
           for pt in (cdr points-list)
           when pt
           do (setq is-cardinal (scxml-cardinal-displacement? pt last-pt))
           do (setq last-pt pt)
           until (not is-cardinal)
           finally return is-cardinal))
(cl-defmethod make-instance ((class (subclass scxml-cardinal-path)) &rest slots)
  "Ensure the points provided are truly a cardinal path before making the instance."
  (let ((points (plist-get slots :points)))
    (unless (or (null points)
                (scxml---is-cardinal-path? points))
      (error "An scxml-cardinal-path must have only vertical or horizontal segments")))
  (cl-call-next-method))

(cl-defmethod scxml-print ((path scxml-path))
  "Return a stringified version of PATH for human eyes."
  (with-slots (points) path
    (format "%s[%s]"
            (if (scxml-cardinal-path-p path)
                "cp"
              "p")
            (mapconcat (lambda (pt) (scxml-print pt))
                       points
                       ", "))))
(cl-defmethod cl-print-object ((object scxml-path) stream)
  "Pretty print the OBJECT to STREAM."
  (princ (scxml-print object) stream))
(cl-defmethod scxml-num-points ((path scxml-path))
  "Return the number of points that make up PATH."
  (length (oref path points)))
(cl-defmethod scxml-nth ((path scxml-path) (N number))
  "Return the N-th point from PATH."
  (nth N (oref path points)))
(cl-defmethod scxml-last ((path scxml-path))
  "Return the last point in PATH."
  (scxml-nth path (1- (scxml-num-points path))))
(cl-defmethod scxml-first ((path scxml-path))
  "Return the first point in PATH."
  (scxml-nth path 0))
(cl-defmethod scxml-start-vector ((path scxml-path))
  "Return a characteristic-vector of PATH's first segment.

May return nil if PATH contains 1 or less points."
  (let ((second (scxml-nth path 1)))
    (when second
      (scxml-subtract second (scxml-nth path 0)))))
(cl-defmethod scxml-end-vector ((path scxml-path))
  "Return a characteristic-vector of PATH's last segment.

May return nil if PATH contains 1 or less points."
  (let ((num-pts (scxml-num-points path)))
    (when (> num-pts 2)
      (scxml-subtract (scxml-nth path (1- num-pts))
                      (scxml-nth path (- num-pts 2))))))
(cl-defmethod scxml-push ((path scxml-path) (pt scxml-point))
  "Modify PATH by pushing PT on to the start."
  (push pt (scxml-points path)))
(cl-defmethod scxml-almost-equal ((A scxml-path) (B scxml-path) &optional tolerance)
  "Return non-nil if A and B are equal within TOLERANCE."
  (let ((a-len (scxml-num-points A))
        (b-len (scxml-num-points B)))
    (if (not (eq a-len b-len))
        'nil
      (let ((i 0)
            (miss-match 'nil))
        (while (and (not miss-match)
                    (< i a-len))
          (when (not (scxml-almost-equal (scxml-nth A i)
                                         (scxml-nth B i)
                                         tolerance))
            (setq miss-match 't))
          (incf i))
        (not miss-match)))))
(cl-defmethod scxml-segments ((path scxml-path))
  "Return an ordered list of scxml-segment objects describing PATH.

May return nil if PATH has less than 2 points."
  (let* ((points (scxml-points path))
         (last-point (car points)))
    (mapcar (lambda (pt)
              (let ((segment (scxml-segment :start last-point
                                            :end pt)))
                (setq last-point pt)
                segment))
            (cdr points))))
(cl-defmethod scxml-length ((path scxml-path))
  "Return the length of the path (Not the displacement or number of points).

Not to be confused with scxml-num-points.  This is the same as
adding up the length of every segment."
  (with-slots (points) path
    (let ((last-pt (first points))
          (len 0.0))
      (mapc (lambda (pt)
              (incf len (scxml-distance last-pt pt))
              (setq last-pt pt))
            (cdr points))
      len)))
(cl-defmethod scxml-pierced? ((A scxml-path) (B scxml-path) &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  "Return non-nil if A pierces B at some point.

End points are considered by their flags."
  (let* ((a-segments (scxml-segments A))
         (b-segments (scxml-segments B))
         (a-first (car a-segments))
         (a-last (last a-segments))
         (b-first (car b-segments))
         (b-last (last b-segments)))
    (block scxml--collision
      (mapc                             ;A -map
       (lambda (a-segment)
         (let ((allow-sa-min (or allow-A-start (not (eq a-first a-segment))))
               (allow-sa-max (or allow-A-end (not (eq a-last a-segment)))))
           (mapc                        ;B -map
            (lambda (b-segment)
              (let ((allow-sb-min (or allow-B-start (not (eq b-first b-segment))))
                    (allow-sb-max (or allow-B-end (not (eq b-last b-segment)))))
                (when (scxml-pierced? a-segment
                                      b-segment
                                      allow-sa-min
                                      allow-sa-max
                                      allow-sb-min
                                      allow-sb-max)
                  (return-from scxml--collision 't))))
            b-segments)))
       a-segments)
      'nil)))
(cl-defmethod scxml-distance ((path scxml-path) (point scxml-point))
  "Return the minimum distance between PATH and POINT.

The implementation is not efficient, use caution."
  (cl-loop for segment in (scxml-segments path)
           for distance = (scxml-distance segment point)
           with best = 'nil
           do (when (or (null best) (< distance best))
                (setq best distance))
           finally return best))

(cl-defmethod scxml-has-intersection ((rect scxml-rect) (path scxml-path) &optional evaluation-mode)
  "Return non-nil if RECT intersects PATH using EVALUATION-MODE."
  (cl-loop for segment in (scxml-segments path)
           when (scxml-has-intersection rect segment evaluation-mode)
           return t
           finally return nil))
(cl-defmethod scxml-simplify ((path scxml-path))
  "Return a simplified version of the PATH.

Removes duplicates and colinear points."
  (with-slots (points) path
    (scxml-path :points (scxml---path-append-simplify points))))

(cl-defmethod scxml---get-deltas ((points list))
  "Given a list of N points, return a list of deltas between the points.

Will return nil if length of the path is 1 or less."
    (if (< (length points) 2)
        'nil
      (let ((last-point (first points))
            (deltas 'nil))
        (mapc (lambda (pt)
                (push (scxml-subtract pt last-point) deltas)
                (setq last-point pt))
              (cdr points))
        (nreverse deltas))))
;; (cl-defmethod scxml---get-deltas ((path scxml-path))
;;   "Given a path of length N, return a list of deltas between the points.
;; Will return nil if length of the path is 1 or less."
;;   (with-slots (points) path
;;     (scxml---get-deltas points)))
(cl-defmethod scxml---path-from-deltas ((deltas list) (start-point scxml-point))
  "Return a list of points starting at START-POINT and having DELTAS as per point deltas.

Something of the opposite of scml---get-deltas."
  (let ((points (list start-point)))
    (mapc (lambda (delta)
            (push (scxml-add delta (first points)) points))
          deltas)
    (nreverse points)))

;; Path *building* functions:
(cl-defmethod scxml-build-path-straight-line ((start scxml-point) (end scxml-point))
  "Make a path which is a single straight line from START to END.

This might be an scxml-path or an scxml-cardinal-path."
  (condition-case nil
      (scxml-cardinal-path :points `(,start ,end))
    (error (scxml-path :points `(,start ,end)))))
(defun scxml---path-cardinal-direction (start-pt entry-direction destination-pt)
  "Given a START-PT point coming from ENTRY-DIRECTION find a vector to get closer to DESTINATION-PT.

This is a helper function for the cardinal direction path
generator."
  (let* ((unit-direction (scxml-normalized entry-direction))
         (delta (scxml-subtract destination-pt start-pt))
         (unit-delta (scxml-normalized delta))
         (dot (scxml-dot-prod unit-direction unit-delta)))
    (cond
     ;; must go _backwards_, turn around.
     ((<= dot scxml--almost-zero)
      ;; go one direction or the other, determine which get closer
      (let ((dir-rot-90 (scxml-rotate-90 unit-direction)))
        (if (>= (scxml-dot-prod dir-rot-90 unit-delta) 0.0)
            ;; guessed direction is correct
            dir-rot-90
          (scxml-additive-inverse dir-rot-90))))
     ;; just keep going?
     ('t
      unit-direction))))
(defun scxml---path-cardinal (start end entry-vector exit-vector min-segment-distance &optional strict-min-segment-distance)
  "Build a cardinal direction path between START and END.

Function will ensure that the path does not conflict with
ENTRY-VECTOR and EXIT-VECTOR constraints.

When STRICT-MIN-SEGMENT-DISTANCE is non-nil, never create a
segment of length less than MIN-SEGMENT-DISTANCE.  Otherwise,
default to making segments of at least MIN-SEGMENT-DISTANCE but
allow shorter segments in extraorinary cases.

This function works by examining the start point and end point
and trying to draw a path from each point that will intersect
someplace (hopefully in the middle).  If this is not possible it
will determine which (start or end) is the furthest away from
being able to join in the middle and will attach a segment to it
to take a step closer to intersectionthe next time this function
is called.  This function will continue to take steps towards
joining start and end, recursing until it joins them."
  (let ((start-vec (scxml---path-cardinal-direction start
                                                    entry-vector
                                                    end))
        (reverse-end-vec (scxml---path-cardinal-direction end
                                                         (scxml-additive-inverse exit-vector)
                                                         start)))
    (cl-flet ((perpendicular-vectors?
               (A B)
               (>= (abs (scxml-cross-prod A B))
                   scxml--almost-zero))
              (perpendicular-collision
               (A-pt A-dir B-pt)
               ;; this collision can only happen at one of two points.
               ;; (scxml-point :x from A-pt and :y from B-pt)
               ;; (scxml-point :x from B-pt and :y from A-pt)
               (if (equal (scxml-x A-dir) 0.0)
                   ;; A-dir is vertical, therefore your intersection must be at A-pt's X
                   ;; determine if the collision is at A or B's Y coordinates
                   (scxml-point :x (scxml-x A-pt) :y (scxml-y B-pt))
                 ;; A-dir is horizontal, therefore your intersection must be at A-pt's Y
                 (scxml-point :x (scxml-x B-pt) :y (scxml-y A-pt)))))
      (cond
       ;; Vectors are heading in perpendicular directions.  If possible join them
       ;; otherwise turn the best candidate towards a future collision.
       ((perpendicular-vectors? start-vec reverse-end-vec)
        (let* ((intersection-pt (perpendicular-collision start start-vec end))
               (start-delta (scxml-subtract intersection-pt start))
               (start-parametric (scxml-dot-prod start-vec start-delta))
               (end-delta (scxml-subtract intersection-pt end))
               (end-parametric (scxml-dot-prod reverse-end-vec end-delta)))
          ;; -if both are positive, you've found a connection and you're done.
          ;; -if one of them is negative, that vec goes min distance the other
          ;; goes zero distance
          ;; -if both of them are negative they both go min-distance
          (if (> start-parametric 0.0)
              (if (> end-parametric 0.0)
                  ;; both are positive, done.
                  (list start intersection-pt end)

                ;; start is positive, end is negative.
                ;; start goes zero distance, end goes min distance, try again.
                (let ((pre-end (scxml-add end (scxml-scaled reverse-end-vec
                                                            min-segment-distance))))
                  (append (scxml---path-cardinal start
                                               pre-end
                                               entry-vector
                                               (scxml-additive-inverse reverse-end-vec)
                                               min-segment-distance)
                          (list end))))
            ;; start-parametric is =< 0
            (if (> end-parametric 0.0)
            ;; start-parametric is negative(or zero), end is positive.
            ;; end goes zero, start goes min distance, try again
                (let ((post-start (scxml-add start
                                             (scxml-scaled start-vec
                                                           min-segment-distance))))
                  (cons start
                        (scxml---path-cardinal post-start
                                             end
                                             start-vec
                                             exit-vector
                                             min-segment-distance)))
              ;; start parametric is negative, end parametric is negative
              ;; both of them go min distance
              (let ((post-start (scxml-add start
                                           (scxml-scaled start-vec
                                                         min-segment-distance)))
                    (pre-end (scxml-add end
                                        (scxml-scaled reverse-end-vec
                                                      min-segment-distance))))
                (cons start
                      (append (scxml---path-cardinal post-start
                                                     pre-end
                                                     start-vec
                                                     (scxml-additive-inverse reverse-end-vec)
                                                     min-segment-distance)
                              (list end))))))))

       ;; the vectors are parallel and going in opposites directions
       ;; - find a midway point along continuations, go there - path jog
       ((< (scxml-dot-prod start-vec reverse-end-vec) 0.0)
        (let* ((delta (scxml-subtract end start))
               (parallel-distance (scxml-dot-prod delta start-vec))
               (perp-distance (abs (scxml-cross-prod delta start-vec))))

          (cond
           ;; if the perp-distance is < almost-zero then just slap them together
           ((and (< perp-distance scxml--almost-zero)
                 (>= parallel-distance min-segment-distance))
            ;; satisfies the constraints for a direct connection
            (list start end))

           ;; things can't be solved in one shot
           ((<= parallel-distance 0.0)
            ;; these vectors don't actually get you any closer together.
            ;; looks like these constraints need a few more segments to be able to solve
            ;; bend one of the paths in the correct direction, travel the minimum
            ;; distance then try again.
            (let ((post-start (scxml-add start
                                         (scxml-scaled start-vec min-segment-distance))))
              (cons start
                    (scxml---path-cardinal post-start
                                         end
                                         start-vec
                                         exit-vector
                                         min-segment-distance))))

            ;; if the perp-distance is < min-segment-distance this algorithm won't work.
            ;; if the parallel distance is < 2*min-segment distance this algorithm won't work
            ;;  in either of those two cases this will have to get a bit smarter.
           ((and strict-min-segment-distance
                 (or (< perp-distance min-segment-distance)
                     (< (* 2.0 parallel-distance) min-segment-distance)))

            (error "Unable to find cardinal path right now, make path router smarter[ start %s, end %s, perp-distance %s, parallel-dist %s]"
                   (scxml-print start)
                   (scxml-print end)
                   perp-distance
                   parallel-distance))


           ;; ok, conditions satisfied for a jog type path.
           ('t
            (let ((midway-displacement (scxml-scaled start-vec (/ parallel-distance 2.0))))
              (list start
                    (scxml-add start midway-displacement)
                    (scxml-subtract end midway-displacement)
                    end))))))

       ;; the vectors are parallel and facing the same direction
       ;; whoever is "ahead" turns, recurse
       ;; use delta to find who is "ahead" - then turn the ahead
       ('t
        (let* ((delta (scxml-subtract end start)))
          (if (> (scxml-dot-prod delta start-vec) 0.0)
              ;;end is further 'ahead'
              (let ((pre-end (scxml-add end
                                        (scxml-scaled reverse-end-vec min-segment-distance))))
                (append (scxml---path-cardinal start
                                             pre-end
                                             entry-vector
                                             (scxml-additive-inverse reverse-end-vec)
                                             min-segment-distance)
                        (list end)))
            ;; start is further 'ahead'
            (let ((post-start (scxml-add start
                                         (scxml-scaled start-vec min-segment-distance))))
              (cons start
                    (scxml---path-cardinal post-start
                                         end
                                         start-vec
                                         exit-vector
                                         min-segment-distance))))))))))
(cl-defgeneric scxml-build-path-cardinal ((start scxml-point) (end scxml-point) (entry-vector scxml-point) (exit-vector scxml-point) (min-segment-distance number) &optional min-start-segment-distance min-end-segment-distance)
  "Create a cardinal path joining START and END.

The ENTRY-VECTOR describes the direction the path should start if
possible and the EXIT-VECTOR describes the direction the path
should end if possible.  The only assurance is that the computed
entry and exit directions will not be 180 degrees different from
the EXIT-VECTOR and ENTRY-VECTOR.

MIN-SEGMENT-DISTANCE is a goal parameter.  The algorithm will try
not to make any connecting segments that are shorter than this
parameter.  No promises are made.
"
  (when (or (not (null min-start-segment-distance))
            (not (null min-end-segment-distance)))
    (error "Write this part, that will handle min start and end segment distances"))
  (let ((points (scxml---path-cardinal start end entry-vector exit-vector min-segment-distance)))
    (scxml-cardinal-path :points points)))
(defun scxml---path-append-simplify (&rest n-path-pts)
  "Take all points from N-PATH-PTS lists and simplify them.

Remove duplicate points.
Remove colinear intermediary points."
  (let ((rev-s-points 'nil)           ;reverse order simplified points
        (last-vec 'nil)
        (last-point 'nil))

    ;; Find the first non-null path and get the first point from it.
    ;; Use that first point to start accumulation.
    (cl-loop for paths-remaining on n-path-pts
             for first-path = (first paths-remaining)
             when first-path
             return (progn
                      (setf last-point (first first-path)
                            n-path-pts paths-remaining)
                      (push last-point rev-s-points)
                      (setcar n-path-pts (cdr first-path))))

    (mapc (lambda (path-pts)
            (mapc (lambda (pt)
                    (when (not (scxml-almost-equal pt last-point))
                      (let ((cur-vec (scxml-normalized (scxml-subtract pt last-point))))
                        (when (and last-vec
                                   (not (scxml-almost-equal cur-vec last-vec)))
                            (push last-point rev-s-points))
                        (setq last-vec cur-vec)))
                    (setq last-point pt))
                  path-pts))
          n-path-pts)
    ;; Push the very last point on to the output.
    (nreverse
     (if (scxml-almost-equal last-point (first rev-s-points))
                 rev-s-points
               (push last-point rev-s-points)))))
(defun scxml---path-stretch (points force-start force-end)
  "Return a path (list) from FORCE-START to FORCE-END which 'feels' like POINTS.

The typical use case is when there was formerly a path
characterized by POINTS which connected two things.  For any
reason one or both of those two things which the path connected
were moved slightly.  This function will return a list of points
which tries to look like the original points but is stretched to
have the desired start and end points."
  (let* ((old-start (first points))
         (old-end (car (last points)))
         (force-displacement (scxml-subtract force-end force-start))
         (old-displacement (scxml-subtract old-end old-start)))
    (when (and (scxml-almost-equal old-start force-start)
               (scxml-almost-equal old-end force-end))
      ;; TODO - deterimine the source of this and attempt to remove.
      ;; (error "Call to path-stretch but no stretching was required?")
      points)

    (cl-flet ((get-scale (force-displacement old-displacement)
                         (if (equal old-displacement 0.0)
                             (if (equal old-displacement force-displacement)
                                 1.0    ;they're the same, no scale needed.
                               nil)    ;they're not the same and the old displacement is zero - no applicable scaling
                           (/ force-displacement old-displacement)))
              (path-stretch-freedom (points)
                                    (cl-loop with x-free = 'nil
                                             with y-free = 'nil
                                             with last-pt = (first points)
                                             for pt in (cdr points)
                                             for delta = (scxml-subtract pt last-pt)
                                             do (setf x-free (or x-free (not (equal (scxml-x delta) 0.0)))
                                                      y-free (or y-free (not (equal (scxml-y delta) 0.0))))
                                             when (and x-free y-free)
                                             return 't
                                             do (setq last-pt pt)
                                             finally return 'nil)))

      (let ((scale-x (get-scale (scxml-x force-displacement) (scxml-x old-displacement)))
            (scale-y (get-scale (scxml-y force-displacement) (scxml-y old-displacement))))
        (if (and scale-x scale-y)
            ;; both scalings are valid, apply them and finish
            (let ((scale (scxml-point :x scale-x :y scale-y)))
              (mapcar (lambda (point)
                        (scxml-add (scxml-scaled (scxml-subtract point old-start) scale)
                                   force-start))
                      points))
          ;; one scaling or the other is not applicable, must jog or offset things.
          (if (not (path-stretch-freedom points))
              ;; there is no freedom in this path, just insert a jog.
              (scxml---path-cardinal force-start
                                     force-end
                                     (scxml-subtract (second points)
                                                     (first points))
                                     (scxml-subtract (car (last points))
                                                     (car (last points 2)))
                                     1.0) ;TODO - this 1.0 is a guess, I'm not sure what it should really be
            ;; You have the liberty to inject displacements into this path to
            ;; make it stretch correctly.  Determine the displacement.
            (let ((deltas (scxml---get-deltas points))
                  (additional-displacement (scxml-subtract force-displacement old-displacement))
                  (vertical-deltas 'nil)
                  (horizontal-deltas 'nil))
              (cl-loop for delta in deltas
                       do (when (not (equal (scxml-x delta) 0.0))
                            (push delta horizontal-deltas))
                       do (when (not (equal (scxml-y delta) 0.0))
                            (push delta vertical-deltas)))
              ;; modify deltas
              (unless (equal (scxml-x additional-displacement) 0.0)
                (let ((additional-horizontal (/ (scxml-x additional-displacement)
                                                (float (length horizontal-deltas)))))
                  (cl-loop for delta in horizontal-deltas
                           do (oset delta x (+ (scxml-x delta) additional-horizontal)))))
              (unless (equal (scxml-y additional-displacement) 0.0)
                (let ((additional-vertical (/ (scxml-y additional-displacement)
                                              (float (length vertical-deltas)))))
                  (cl-loop for delta in vertical-deltas
                           do (oset delta y (+ (scxml-y delta) additional-vertical)))))
                (scxml---path-from-deltas deltas force-start))))))))

(defun scxml---nudge-path-start (points move-vector)
  "Move the first element of POINTS by MOVE-VECTOR and update subsequent points."
  (unless points
    (error "Must provide at least one point for nudge-path-start"))
  (let ((num-points (length points)))
    (if (= num-points 1)
        ;; if there's only one point, just move it.
        (list (scxml-add (car points) move-vector))
      ;; there is more than one point.
      (let* ((first-point (first points))
             (raw-offset (scxml-subtract (second points) first-point))
             (raw-box-magnitude (scxml-box-magnitude raw-offset))
             (first-offset (if (scxml-almost-zero raw-box-magnitude)
                               ;; If there's no displacement between these two points
                               ;; then they're likely in the exact same place.
                               ;; If true I can separate them in any direction I choose
                               ;; so I choose move-vector
                               move-vector
                             ;; normal path - these two points are not the same.
                             raw-offset))
             (first-unit-offset (scxml-normalized first-offset))
             (parallel-move (scxml-scaled first-unit-offset
                                          (scxml-dot-prod first-unit-offset move-vector)))
             (perpendicular-move (scxml-subtract move-vector parallel-move)))

        (if (scxml-almost-zero (scxml-box-magnitude perpendicular-move))
            ;; You're requesting a move in a direction parallel to your
            ;; current segment direction, just allow the movement.
            (cons (scxml-add first-point move-vector)
                  (cdr points))
          ;; You're moving in a direction _not_entirely_ parallel
          ;; move your first point by move-vec and append to
          ;; recursion with (cdr path) and perpendicular-move
          (cons (scxml-add first-point move-vector)
                (scxml---nudge-path-start (cdr points) perpendicular-move)))))))
(cl-defmethod scxml-nudge-path ((path list) (point-idx integer) (move-vector scxml-point))
  "Nudge POINT-IDX point in PATH by MOVE-VECTOR and update surrounding points."
  (when (or (< point-idx 0) (>= point-idx (length path)))
    (error "Error: point-idx must be < path length"))
  (let ((before-path (list (first path)))
        (after-path path))
    (dotimes (idx point-idx)
      (setq after-path (cdr after-path))
      (setq before-path (cons (first after-path) before-path)))
    (append (reverse (scxml---nudge-path-start before-path move-vector))
            (cdr (scxml---nudge-path-start after-path move-vector)))))

(provide 'scxml-geometry-path)
;;; scxml-geometry-path.el ends here
