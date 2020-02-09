;;; 2dg-geometry-segment.el --- scxml geometry segment helpers -*- lexical-binding: t -*-

;;; Commentary:
;; 2dg-segment is a line segment that connects two points
;; It has a direction, starting at START and ending at END.

;;; Code:
(require 'eieio)
(require 'scxml-geometry-point)
(require 'scxml-geometry-span)

(defclass 2dg-segment ()
  ((start :initarg :start
          :accessor 2dg-start
          :type 2dg-point)
   (end :initarg :end
        :accessor 2dg-end
        :type 2dg-point))
  :documentation "2d Line segment that can go in any direction (omnidirectional)")
(cl-defmethod 2dg-pprint ((segment 2dg-segment))
  "Return a stringified version of SPAN for human eyes."
  (with-slots (start end) segment
    (format "s[%s -> %s]" (2dg-pprint start) (2dg-pprint end))))
(cl-defmethod cl-print-object ((segment 2dg-segment) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dg-pprint segment) stream))
(cl-defmethod 2dg-characteristic-vector ((segment 2dg-segment))
  "Return the characteristic vector of SEGMENT.

Characteristic vector is:
  (SEGMENT end) - (SEGMENT start)."
  (with-slots (start end) segment
    (2dg-subtract end start)))
(cl-defmethod 2dg-unit-vector ((segment 2dg-segment))
  "Return a normalized characteristic vector."
  (2dg-normalized (2dg-characteristic-vector segment)))
(cl-defmethod 2dg-parametric ((segment 2dg-segment) parametric-coord)
  "Return the point at PARAMETRIC-COORD along SEGMENT.

Valid parametric coordinates range from 0 to 1, inclusive.  A
PARAMETRIC-COORD of 0 will yield the start point, 1 will yield
the end point and 0.5 will yield the mid point."
  (let ((char-vec (2dg-characteristic-vector segment))
        (start (2dg-start segment)))
    (2dg-point :x (+ (2dg-x start)
                       (* parametric-coord (2dg-x char-vec)))
                 :y (+ (2dg-y start)
                       (* parametric-coord (2dg-y char-vec))))))
(cl-defmethod 2dg-length ((segment 2dg-segment))
  "Return the length of SEGMENT."
  (with-slots (start end) segment
    (2dg-distance start end)))
(cl-defmethod 2dg-box-magnitude ((segment 2dg-segment))
  "Return the box magnitude of SEGMENT.

Box magnitude is defined as the edge length of the smallest
square which could contain this SEGMENT."
  (2dg-box-magnitude (2dg-characteristic-vector segment)))
(cl-defmethod 2dg-almost-equal ((A 2dg-segment) (B 2dg-segment) &optional tolerance)
  "Return non-nil if A and B almost equal within a TOLERANCE"
  (and (2dg-almost-equal (2dg-start A) (2dg-start B) tolerance)
       (2dg-almost-equal (2dg-end A) (2dg-end B) tolerance)))
(cl-defmethod 2dg-flipped ((segment 2dg-segment))
  "Return a new segment having reversed start and end of SEGMENT."
  (with-slots (start end) segment
    (2dg-segment :start end :end start)))
(cl-defmethod 2dg-centroid ((segment 2dg-segment))
  "Return the midpoint of SEGMENT."
  (with-slots (start end) segment
    (2dg-point :x (/ (+ (2dg-x start) (2dg-x end)) 2.0)
                 :y (/ (+ (2dg-y start) (2dg-y end)) 2.0))))
(defun 2dg---distance-parallel-parametrics (A B)
  "Determine parametric coordinates for two parallel lines where they are closest.

Returns a list consisting of '(A-parametric-range B-parmetric-range).
It is assumed that you made sure A and B are parallel before calling."
  ;; get the distance along A to B.start and B.end
  ;; get the distance along B to A.start and B.end
  (let ((A-norm (2dg-normalized (2dg-characteristic-vector A)))
        (A-length (2dg-length A))
        (A-start-to-B-start (2dg-subtract (2dg-start B) (2dg-start A)))
        (A-start-to-B-end (2dg-subtract (2dg-end B) (2dg-start A)))
        (B-norm (2dg-normalized (2dg-characteristic-vector B)))
        (B-length (2dg-length B))
        (B-start-to-A-start (2dg-subtract (2dg-start A) (2dg-start B)))
        (B-start-to-A-end (2dg-subtract (2dg-end A) (2dg-start B))))
    (list
     ;; A-parametrics
     (2dg-inverse-scaled
      (2dg-span :start (2dg-dot-prod A-norm A-start-to-B-start)
                  :end (2dg-dot-prod A-norm A-start-to-B-end))
      A-length)
     ;; B-parametrics
     (2dg-inverse-scaled
      (2dg-span :start (2dg-dot-prod B-norm B-start-to-A-start)
                  :end (2dg-dot-prod B-norm B-start-to-A-end))
      B-length))))
(defun 2dg---distance-parallel (A B)
  "Return the minimum distance between two parallel segments.

It is assumed you made sure A and B are parallel before calling."
  ;; get the distance along A to B.start and B.end
  ;; get the distance along B to A.start and B.end
  (let* ((parametrics (2dg---distance-parallel-parametrics A B))
         (A-parametric (car parametrics))
         (B-parametric (cadr parametrics)))
    (cond
     ;; segment A, voronoi region of start
     ((< (2dg-end A-parametric) 0.0)
      (2dg-distance B (2dg-start A)))
     ;; segment A, voronoi region of end
     ((> (2dg-start A-parametric) 1.0)
      (2dg-distance B (2dg-end A)))
     ;; else, it's in the voronoi region of A's line segment.
     ;; segment B, voronoi regino of start
     ((< (2dg-end B-parametric) 0.0)
      (2dg-distance A (2dg-start B)))
     ;; segment B, voronoi region of end
     ((> (2dg-start B-parametric) 1.0)
      (2dg-distance A (2dg-end B)))
     ;; ok, line to line perpendicular distance - no segment ends involved.
     ('t
      (let ((A-start-to-B-start (2dg-subtract (2dg-start B) (2dg-start A)))
            (A-norm (2dg-unit-vector A)))
        (abs (2dg-dot-prod A-start-to-B-start
                             (2dg-rotate-90 A-norm 1))))))))
(defun 2dg---segment-collision-parametrics (A B)
  "Find the per-segment parametric coordinates where these two
segments hit (non-parallel segments)."
  (let* ((A-start (2dg-start A))
         (A-char-vec (2dg-characteristic-vector A))
         (B-start (2dg-start B))
         (B-char-vec (2dg-characteristic-vector B)))
    (2dg---segment-collision-parametrics-vectorized A-start A-char-vec B-start B-char-vec)))
(defun 2dg---segment-collision-parametrics-vectorized (A-start A-char-vec B-start B-char-vec)
  ;; (cl-defmethod 2dg---segment-collision-parametrics-vectorized ((A-start 2dg-point) (A-char-vec 2dg-point) (B-start 2dg-point) (B-char-vec 2dg-point))
  "Find the per-segment parametric coordinates where these two segments hit (non-parallel segments).

This is returned as (cons a-parametric b-parametric)."
  ;; (Ax(t), Ay(t)) = (Bx(s), By(s))
  ;; -------------------------
  ;; Asx + t Adx = Bsx + Bdx s
  ;; Asy + t Ady = Bsy + Bdy s
  ;; -------------------------
  ;; Adx t - Bdx s = Bsx - Asx
  ;; Ady t - Bdy s = Bsy - Asy
  ;; -------------------------------------------
  ;; [ Adx -Bdx ] [ t ] = [ Bsx - Asx ] = [ S1 ]
  ;; [ Ady -Bdy ] [ s ] = [ Bsy - Asy ] = [ S2 ]
  ;; -------------------------------------------
  (let* ((As A-start)
         (Ad A-char-vec)
         (Adx (2dg-x Ad))
         (Ady (2dg-y Ad))
         (Bs B-start)
         (Bd B-char-vec)
         (Bdx (2dg-x Bd))
         (Bdy (2dg-y Bd))
         (determinant (- (* Adx -1.0 Bdy)
                         (* -1.0 Bdx Ady))))
    (if (2dg-almost-zero determinant)
        (error "Segments are parallel: [start:%s dir:%s], [start:%s dir:%s]"
               (2dg-pprint A-start)
               (2dg-pprint A-char-vec)
               (2dg-pprint B-start)
               (2dg-pprint B-char-vec))

      ;; matrix inverse =
      ;; a b => d -b
      ;; c d => -c a divided by det
      ;; ---------------------------
      ;; Adx -Bdx => -Bdy Bdx
      ;; Ady -Bdy => -Ady Adx
      (let* ((S1 (- (2dg-x Bs) (2dg-x As)))
             (S2 (- (2dg-y Bs) (2dg-y As)))
             (a-parametric (/ (+ (* -1.0 Bdy S1) (* Bdx S2)) determinant))
             (b-parametric (/ (+ (* -1.0 Ady S1) (* Adx S2)) determinant)))
        (cons a-parametric b-parametric)))))
(cl-defmethod 2dg-distance ((A 2dg-segment) (B 2dg-segment))
  "Return  the minimum distances between A and B"
  (let* ((parametrics (2dg---segment-collision-parametrics A B))
         (a-parametric (car parametrics))
         (b-parametric (cdr parametrics)))
    (cond
     ;; A-start voronoi region
     ((< a-parametric 0.0)
      (cond
       ;; B-start voronoi region
       ((< b-parametric 0.0)
        (2dg-distance (2dg-start A) (2dg-start B)))
       ;; B-end voronoi region
       ((> b-parametric 1.0)
        (2dg-distance (2dg-start A) (2dg-end B)))
       ;; B-segment voronoi region
       ('t
        (2dg-distance B (2dg-start A)))))
     ;; A-end voronoi region
     ((> a-parametric 1.0)
      (cond
       ;; B-start voronoi region
       ((< b-parametric 0.0)
        (2dg-distance (2dg-end A) (2dg-start B)))
       ;; B-end voronoi region
       ((> b-parametric 1.0)
        (2dg-distance (2dg-end A) (2dg-end B)))
       ;; b-segment voronoi region
       ('t
        (2dg-distance B (2dg-end A)))))
     ;; A-segment voronoi region
     ('t
      (cond
       ;; B-start voronoi region
       ((< b-parametric 0.0)
        (2dg-distance A (2dg-start B)))
       ;; B-end voronoi region
       ((> b-parametric 1.0)
        (2dg-distance A (2dg-end B)))
       ;; B-segment voronoi region - special case - they touch
       ('t
        0.0))))))
(defun 2dg---segment-point-distance (A-segment B-point)
  "Return the distance information for A-SEGMENT and B-POINT.

Return the actual distance as well as the voronoi zone of A in
the form of (cons distance voronoi-zone).  Voronoi zone will be
returned as one of: 'start, 'end or 'middle (of A-SEGMENT)."
  (let* ((A-start-to-B (2dg-subtract B-point (2dg-start A-segment)))
         (A-char-vec (2dg-characteristic-vector A-segment))
         (A-parametric (2dg-dot-prod (2dg-normalized A-char-vec)
                                       A-start-to-B))
         (A-length (2dg-length A-segment)))
    (cond ((<= A-parametric 0)
           (cons (2dg-distance (2dg-start A-segment) B-point)
                 'start))
          ((>= A-parametric A-length)
           (cons (2dg-distance (2dg-end A-segment) B-point)
                 'end))

          ('t                           ;; not in an end point voronoi zone
           (cons (abs (2dg-dot-prod A-start-to-B
                                      (2dg-rotate-90 (2dg-normalized A-char-vec) 1)))
                 'middle)))))
(cl-defmethod 2dg-distance ((A 2dg-segment) (B 2dg-point))
  "distance between a line and a point."
  (car (2dg---segment-point-distance A B)))
(cl-defmethod 2dg-has-intersection ((A 2dg-segment) (B 2dg-point) &optional evaluation-mode)
  "Does A ever intersect B.

Because B is a point, it doesn't have to be an exact intersection.
It only has to be within an almost-zero distance."
  (let* ((distance-info (2dg---segment-point-distance A B))
         (distance (car distance-info))
         (voronoi-region (cdr distance-info)))
    (cond ((eq evaluation-mode 'strict)   ; don't allow either end point of A
           (and (eq voronoi-region 'middle)
                (let ((start-distance (2dg-distance (2dg-start A) B))
                      (end-distance (2dg-distance (2dg-end A) B)))
                  (< distance (max start-distance end-distance) 2dg--almost-zero))))
          ((eq evaluation-mode 'stacked)  ; don't allow the 'end' end point of A
           (and (not (eq voronoi-region 'end))
                (let ((end-distance (2dg-distance (2dg-end A) B)))
                  (< distance end-distance 2dg--almost-zero))))
          (t                            ; allow any part of A or B.
           (2dg-almost-zero distance)))))
(cl-defmethod 2dg-has-intersection ((A 2dg-segment) (B 2dg-segment) &optional evaluation-mode)
  "Does A ever intersect B."
  (2dg-pierced-p A B
                  (not (eq evaluation-mode 'strict))
                  (not (or (eq evaluation-mode 'strict)
                           (eq evaluation-mode 'stacked)))
                  t
                  t))
(cl-defmethod 2dg-pierced-p ((A 2dg-segment) (B 2dg-segment) &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  "Does A pierce B at some point (not_ at end points, unless specified)."
  (let ((A-is-point (2dg-almost-zero (2dg-box-magnitude A)))
        (B-is-point (2dg-almost-zero (2dg-box-magnitude B))))
    ;; if either one has a box magnitude that's so small it indicates it's a point
    ;; then the strategy needs to be altered.
    (if (not (or A-is-point B-is-point))
        ;; normal collision!
        (2dg---unsafe-pierced-p A B allow-A-start allow-A-end allow-B-start allow-B-end)
      ;; segment->point or point->point collision.
      (let ((corrected-A (cond ((not A-is-point) A)
                               ((and allow-A-start allow-A-end) (2dg-centroid A))
                               (t nil)))
            (corrected-B (cond ((not B-is-point) B)
                               ((and allow-B-start allow-B-end) (2dg-centroid B))
                               (t nil))))
        (if (and corrected-A corrected-B)
            (if (2dg-segment-p corrected-A)
                (if (not (and allow-A-start allow-A-end))
                    (2dg-has-intersection corrected-A corrected-B 'strict)
                  (error "Need to program this for A"))
              (if (2dg-segment-p corrected-B)
                  (if (not (and allow-B-start allow-B-end))
                      (2dg-has-intersection corrected-B corrected-A 'strict)
                    (error "Need to program this for B"))
                ;; they're both points... so... :shrug:
                (2dg-almost-equal corrected-A corrected-B))))))))
(defun 2dg---unsafe-pierced-p (A B  &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  ;; (cl-defmethod 2dg---unsafe-pierced-p ((A 2dg-segment) (B 2dg-segment) &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  "Does A pierce B at some point (_not_ at end points, unless specified).

Neither A nor B should be near-zero length segments OR parallel.
That should be sorted out before calling this."
  (let* ((a-min (if allow-A-start (* -1.0 2dg--almost-zero) 2dg--almost-zero))
         (a-max (if allow-A-end (+ 1.0 2dg--almost-zero) (- 1.0 2dg--almost-zero)))
         (b-min (if allow-B-start (* -1.0 2dg--almost-zero) 2dg--almost-zero))
         (b-max (if allow-B-end (+ 1.0 2dg--almost-zero) (- 1.0 2dg--almost-zero)))
         (a-norm-vec (2dg-normalized (2dg-characteristic-vector A)))
         (b-norm-vec (2dg-normalized (2dg-characteristic-vector B)))
         (norm-cross-prod (2dg-cross-prod a-norm-vec b-norm-vec)))
    ;; are the segments parallel?
    (if (< (abs norm-cross-prod) 2dg--almost-zero)
        (let ((perp-distance (2dg-dot-prod (2dg-rotate-90 a-norm-vec)
                                             (2dg-subtract (2dg-start B)
                                                             (2dg-start A)))))
          (if (>= (abs perp-distance) 2dg--almost-zero)
              ;; too far away to touch
              'nil
            (let* ((parametrics (2dg---distance-parallel-parametrics A B))
                   (a-parametric (car parametrics))
                   (b-parametric (cadr parametrics))
                   (start-range (2dg-span :start (* -1.0 2dg--almost-zero)
                                            :end 2dg--almost-zero))
                   (end-range (2dg-span :start (- 1.0 2dg--almost-zero)
                                          :end (+ 1.0 2dg--almost-zero)))
                   (mid-range (2dg-span :start 2dg--almost-zero
                                          :end (- 1.0 2dg--almost-zero))))
              ;; parallel, colinear segments that could touch...
              ;; and now for a giant conditional statement.
              (and
               ;; A-hit
               (or (2dg-intersection mid-range a-parametric)
                   (and allow-A-start
                        (2dg-intersection start-range a-parametric))
                   (and allow-A-end
                        (2dg-intersection end-range a-parametric)))
               ;; B-hit
               (or (2dg-intersection mid-range b-parametric)
                   (and allow-B-start
                        (2dg-intersection start-range b-parametric))
                   (and allow-B-end
                        (2dg-intersection end-range b-parametric)))))))
      (let* ((parametrics (2dg---segment-collision-parametrics A B))
             (a-parametric (car parametrics))
             (b-parametric (cdr parametrics)))
        (if (and (> a-parametric a-min)
                 (< a-parametric a-max)
                 (> b-parametric b-min)
                 (< b-parametric b-max))
            't
          'nil)))))
(cl-defmethod 2dg-coarse-direction ((segment 2dg-segment))
  (2dg-coarse-direction (2dg-characteristic-vector segment)))

(cl-defmethod 2dg-get-closest-parametric ((segment 2dg-segment) (pt 2dg-point) &optional bounded)
  "Get the parametric coordinate of the closest point along SEGMENT to PT.

Output is bounded to be between [0, 1] inclusive when BOUNDED is t."
  (with-slots (start) segment
    (let* ((seg-start-to-pt (2dg-subtract pt start))
           (char-vec (2dg-characteristic-vector segment))
           (parametric (/ (2dg-dot-prod (2dg-normalized char-vec)
                                          seg-start-to-pt)
                          (2dg-length segment))))
      (if bounded
          (min (max parametric 0.0) 1.0)
        parametric))))

(cl-defmethod 2dg-absolute-coordinates ((base-segment 2dg-segment) (coordinate number))
  "Return the point along BASE-SEGMENT at the segments parametric COORDINATE."
  (with-slots (start) base-segment
    (2dg-add start
               (2dg-scaled (2dg-characteristic-vector base-segment)
                             coordinate))))

(cl-defmethod 2dg-get-parametric ((segment 2dg-segment) (pt 2dg-point) &optional distance-tolerance)
  "Get the parametric coordinate of PT along SEGMENT.

Note: this is similar to 2dg-relative-coordinate but as it
could return a nil value it's differentiated with the
-get-parametric name."
  ;; TODO -replace the first few let clauses with a call to 2dg-get-closest-parametric?
  (with-slots (start) segment
    (let* ((seg-start-to-pt (2dg-subtract pt start))
           (char-vec (2dg-characteristic-vector segment))
           (parametric (/ (2dg-dot-prod (2dg-normalized char-vec)
                                          seg-start-to-pt)
                          (2dg-length segment)))
           (ideal-pt (2dg-add start (2dg-scaled char-vec parametric)))
           (distance-sq (2dg-distance-sq ideal-pt pt))
           (distance-tolerance (if (numberp distance-tolerance) distance-tolerance 2dg--almost-zero)))
      (if (> distance-sq (* distance-tolerance distance-tolerance))
          'nil
        parametric))))

(provide 'scxml-geometry-segment)
;;; scxml-geometry-segment.el ends here
