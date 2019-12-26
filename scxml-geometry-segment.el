;;; scxml-geometry-segment.el --- scxml geometry segment helpers -*- lexical-binding: t -*-

;;; Commentary:
;; scxml-segment is a line segment that connects two points
;; It is directional, starting at START and ending at END.

;;; Code:
(require 'scxml-geometry-point)
(require 'scxml-geometry-span)

(defclass scxml-segment ()
  ((start :initarg :start
          :accessor scxml-start
          :type scxml-point)
   (end :initarg :end
        :accessor scxml-end
        :type scxml-point))
  :documentation "2d Line segment that can go in any direction (omnidirectional)")
(cl-defmethod scxml-print ((segment scxml-segment))
  "Return a stringified version of SPAN for human eyes."
  (with-slots (start end) segment
    (format "s[%s -> %s]" (scxml-print start) (scxml-print end))))
(cl-defmethod cl-print-object ((segment scxml-segment) stream)
  "This seems to be used only for edebug sessions."
  (princ (scxml-print segment) stream))
(cl-defmethod scxml-characteristic-vector ((segment scxml-segment))
  "Return the characteristic vector of SEGMENT.

Characteristic vector is:
  (SEGMENT end) - (SEGMENT start)."
  (with-slots (start end) segment
    (scxml-subtract end start)))
(cl-defmethod scxml-unit-vector ((segment scxml-segment))
  "Return a normalized characteristic vector."
  (scxml-normalized (scxml-characteristic-vector segment)))
(cl-defmethod scxml-parametric ((segment scxml-segment) parametric-coord)
  "Return the point at PARAMETRIC-COORD along SEGMENT.

Valid parametric coordinates range from 0 to 1, inclusive.  A
PARAMETRIC-COORD of 0 will yield the start point, 1 will yield
the end point and 0.5 will yield the mid point."
  (let ((char-vec (scxml-characteristic-vector segment))
        (start (scxml-start segment)))
    (scxml-point :x (+ (scxml-x start)
                       (* parametric-coord (scxml-x char-vec)))
                 :y (+ (scxml-y start)
                       (* parametric-coord (scxml-y char-vec))))))
(cl-defmethod scxml-length ((segment scxml-segment))
  "Return the length of SEGMENT."
  (with-slots (start end) segment
    (scxml-distance start end)))
(cl-defmethod scxml-box-magnitude ((segment scxml-segment))
  "Return the box magnitude of SEGMENT.

Box magnitude is defined as the edge length of the smallest
square which could contain this SEGMENT."
  (scxml-box-magnitude (scxml-characteristic-vector segment)))
(cl-defmethod scxml-almost-equal ((A scxml-segment) (B scxml-segment) &optional tolerance)
  "Return non-nil if A and B almost equal within a TOLERANCE"
  (and (scxml-almost-equal (scxml-start A) (scxml-start B) tolerance)
       (scxml-almost-equal (scxml-end A) (scxml-end B) tolerance)))
(cl-defmethod scxml-flipped ((segment scxml-segment))
  "Return a new segment having reversed start and end of SEGMENT."
  (with-slots (start end) segment
    (scxml-segment :start end :end start)))
(cl-defmethod scxml-centroid ((segment scxml-segment))
  "Return the midpoint of SEGMENT."
  (with-slots (start end) segment
    (scxml-point :x (/ (+ (scxml-x start) (scxml-x end)) 2.0)
                 :y (/ (+ (scxml-y start) (scxml-y end)) 2.0))))
(cl-defmethod scxml---distance-parallel-parametrics ((A scxml-segment) (B scxml-segment))
  "Determine parametric coordinates for two parallel lines where they are closest.

Returns a list consisting of '(A-parametric-range B-parmetric-range).
It is assumed that you made sure A and B are parallel before calling."
  ;; get the distance along A to B.start and B.end
  ;; get the distance along B to A.start and B.end
  (let ((A-norm (scxml-normalized (scxml-characteristic-vector A)))
        (A-length (scxml-length A))
        (A-start-to-B-start (scxml-subtract (scxml-start B) (scxml-start A)))
        (A-start-to-B-end (scxml-subtract (scxml-end B) (scxml-start A)))
        (B-norm (scxml-normalized (scxml-characteristic-vector B)))
        (B-length (scxml-length B))
        (B-start-to-A-start (scxml-subtract (scxml-start A) (scxml-start B)))
        (B-start-to-A-end (scxml-subtract (scxml-end A) (scxml-start B))))
    (list
     ;; A-parametrics
     (scxml-inverse-scaled
      (scxml-span :start (scxml-dot-prod A-norm A-start-to-B-start)
                  :end (scxml-dot-prod A-norm A-start-to-B-end))
      A-length)
     ;; B-parametrics
     (scxml-inverse-scaled
      (scxml-span :start (scxml-dot-prod B-norm B-start-to-A-start)
                  :end (scxml-dot-prod B-norm B-start-to-A-end))
      B-length))))
(cl-defmethod scxml---distance-parallel ((A scxml-segment) (B scxml-segment))
  "Return the minimum distance between two parallel segments.

It is assumed you made sure A and B are parallel before calling."
  ;; get the distance along A to B.start and B.end
  ;; get the distance along B to A.start and B.end
  (let* ((parametrics (scxml---distance-parallel-parametrics A B))
         (A-parametric (car parametrics))
         (B-parametric (cadr parametrics)))
    (cond
     ;; segment A, voronoi region of start
     ((< (scxml-end A-parametric) 0.0)
      (scxml-distance B (scxml-start A)))
     ;; segment A, voronoi region of end
     ((> (scxml-start A-parametric) 1.0)
      (scxml-distance B (scxml-end A)))
     ;; else, it's in the voronoi region of A's line segment.
     ;; segment B, voronoi regino of start
     ((< (scxml-end B-parametric) 0.0)
      (scxml-distance A (scxml-start B)))
     ;; segment B, voronoi region of end
     ((> (scxml-start B-parametric) 1.0)
      (scxml-distance A (scxml-end B)))
     ;; ok, line to line perpendicular distance - no segment ends involved.
     ('t
      (let ((A-start-to-B-start (scxml-subtract (scxml-start B) (scxml-start A)))
            (A-norm (scxml-unit-vector A)))
        (abs (scxml-dot-prod A-start-to-B-start
                             (scxml-rotate-90 A-norm 1))))))))
(cl-defmethod scxml---segment-collision-parametrics ((A scxml-segment) (B scxml-segment))
  "Find the per-segment parametric coordinates where these two
segments hit (non-parallel segments)."
  (let* ((A-start (scxml-start A))
         (A-char-vec (scxml-characteristic-vector A))
         (B-start (scxml-start B))
         (B-char-vec (scxml-characteristic-vector B)))
    (scxml---segment-collision-parametrics-vectorized A-start A-char-vec B-start B-char-vec)))
(cl-defmethod scxml---segment-collision-parametrics-vectorized ((A-start scxml-point) (A-char-vec scxml-point) (B-start scxml-point) (B-char-vec scxml-point))
  "Find the per-segment parametric coordinates where these two segments hit (non-parallel segments).

This is returned as a plist of the form:
('b-parametric b-parametric 'a-parametric a-parametric)
TODO: this should probably just return a cons?"
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
         (Adx (scxml-x Ad))
         (Ady (scxml-y Ad))
         (Bs B-start)
         (Bd B-char-vec)
         (Bdx (scxml-x Bd))
         (Bdy (scxml-y Bd))
         (determinant (- (* Adx -1.0 Bdy)
                         (* -1.0 Bdx Ady))))
    (if (scxml-almost-zero determinant)
        (error "Segments are parallel: [start:%s dir:%s], [start:%s dir:%s]"
               (scxml-print A-start)
               (scxml-print A-char-vec)
               (scxml-print B-start)
               (scxml-print B-char-vec))

      ;; matrix inverse =
      ;; a b => d -b
      ;; c d => -c a divided by det
      ;; ---------------------------
      ;; Adx -Bdx => -Bdy Bdx
      ;; Ady -Bdy => -Ady Adx
      (let* ((S1 (- (scxml-x Bs) (scxml-x As)))
             (S2 (- (scxml-y Bs) (scxml-y As)))
             (a-parametric (/ (+ (* -1.0 Bdy S1) (* Bdx S2)) determinant))
             (b-parametric (/ (+ (* -1.0 Ady S1) (* Adx S2)) determinant)))
        (plist-put (plist-put '() 'b-parametric b-parametric)
                   'a-parametric a-parametric)))))
(cl-defmethod scxml-distance ((A scxml-segment) (B scxml-segment))
  "Return  the minimum distances between A and B"
  (let* ((parametrics (scxml---segment-collision-parametrics A B))
         (a-parametric (plist-get parametrics 'a-parametric))
         (b-parametric (plist-get parametrics 'b-parametric)))
    (cond
     ;; A-start voronoi region
     ((< a-parametric 0.0)
      (cond
       ;; B-start voronoi region
       ((< b-parametric 0.0)
        (scxml-distance (scxml-start A) (scxml-start B)))
       ;; B-end voronoi region
       ((> b-parametric 1.0)
        (scxml-distance (scxml-start A) (scxml-end B)))
       ;; B-segment voronoi region
       ('t
        (scxml-distance B (scxml-start A)))))
     ;; A-end voronoi region
     ((> a-parametric 1.0)
      (cond
       ;; B-start voronoi region
       ((< b-parametric 0.0)
        (scxml-distance (scxml-end A) (scxml-start B)))
       ;; B-end voronoi region
       ((> b-parametric 1.0)
        (scxml-distance (scxml-end A) (scxml-end B)))
       ;; b-segment voronoi region
       ('t
        (scxml-distance B (scxml-end A)))))
     ;; A-segment voronoi region
     ('t
      (cond
       ;; B-start voronoi region
       ((< b-parametric 0.0)
        (scxml-distance A (scxml-start B)))
       ;; B-end voronoi region
       ((> b-parametric 1.0)
        (scxml-distance A (scxml-end B)))
       ;; B-segment voronoi region - special case - they touch
       ('t
        0.0))))))
(cl-defmethod scxml-distance ((A scxml-segment) (B scxml-point))
  "distance between a line and a point."
  (let* ((A-start-to-B (scxml-subtract B (scxml-start A)))
         (A-char-vec (scxml-characteristic-vector A))
         (A-parametric (scxml-dot-prod (scxml-normalized A-char-vec)
                                       A-start-to-B))
         (A-length (scxml-length A)))
    (cond ((< A-parametric 0)
           (scxml-distance (scxml-start A) B))
          ((> A-parametric A-length)
           (scxml-distance (scxml-end A) B))
          ('t                           ; not in an end point voronoi zone
           (abs (scxml-dot-prod A-start-to-B
                                (scxml-rotate-90 (scxml-normalized A-char-vec) 1)))))))
(cl-defmethod scxml-has-intersection ((A scxml-segment) (B scxml-point) &optional evaluation-mode)
  "Does A ever intersect B.

Because B is a point, it doesn't have to be an exact intersection.
It only has to be within an almost-zero distance."
  ;; TODO: this is a bit of a hacked way to do this, probably a faster way.
  (let ((base-distance (scxml-distance A B)))
    (cond ((eq evaluation-mode 'strict)   ;don't allow either end point of A
           (let ((start-distance (scxml-distance (scxml-start A) B))
                 (end-distance (scxml-distance (scxml-end A) B)))
             (< base-distance (max start-distance end-distance) scxml--almost-zero)))
          ((eq evaluation-mode 'stacked)  ;don't allow the 'end' end point of A
           (let ((end-distance (scxml-distance (scxml-end A) B)))
             (< base-distance end-distance scxml--almost-zero)))
          (t
           (scxml-almost-zero base-distance)))))
(cl-defmethod scxml-has-intersection ((A scxml-segment) (B scxml-segment) &optional evaluation-mode)
  "Does A ever intersect B."
  (scxml-pierced? A B
                  (not (eq evaluation-mode 'strict))
                  (not (or (eq evaluation-mode 'strict)
                           (eq evaluation-mode 'stacked)))
                  t
                  t))
(cl-defmethod scxml-pierced? ((A scxml-segment) (B scxml-segment) &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  "Does A pierce B at some point (not_ at end points, unless specified)."
  (let ((A-is-point (scxml-almost-zero (scxml-box-magnitude A)))
        (B-is-point (scxml-almost-zero (scxml-box-magnitude B))))
    ;; if either one has a box magnitude that's so small it indicates it's a point
    ;; then the strategy needs to be altered.
    (if (not (or A-is-point B-is-point))
        ;; normal collision!
        (scxml---unsafe-pierced? A B allow-A-start allow-A-end allow-B-start allow-B-end)
      ;; segment->point or point->point collision.
      (let ((corrected-A (cond ((not A-is-point) A)
                               ((and allow-A-start allow-A-end) (scxml-centroid A))
                               (t nil)))
            (corrected-B (cond ((not B-is-point) B)
                               ((and allow-B-start allow-B-end) (scxml-centroid B))
                               (t nil))))
        (if (and corrected-A corrected-B)
            (if (scxml-segment-p corrected-A)
                (if (not (and allow-A-start allow-A-end))
                    (scxml-has-intersection corrected-A corrected-B 'strict)
                  (error "Need to program this for A"))
              (if (scxml-segment-p corrected-B)
                  (if (not (and allow-B-start allow-B-end))
                      (scxml-has-intersection corrected-B corrected-A 'strict)
                    (error "Need to program this for B"))
                ;; they're both points... so... :shrug:
                (scxml-almost-equal corrected-A corrected-B))))))))

(cl-defmethod scxml---unsafe-pierced? ((A scxml-segment) (B scxml-segment) &optional allow-A-start allow-A-end allow-B-start allow-B-end)
  "Does A pierce B at some point (_not_ at end points, unless specified).

Neither A nor B should be near-zero length segments OR parallel.
That should be sorted out before calling this."
  (let* ((a-min (if allow-A-start (* -1.0 scxml--almost-zero) scxml--almost-zero))
         (a-max (if allow-A-end (+ 1.0 scxml--almost-zero) (- 1.0 scxml--almost-zero)))
         (b-min (if allow-B-start (* -1.0 scxml--almost-zero) scxml--almost-zero))
         (b-max (if allow-B-end (+ 1.0 scxml--almost-zero) (- 1.0 scxml--almost-zero)))
         (a-norm-vec (scxml-normalized (scxml-characteristic-vector A)))
         (b-norm-vec (scxml-normalized (scxml-characteristic-vector B)))
         (norm-cross-prod (scxml-cross-prod a-norm-vec b-norm-vec)))
    ;; are the segments parallel?
    (if (< (abs norm-cross-prod) scxml--almost-zero)
        (let ((perp-distance (scxml-dot-prod (scxml-rotate-90 a-norm-vec)
                                             (scxml-subtract (scxml-start B)
                                                             (scxml-start A)))))
          (if (>= (abs perp-distance) scxml--almost-zero)
              ;; too far away to touch
              'nil
            (let* ((parametrics (scxml---distance-parallel-parametrics A B))
                   (a-parametric (car parametrics))
                   (b-parametric (cadr parametrics))
                   (start-range (scxml-span :start (* -1.0 scxml--almost-zero)
                                            :end scxml--almost-zero))
                   (end-range (scxml-span :start (- 1.0 scxml--almost-zero)
                                          :end (+ 1.0 scxml--almost-zero)))
                   (mid-range (scxml-span :start scxml--almost-zero
                                          :end (- 1.0 scxml--almost-zero))))
              ;; parallel, colinear segments that could touch...
              ;; and now for a giant conditional statement.
              (and
               ;; A-hit
               (or (scxml-intersection mid-range a-parametric)
                   (and allow-A-start
                        (scxml-intersection start-range a-parametric))
                   (and allow-A-end
                        (scxml-intersection end-range a-parametric)))
               ;; B-hit
               (or (scxml-intersection mid-range b-parametric)
                   (and allow-B-start
                        (scxml-intersection start-range b-parametric))
                   (and allow-B-end
                        (scxml-intersection end-range b-parametric)))))))
      (let* ((parametrics (scxml---segment-collision-parametrics A B))
             (a-parametric (plist-get parametrics 'a-parametric))
             (b-parametric (plist-get parametrics 'b-parametric)))
        (if (and (> a-parametric a-min)
                 (< a-parametric a-max)
                 (> b-parametric b-min)
                 (< b-parametric b-max))
            't
          'nil)))))
(cl-defmethod scxml-coarse-direction ((segment scxml-segment))
  (scxml-coarse-direction (scxml-characteristic-vector segment)))
(cl-defmethod scxml-get-parametric ((segment scxml-segment) (pt scxml-point) &optional distance-tolerance)
  ;; todo - should this be a relative/global coordinate thing? I think it should...
  "Get the parametric coordinate of this point along the segment."
  (with-slots (start) segment
    (let* ((seg-start-to-pt (scxml-subtract pt start))
           (char-vec (scxml-characteristic-vector segment))
           (parametric (/ (scxml-dot-prod (scxml-normalized char-vec)
                                          seg-start-to-pt)
                          (scxml-length segment)))
           (ideal-pt (scxml-add start (scxml-scaled char-vec parametric)))
           (distance-sq (scxml-distance-sq ideal-pt pt))
           (distance-tolerance (if (numberp distance-tolerance) distance-tolerance scxml--almost-zero)))
      (if (> distance-sq (* distance-tolerance distance-tolerance))
          'nil
        parametric))))

(provide 'scxml-geometry-segment)
;;; scxml-geometry-segment.el ends here
