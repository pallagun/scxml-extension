;;; 2dg-geometry-span.el --- geometry span helper -*- lexical-binding: t -*-

;;; Commentary:
;; A 'span' represents an ordered start and end scalar value set in a
;; single dimension.  While the order of the values (start and end) is
;; significant there is no requirement that start <= end.

;;; Code:
(require 'eieio)

(defclass 2dg-span ()
  ((start :initarg :start
          :accessor 2dg-start
          :type float)
   (end :initarg :end
        :accessor 2dg-end
        :type float)))

(cl-defmethod 2dg-pprint ((span 2dg-span))
  "Return a stringified version of SPAN for human eyes."
  (format "s[%f -> %f]"
          (2dg-start span)
          (2dg-end span)))
(cl-defmethod cl-print-object ((object 2dg-span) stream)
  "This seems to be used only for edebug sessions."
  (princ (2dg-print object) stream))
(cl-defmethod 2dg-scaled ((span 2dg-span) alpha)
  "Return a span of SPAN scaled by ALPHA."
  (2dg-span :start (* alpha (2dg-start span))
              :end (* alpha (2dg-end span))))
(cl-defmethod 2dg-ordered ((range 2dg-span))
  "Return an ordered version of RANGE where start <= end."
  (let ((A (2dg-start range))
        (B (2dg-end range)))
    (if (>= A B)
        (2dg-span :start B :end A)
      (2dg-span :start A :end B))))
(defun 2dg---span-ordered-if-not (span)
  "Internal function, ensure ordered."
  (let ((A (2dg-start span))
        (B (2dg-end span)))
    (if (>= A B)
        (2dg-span :start B :end A)
      span)))
(defun 2dg---span-ordered-intersection (A B)
  "Get the intersection of the ordered spans A and B.

A and B must be ordered before calling."
  ;; TODO - consider having this return an actual float when appropriate.
  (let ((start (max (2dg-start A) (2dg-start B)))
        (end (min (2dg-end A) (2dg-end B))))
    (if (<= start end)
        (2dg-span :start start :end end)
      'nil)))
(cl-defmethod 2dg-intersection ((A 2dg-span) (B 2dg-span))
  "Get the intersection of two spans.

Result will be ordered."
  (2dg---span-ordered-intersection
   (2dg---span-ordered-if-not A)
   (2dg---span-ordered-if-not B)))
(cl-defmethod 2dg-contains ((container 2dg-span) (coordinate number) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains COORDINATE using EVALUATION-MODE."
  (let ((container (2dg---span-ordered-if-not container)))
    (cond ((eq evaluation-mode 'strict)
           (< (2dg-start container) coordinate (2dg-end container)))
          ((eq evaluation-mode 'stacked)
           (and (<= (2dg-start container) coordinate)
                (< coordinate (2dg-end container))))
          (t                              ;normal
           (<= (2dg-start container) coordinate (2dg-end container))))))
(cl-defmethod 2dg-contains ((container 2dg-span) (containee 2dg-span) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains CONTAINEE using EVALUATION-MODE."
  (let ((container (2dg---span-ordered-if-not container))
        (containee (2dg---span-ordered-if-not containee)))
    (let ((bound-start (2dg-start container))
          (bound-end (2dg-end container))
          (test-start (2dg-start containee))
          (test-end (2dg-end containee)))
      (cond ((eq evaluation-mode 'strict)
             (and (< bound-start test-start)
                  (< test-end bound-end)))
            ((eq evaluation-mode 'stacked)
             (and (<= bound-start test-start)
                  (< test-end bound-end)))
            (t                              ;normal
             (and (<= bound-start test-start)
                  (<= test-end bound-end)))))))
(cl-defmethod 2dg-has-intersection ((A 2dg-span) (B number) &optional evaluation-mode)
  "Return non-nil if A contains B using EVALUATION-MODE."
  (2dg-contains A B evaluation-mode))
(cl-defmethod 2dg-has-intersection  ((A 2dg-span) (B 2dg-span) &optional evaluation-mode)
  "Return non-nil if A contains B using EVALUATION-MODE."
  (let ((A (2dg---span-ordered-if-not A))
        (B (2dg---span-ordered-if-not B)))
    (let ((intersection (2dg---span-ordered-intersection A B)))
      (and intersection
           (cond ((eq evaluation-mode 'strict)
                  (and (< (2dg-start intersection) (2dg-end A))
                       (> (2dg-end intersection) (2dg-start A))))
                 ((eq evaluation-mode 'stacked)
                  (< (2dg-start intersection) (2dg-end A)))
                 (t
                  t))))))
(cl-defmethod 2dg-parametric ((span 2dg-span) parametric-coord)
  "Get a point along SPAN by PARAMETRIC-COORD between [0,1]."
  (with-slots (start end) span
    (+ start (* (- end start) parametric-coord))))
(cl-defmethod 2dg-length ((span 2dg-span))
  "How large is this SPAN (can be negative if the span is flipped)."
  (with-slots (start end) span (- end start)))
(cl-defmethod 2dg-relative-coordinates ((base-span 2dg-span) (span 2dg-span))
  "Given a BASE-SPAN, return SPAN's relative coordinates."
  (with-slots (start) base-span
    (let ((width (float (2dg-length base-span)))
          (start (float start)))
      (2dg-span :start (/ (- (float (2dg-start span)) start) width)
                  :end (/ (- (float (2dg-end span)) start) width)))))
(cl-defmethod 2dg-relative-coordinates ((base-span 2dg-span) (scalar number))
  "Given a BASE-SPAN, return the relative coordinate of the SCALAR."
  (with-slots (start) base-span
    (let ((width (float (2dg-length base-span))))
      (/ (- (float scalar) start) width))))
(cl-defmethod 2dg-absolute-coordinates ((base-span 2dg-span) (scalar number))
  "Given a BASE-SPAN, return the absolute coordinate of the relative SCALAR."
  (with-slots (start) base-span
    (let ((width (float (2dg-length base-span))))
      (+ start (* scalar width)))))
(cl-defmethod 2dg-absolute-coordinates ((base-span 2dg-span) (relative-span 2dg-span))
  "Given BASE-SPAN, return RELATIVE-SPAN's absolute coordinates

2dg-relative-coordinates reversed"
  (with-slots (start) base-span
    (let ((width (float (2dg-length base-span)))
          (start (float start)))
      (2dg-span :start (+ start (* (float (2dg-start relative-span)) width))
                  :end (+ start (* (float (2dg-end relative-span)) width))))))

(provide 'scxml-geometry-span)
;;; scxml-geometry-span.el ends here
