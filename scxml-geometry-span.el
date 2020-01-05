;;; scxml-geometry-span.el --- scxml geometry span helper -*- lexical-binding: t -*-

;;; Commentary:
;; A 'span' represents an ordered start and end point in a single
;; dimension.  While the order of the points (start and end) is
;; significant there is no requirement that start <= end.

;;; Code:
(require 'eieio)

(defclass scxml-span ()
  ((start :initarg :start
          :accessor scxml-start
          :type float)
   (end :initarg :end
        :accessor scxml-end
        :type float)))

(cl-defmethod scxml-print ((span scxml-span))
  "Return a stringified version of SPAN for human eyes."
  (format "s[%f -> %f]"
          (scxml-start span)
          (scxml-end span)))
(cl-defmethod cl-print-object ((object scxml-span) stream)
  "This seems to be used only for edebug sessions."
  (princ (scxml-print object) stream))
(cl-defmethod scxml-scaled ((span scxml-span) alpha)
  "Return a span of SPAN scaled by ALPHA."
  (scxml-span :start (* alpha (scxml-start span))
              :end (* alpha (scxml-end span))))
(cl-defmethod scxml-ordered ((range scxml-span))
  "Return an ordered version of RANGE where start <= end."
  (let ((A (scxml-start range))
        (B (scxml-end range)))
    (if (>= A B)
        (scxml-span :start B :end A)
      (scxml-span :start A :end B))))
(defun scxml---span-ordered-if-not (span)
  "Internal function, ensure ordered."
  (let ((A (scxml-start span))
        (B (scxml-end span)))
    (if (>= A B)
        (scxml-span :start B :end A)
      span)))
(cl-defmethod scxml-inverse-scaled ((range scxml-span) (alpha number))
  "Return a span of SPAN scaled by 1/ALPHA."
  (scxml-scaled range (/ 1.0 alpha)))
(defun scxml---span-ordered-intersection (A B)
  "Get the intersection of the ordered spans A and B.

A and B must be ordered before calling."
  ;; TODO - consider having this return an actual float when appropriate.
  (let ((start (max (scxml-start A) (scxml-start B)))
        (end (min (scxml-end A) (scxml-end B))))
    (if (<= start end)
        (scxml-span :start start :end end)
      'nil)))
(cl-defmethod scxml-intersection ((A scxml-span) (B scxml-span))
  "Get the intersection of two spans.

Result will be ordered."
  (scxml---span-ordered-intersection
   (scxml---span-ordered-if-not A)
   (scxml---span-ordered-if-not B)))
(cl-defmethod scxml-contains ((container scxml-span) (coordinate number) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains COORDINATE using EVALUATION-MODE."
  (let ((container (scxml---span-ordered-if-not container)))
    (cond ((eq evaluation-mode 'strict)
           (< (scxml-start container) coordinate (scxml-end container)))
          ((eq evaluation-mode 'stacked)
           (and (<= (scxml-start container) coordinate)
                (< coordinate (scxml-end container))))
          (t                              ;normal
           (<= (scxml-start container) coordinate (scxml-end container))))))
(cl-defmethod scxml-contains ((container scxml-span) (containee scxml-span) &optional evaluation-mode)
  "Return non-nil if the CONTAINER contains CONTAINEE using EVALUATION-MODE."
  (let ((container (scxml---span-ordered-if-not container))
        (containee (scxml---span-ordered-if-not containee)))
    (let ((bound-start (scxml-start container))
          (bound-end (scxml-end container))
          (test-start (scxml-start containee))
          (test-end (scxml-end containee)))
      (cond ((eq evaluation-mode 'strict)
             (and (< bound-start test-start)
                  (< test-end bound-end)))
            ((eq evaluation-mode 'stacked)
             (and (<= bound-start test-start)
                  (< test-end bound-end)))
            (t                              ;normal
             (and (<= bound-start test-start)
                  (<= test-end bound-end)))))))
(cl-defmethod scxml-has-intersection ((A scxml-span) (B number) &optional evaluation-mode)
  "Return non-nil if A contains B using EVALUATION-MODE."
  (scxml-contains A B evaluation-mode))
(cl-defmethod scxml-has-intersection  ((A scxml-span) (B scxml-span) &optional evaluation-mode)
  "Return non-nil if A contains B using EVALUATION-MODE."
  (let ((A (scxml---span-ordered-if-not A))
        (B (scxml---span-ordered-if-not B)))
    (let ((intersection (scxml---span-ordered-intersection A B)))
      (and intersection
           (cond ((eq evaluation-mode 'strict)
                  (and (< (scxml-start intersection) (scxml-end A))
                       (> (scxml-end intersection) (scxml-start A))))
                 ((eq evaluation-mode 'stacked)
                  (< (scxml-start intersection) (scxml-end A)))
                 (t
                  t))))))
(cl-defmethod scxml-parametric ((span scxml-span) parametric-coord)
  "Get a point along SPAN by PARAMETRIC-COORD between [0,1]."
  (with-slots (start end) span
    (+ start (* (- end start) parametric-coord))))
(cl-defmethod scxml-length ((span scxml-span))
  "How large is this SPAN (can be negative if the span is flipped)."
  (with-slots (start end) span (- end start)))
(cl-defmethod scxml-relative-coordinates ((base-span scxml-span) (span scxml-span))
  "Given a BASE-SPAN, return SPAN's relative coordinates."
  (with-slots (start) base-span
    (let ((width (float (scxml-length base-span)))
          (start (float start)))
      (scxml-span :start (/ (- (float (scxml-start span)) start) width)
                  :end (/ (- (float (scxml-end span)) start) width)))))
(cl-defmethod scxml-relative-coordinates ((base-span scxml-span) (scalar number))
  "Given a BASE-SPAN, return the relative coordinate of the SCALAR."
  (with-slots (start) base-span
    (let ((width (float (scxml-length base-span))))
      (/ (- (float scalar) start) width))))
(cl-defmethod scxml-absolute-coordinates ((base-span scxml-span) (scalar number))
  "Given a BASE-SPAN, return the absolute coordinate of the relative SCALAR."
  (with-slots (start) base-span
    (let ((width (float (scxml-length base-span))))
      (+ start (* scalar width)))))
(cl-defmethod scxml-absolute-coordinates ((base-span scxml-span) (relative-span scxml-span))
  "Given BASE-SPAN, return RELATIVE-SPAN's absolute coordinates

scxml-relative-coordinates reversed"
  (with-slots (start) base-span
    (let ((width (float (scxml-length base-span)))
          (start (float start)))
      (scxml-span :start (+ start (* (float (scxml-start relative-span)) width))
                  :end (+ start (* (float (scxml-end relative-span)) width))))))

(provide 'scxml-geometry-span)
;;; scxml-geometry-span.el ends here
