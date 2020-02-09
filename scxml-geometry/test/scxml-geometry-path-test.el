(require 'ert)
(require 'scxml-geometry-path)

(ert-deftest scxml-geometry-path-is-cardinal-path? ()
  ;; Needs to say that a path with a single point is indeed cardinal
  (let ((paths (list
                (scxml-path :points (list (2dg-point- 0 20)))
                (scxml-path :points (list (2dg-point- 0 0)
                                          (2dg-point- 1 0)))
                (scxml-path :points (list (2dg-point- 0 0)
                                          (2dg-point- 1 0)
                                          (2dg-point- 1 1)))
                (scxml-path :points (list (2dg-point- 30.5 22.0)
                                          (2dg-point- 30.500000000001 17.0))))))
    (mapcar (lambda (path)
              (should (2dg---is-cardinal-path-p (scxml-points path))))
            paths)))

(ert-deftest scxml-geometry-path-cardinal ()
  (let ((right-vec (2dg-point- 1 0))
        (up-vec (2dg-point- 0 1))
        (down-vec (2dg-point- 0 -1))
        (left-vec (2dg-point- -1 0))
        (min-dist 1.0))
    (cl-flet ((should-be-almost-equal
               (A B)
               (should
                (2dg-almost-equal (scxml-path :points A) (scxml-path :points B)))))

      ;; weird one I ran into.
      (let* ((start (2dg-point- 2.25 36.75))
             (end (2dg-point- 13.0 37.0))
             (entry-vector (2dg-point- 1.0 -0.0))
             (exit-vector (2dg-point- -1.0 0.0))
             (min-segment-distance 1.0)
             (expected-path (list start
                                  (2dg-point- 13.0 36.75)
                                  end)))
        (should (eq 3 (length (scxml---path-cardinal start end entry-vector exit-vector min-segment-distance))))
        (should-be-almost-equal (scxml---path-cardinal start end entry-vector exit-vector min-segment-distance)
                                expected-path))

      ;; simple 90 degree
      (let ((start (2dg-point- 0 0))
            (end (2dg-point- 5 5)))
        (let ((bottom-right-path (list start (2dg-point- 5 0) end)))
          (should-be-almost-equal (scxml---path-cardinal start end
                                                       right-vec up-vec
                                                       min-dist)
                                  bottom-right-path)
          (should-be-almost-equal (scxml---path-cardinal start end
                                                       down-vec up-vec
                                                       min-dist)
                                  bottom-right-path)
          (should-be-almost-equal (scxml---path-cardinal start end
                                                       right-vec left-vec
                                                       min-dist)
                                  bottom-right-path))
        (let ((top-left-path (list start (2dg-point- 0 5) end)))
          (should-be-almost-equal (scxml---path-cardinal start end
                                                       up-vec right-vec
                                                       min-dist)
                                  top-left-path)
          (should-be-almost-equal (scxml---path-cardinal start end
                                                       up-vec down-vec
                                                       min-dist)
                                  top-left-path)
          (should-be-almost-equal (scxml---path-cardinal start end
                                                       left-vec right-vec
                                                       min-dist)
                                  top-left-path)))
      ;; ;; straight shot
      (let* ((start (2dg-point- 0 0))
             (end (2dg-point- 5 0))
             (expected-path (list start end)))
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     up-vec up-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     down-vec down-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     up-vec down-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     down-vec up-vec
                                                     min-dist)
                                expected-path))
      ;; ;; U joint
      (let* ((start (2dg-point- 0 0))
             (end (2dg-point- 5 0))
             (expected-path (list start
                                  (2dg-point- 0 min-dist)
                                  (2dg-point- 5 min-dist)
                                  end)))
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     left-vec left-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     left-vec down-vec
                                                     min-dist)
                                expected-path))
      ;; jog - horizontal first.
      (let* ((start (2dg-point- 0 0))
             (end (2dg-point- 5 5))
             (expected-path (list start
                                  (2dg-point- 2.5 0.0)
                                  (2dg-point- 2.5 5.0)
                                  end)))
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     right-vec right-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     down-vec right-vec
                                                     min-dist)
                                expected-path)
        (should-be-almost-equal (scxml---path-cardinal start end
                                                     right-vec down-vec
                                                     min-dist)
                                expected-path))
      ;; oddball cases that don't violate min-distance
      (let* ((start (2dg-point- 7 5))
             (end (2dg-point- 8 8))
             (expected-path (list start
                                  (2dg-point- 7.0 6.5)
                                  (2dg-point- 8.0 6.5)
                                  end)))
        (should-be-almost-equal (scxml---path-cardinal start end
                                                       up-vec up-vec
                                                       0.0)
                                expected-path))
      )))

(ert-deftest scxml-geometry-path-append-simplify ()
  (cl-flet ((should-be-almost-equal
             (A B)
             (should
              (2dg-almost-equal (scxml-path :points A) (scxml-path :points B)))))
    ;; (let ((A (list (2dg-point :x 0.0 :y 0.0)
    ;;                (2dg-point :x 1.0 :y 0.0)
    ;;                (2dg-point :x 2.0 :y 0.0)))
    ;;       (B (list (2dg-point :x 2.0 :y 0.0)
    ;;                (2dg-point :x 2.0 :y 1.0)
    ;;                (2dg-point :x 2.0 :y 2.0))))
    ;;   (should-be-almost-equal (scxml---path-append-simplify A B)
    ;;                           (list (2dg-point :x 0.0 :y 0.0)
    ;;                                 (2dg-point :x 2.0 :y 0.0)
    ;;                                 (2dg-point :x 2.0 :y 2.0))))
    ;; (let ((A (list (2dg-point- -2 0)
    ;;                (2dg-point- -1 0)
    ;;                (2dg-point- 0 0)
    ;;                (2dg-point- 4 3)))
    ;;       (B (list (2dg-point- 4 3)
    ;;                (2dg-point- 8 6)
    ;;                (2dg-point- 8 6)
    ;;                (2dg-point- 9 6)
    ;;                (2dg-point- 10 6))))
    ;;   (should-be-almost-equal (scxml---path-append-simplify A B)
    ;;                           (list (2dg-point- -2 0)
    ;;                                 (2dg-point- 0 0)
    ;;                                 (2dg-point- 8 6)
    ;;                                 (2dg-point- 10 6))))
    ;; A few edge cases.
    (let ((A (list (2dg-point- 0 0)
                   (2dg-point- 1 0)
                   (2dg-point- 2 1)))
          (B (list (2dg-point- 2 1)
                   (2dg-point- 3 1)
                   (2dg-point- 3 1)
                   (2dg-point- 4 1)))
          (expected (list (2dg-point- 0 0)
                          (2dg-point- 1 0)
                          (2dg-point- 2 1)
                          (2dg-point- 4 1))))

      (should-be-almost-equal (scxml-simplified A B)
                              expected)
      (should (eq 3 (length A)))
      (should-be-almost-equal (scxml-simplified nil A B)
                              expected)
      (should-be-almost-equal (scxml-simplified nil A nil B nil)
                              expected)
      (should-be-almost-equal (scxml-simplified nil nil A nil B nil nil)
                              expected))))

(ert-deftest scxml-geometry-path-nudge-path ()
  (let ((path-pts (list (2dg-point :x 0.0 :y 0.0)
                        (2dg-point :x 1.0 :y 0.0)
                        (2dg-point :x 1.0 :y 0.0)
                        (2dg-point :x 2.0 :y 0.0))))
    (should (2dg-almost-equal
             (scxml-path :points (scxml-nudge-path path-pts
                                                     3
                                                     (2dg-point :x 0.0 :y 0.4)))
             (scxml-path :points (list (2dg-point :x 0.0 :y 0.0)
                                       (2dg-point :x 1.0 :y 0.0)
                                       (2dg-point :x 1.0 :y 0.4)
                                       (2dg-point :x 2.0 :y 0.4))))))
  (let ((path-pts (list (2dg-point :x 0.0 :y 0.0)
                        (2dg-point :x 1.0 :y 0.0)
                        (2dg-point :x 1.0 :y 1.0)
                        (2dg-point :x 2.0 :y 1.0)
                        (2dg-point :x 2.0 :y 2.0))))
    (should (2dg-almost-equal
             (scxml-path :points (scxml-nudge-path path-pts
                                                     2
                                                     (2dg-point :x 0.2 :y 0.2)))
             (scxml-path :points (list (2dg-point :x 0.0 :y 0.0)
                                       (2dg-point :x 1.2 :y 0.0)
                                       (2dg-point :x 1.2 :y 1.2)
                                       (2dg-point :x 2.0 :y 1.2)
                                       (2dg-point :x 2.0 :y 2.0)))))))

(ert-deftest scxml-geometry-path-stretch ()
  (cl-flet ((should-be-almost-equal
             (A B)
             (should
              (2dg-almost-equal (scxml-path :points A) (scxml-path :points B)))))
    ;; normal stretches that don't need any special cases
    (progn
      (let* ((start-pt (2dg-point :x 0.0 :y 0.0))
             (points (list start-pt
                           (2dg-point :x 5.0 :y 0.0)
                           (2dg-point :x 5.0 :y 5.0))))
        (should-be-almost-equal (scxml---path-stretch points
                                                        start-pt
                                                        (2dg-point :x 10.0 :y 10.0))
                                (list start-pt
                                      (2dg-point :x 10.0 :y 0.0)
                                      (2dg-point :x 10.0 :y 10.0))))
      (let* ((start-pt (2dg-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (2dg-point :x 6.0 :y 1.0)
                           (2dg-point :x 6.0 :y 6.0))))
        (should-be-almost-equal (scxml---path-stretch points
                                                        (2dg-point :x 0.0 :y 0.0)
                                                        (2dg-point :x 10.0 :y 10.0))
                                (list (2dg-point :x 0.0 :y 0.0)
                                      (2dg-point :x 10.0 :y 0.0)
                                      (2dg-point :x 10.0 :y 10.0)))))

    ;; here are two impossible stretches
    (progn
      (let* ((start-pt (2dg-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (2dg-point :x 3.0 :y 1.0)))
             (force-start start-pt)
             (force-end (2dg-point :x 3.0 :y 3.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (scxml---path-stretch points
                                                      force-start
                                                      force-end)
                                (scxml---path-cardinal force-start
                                                       force-end
                                                       (2dg-point :x 1.0 :y 0.0)
                                                       (2dg-point :x 1.0 :y 0.0)
                                                       0.5)))
      (let* ((start-pt (2dg-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (2dg-point :x 1.0 :y 3.0)))
             (force-start start-pt)
             (force-end (2dg-point :x 3.0 :y 3.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (scxml---path-stretch points
                                                        force-start
                                                        force-end)
                                (scxml---path-cardinal force-start
                                                       force-end
                                                       (2dg-point :x 0.0 :y 1.0)
                                                       (2dg-point :x 0.0 :y 1.0)
                                                       0.5))))

    ;; displacement injection stretches
    ;; TODO - more tests
    (progn
      (let* ((start-pt (2dg-point :x 0.0 :y 0.0))
             (end-pt (2dg-point :x 3.0 :y 0.0))
             (points (list start-pt
                           (2dg-point :x 1.0 :y 0.0)
                           (2dg-point :x 1.0 :y 1.0)
                           (2dg-point :x 2.0 :y 1.0)
                           (2dg-point :x 2.0 :y 0.0)
                           end-pt))
             (force-start start-pt)
             (force-end (2dg-point :x 3.0 :y 1.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (scxml---path-stretch points
                                                        force-start
                                                        force-end)
                                (list force-start
                                      (2dg-point :x 1.0 :y 0.0)
                                      (2dg-point :x 1.0 :y 1.5)
                                      (2dg-point :x 2.0 :y 1.5)
                                      (2dg-point :x 2.0 :y 1.0)
                                      force-end))))

    ))

(ert-deftest scxml-geometry-path-cardinal-direction ()
  (let ((start-pt (2dg-point :x 0.0 :y 0.0))
        (right-vec (2dg-point :x 1.0 :y 0.0))
        (up-vec (2dg-point :x 0.0 :y 1.0))
        (down-vec (2dg-point :x 0.0 :y -1.0)))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 1.0 :y 0.0))
             right-vec))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 1.0 :y 1.0))
             right-vec))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 1.0 :y -1.0))
             right-vec))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 0.0 :y 1.0))
             up-vec))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x 0.0 :y -1.0))
             down-vec))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x -1.0 :y 1.0))
             up-vec))
    (should (2dg-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (2dg-point :x -1.0 :y -1.0))
             down-vec))
    (let ((totally-backwards (scxml---path-cardinal-direction start-pt
                                                              right-vec
                                                              (2dg-point :x -1.0 :y 0.0))))
      (should (or (2dg-almost-equal totally-backwards
                                      up-vec)
                  (2dg-almost-equal totally-backwards
                                      down-vec))))))

(ert-deftest scxml-geometry-path-create-cardinal ()
  ;; ensure you're not allowed to create a cardinal path without actual cardinal data.
  (let ((invalid-cardinal-path (list (2dg-point- 0 0)
                                     (2dg-point- 1 0)
                                     (2dg-point- 2 1)))
        (found-expected-error nil))
    (condition-case nil
        (scxml-cardinal-path :points invalid-cardinal-path)
      (error (setq found-expected-error t)))
    (should found-expected-error)))

(ert-deftest scxml-geometry-path-build-straight-line ()
  (let ((start (2dg-point- 1 1))
        (end (2dg-point- 2 2))
        (cardinal-end (2dg-point- 2 1)))
    (should (scxml-cardinal-path-p
             (scxml-build-path-straight-line start cardinal-end)))
    (should (scxml-path-p
             (scxml-build-path-straight-line start end)))))

(provide 'scxml-geometry-path-test)
