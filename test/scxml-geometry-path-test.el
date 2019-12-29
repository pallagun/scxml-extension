(require 'ert)
(require 'scxml-geometry-path)

(ert-deftest scxml-geometry-path-is-cardinal-path? ()
  ;; Needs to say that a path with a single point is indeed cardinal
  (let ((paths (list
                (scxml-path :points (list (scxml-point- 0 20)))
                (scxml-path :points (list (scxml-point- 0 0)
                                          (scxml-point- 1 0)))
                (scxml-path :points (list (scxml-point- 0 0)
                                          (scxml-point- 1 0)
                                          (scxml-point- 1 1)))
                (scxml-path :points (list (scxml-point- 30.5 22.0)
                                          (scxml-point- 30.500000000001 17.0))))))
    (mapcar (lambda (path)
              (should (scxml---is-cardinal-path? (scxml-points path))))
            paths)))

(ert-deftest scxml-geometry-path-cardinal ()
  (let ((right-vec (scxml-point- 1 0))
        (up-vec (scxml-point- 0 1))
        (down-vec (scxml-point- 0 -1))
        (left-vec (scxml-point- -1 0))
        (min-dist 1.0))
    (cl-flet ((should-be-almost-equal
               (A B)
               (should
                (scxml-almost-equal (scxml-path :points A) (scxml-path :points B)))))

      ;; weird one I ran into.
      (let* ((start (scxml-point- 2.25 36.75))
             (end (scxml-point- 13.0 37.0))
             (entry-vector (scxml-point- 1.0 -0.0))
             (exit-vector (scxml-point- -1.0 0.0))
             (min-segment-distance 1.0)
             (expected-path (list start
                                  (scxml-point- 13.0 36.75)
                                  end)))
        (should (eq 3 (length (scxml---path-cardinal start end entry-vector exit-vector min-segment-distance))))
        (should-be-almost-equal (scxml---path-cardinal start end entry-vector exit-vector min-segment-distance)
                                expected-path))

      ;; simple 90 degree
      (let ((start (scxml-point- 0 0))
            (end (scxml-point- 5 5)))
        (let ((bottom-right-path (list start (scxml-point- 5 0) end)))
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
        (let ((top-left-path (list start (scxml-point- 0 5) end)))
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
      (let* ((start (scxml-point- 0 0))
             (end (scxml-point- 5 0))
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
      (let* ((start (scxml-point- 0 0))
             (end (scxml-point- 5 0))
             (expected-path (list start
                                  (scxml-point- 0 min-dist)
                                  (scxml-point- 5 min-dist)
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
      (let* ((start (scxml-point- 0 0))
             (end (scxml-point- 5 5))
             (expected-path (list start
                                  (scxml-point- 2.5 0.0)
                                  (scxml-point- 2.5 5.0)
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
      (let* ((start (scxml-point- 7 5))
             (end (scxml-point- 8 8))
             (expected-path (list start
                                  (scxml-point- 7.0 6.5)
                                  (scxml-point- 8.0 6.5)
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
              (scxml-almost-equal (scxml-path :points A) (scxml-path :points B)))))
    (let ((A (list (scxml-point :x 0.0 :y 0.0)
                   (scxml-point :x 1.0 :y 0.0)
                   (scxml-point :x 2.0 :y 0.0)))
          (B (list (scxml-point :x 2.0 :y 0.0)
                   (scxml-point :x 2.0 :y 1.0)
                   (scxml-point :x 2.0 :y 2.0))))
      (should-be-almost-equal (scxml---path-append-simplify A B)
                              (list (scxml-point :x 0.0 :y 0.0)
                                    (scxml-point :x 2.0 :y 0.0)
                                    (scxml-point :x 2.0 :y 2.0))))))

(ert-deftest scxml-geometry-path-nudge-path ()
  (let ((path-pts (list (scxml-point :x 0.0 :y 0.0)
                        (scxml-point :x 1.0 :y 0.0)
                        (scxml-point :x 1.0 :y 0.0)
                        (scxml-point :x 2.0 :y 0.0))))
    (should (scxml-almost-equal
             (scxml-path :points (scxml-nudge-path path-pts
                                                     3
                                                     (scxml-point :x 0.0 :y 0.4)))
             (scxml-path :points (list (scxml-point :x 0.0 :y 0.0)
                                       (scxml-point :x 1.0 :y 0.0)
                                       (scxml-point :x 1.0 :y 0.4)
                                       (scxml-point :x 2.0 :y 0.4))))))
  (let ((path-pts (list (scxml-point :x 0.0 :y 0.0)
                        (scxml-point :x 1.0 :y 0.0)
                        (scxml-point :x 1.0 :y 1.0)
                        (scxml-point :x 2.0 :y 1.0)
                        (scxml-point :x 2.0 :y 2.0))))
    (should (scxml-almost-equal
             (scxml-path :points (scxml-nudge-path path-pts
                                                     2
                                                     (scxml-point :x 0.2 :y 0.2)))
             (scxml-path :points (list (scxml-point :x 0.0 :y 0.0)
                                       (scxml-point :x 1.2 :y 0.0)
                                       (scxml-point :x 1.2 :y 1.2)
                                       (scxml-point :x 2.0 :y 1.2)
                                       (scxml-point :x 2.0 :y 2.0)))))))

(ert-deftest scxml-geometryy-pathstretch ()
  (cl-flet ((should-be-almost-equal
             (A B)
             (should
              (scxml-almost-equal (scxml-path :points A) (scxml-path :points B)))))
    ;; normal stretches that don't need any special cases
    (progn
      (let* ((start-pt (scxml-point :x 0.0 :y 0.0))
             (points (list start-pt
                           (scxml-point :x 5.0 :y 0.0)
                           (scxml-point :x 5.0 :y 5.0))))
        (should-be-almost-equal (scxml---path-stretch points
                                                        start-pt
                                                        (scxml-point :x 10.0 :y 10.0))
                                (list start-pt
                                      (scxml-point :x 10.0 :y 0.0)
                                      (scxml-point :x 10.0 :y 10.0))))
      (let* ((start-pt (scxml-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (scxml-point :x 6.0 :y 1.0)
                           (scxml-point :x 6.0 :y 6.0))))
        (should-be-almost-equal (scxml---path-stretch points
                                                        (scxml-point :x 0.0 :y 0.0)
                                                        (scxml-point :x 10.0 :y 10.0))
                                (list (scxml-point :x 0.0 :y 0.0)
                                      (scxml-point :x 10.0 :y 0.0)
                                      (scxml-point :x 10.0 :y 10.0)))))

    ;; here are two impossible stretches
    (progn
      (let* ((start-pt (scxml-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (scxml-point :x 3.0 :y 1.0)))
             (force-start start-pt)
             (force-end (scxml-point :x 3.0 :y 3.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (scxml---path-stretch points
                                                      force-start
                                                      force-end)
                                (scxml---path-cardinal force-start
                                                       force-end
                                                       (scxml-point :x 1.0 :y 0.0)
                                                       (scxml-point :x 1.0 :y 0.0)
                                                       0.5)))
      (let* ((start-pt (scxml-point :x 1.0 :y 1.0))
             (points (list start-pt
                           (scxml-point :x 1.0 :y 3.0)))
             (force-start start-pt)
             (force-end (scxml-point :x 3.0 :y 3.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (scxml---path-stretch points
                                                        force-start
                                                        force-end)
                                (scxml---path-cardinal force-start
                                                       force-end
                                                       (scxml-point :x 0.0 :y 1.0)
                                                       (scxml-point :x 0.0 :y 1.0)
                                                       0.5))))

    ;; displacement injection stretches
    ;; TODO - more tests
    (progn
      (let* ((start-pt (scxml-point :x 0.0 :y 0.0))
             (end-pt (scxml-point :x 3.0 :y 0.0))
             (points (list start-pt
                           (scxml-point :x 1.0 :y 0.0)
                           (scxml-point :x 1.0 :y 1.0)
                           (scxml-point :x 2.0 :y 1.0)
                           (scxml-point :x 2.0 :y 0.0)
                           end-pt))
             (force-start start-pt)
             (force-end (scxml-point :x 3.0 :y 1.0)))
        ;; this stretch is not possible, it should default to path construction.
        (should-be-almost-equal (scxml---path-stretch points
                                                        force-start
                                                        force-end)
                                (list force-start
                                      (scxml-point :x 1.0 :y 0.0)
                                      (scxml-point :x 1.0 :y 1.5)
                                      (scxml-point :x 2.0 :y 1.5)
                                      (scxml-point :x 2.0 :y 1.0)
                                      force-end))))

    ))

(ert-deftest scxml-geometry-path-cardinal-direction ()
  (let ((start-pt (scxml-point :x 0.0 :y 0.0))
        (right-vec (scxml-point :x 1.0 :y 0.0))
        (up-vec (scxml-point :x 0.0 :y 1.0))
        (down-vec (scxml-point :x 0.0 :y -1.0)))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x 1.0 :y 0.0))
             right-vec))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x 1.0 :y 1.0))
             right-vec))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x 1.0 :y -1.0))
             right-vec))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x 0.0 :y 1.0))
             up-vec))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x 0.0 :y -1.0))
             down-vec))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x -1.0 :y 1.0))
             up-vec))
    (should (scxml-almost-equal
             (scxml---path-cardinal-direction start-pt
                                              right-vec
                                              (scxml-point :x -1.0 :y -1.0))
             down-vec))
    (let ((totally-backwards (scxml---path-cardinal-direction start-pt
                                                              right-vec
                                                              (scxml-point :x -1.0 :y 0.0))))
      (should (or (scxml-almost-equal totally-backwards
                                      up-vec)
                  (scxml-almost-equal totally-backwards
                                      down-vec))))))

(ert-deftest scxml-geometry-path-create-cardinal ()
  ;; ensure you're not allowed to create a cardinal path without actual cardinal data.
  (let ((invalid-cardinal-path (list (scxml-point- 0 0)
                                     (scxml-point- 1 0)
                                     (scxml-point- 2 1)))
        (found-expected-error nil))
    (condition-case nil
        (scxml-cardinal-path :points invalid-cardinal-path)
      (error (setq found-expected-error t)))
    (should found-expected-error)))

(provide 'scxml-geometry-path-test)
