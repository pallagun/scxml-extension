;; tests for 2dg-point


(require 'ert)
(require '2dg-point)

(ert-deftest 2dg-point-factory ()
  (should (equal (2dg-point :x 1.0 :y 1.0)
                 (2dg-point- 1 1)))
  (should (equal (2dg-point :x 0.0 :y 1.2)
                 (2dg-point- 0 1.2))))

(ert-deftest 2dg-point-distance ()
  (let ((A (2dg-point- 0 0))
        (B (2dg-point- 3 4)))
    (should (2dg-almost-equal (2dg-distance A B) 5))))

(ert-deftest 2dg-point-vector-from-direction ()
  (should (equal (2dg-point- 1 0)
                 (2dg-vector-from-direction 'right)))
  (should (equal (2dg-point- -1 0)
                 (2dg-vector-from-direction 'left)))
  (should (equal (2dg-point- 0 1)
                 (2dg-vector-from-direction 'up))))

(ert-deftest 2dg-point-cardinal-direction-vector ()
  (should (2dg-cardinal-direction-vector-p (2dg-point- 1 0)))
  (should (2dg-cardinal-direction-vector-p (2dg-point- 0 -2)))
  (should-not (2dg-cardinal-direction-vector-p (2dg-point- 1 -2)))
  ;; For now, scxml-cardinal-direction-vector should consider zero
  ;; displacement as a cardinal-direction-vector
  (should (2dg-cardinal-direction-vector-p (2dg-point- 0 0))))

(ert-deftest 2dg-point-cardinal-displacement ()
  (should (2dg-cardinal-displacement-p (2dg-point- 0 0)
                                        (2dg-point- 20 0)))
  (should (2dg-cardinal-displacement-p (2dg-point- 0 -20)
                                        (2dg-point- 0 20))))

(provide '2dg-point-test)
