;; tests for scxml-geometry-point


(require 'ert)
(require 'scxml-geometry-point)

(ert-deftest scxml-geometry-point-factory ()
  (should (equal (scxml-point :x 1.0 :y 1.0)
                 (scxml-point- 1 1)))
  (should (equal (scxml-point :x 0.0 :y 1.2)
                 (scxml-point- 0 1.2))))

(ert-deftest scxml-geometry-point-distance ()
  (let ((A (scxml-point- 0 0))
        (B (scxml-point- 3 4)))
    (should (scxml-almost-equal (scxml-distance A B) 5))))

(ert-deftest scxml-geometry-point-vector-from-direction ()
  (should (equal (scxml-point- 1 0)
                 (scxml-vector-from-direction 'right)))
  (should (equal (scxml-point- -1 0)
                 (scxml-vector-from-direction 'left)))
  (should (equal (scxml-point- 0 1)
                 (scxml-vector-from-direction 'up))))

(ert-deftest scxml-geometry-point-cardinal-direction-vector ()
  (should (scxml-cardinal-direction-vector? (scxml-point- 1 0)))
  (should (scxml-cardinal-direction-vector? (scxml-point- 0 -2)))
  (should-not (scxml-cardinal-direction-vector? (scxml-point- 1 -2)))
  ;; For now, scxml-cardinal-direction-vector should consider zero
  ;; displacement as a cardinal-direction-vector
  (should (scxml-cardinal-direction-vector? (scxml-point- 0 0))))

(ert-deftest scxml-geometry-point-cardinal-displacement ()
  (should (scxml-cardinal-displacement? (scxml-point- 0 0)
                                        (scxml-point- 20 0)))
  (should (scxml-cardinal-displacement? (scxml-point- 0 -20)
                                        (scxml-point- 0 20))))
