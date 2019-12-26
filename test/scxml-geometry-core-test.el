;; core testing

(require 'ert)

(ert-deftest scxml-geometry-core-almost-equal ()

  ;; default threshold for almost-equal should be 1E-5
  (should (scxml-almost-equal 1 1.0))
  (should (scxml-almost-equal 0 0.000001))
  (should-not (scxml-almost-equal 1 2))
  (should-not (scxml-almost-equal -2 2))
  (should-not (scxml-almost-equal 1.01 2.01))

  ;; adjustable threshold
  (should (scxml-almost-equal 10 12 4))
  (should (scxml-almost-equal 10 12 3))
  (should-not (scxml-almost-equal 10 12 2))
  (should-not (scxml-almost-equal 10 12 1)))
