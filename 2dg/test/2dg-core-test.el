;; core testing

(require 'ert)
(require '2dg-core)

(ert-deftest 2dg-core-almost-equal ()

  ;; default threshold for almost-equal should be 1E-5
  (should (2dg-almost-equal 1 1.0))
  (should (2dg-almost-equal 0 0.000001))
  (should-not (2dg-almost-equal 1 2))
  (should-not (2dg-almost-equal -2 2))
  (should-not (2dg-almost-equal 1.01 2.01))

  ;; adjustable threshold
  (should (2dg-almost-equal 10 12 4))
  (should (2dg-almost-equal 10 12 3))
  (should-not (2dg-almost-equal 10 12 2))
  (should-not (2dg-almost-equal 10 12 1)))

(ert-deftest 2dg-core-reverse ()
  (should (eq (2dg-reverse 'up) 'down))
  (should (eq (2dg-reverse 'down) 'up))
  (should (eq (2dg-reverse 'left) 'right))
  (should (eq (2dg-reverse 'right) 'left))
  (should-error (2dg-reverse 'other))
  (should-error (2dg-reverse 12)))

(provide '2dg-core-test)
