(require 'ert)
(require 'scxml-interpreter)


(ert-deftest scxml-interpreter-smoke-test ()
  "basics"
  (let ((machine (scxml-scxml :name "anything" :initial "one"))
        (state-1 (scxml-state :id "one"))
        (state-2 (scxml-state :id "two")))
    (scxml-add-children machine state-1 state-2)
    (scxml-add-children state-1 (scxml-transition :target "two" :events '("right")))
    (scxml-add-children state-2 (scxml-transition :target "one" :events '("left")))
    (let ((instance (scxml-build-instance machine)))
      instance)))


(defvar test nil)
(progn
  (setq test
        (let ((machine (scxml-scxml :name "anything" :initial "left"))
              (left (scxml-state :id "left"))
              (right (scxml-state :id "right")))
          (scxml-add-children machine left right)
          (scxml-add-children left (scxml-transition :target "right" :events '("go-right")))
          (scxml-add-children right (scxml-transition :target "left" :events '("go-left")))
          (let ((instance (scxml-build-instance machine)))
            instance)))
  (scxml-run-instance test t))

(scxml-print test)
(scxml-enqueue-event test (scxml-event :name "go-right"))
(scxml-enqueue-event test (scxml-event :name "go-left"))
(scxml-continue test t)
