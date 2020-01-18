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

(setq test
  (let ((machine (scxml-scxml :name "anything" :initial "one"))
        (state-1 (scxml-state :id "one"))
        (state-2 (scxml-state :id "two")))
    (scxml-add-children machine state-1 state-2)
    (scxml-add-children state-1 (scxml-transition :target "two" :events '("right")))
    (scxml-add-children state-2 (scxml-transition :target "one" :events '("left")))
    (let ((instance (scxml-build-instance machine)))
      instance)))

(princ test)

(scxml-xml-string (oref test _type))
