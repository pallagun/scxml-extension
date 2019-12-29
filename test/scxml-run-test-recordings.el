(require 'ert)
(require 'scxml)
(require 'test-recorder)

(setq scxml-test-recordings-folder
      (format "%s/%s/"
              default-directory
              "test-recordings"))

(ert-deftest scxml-replay-0001-two-asserts ()
  (let ((file-name (format "%s/%s"
                           scxml-test-recordings-folder
                           "0001-two-asserts.scxml-recording")))
    (scxml-replay-test file-name t)))


(provide 'scxml-run-test-recordings)
