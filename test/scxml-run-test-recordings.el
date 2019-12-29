(require 'ert)
(require 'scxml)
(require 'test-recorder)

(setq scxml-test-recordings-folder
      (format "%s/%s/"
              default-directory
              "test-recordings"))

(ert-deftest scxml-replay-0001-many-basic-moves ()
  (let ((file-name (format "%s/%s"
                           scxml-test-recordings-folder
                           "0001-many-basic-moves.scxml-recording")))
    (scxml-replay-test file-name t)))

(ert-deftest scxml-replay-0002-zooming-and-panning ()
  (let ((file-name (format "%s/%s"
                           scxml-test-recordings-folder
                           "0002-zooming-and-panning.scxml-recording")))
    (scxml-replay-test file-name t)))


(provide 'scxml-run-test-recordings)
