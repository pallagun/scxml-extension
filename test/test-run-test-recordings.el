(require 'ert)

(setq scxml-test-recordings-folder
      "/home/gator/github/pallagun/scxml-extension/test/test-recordings/")

(ert-deftest scxml-replay-0001-two-asserts ()
  (let ((file-name (format "%s/%s"
                           scxml-test-recordings-folder
                           "0001-two-asserts.scxml-recording")))
    (scxml-replay-test file-name t)))
