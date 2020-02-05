(require 'ert)
(require 'scxml)
(require 'test-recorder)

(defun scxml---resolve-recording (file-name)
  (scxml-resolve-file (format "test/test-recordings/%s" file-name)))

(ert-deftest scxml-replay-0001-many-basic-moves ()
  (scxml-replay-test
   (scxml---resolve-recording "0001-many-basic-moves.scxml-recording")
   t))

(ert-deftest scxml-replay-0002-zooming-and-panning ()
  (scxml-replay-test
   (scxml---resolve-recording "0002-zooming-and-panning.scxml-recording")
   t))

(ert-deftest scxml-replay-0003-building-two-states-with-initial-attribs ()
  (scxml-replay-test
   (scxml---resolve-recording "0003-building-two-states-with-initial-attribs")
   t))

(ert-deftest scxml-replay-0004-editing-synthetic-drawings ()
  (scxml-replay-test
   (scxml---resolve-recording "0004-editing-synthetic-drawings")
   t))

(ert-deftest scxml-replay-0005-parallel-add-children-simple ()
  (scxml-replay-test
   (scxml---resolve-recording "0005-parallel-add-children-simple")
   t))

(ert-deftest scxml-replay-0006-add-initial-to-hinted-state ()
  (scxml-replay-test
   (scxml---resolve-recording "0006-add-initial-to-hinted-state")
   t))

(ert-deftest scxml-replay-0007-setting-transition-targets-blank ()
  (scxml-replay-test
   (scxml---resolve-recording "0007-setting-transition-targets-blank")
   t))

(ert-deftest scxml-replay-0008-retargeting-hinted-transition ()
  (scxml-replay-test
   (scxml---resolve-recording "0008-retargeting-hinted-transition")
   t))



(provide 'scxml-run-test-recordings)
