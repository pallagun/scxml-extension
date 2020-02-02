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


(provide 'scxml-run-test-recordings)
