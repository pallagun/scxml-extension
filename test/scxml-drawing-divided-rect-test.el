(require 'ert)

(require 'scxml-drawing-divided-rect)


(ert-deftest scxml-drawing-divided-rect-get-divisions ()
  (cl-flet ((division-equal
             (A B)
             (and
              ;; cell coordinates should be the same
              (equal (car A) (car B))
              ;; cell rectangles need to be almost equal
              (2dg-almost-equal (cdr A) (cdr B)))))
    (let* ((stripe (scxml--set-layout (scxml---nest-stripe) 2 2))
           (rect (scxml-rect :x-min 0.0 :y-min 0.0 :x-max 10.0 :y-max 10.0))
           (divisions (scxml---get-divisions stripe rect)))
      (let ((expected-BL (cons '(0 0) (scxml-rect :x-min 0.0
                                                  :x-max 5.0
                                                  :y-min 0.0
                                                  :y-max 5.0)))
            (expected-BR (cons '(0 1) (scxml-rect :x-min 5.0
                                                  :x-max 10.0
                                                  :y-min 0.0
                                                :y-max 5.0))))
        (should (member-if (lambda (cell)
                             (division-equal cell expected-BL))
                           divisions))
        (should (member-if (lambda (cell)
                             (division-equal cell expected-BR))
                           divisions))))
    ))

(provide 'scxml-drawing-divided-rect-test)
