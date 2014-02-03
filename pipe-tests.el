;;; pipe-tests.el -- tests for pipe stuff

(require 'pipe)

(ert-deftest pipe/thunk-bind ()
  (should
   (let ((lines '("line1" "line2" "line3"))
         collect)
     (-each
      lines (pipe/thunk-bind
             (lambda ()
               (-each (thuncall)
                      (lambda (item)
                        (push item collect))))))
     (equal collect (reverse lines)))))

(ert-deftest pipe/buffer-lines ()
  (should
   (with-temp-buffer
     (insert "a full line\r
another full line\r
a part of a line")
     (equal
      (list
       (pipe/buffer-lines (current-buffer) :line-ending "\r\n" :delete t)
       (buffer-string)
       (progn
         (insert " that's filled in\r\n")
         (pipe/buffer-lines (current-buffer) :line-ending "\r\n" :delete t))
       (pipe/buffer-lines (current-buffer) :line-ending "\r\n" :delete t))
      (list '("a full line" "another full line")
            "a part of a line"
            '("a part of a line that's filled in")
            nil)))))

;;; pipe-tests.el ends here
