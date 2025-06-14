;;; recspecs-tests.el --- tests for recspecs.el -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "recspecs.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest recspecs--expect-pos-curly ()
  (with-temp-buffer
    (insert "(expect (+ 1 2) { 3 })")
    (goto-char (point-max))
    (search-backward "3")
    (should (= (recspecs--expect-pos) (point)))))

(ert-deftest recspecs--expect-pos-string ()
  (with-temp-buffer
    (insert "(expect (display \"hi\") \"hi\")")
    (goto-char (point-max))
    (search-backward "\"hi\"")
    (should (= (recspecs--expect-pos) (point)))))

(ert-deftest recspecs--expect-pos-at-form ()
  (with-temp-buffer
    (insert "@expect (identity 1) {1}")
    (goto-char (point-max))
    (search-backward "1}")
    (should (= (recspecs--expect-pos) (point)))))

(ert-deftest recspecs--expect-pos-at-exp-newline ()
  (with-temp-buffer
    (insert "#lang at-exp racket/base\n\n(require recspecs)\n\n@expect[(print 400)]{\n 4000\n}\n")
    (goto-char (point-max))
    (search-backward "4000")
    (should (= (recspecs--expect-pos) (point)))))

(ert-deftest recspecs--expect-pos-no-form ()
  (with-temp-buffer
    (insert "(display 1)")
    (goto-char (point-max))
    (should-error (recspecs--expect-pos))))

(ert-deftest recspecs-update-at-point-calls-compile ()
  (let ((compile-cmd nil)
        (compile-env nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd)
                 (setq compile-cmd cmd
                       compile-env process-environment))))
      (with-temp-buffer
        (insert "(expect (display \"hi\") \"hi\")")
        (goto-char (point-max))
        (search-backward "\"hi\"")
        (let ((buffer-file-name "/tmp/test.rkt"))
          (recspecs-update-at-point))
        (should (string-match "raco test" compile-cmd))
        (should (member "RECSPECS_UPDATE=1" compile-env))
        (should (seq-some (lambda (x) (string-match "^RECSPECS_UPDATE_TEST=/tmp/test.rkt:" x))
                          compile-env))))))

(ert-deftest recspecs-update-at-point-at-exp-newline ()
  (let ((compile-cmd nil)
        (compile-env nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd)
                 (setq compile-cmd cmd
                       compile-env process-environment))))
      (with-temp-buffer
        (insert "#lang at-exp racket/base\n\n(require recspecs)\n\n@expect[(print 400)]{\n 4000\n}\n")
        (goto-char (point-max))
        (search-backward "4000")
        (let ((buffer-file-name "/tmp/test.rkt"))
          (recspecs-update-at-point))
        (should (string-match "raco test" compile-cmd))
        (should (member "RECSPECS_UPDATE=1" compile-env))
        (should (seq-some (lambda (x) (string-match "^RECSPECS_UPDATE_TEST=/tmp/test.rkt:" x))
                          compile-env))))))

(provide 'recspecs-tests)
;;; recspecs-tests.el ends here
