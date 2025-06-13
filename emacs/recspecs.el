;;; recspecs.el --- helpers for recspecs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides `recspecs-update-at-point` to update the expect test at point.
;; The command works with racket-mode and calls `racket-test` when
;; available.  It sets the environment variables `RECSPECS_UPDATE` and
;; `RECSPECS_UPDATE_TEST` based on the current buffer and the location of
;; the expectation string.  After the command finishes running the test,
;; the buffer is automatically reverted so any updated expectations are
;; loaded from disk.

;;; Code:

(require 'subr-x)

(defun recspecs--expect-pos ()
  "Return the character position of the expectation string at point.
Searches backward for an `expect` form and returns the position of its
expectation string.  Signal an error if none is found."
  (save-excursion
    (unless (re-search-backward
             (rx (or (seq "("
                         (or "expect" "expect-exn" "expect-file")
                         symbol-end)
                     (seq "@" (? "(")
                         (or "expect" "expect-exn" "expect-file")
                         symbol-end)))
             nil t)
      (error "No expect form found"))
    (let ((start (point)))
      (when (looking-at "@")
        (forward-char 1)
        (when (looking-at "(")
          (forward-char 1)))
      (when (looking-at "(")
        (forward-char 1)) ;; skip opening paren if present
      (forward-symbol 1) ;; skip expect / expect-exn / expect-file
      (skip-chars-forward "\s-")
      (forward-sexp 1) ;; expression or path
      (skip-chars-forward "\s-")
      (if (looking-at "{")
          (progn
            (forward-char 1)
            (skip-chars-forward "\s-")
            (if (looking-at "}")
                start
              (point)))
        (point)))))

;;;###autoload
(defun recspecs-update-at-point ()
  "Run `racket-test` with update flags for the expectation at point."
  (interactive)
  (let* ((file (buffer-file-name))
         (pos (recspecs--expect-pos))
         (env (list "RECSPECS_UPDATE=1"
                    (format "RECSPECS_UPDATE_TEST=%s:%d" file pos)))
         (process-environment (append env process-environment))
         (target (current-buffer))
         hook)
    (setq hook (lambda (_buf _msg)
                 (with-current-buffer target
                   (revert-buffer t t))
                 (remove-hook 'compilation-finish-functions hook)))
    (add-hook 'compilation-finish-functions hook)
    (cond
     (t (compile (format "raco test %s" (shell-quote-argument file)))))))

(provide 'recspecs)

;;; recspecs.el ends here
