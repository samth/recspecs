;;; recspecs.el --- helpers for recspecs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides `recspecs-update-at-point` to update the expect test at point.
;; The command works with racket-mode and calls `racket-test` when
;; available.  It sets the environment variables `RECSPECS_UPDATE` and
;; `RECSPECS_UPDATE_TEST` based on the current buffer and the location of
;; the expectation string.

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
    (point)))

;;;###autoload
(defun recspecs-update-at-point ()
  "Run `racket-test` with update flags for the expectation at point."
  (interactive)
  (let* ((file (buffer-file-name))
         (pos (recspecs--expect-pos))
         (env (list "RECSPECS_UPDATE=1"
                    (format "RECSPECS_UPDATE_TEST=%s:%d" file pos)))
         (process-environment (append env process-environment)))
    (cond
     ((fboundp 'racket-test) (racket-test))
     ((fboundp 'racket-run) (racket-run))
     (t (compile (format "raco test %s" (shell-quote-argument file))))))

(provide 'recspecs)

;;; recspecs.el ends here
