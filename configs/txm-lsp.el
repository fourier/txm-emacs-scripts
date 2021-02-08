;;; -*- lexical-binding: t -*-


(require 'lsp-mode)
(require 'lsp-ui)
(when (eq system-type 'windows) 
  (setq lsp-clients-clangd-executable "c:/Program Files/LLVM/bin/clangd.exe"))
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

;; performance tweaks from https://github.com/emacs-lsp/lsp-mode#performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-idle-delay 0.500)
;; use the eldoc
(setq lsp-eldoc-render-all nil)
;;(setq lsp-semantic-highlighting :immediate)
(setq lsp-semantic-highlighting nil)
;; disable tooltips
(setq lsp-ui-doc-enable nil)

(defmacro with-demoted-errors (format &rest body)
  "Run BODY and demote any errors to simple messages.
FORMAT is a string passed to `message' to format any error message.
It should contain a single %-sequence; e.g., \"Error: %S\".

If `debug-on-error' is non-nil, run BODY without catching its errors.
This is to be used around code which is not expected to signal an error
but which should be robust in the unexpected case that an error is signaled.

For backward compatibility, if FORMAT is not a constant string, it
is assumed to be part of BODY, in which case the message format
used is \"Error: %S\"."
  (declare (debug t) (indent 1))
  (let ((err (make-symbol "err"))
        (format (if (and (stringp format) body) format
                  (prog1 "Error: %S"
                    (if format (push format body))))))
    `(condition-case-unless-debug ,err
         ,(macroexp-progn body)
       (ignore (message ,format ,err) nil))))

(defun lsp-warn (message &rest args)
  "Display a warning message made from (`format-message' MESSAGE ARGS...).
This is equivalent to `display-warning', using `lsp-mode' as the type and
`:warning' as the level."
  (ignore))


(provide 'txm-lsp)
