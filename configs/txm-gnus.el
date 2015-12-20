;;
;; GNUS customizations
;;

(require 'gnus)
(require 'nnir)
(require 'smtpmail)
(require 'smtpmail-multi)
(load "txm-gnus-auto.el")
;;(setq user-full-name (user-full-name))

;; default sending method - using internal smtp client
;; with the smtpmail-multi package to handle multiple email
;; accounts
(setq message-send-mail-function 'smtpmail-multi-send-it)

;; and workaround for Gmail folders
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; verbose printing of SMTP issues
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verbose t)

;; set gnus-parameter
(setq gnus-parameters
  '(("nnimap.*"
     (gnus-use-scoring nil)
     (expiry-wait . 2)
     (display . all))))

;;[[http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus]]
(setq gnus-large-newsgroup 'nil)


;; organize groups by topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; sort threads
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

;; organize in threads
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; cache for offline reading
(setq gnus-use-cache t)


