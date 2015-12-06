;;
;; GNUS customizations
;;

(require 'gnus)
(require 'nnir)
(setq gnus-select-method `(nntp ,(rot13-string "arjf.vaqvivqhny.arg")))
(setq user-full-name (rot13-string "Nyrkrl Irergraavxbi"))
(setq user-mail-address (rot13-string "nyrkrl.irergraavxbi@tznvy.pbz"))

;; mail servers configuration
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "freedommail"
                      (nnimap-address "imap.kolabnow.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)))

(add-to-list 'gnus-secondary-select-methods
             `(nnimap ,(rot13-string "gkz.sbhevre")
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)))


;; default sending method - using internal smtp client
(setq message-send-mail-function 'smtpmail-send-it)

;; make default port for smtp 
(setq smtpmail-smtp-service 587)

;;and workaround for Gmail folders
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; set the reply from the mail address addressed in the incoming mail
(setq gnus-posting-styles
      `(((header "to" ,(rot13-string "sbhevre@serrqbzznvy.pu"))
         (address ,(rot13-string "sbhevre@serrqbzznvy.pu")))
        ((header "to" ,(rot13-string "gkz.sbhevre@tznvy.pbz"))
         (address ,(rot13-string "gkz.sbhevre@tznvy.pbz")))
        ((header "cc" ,(rot13-string "sbhevre@serrqbzznvy.pu"))
         (address ,(rot13-string "sbhevre@serrqbzznvy.pu")))
        ((header "cc" ,(rot13-string "gkz.sbhevre@tznvy.pbz"))
         (address ,(rot13-string "gkz.sbhevre@tznvy.pbz")))))

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


