;;
;; GNUS customizations
;;

(require 'gnus)
(require 'nnir)
(require 'smtpmail)
(require 'smtpmail-multi)
(require 'gnus-art)
(require 'mailcap)


(load "txm-gnus-auto.el")
(load "txm-gnus-contacts.el")


;; initialize contacts support from txm-gnus-contacts.el
(txm-org-contacts-initialize)

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
  '(("^nnimap.*"
     (gnus-use-scoring nil)
     (expiry-wait . 2)
     (display . all))
    (".*/emacs-devel$"
      (to-address . "emacs-devel@gnu.org")
      (to-list . "emacs-devel@gnu.org"))
    (".*/bug-gnu-emacs$"
     (to-list . "bug-gnu-emacs@gnu.org"))
    (".*/emacs-orgmode$"
     (to-address . "emacs-orgmode@gnu.org")
     (to-list . "emacs-orgmode@gnu.org"))))

;; No group considered big, download everything
;; see http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus
;; for examples
;;(setq gnus-large-newsgroup 'nil)
(setq gnus-large-newsgroup 100)


;; cache for offline reading
(setq gnus-use-cache t)

;; do not use .newsrc file shared with other newsreaders 
(setq gnus-save-newsrc-file nil)

;; do not store/read new newsgroups. We will subscribe
;; manually
(setq gnus-save-killed-list nil
      gnus-check-new-newsgroups nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files and directories
;; General idea: move everything to .emacs.d/gnus-files

;; path to the file with newsgroups information, "~/.newsrc.eld"
(setq gnus-startup-file "~/.emacs.d/gnus-files/newsrc")

;; path to the directory where GNUS stores all the mail
(setq message-directory "~/.emacs.d/gnus-files/Mail")
;; path to the directory where GNUS stores all news related files
(setq gnus-directory "~/.emacs.d/gnus-files/News")
;; where to save articles
(setq gnus-article-save-directory gnus-directory)
;; articles cache
(setq gnus-cache-directory (concat (file-name-as-directory gnus-directory) "cache"))
;; the cache active file
(setq gnus-cache-active-file (concat (file-name-as-directory gnus-cache-directory) "active"))
;; directory where kill files are stored
(setq gnus-kill-files-directory gnus-directory)
;; name of nnfolder-directory
(setq nnfolder-directory (concat (file-name-as-directory message-directory) "archive"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups view
;; 
;; organize groups by topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Format of the group name presented.
;; see https://www.gnu.org/software/emacs/manual/html_mono/gnus.html#Group-Line-Specification
;; for details
;; The format is "group-name (number of unread messages)"
;; if number of unread messages is 0, do not show them
;; if the group-name is an inbox, take the account instead
;; i.e. if the group name is "nnimap+gmail-my.account:INBOX"
;; the printed name will be "gmail-my.account"
;; In order to achive this one have to implement custom formatting
;; function `gnus-user-format-function-group-line'
;; 
(setf gnus-group-line-format "%p%3P%*%u&group-line;\n")

(defun txm-gnus-news-group-is-imap-inbox (group)
  "Determine if GROUP is the IMAP inbox"
  (string-match "^nnimap\\+.*INBOX$" group))

(defun txm-gnus-account-name-from-group (group)
  "Extract account name from the GROUP"
 (string-match "\\(^.*\\)\\+\\(.*\\):\\(.*\\)" group)
 (let ((addr (match-string 2 group)))
   (if (null addr) "(unknown)" addr)))

(defun gnus-user-format-function-group-line (dummy)
  (let ((number-of-msgs-suffix
         ;; check if number of unread messages > 0
         (if (> (string-to-number gnus-tmp-number-of-unread) 0)
             (concat " (" gnus-tmp-number-of-unread ")")
           ;; otherwise do no show anything
           ""))
        ;; if it is an inbox, extract the account, since we want all
        ;; inboxes to be in one separate topic [Inbox]
        (group-name (if (txm-gnus-news-group-is-imap-inbox gnus-tmp-group)
                        (txm-gnus-account-name-from-group gnus-tmp-group)
                      gnus-tmp-qualified-group)))
    (concat group-name
            number-of-msgs-suffix)))

;; make all subscribed groups visible even if they don't have unread messages
(setq gnus-permanently-visible-groups ".*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Topics in Groups view
;; 

;; By default GNUS doesn't support topic faces.
;; Let's add them, following the hack described here:
;; http://www.emacswiki.org/emacs/GnusFormatting#toc6
(defface gnus-topic-face 
  '((t (:inherit default :weight bold)))
  "Face for GNUS topics.")


(defface gnus-topic-empty-face
  '((t (:inherit default)))
  "Face for GNUS emtpy topics.")

;; format line like "Name (3)" with proper indentation
;; for more details, see
;; http://www.gnu.org/software/emacs/manual/html_node/gnus/Topic-Variables.html#Topic-Variables
(setq gnus-topic-line-format "%i%u&topic-line; %v\n")

;; this corresponds to a topic line format of "%n (%a)"
(defun gnus-user-format-function-topic-line (dummy)
  ;; TODO: this works only knowing GNUS internals.
  ;; therefore it could be broken later
  (let* ((num-unread (gnus-topic-articles-in-topic entries))
         (topic-face (if (zerop num-unread)
                        'gnus-topic-empty-face
                      'gnus-topic-face)))
    (propertize
     (format "%s (%d)" name num-unread)
     'face topic-face)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management
;;

;; Prevent changing the Emacs window layout
(setq gnus-use-full-window nil)

;; Bind to F5 force refresh all mail
;;(gnus-activate-all-groups)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary buffer (list of messages)
;;

;; use exact address components extraction
(setq gnus-extract-address-components 'mail-extract-address-components)

;; Set the line format
;; the default one:
;;(setf gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
;; Explanation:
;; Unread, Secondary mark, Zcore(%z), indentation
;; (start highlighted text
;; [opening bracket
;; 4 chars for number of lines
;; ':' and 23 characters of the name, To field or Newgroups header
;; ]closing)end highlighted text
;; space and subject
;; 
(when nil
  (setq gnus-summary-line-format "%U%R%B %*%-40,40S %(%[%a%]%)  %o\n")
  (setq gnus-show-threads nil)
  (setq gnus-article-sort-functions '((not gnus-article-sort-by-date))))

;; turn on threading
(setq gnus-show-threads t)

;; organize in threads
;; the format and commands below are based on
;; http://www.emacswiki.org/emacs/GnusFormatting#toc4
;; with some modifications
;; 1) Fixed date/time size format
;; 2) date/time as in Thunderbird, today's messages with hh:mm and all others
;;    with dd/mm/yy
;; 3) Removed "Unread" flag, we use faces for it
(setq gnus-summary-line-format "%R %(%-15,15&user-date;%-20,20a %B%s%)\n")
(setq gnus-user-date-format-alist '(((gnus-seconds-today)
                                     . "%H:%M")
                                    (t . "%d/%m/%y %H:%M")))
;; organize threads by Reference header (subject not always works as it
;; groups sometimes all messages with the same subject as thread, which is
;; not correct for regular mails
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
;; sort by date descending (latest first)
(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))
;; set of variables specifying how to show thread characters
(setq gnus-sum-thread-tree-false-root "")
(setq gnus-sum-thread-tree-indent " ")
(setq gnus-sum-thread-tree-leaf-with-other "├► ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-single-leaf "╰► ")
(setq gnus-sum-thread-tree-vertical "│")

;; hook on after reading arcticle
(setq gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read))

;; when in windowed system use unicode characters to indicate
;; replied/forwarded mails
(when (window-system)
  (setq gnus-replied-mark 8592
        gnus-forwarded-mark 8594))

;; set GNUS to prefectch article asynchronously
(setq gnus-asynchronous t)

;; add article to cache- *, remove - M-*


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing message customizations
;;

;; create a signature with the first name from full name
(when (and (user-full-name)
           (stringp (user-full-name)))
  (let ((name (car (split-string (user-full-name) " "))))
    (add-to-list 'gnus-posting-styles `(".*" (signature ,(concat "Br,\n/" name))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some hotkeys redefinition
;;

;; use 'r' to reply to message with citing in both summary and article modes
(define-key gnus-summary-mode-map "r" 'gnus-summary-reply-with-original)
(define-key gnus-article-mode-map "r" 'gnus-article-reply-with-original)

;; use 'R' to reply to all with citing in both summary and article modes
(define-key gnus-summary-mode-map "R" 'gnus-summary-wide-reply-with-original)
(define-key gnus-article-mode-map "R" 'gnus-article-wide-reply-with-original)

;; forward mail with 'f'
(define-key gnus-summary-mode-map "f" 'gnus-summary-mail-forward)
;; use 'F' for followups with original message
(define-key gnus-summary-mode-map "F" 'gnus-summary-followup-with-original)


;; use g instead of Y g (gnus-summary-prepare) to refresh state of the summary buffer
;; to set read/unread etc
(define-key gnus-summary-mode-map "g" 'gnus-summary-prepare)

;; 'u' will mark the current mail as unread and move to the next line
(define-key gnus-summary-mode-map "u" '(lambda () (interactive) 
                                         (gnus-summary-clear-mark-forward 1)
                                         (let ((line (line-number-at-pos)))
                                               (gnus-summary-prepare)
                                               (goto-line line))))
                                         
;; Bind M-up/down to scroll the article
(define-key gnus-summary-mode-map [M-up] (lambda () (interactive) (gnus-summary-scroll-up -1)))
(define-key gnus-summary-mode-map [M-down] (lambda () (interactive) (gnus-summary-scroll-up 1)))

(defun txm-gnus-esc-dwim ()
  "Handle the ESC key in GNUS.
If any temporary windows opened, close them; otherwise close the article window."
  (interactive)
  (unless
      (txm-close-temporary-window)
    (gnus-summary-expand-window)))

;; Esc should close the article window in both arcticle and summary mode
(define-key gnus-summary-mode-map (if window-system (kbd "<escape>") "\M-q") 'txm-gnus-esc-dwim)
(define-key gnus-article-mode-map (if window-system (kbd "<escape>") "\M-q") 'txm-gnus-esc-dwim)
;; additional to it 'q' in article mode should close the article window
(define-key gnus-article-mode-map "q" 'gnus-summary-expand-window)

;; TAB should switch focus between article and summary
(define-key gnus-article-mode-map (kbd "TAB") 'gnus-article-show-summary)
(define-key gnus-summary-mode-map (kbd "TAB") 'gnus-summary-select-article-buffer)

;; Enter in summary mode should open article full window
(define-key gnus-summary-mode-map (kbd "\r") '(lambda () (interactive)
                                                (let ((gnus-widen-article-window t))
                                                  (gnus-summary-select-article-buffer))))

;; bind d to Delete message
(define-key gnus-summary-mode-map "d" 'gnus-summary-delete-article)

;; Attachement commands
;; bind Enter to save
(define-key gnus-mime-button-map "\r" 'gnus-mime-save-part)
(define-key gnus-mime-button-map " " 'gnus-mime-view-part-externally)

;; Use ctrl up/down to move between topics in group view
(define-key gnus-group-mode-map [C-up] 'gnus-topic-goto-previous-topic)
(define-key gnus-group-mode-map [C-down] 'gnus-topic-goto-next-topic)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some attachement types handling
;; (to avoid using mailcap)

;; handle attachements with default OSX previewers
(mailcap-add-mailcap-entry "application" "pdf" '((viewer "/usr/bin/qlmanage -p %s") (type . "application/pdf")))
(mailcap-add-mailcap-entry "image" "jpeg" '((viewer "/usr/bin/qlmanage -p %s") (type . "image/*")))

;; test with the following examples:
;; (cl-prettyprint
;;  (mailcap-possible-viewers (cdr (assoc "application" mailcap-mime-data)) "pdf"))
;; (mailcap-mime-info "image/jpeg")


