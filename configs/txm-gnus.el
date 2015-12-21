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
  '(("^nnimap.*"
     (gnus-use-scoring nil)
     (expiry-wait . 2)
     (display . all))))

;; No group considered big, download everything
;; see http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus
;; for examples
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

;; Format of the group name presented.
;; see https://www.gnu.org/software/emacs/manual/html_mono/gnus.html#Group-Line-Specification
;; for details
(setf gnus-group-line-format "%S%p%P%5y:%B%(%g%)\n")
;; the header/topic line format

;; show all subscribed groups by default. The default behavior is to show
;; only those with unread messages
(add-hook 'gnus-group-mode-hook 'gnus-group-list-all-groups)

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
